
library(nnet)

# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This file contains functions to estimate descriptive and counterfactual disparities

# Descriptive disparity estimator
disparity_estimator <- function(weight_name) {
  # Estimate the overall prevalence in the population
  prevalence <- full_population %>%
    rename_at(.vars = vars(weight_name), .funs = function(x) "weight") %>%
    filter(!is.na(weight)) %>%
    group_by(RACE) %>%
    summarize(estimate = weighted.mean(y, w = weight)) %>%
    mutate(estimand = "prevalence")
  # Estimate the disparity in onset among the employed
  onset <- d_onset %>%
    rename_at(.vars = vars(weight_name), .funs = function(x) "weight") %>%
    filter(!is.na(weight)) %>%
    group_by(RACE) %>%
    summarize(estimate = weighted.mean(y, w = weight)) %>%
    mutate(estimand = "onset")
  prevalence %>%
    bind_rows(onset)
}

# Counterfactual disparity estimator function
counterfactual_estimator <- function(
  weight_name = "ASECWT",
  data = d,
  outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                              s(OCC2010, bs = "re") + 
                              SEX + EDUC + foreign_born + s(AGE) + s(YEAR, k = 8) + questionnaire_redesign +
                              factor(HEALTH)),
  treatment_formula = NULL,
  save_intermediate = F,
  interactions = "with_race"
) {
  data <- data %>%
    rename_at(.vars = vars(weight_name), .funs = function(x) "weight") %>%
    # Now that we have renamed, remove all the replicate weights to drop memory
    select(-starts_with("REPWTP")) %>%
    # For the GAM, we want the weight normalized to sum to the number of observations
    # per the MGCV documentation.
    group_by() %>%
    mutate(weight = weight / mean(weight))
  
  # Estimate the race/ethnicity distribution in each occupation
  fitted_category_proportions <- foreach(race_case = c("Hispanic","Non-Hispanic Black","Other"), .combine = "cbind") %do% {
    fit <- bam(as.numeric(RACE == race_case) ~ s(OCC2010, bs = "re"),
               data = data,
               weights = weight)
    if (weight_name == "ASECWT" & save_intermediate) {
      save(fit, file = paste0("intermediate/prop_",gsub("-| ","",race_case),"_fit.Rdata"))
    }
    return(data.frame(prop = fitted(fit)) %>%
             rename_all(.funs = function(x) paste0("prop_",gsub(" |-","",race_case))))
  }
  # Append those to the dataset
  data <- data %>%
    bind_cols(fitted_category_proportions) %>%
    mutate(RACE = factor(RACE)) %>%
    # Add the major occupational group
    mutate(major = substr(as.character(OCC2010),1,1))
  
  # If a treatment model is called for, fit the treatment model within categories of race
  if (!is.null(treatment_formula)) {
    # Create a weight for the treatment models that only weights
    # occupations that collectively comprise 95% of the population
    zeroed_occupations <- data %>%
      group_by(OCC2010) %>%
      summarize(prop = sum(weight)) %>%
      group_by() %>%
      mutate(prop = prop / sum(prop)) %>%
      arrange(-prop) %>%
      mutate(cum_prop = cumsum(prop)) %>%
      mutate(zeroed = cum_prop > .9) %>%
      select(OCC2010, zeroed)
    print("Total number of occupations in most analyses")
    print(nrow(zeroed_occupations))
    print("Occupations comprising 90% of the population")
    print(sum(!zeroed_occupations$zeroed))
    data <- data %>%
      left_join(zeroed_occupations, by = "OCC2010")
    major_treatment_formula <- as.character(treatment_formula)
    major_treatment_formula[2] <- "major"
    major_treatment_formula <- formula(paste0(major_treatment_formula[2],"~",major_treatment_formula[3]))
    t0_m_fit <- Sys.time()
    fitted_m <- foreach (r = unique(data$RACE), .combine = "rbind") %do% {
      print(paste("Starting m_hat fit for",r))
      # Restrict to those within a race category
      this_case <- data %>% 
        filter(RACE == r) %>%
        # Make occupation a character for the multinom call
        mutate(OCC2010 = as.character(OCC2010)) %>%
        filter(!zeroed)
      fit_major <- multinom(major_treatment_formula,
                            data = this_case,
                            maxit = 1000,
                            weights = weight)
      fitted_this_case <- foreach(major_value = unique(data$major), .combine = "rbind") %do% {
        print(paste("Starting m_hat fit for",r,"major category",major_value))
        this_major <- this_case %>% 
          filter(major == major_value)
        fit_minor <- multinom(treatment_formula,
                              data = this_major,
                              maxit = 1000,
                              weights = weight)
        # Convert those to fitted values
        fitted_major <- predict(fit_major, newdata = this_major, type = "prob")
        fitted_minor <- predict(fit_minor, type = "prob")
        fitted_this_major <- rep(NA,nrow(this_major))
        for (i in 1:length(fitted_this_major)) {
          mhat_major <- fitted_major[i,which(colnames(fitted_major) == this_major$major[i])]
          mhat_minor <- fitted_minor[i,which(colnames(fitted_minor) == this_major$OCC2010[i])]
          fitted_this_major[i] <- mhat_major * mhat_minor
        }
        return(this_major %>%
                 group_by() %>%
                 select(CPSIDP) %>%
                 mutate(m_hat = fitted_this_major))
      }
      return(fitted_this_case)
    }
    save(fitted_m, file = "intermediate/fitted_m.Rdata")
    print("Spent on fitting m:")
    print(difftime(Sys.time(),t0_m_fit))
    
    # Merge in the fitted m and get the pi_hat and counterfactual weight
    data <- data %>%
      left_join(fitted_m, by = "CPSIDP") %>%
      # Get the counterfactual assignment rule for the observed occupation: Prevalence within education
      group_by(EDUC, OCC2010) %>%
      mutate(pi_hat = sum(weight * !zeroed)) %>%
      group_by(EDUC) %>%
      mutate(pi_hat = pi_hat / sum(pi_hat)) %>%
      group_by() %>%
      mutate(counterfactual_weight = ifelse(pi_hat > 0, weight * pi_hat / m_hat, 0))
    
    save(data, file = "intermediate/data_with_counterfactual_weight.Rdata")
  }

  # If no interactions, fit one outcome model and one treatment model that we will reference several times
  if (interactions == "none") {
    fit <- bam(outcome_formula,
               weights = weight,
               data = data)
    if (weight_name == "ASECWT" & save_intermediate) {
      save(fit, file = "intermediate/outcome_pooled_fit.Rdata")
    }
  }
  
  # Get the counterfactual predictions by looping over race categories
  counterfactual_predictions <- foreach(r = unique(data$RACE), .combine = "rbind") %do% {
    # Restrict to those within a race category
    this_case <- data %>% filter(RACE == r)
    
    # Fit an outcome model within race if we are estimating models that way
    if (interactions == "with_race") {
      fit <- bam(outcome_formula,
                 weights = weight,
                 data = this_case)
      if (weight_name == "ASECWT" & save_intermediate) {
        save(fit, file = paste0("intermediate/outcome_",gsub("-| ","",r),"_fit.Rdata"))
      }
    }
    
    # If a treatment model was specified, get the bias correction
    if (!is.null(treatment_formula)) {
      bias_correction <- weighted.mean(fitted(fit) - this_case$y, w = this_case$counterfactual_weight)
    }
    
    # Extract the coefficients on the linear basis
    beta <- coef(fit)
    
    # Predict the model matrix for everyone
    X_all <- predict(fit, type = "lpmatrix", newdata = data)
    
    # Predict the model matrix for this case
    X <- predict(fit, type = "lpmatrix", newdata = this_case)
    
    # Initialize matrices to hold the predictors after equalization
    X_equalized_within_educ <- X_equalized_marginal <- X
    
    # Determine which rows correspond to each educational category
    educ_values <- unique(data$EDUC)
    educ_rows_data <- lapply(educ_values, function(val) which(data$EDUC == val))
    educ_rows_this_case <- lapply(educ_values, function(val) which(this_case$EDUC == val))
    names(educ_rows_data) <- names(educ_rows_this_case) <- educ_values
    
    # Determine the variables to be equalized
    equalized <- which(grepl("prop_|OCC",colnames(X_all)))
    
    # Carry out marginal equalization
    # We only do this if this is not a doubly-robust run.
    if (is.null(treatment_formula)) {
      # Replace the equalized columns
      X_equalized_marginal[,equalized] <- apply(
        X_all[,equalized], 
        # With the weighted average of that column in X_all
        2, 
        # over all cases
        function(x) rep(weighted.mean(x, w = data$weight), 
                        # Repeated as many rows as in this_case
                        nrow(this_case))
      )
    }
    
    # Carry out equalization within educational categories
    for (e in educ_values) {
      # Determine which rows of this case have this education category
      rows_this_case <- educ_rows_this_case[[e]]
      # Determine which rows of everyone have this education category
      rows_data <- educ_rows_data[[e]]
      # Carry out marginal equalization
      # Replace the equalized columns
      X_equalized_within_educ[rows_this_case,equalized] <- apply(
        # With the weighted average of that column in X_all in this education category
        X_all[rows_data,equalized],
        2,
        # Take the weighted average over all cases in this education category
        function(x) rep(weighted.mean(x, w = data$weight[rows_data]),
                        # Repeat for as many rows as are in this education category of this case   
                        length(rows_this_case))
      )
    }
    
    # For comparison to the gap-closing estimand, predict the outcomes for people in this category
    # compared with the outcomes of observationally-similar non-Hispanic white people
    # We only do this if this is not a doubly-robust run.
    
    if (is.null(treatment_formula)) {
      if (r == "Non-Hispanic White") {
        conditional_disparity <- rep(0,nrow(this_case))
      } else if (interactions == "with_race") {
        # Load the white fit for comparison
        fit_this_case <- fit
        load("intermediate/outcome_NonHispanicWhite_fit.Rdata")
        conditional_disparity <- predict(fit_this_case, newdata = this_case) -
          predict(fit, newdata = this_case)
      } else if (interactions == "none") {
        conditional_disparity <- rep(
          ifelse(r == "Hispanic", 0 - coef(fit)["RACENon-Hispanic White"],
                 coef(fit)[paste0("RACE",r)] - coef(fit)["RACENon-Hispanic White"]),
          nrow(this_case)
        )
      }
    }
    
    # Predict the predicted counterfactual outcome by each equalization rule
    if (is.null(treatment_formula)) {
      counterfactual_predictions_this_case <- this_case %>%
        select(CPSIDP,RACE,y,weight) %>%
        mutate(counterfactual_marginal = as.vector(X_equalized_marginal %*% beta),
               counterfactual_within_educ = as.vector(X_equalized_within_educ %*% beta),
               not_equalized = as.vector(X %*% beta),
               conditional_vs_white = conditional_disparity)
    } else {
      counterfactual_predictions_this_case <- this_case %>%
        select(CPSIDP,RACE,y,weight) %>%
        mutate(counterfactual_marginal = NA,
               counterfactual_within_educ = as.vector(X_equalized_within_educ %*% beta) - bias_correction,
               not_equalized = NA,
               conditional_vs_white = NA)
    }
    
    return(counterfactual_predictions_this_case)
  }
  
  # Save those for all replicates in case you later want to produce
  # an estimate for a subgroup
  if (save_intermediate) {
    save(counterfactual_predictions,file = "intermediate/counterfactual_predictions_race_interactions.Rdata")
  }
  
  # Aggregate to counterfactual means in each race/ethnicity category
  aggregated_predictions <- counterfactual_predictions %>%
    group_by(RACE) %>%
    summarize(counterfactual_marginal = weighted.mean(counterfactual_marginal, w = weight),
              counterfactual_within_educ = weighted.mean(counterfactual_within_educ, w = weight),
              factual = weighted.mean(y, w = weight),
              conditional_vs_white = weighted.mean(conditional_vs_white, w = weight)) %>%
    melt(id = "RACE", variable.name = "estimand", value.name = "estimate")
  rm(counterfactual_predictions)
  return(aggregated_predictions)
}

# Function to aggregate point and replicate estimates into a point estimate and a confidence interval
estimate_from_point_reps <- function(point, reps) {
  estimates_with_disparities <- reps %>%
    rename(estimate_star = estimate) %>%
    left_join(point,
              by = c("RACE","estimand")) %>%
    mutate(target = ifelse(estimand == "conditional_vs_white", "disparity_vs_white","proportion")) %>%
    # Add the disparity vs. white
    bind_rows(reps %>%
                filter(estimand != "conditional_vs_white") %>%
                rename(estimate_star = estimate) %>%
                left_join(point,
                          by = c("RACE","estimand")) %>%
                group_by() %>%
                melt(id = c("RACE","replicate","estimand"), variable.name = "quantity") %>%
                group_by(replicate,estimand,quantity) %>%
                mutate(reference = mean(case_when(RACE == "Non-Hispanic White" ~ value), na.rm = T)) %>%
                filter(RACE != "Non-Hispanic White") %>%
                group_by() %>%
                mutate(value = value - reference, 
                       target = "disparity_vs_white") %>%
                select(-reference) %>%
                spread(key = quantity, value = value))
  
  differences <- estimates_with_disparities %>%
    melt(id = c("RACE","replicate","estimand","target"),
         variable.name = "quantity", value.name = "value") %>%
    spread(key = estimand, value = value) %>%
    mutate(difference_marginal_vs_factual = counterfactual_marginal - factual,
           difference_within_vs_factual = counterfactual_within_educ - factual,
           difference_marginal_vs_within = counterfactual_marginal - counterfactual_within_educ) %>%
    select(-counterfactual_marginal, -counterfactual_within_educ, -factual) %>%
    melt(id = c("RACE","replicate","target","quantity"),
         variable.name = "estimand") %>%
    spread(key = quantity, value = value)
  
  # Get the pooled estimate and standard error
  estimates_with_disparities %>%
    bind_rows(differences) %>%
    group_by(RACE, target, estimand) %>%
    summarize(point = mean(estimate),
              se = sqrt(4 / 160 * sum((estimate_star - estimate) ^ 2)))
}

