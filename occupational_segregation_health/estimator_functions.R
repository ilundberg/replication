# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A gap-closing perspective
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

# Counterfactual disparity estimator
counterfactual_estimator <- function(
  weight_name, 
  data = d_onset, 
  outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                              s(OCC2010, bs = "re") + 
                              SEX + EDUC + RACE + foreign_born + s(AGE) + s(YEAR, k = 8) + new_question +
                              factor(HEALTH)),
  save_intermediate = F
) {
  d <- data %>%
    rename_at(.vars = vars(weight_name), .funs = function(x) "weight") %>%
    # Now that we have renamed, remove all the replicate weights to drop memory
    select(-starts_with("REPWTP")) %>%
    # For the GAM, we want the weight normalized to sum to the number of observations
    # per the MGCV documentation.
    group_by() %>%
    mutate(weight = weight / mean(weight))
  rm(data)
  # Estimate the race/ethnicity distribution in each occupation
  fitted_category_proportions <- foreach(race_case = c("Hispanic","Non-Hispanic Black","Other"), .combine = "cbind") %do% {
    fit <- bam(as.numeric(RACE == race_case) ~ s(OCC2010, bs = "re"),
               data = d,
               weights = weight)
    if (weight_name == "ASECWT" & save_intermediate) {
      save(fit, file = paste0("intermediate/prop_",gsub("-| ","",race_case),"_fit.Rdata"))
    }
    return(data.frame(prop = fitted(fit)) %>%
             rename_all(.funs = function(x) paste0("prop_",gsub(" |-","",race_case))))
  }
  # Append those to the dataset
  d <- d %>%
    bind_cols(fitted_category_proportions)
  # Fit an outcome model
  outcome_fit <- bam(outcome_formula,
                     weights = weight,
                     data = d)
  if (weight_name == "ASECWT" & save_intermediate) {
    save(outcome_fit, file = "intermediate/outcome_fit.Rdata")
  }
  # Extract the coefficients and model matrix on the linear basis
  beta <- coef(outcome_fit)
  X <- model.matrix(outcome_fit)
  rm(outcome_fit)
  
  # Determine the variables to be equalized
  equalized <- which(grepl("prop_|OCC",colnames(X)))
  
  # Prepare a matrix to hold the predictors after equalization
  X_equalized_within_educ <- X
  X_equalized_marginal <- X
  
  # Determine which rows correspond to each educational category
  educ_categories <- list(
    Less_than_HS = which(d$EDUC == "Less than HS"),
    High_school = which(d$EDUC == "High school"),
    Some_college = which(d$EDUC == "Some college"),
    College = which(d$EDUC == "College")
  )
  
  # Carry out marginal equalization
  X_equalized_marginal[,equalized] <- apply(X_equalized_marginal[,equalized],2,
                                            function(x) rep(weighted.mean(x, w = d$weight), length(x)))
    
  # Carry out equalization within educational categories
  for (rows in educ_categories) {
    X_equalized_within_educ[rows,equalized] <- apply(X_equalized_within_educ[rows,equalized],2,
                                                     function(x) rep(weighted.mean(x, w = d$weight[rows]), length(rows)))
  }
  
  # Predict the predicted counterfactual outcome
  counterfactual_predictions <- d %>%
    select(CPSIDP,RACE,y,weight) %>%
    mutate(counterfactual_marginal = as.vector(X_equalized_marginal %*% beta),
           counterfactual_within_educ = as.vector(X_equalized_within_educ %*% beta),
           not_equalized = as.vector(X %*% beta))
  # Save those for all replicates in case you later want to produce
  # an estimate for a subgroup
  if (save_intermediate) {
    save(counterfactual_predictions,file = "intermediate/counterfactual_predictions.Rdata")
  }
  
  # Aggregate to counterfactual means in each race/ethnicity category
  aggregated_predictions <- counterfactual_predictions %>%
    group_by(RACE) %>%
    summarize(counterfactual_marginal = weighted.mean(counterfactual_marginal, w = weight),
              counterfactual_within_educ = weighted.mean(counterfactual_within_educ, w = weight),
              factual = weighted.mean(y, w = weight)) %>%
    melt(id = "RACE", variable.name = "estimand", value.name = "estimate")
  rm(counterfactual_predictions)
  return(aggregated_predictions)
}

estimate_from_point_reps <- function(point, reps) {
  estimates_with_disparities <- reps %>%
    rename(estimate_star = estimate) %>%
    left_join(point,
              by = c("RACE","estimand")) %>%
    mutate(target = "proportion") %>%
    # Add the disparity vs. white
    bind_rows(reps %>%
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



# Counterfactual disparity estimator function that conducts separate outcome models by race subgroups
counterfactual_estimator_race_interactions <- function(
  weight_name, 
  data = d_onset,
  save_intermediate = F
) {
  d <- data %>%
    rename_at(.vars = vars(weight_name), .funs = function(x) "weight") %>%
    # Now that we have renamed, remove all the replicate weights to drop memory
    select(-starts_with("REPWTP")) %>%
    # For the GAM, we want the weight normalized to sum to the number of observations
    # per the MGCV documentation.
    group_by() %>%
    mutate(weight = weight / mean(weight))
  rm(data)
  # Estimate the race/ethnicity distribution in each occupation
  fitted_category_proportions <- foreach(race_case = c("Hispanic","Non-Hispanic Black","Other"), .combine = "cbind") %do% {
    fit <- bam(as.numeric(RACE == race_case) ~ s(OCC2010, bs = "re"),
               data = d,
               weights = weight)
    if (weight_name == "ASECWT" & save_intermediate) {
      save(fit, file = paste0("intermediate/prop_",gsub("-| ","",race_case),"_fit.Rdata"))
    }
    return(data.frame(prop = fitted(fit)) %>%
             rename_all(.funs = function(x) paste0("prop_",gsub(" |-","",race_case))))
  }
  # Append those to the dataset
  d <- d %>%
    bind_cols(fitted_category_proportions) %>%
    mutate(RACE = factor(RACE))
  
  # Restrict to the occupations observed in every race category
  d <- d %>%
    group_by(OCC2010) %>%
    filter(n_distinct(RACE) == 4) %>%
    group_by()
  
  # Get the counterfactual predictions by looping over race categories
  counterfactual_predictions <- foreach(r = unique(d$RACE), .combine = "rbind") %do% {
    # Restrict to those within a race category
    this_case <- d %>% filter(RACE == r)
    
    # Fit an outcome model
    fit <- bam(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                 s(OCC2010, bs = "re") + 
                 SEX + EDUC + foreign_born + s(AGE) + s(YEAR, k = 8) + new_question +
                 factor(HEALTH),
               weights = weight,
               data = this_case)
    
    # Extract the coefficients and model matrix on the linear basis
    beta <- coef(fit)
    X <- model.matrix(fit)
    
    # Predict the model matrix for everyone
    X_all <- predict(fit, type = "lpmatrix", newdata = d)
    
    # Prepare a matrix to hold the predictors after equalization
    X_equalized_within_educ <- X
    X_equalized_marginal <- X
    
    # Determine which rows correspond to each educational category
    educ_categories_all <- list(
      Less_than_HS = which(d$EDUC == "Less than HS"),
      High_school = which(d$EDUC == "High school"),
      Some_college = which(d$EDUC == "Some college"),
      College = which(d$EDUC == "College")
    )
    educ_categories_this_case <- list(
      Less_than_HS = which(this_case$EDUC == "Less than HS"),
      High_school = which(this_case$EDUC == "High school"),
      Some_college = which(this_case$EDUC == "Some college"),
      College = which(this_case$EDUC == "College")
    )
    
    # Determine the variables to be equalized
    equalized <- which(grepl("prop_|OCC",colnames(X)))
    
    # Carry out marginal equalization
    X_equalized_marginal[,equalized] <- apply(
      # Average in all cases, for equalized columns
      X_all[,equalized], 
      # Take averages within columns
      2, 
      # Take the weighted average over all cases
      # Repeat for as many rows as are in this case-specific matrix
      function(x) rep(weighted.mean(x, w = d$weight), nrow(X_equalized_marginal))
    )
    
    # Carry out equalization within educational categories
    for (e in names(educ_categories_all)) {
      # Determine which rows of this case have this education category
      rows_this_case <- educ_categories_this_case[[e]]
      # Determine which rows of everyone have this education category
      rows_all <- educ_categories_all[[e]]
      # Equalize
      X_equalized_within_educ[rows_this_case,equalized] <- apply(
        # Average in all cases, in the rows of this education category, for equalized columns
        X_all[rows_all,equalized],
        # Take averages within columns
        2,
        # Take the weighted average over all cases in this education category
        # Repeat for as many rows as are in this case-specific matrix    
        function(x) rep(weighted.mean(x, w = d$weight[rows_all]), nrow(X_equalized_within_educ))
      )
    }
    # Predict the predicted counterfactual outcome by each equalization rule
    counterfactual_predictions_this_case <- this_case %>%
      select(CPSIDP,RACE,y,weight) %>%
      mutate(counterfactual_marginal = as.vector(X_equalized_marginal %*% beta),
             counterfactual_within_educ = as.vector(X_equalized_within_educ %*% beta),
             not_equalized = as.vector(X %*% beta))
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
              factual = weighted.mean(y, w = weight)) %>%
    melt(id = "RACE", variable.name = "estimand", value.name = "estimate")
  rm(counterfactual_predictions)
  return(aggregated_predictions)
}

