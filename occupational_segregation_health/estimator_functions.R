
# See run_all.R to see how this file is called

# This file contains functions to estimate descriptive and counterfactual disparities

# Descriptive disparity estimator
disparity_estimator <- function(weight_name, full_population = full_population, d_onset = d_onset) {
  # Estimate the overall prevalence in the population
  prevalence <- full_population %>%
    rename_at(.vars = all_of(weight_name), .funs = function(x) "weight") %>%
    filter(!is.na(weight)) %>%
    group_by(RACE) %>%
    summarize(estimate = weighted.mean(y, w = weight)) %>%
    mutate(estimand = "prevalence")
  # Estimate the disparity in onset among the employed
  onset <- d_onset %>%
    rename_at(.vars = all_of(weight_name), .funs = function(x) "weight") %>%
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
  save_intermediate = F,
  outcome_formula = formula(y ~ prop_NonHispanicBlack + prop_Hispanic + prop_Other + 
                              s(OCC2010, bs = "re") + 
                              SEX + EDUC + foreign_born + AGE + YEAR + questionnaire_redesign + 
                              factor(HEALTH))
) {
  data <- data %>%
    rename_at(.vars = all_of(weight_name), .funs = function(x) "weight") %>%
    # Now that we have renamed, remove all the replicate weights to drop memory
    select(-starts_with("REPWTP")) %>%
    # For the GAM, we want the weight normalized to sum to the number of observations
    # per the MGCV documentation.
    group_by() %>%
    mutate(weight = weight / mean(weight)) |>
    mutate(AGE = haven::zap_labels(AGE))
  
  # Estimate the race/ethnicity distribution in each occupation
  race_within_occupation <- data |>
    group_by(OCC2010, RACE) |>
    summarize(weight = sum(weight), .groups = "drop") |>
    group_by(OCC2010) |>
    mutate(prop = weight / sum(weight)) |>
    ungroup() |>
    select(-weight) |>
    mutate(RACE = str_remove_all(RACE,"-| ")) |>
    pivot_wider(names_from = "RACE", values_from = "prop", names_prefix = "prop_") |>
    # Fill in rows that should be 0, no one observed of a category in that occupation
    mutate(across(2:5, .f = \(x) ifelse(!is.na(x), x, 0)))
  
  data <- data |>
    left_join(race_within_occupation, by = join_by(OCC2010))
  
  # Get the counterfactual predictions by looping over race categories
  counterfactual_predictions <- foreach(r = unique(data$RACE), .combine = "rbind") %do% {
    # Restrict to those within a race category
    this_case <- data %>% 
      filter(RACE == r)
  
    fit <- bam(outcome_formula,
               weights = weight,
               data = this_case)
    
    # Extract the coefficients on the linear basis
    beta <- coef(fit)
    
    # Predict the model matrix for everyone
    X_all <- predict(fit, type = "lpmatrix", newdata = data)
    
    # Predict the model matrix for this case
    X <- predict(fit, type = "lpmatrix", newdata = this_case)
    
    # Initialize matrices to hold the predictors after equalization
    X_equalized_within_educ <- X
    
    # Determine which rows correspond to each educational category
    educ_values <- unique(data$EDUC)
    educ_rows_data <- lapply(educ_values, function(val) which(data$EDUC == val))
    educ_rows_this_case <- lapply(educ_values, function(val) which(this_case$EDUC == val))
    names(educ_rows_data) <- names(educ_rows_this_case) <- educ_values
    
    # Determine the variables to be equalized
    equalized <- which(grepl("prop_|OCC",colnames(X_all)))
    
    # Carry out equalization within educational categories
    for (e in educ_values) {
      # Determine which rows of this case have this education category
      rows_this_case <- educ_rows_this_case[[e]]
      # Determine which rows of everyone have this education category
      rows_data <- educ_rows_data[[e]]
      # Carry out marginal equalization
      # Replace the equalized columns
      for (j in equalized) {
        X_equalized_within_educ[rows_this_case,j] <- rep(
          weighted.mean(
            X_all[rows_data,j],
            w = data$weight[rows_data]
          ),
          length(rows_this_case)
        )
      }
    }
    
    counterfactual_predictions_this_case <- this_case %>%
      select(CPSIDP,RACE,y,weight) %>%
      mutate(counterfactual_within_educ = as.vector(X_equalized_within_educ %*% beta))
    
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
    summarize(counterfactual_within_educ = weighted.mean(counterfactual_within_educ, w = weight),
              factual = weighted.mean(y, w = weight)) %>%
    pivot_longer(cols = -RACE, names_to = "estimand", values_to = "estimate")
  rm(counterfactual_predictions)
  return(aggregated_predictions)
}

# Function to aggregate point and replicate estimates into a point estimate and a confidence interval
estimate_from_point_reps <- function(point, reps) {
  
  # Estimates for P(Y) within each race x estimand
  p_y <- reps |>
    rename(estimate_star = estimate) |>
    left_join(point, by = c("RACE","estimand")) |>
    group_by(RACE, estimand) |>
    summarize(
      estimate = unique(estimate),
      se = sqrt(4 / 160 * sum((estimate_star - estimate) ^ 2)),
      .groups = "drop"
    )
  
  # Estimates for differences vs white
  vs_white <- reps |>
    rename(estimate_star = estimate) |>
    left_join(point, by = c("RACE","estimand")) |>
    group_by(replicate, estimand) |>
    mutate(
      estimate = estimate - na.omit(case_when(RACE == "Non-Hispanic White" ~ estimate)),
      estimate_star = estimate_star - na.omit(case_when(RACE == "Non-Hispanic White" ~ estimate_star))
    ) |>
    group_by(RACE, estimand) |>
    summarize(
      estimate = unique(estimate),
      se = sqrt(4 / 160 * sum((estimate_star - estimate) ^ 2)),
      .groups = "drop"
    ) |>
    mutate(RACE = paste0(RACE, " - Non-Hispanic White"))
  
  return(p_y |> bind_rows(vs_white))
  
}

