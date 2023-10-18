
sink("logs/main.txt")
t0 <- Sys.time()
print(t0)

# Set seed for reproducibility
set.seed(90095)

# Load packages
library(tidyverse)
theme_set(theme_bw())
library(foreach)
library(doParallel)
library(doRNG)
library(mgcv)
library(grid)
library(gridExtra)

# Check that all required code is in the directory
source("code/check_environment.R")

# Define outcomes
outcomes <- c("enrolled_any","enrolled_4yr",
              "completed_25","completed_30")
# Define the magnitude of the hypothetical interventions considered
delta_values <- c(10e3,25e3)
# Define the number of bootstrap samples
bs_reps <- 1000
# Define parallel computing cores
num_cores <- 6

# Create cluster for parallel computing
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Identify the objects to keep between steps of code
objects_to_keep <- c(ls(),"objects_to_keep","clear_environment")
# Create a function to remove all others
clear_environment <- function() {
  rm(list = setdiff(ls(pos = 1), objects_to_keep),
     pos = 1)
}

# Prepare data
print(Sys.time())
print("Prepare data")
source("code/prepare_data.R")
clear_environment()

# Descriptive estimation
print(Sys.time())
print("Descriptive estimation")
source("code/descriptive_smoother.R")
descriptive_smooths <- foreach(
  outcome_name = outcomes,
  .combine = "rbind"
) %do% {
  descriptive_smoother(outcome_name = outcome_name, delta = delta_values, bs_reps = bs_reps)
}
saveRDS(descriptive_smooths, file = "intermediate/descriptive_smooths.RDS")
clear_environment()

source("code/descriptive_binner.R")
descriptive_bins <- foreach(
  outcome_name = c("enrolled_any","enrolled_4yr","completed_25","completed_30"),
  .combine = "rbind"
) %do% {
  descriptive_binner(outcome_name = outcome_name)
}
saveRDS(descriptive_bins, file = "intermediate/descriptive_bins.RDS")
clear_environment()

# Pedagogical figure: Strong confounding
print(Sys.time())
print("Extrapolation danger")
source("code/strong_confounding.R")
clear_environment()

# Pedagogical figure: Extrapolation danger
print(Sys.time())
print("Extrapolation danger")
source("code/extrapolation_danger.R")
clear_environment()

# Causal estimation: Full sample
print(Sys.time())
print("Causal estimation")
for (outcome_value in outcomes) {
  print(paste("BEGIN OUTCOME",which(outcomes == outcome_value),
              "OF",length(outcomes)))
  source("code/causal_estimator.R")
  estimate.out <- causal_estimator(
    outcome = outcome_value, 
    # Number of bootstrap reps
    bs_reps = bs_reps,
    delta = delta_values
  )
  saveRDS(estimate.out, 
          file = paste0("intermediate/causal_",outcome_value,".RDS"))
}
clear_environment()

# Causal estimation: Within sample splits
print(Sys.time())
print("Causal estimation within sample splits")
inductive <- foreach(outcome_value = outcomes, .combine = "rbind") %do% {
  print(paste("BEGIN OUTCOME",which(outcomes == outcome_value),
              "OF",length(outcomes)))
  source("code/load_data.R")
  
  # Define sample split for this outcome
  split <- load_data(outcome_value) %>%
    # Conduct sample split within strata of confounders
    group_by(race, educJoint, label_wealth) %>%
    # Assign each case randomly to set 1 or 2
    mutate(set = sample(rep(1:2, ceiling(n() / 2)),n()))
  
  source("code/causal_estimator.R")
  estimate.learning.sets <- foreach(set_value = 1:2, .combine = "rbind") %do% {
    
    # Apply the causal estimator, learning in set set_value
    causal_estimator.out <- causal_estimator(
      outcome = outcome_value, 
      # Number of bootstrap reps
      bs_reps = bs_reps,
      delta = delta_values,
      learning_pubids = split$PUBID[split$set == set_value]
    )
    
    # Calculate the point estimate
    point <- causal_estimator.out$estimate %>%
      left_join(causal_estimator.out$data %>%
                  select(-w),
                by = "PUBID") %>%
      group_by(race, educJoint, label_wealth, label_income, delta) %>%
      summarize(estimate = weighted.mean(effect, w = w),
                num = n(),
                .groups = "drop")
    
    # Calculate the standard error
    se <- causal_estimator.out$bootstrap %>%
      left_join(causal_estimator.out$data %>%
                  select(-w),
                by = "PUBID") %>%
      group_by(race, educJoint, label_wealth, label_income, delta, bs) %>%
      summarize(estimate = weighted.mean(effect, w = w),
                .groups = "drop_last") %>%
      summarize(se = sd(estimate),
                .groups = "drop")
    
    # Return the estimates
    estimate <- point %>%
      left_join(se, by = c("race","educJoint","label_wealth","label_income","delta")) %>%
      mutate(ci.min = estimate - qnorm(.975) * se,
             ci.max = estimate + qnorm(.975) * se) %>%
      mutate(set = set_value,
             outcome = outcome_value)
    return(estimate)
  }
  
  # Select big- and small-effect subgroups inductively
  estimates.selected <- estimate.learning.sets %>%
    # store learning and estimation set results in same row
    mutate(set = ifelse(set == 1, "learning", "estimation")) %>%
    pivot_wider(names_from = set,
                values_from = c("estimate","se","ci.min","ci.max","num")) %>%
    # within outcome x delta subgroups, find big and small effects
    group_by(outcome, delta) %>%
    # identify the biggest 3 effects from the learning set
    arrange(-ci.min_learning) %>%
    mutate(rank_big = 1:n()) %>%
    # identify the 3 most-precise-zero effects from the learning set
    mutate(abs_ci = abs(ci.min_learning) + abs(ci.max_learning)) %>%
    arrange(abs_ci) %>%
    mutate(rank_small = 1:n()) %>%
    select(-abs_ci)
  
  return(estimates.selected)
}
saveRDS(inductive, file = "intermediate/inductive.RDS")
clear_environment()

# Curve estimate for visualization of the model
source("code/analyze_to_visualize_model.R")
print(Sys.time())
print("Visualize model: Analyze")
for_visualize_model <- foreach(outcome = outcomes, .combine = "rbind") %do% {
  print(paste("BEGIN OUTCOME",which(outcomes == outcome),
              "OF",length(outcomes)))
  analyze_to_visualize_model(outcome, bs_reps = 1) #bs_reps) # modified since will not visualize CI anyway
}
saveRDS(for_visualize_model, file = "intermediate/for_visualize_model.RDS")
clear_environment()

# Visualize the estimated curves with wealth fixed
source("code/visualize_model.R")
print(Sys.time())
print("Visualize model: Visualize")
source("code/visualize_model.R")
clear_environment()

# Descriptive visualization
print(Sys.time())
print("Descriptive visualization")
source("code/visualize_descriptive.R")
clear_environment()

# Deductive causal visualization
print(Sys.time())
print("Deductive causal visualization")
source("code/visualize_causal.R")
clear_environment()

# Causal estimates in interactive subgroups
print(Sys.time())
print("Causal estimates in interactive subgroups")
source("code/visualize_causal_subgroup_estimates.R")
clear_environment()

# Describe confounders of those with low, middle, and high effects sizes
print(Sys.time())
print("Covariates given effect size")
source("code/summarize_x_given_effect.R")
clear_environment()

# Visualize inductive results
print(Sys.time())
print("Visualize inductive")
source("code/visualize_inductive.R")
clear_environment()

print(sessionInfo())

print(paste("FINISH TIME:",Sys.time()))
print(paste("bs_reps:",bs_reps))
print(paste("outcomes:",paste(outcomes,collapse = " ")))
print(paste("delta_values:",paste(delta_values,collapse = " ")))
print(paste("num_cores:",num_cores))
print(("TIME SPENT:"))
print(difftime(Sys.time(),t0))

sink()
