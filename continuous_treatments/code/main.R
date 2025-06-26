
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
library(grf)
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
num_cores <- detectCores()

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

# Pedagogical figures involving no data
print(Sys.time())
print("Conceptual figure of nonlinear and heterogeneous effects")
source("code/nonlinear_heterogeneous_conceptual.R")
clear_environment()
print("Conceptual figure of strong confounding")
source("code/strong_confounding.R")
clear_environment()
print("Conceptual figure with heterogeneity and nonlinearity empirically equivalent")
source("code/simulation_no_tradeoff.R")
clear_environment()

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

# Causal estimation: Logistic regression in full sample
print(Sys.time())
print("Causal estimation: Logit")
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

# Causal estimation: GAM in full sample
print(Sys.time())
print("Causal estimation: GAM")
for (outcome_value in outcomes) {
  print(paste("BEGIN OUTCOME",which(outcomes == outcome_value),
              "OF",length(outcomes)))
  source("code/causal_estimator_gam.R")
  estimate.out <- causal_estimator(
    outcome = outcome_value, 
    # Number of bootstrap reps
    bs_reps = bs_reps,
    delta = delta_values
  )
  saveRDS(estimate.out, 
          file = paste0("intermediate/causal_",outcome_value,"_gam.RDS"))
}
clear_environment()

# Descriptive visualization
print(Sys.time())
print("Descriptive visualization")
source("code/visualize_descriptive.R")
clear_environment()

# Causal visualization
print(Sys.time())
print("Deductive causal visualization")
source("code/visualize_causal.R")
visualize_causal(model = "logit")
visualize_causal(model = "gam")
clear_environment()

# Visualize the similarity of logit and GAM estimates
print(Sys.time())
print("Logit vs GAM scatter")
source("code/visualize_logit_gam_scatter.R")
clear_environment()

# Describe confounders of those with low, middle, and high effects sizes
# using logit model estimates
print(Sys.time())
print("Covariates given effect size")
source("code/summarize_x_given_effect.R")
clear_environment()

# Visualize the estimated model
print(Sys.time())
print("Visualize model")
source("code/visualize_model.R")
clear_environment()

# Simulations
print(Sys.time())
print("Simulation: Forest can underperform")
source("code/simulation_forest_can_underperform.R")
clear_environment()
print("Simulation: Jointly interactive and nonlinear DGP")
source("code/simulation_interactive_nonlinear_mse.R")
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
