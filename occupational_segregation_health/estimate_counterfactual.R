
# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
sink("logs/estimate_counterfactual.txt")

print("Produce counterfactual estimates")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load the packages
library(tidyverse)
library(reshape2)
library(foreach)
library(doParallel)
library(mgcv)

# Load estimator functions
source("code/estimator_functions.R")

# Prepare for parallel computing. Limit number of cores because this is memory-intensive.
cl <- makeCluster(4, outfile = "")
registerDoParallel(cl)

# Load the data
d <- readRDS("intermediate/d.Rds") %>%
  filter(YEAR >= 2005)

# Calculate the point estimates
counterfactual_point <- counterfactual_estimator(weight_name = "ASECWT",
                                                 save_intermediate = T)

# Re-estimate with replicate weights
counterfactual_reps <- foreach(
  i = 1:160,
  .combine = "rbind", 
  .packages = c("tidyverse","reshape2","foreach","mgcv")
) %dopar% {
  print(paste("Replicate",i))
  counterfactual_estimator(weight_name = paste0("REPWTP",i)) %>%
    mutate(replicate = i)
}

# Combine the above to produce point estimates and standard errors
counterfactual_estimate <- estimate_from_point_reps(counterfactual_point, counterfactual_reps) 

# Save the result
counterfactual_results <- list(counterfactual_estimate = counterfactual_estimate,
                               counterfactual_point = counterfactual_point,
                               counterfactual_reps = counterfactual_reps)
saveRDS(counterfactual_results, file = "intermediate/counterfactual_results.Rds")

print("Time spent")
print(difftime(Sys.time(),t0))

# Stop the parallel computing cluster
stopCluster(cl)

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))