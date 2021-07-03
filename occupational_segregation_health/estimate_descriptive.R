
# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
sink("logs/estimate_descriptive.txt")

print("Produce descriptive estimates")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load packages
library(tidyverse)
library(reshape2)
library(foreach)
library(doParallel)

# Load estimator functions
source("code/estimator_functions.R")

# Prepare for parallel computing
cl <- makeCluster(8)
registerDoParallel(cl)

# Load the data
full_population <- readRDS("intermediate/full_population.Rds") %>%
  filter(YEAR >= 2005)
d_onset <- readRDS("intermediate/d_onset.Rds") %>%
  filter(YEAR >= 2005)
d <- readRDS("intermediate/d.Rds") %>%
  filter(YEAR >= 2005)

# Calculate the point estimates
disparity_point <- disparity_estimator("ASECWT")

# Re-estimate with replicate weights
disparity_reps <- foreach(
  i = 1:160, 
  .combine = "rbind", 
  .packages = c("tidyverse")
) %dopar% {
  disparity_estimator(paste0("REPWTP",i)) %>%
    mutate(replicate = i)
}

# Combine the above to produce point estimates and standard errors
disparity_estimate <- disparity_reps %>%
  rename(estimate_star = estimate) %>%
  left_join(disparity_point,
            by = c("RACE","estimand")) %>%
  mutate(target = "proportion") %>%
  # Append the disparity vs. white
  bind_rows(disparity_reps %>%
              rename(estimate_star = estimate) %>%
              left_join(disparity_point,
                        by = c("RACE","estimand")) %>%
              group_by() %>%
              melt(id = c("RACE","replicate","estimand"), variable.name = "quantity") %>%
              group_by(replicate) %>%
              mutate(reference = mean(case_when(RACE == "Non-Hispanic White" ~ value), na.rm = T)) %>%
              filter(RACE != "Non-Hispanic White") %>%
              group_by() %>%
              mutate(value = value - reference, 
                     target = "disparity_vs_white") %>%
              select(-reference) %>%
              spread(key = "quantity", value = "value")) %>%
  # Get the pooled estimate and standard error
  group_by(RACE, estimand, target) %>%
  summarize(point = mean(estimate),
            se = sqrt(4 / 160 * sum((estimate_star - estimate) ^ 2)))

# Save the result
disparity_results <- list(disparity_estimate = disparity_estimate,
                          disparity_point = disparity_point,
                          disparity_reps = disparity_reps)
saveRDS(disparity_results, file = "intermediate/disparity_results.Rds")

print("Time spent")
print(difftime(Sys.time(),t0))

# Stop the parallel computing cluster
stopCluster(cl)

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))