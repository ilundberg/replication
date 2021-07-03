
# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
sink("logs/estimate_forwardLinkingWeight.txt")

print("Produce alternative specification using forward-linking weights")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

library(tidyverse)
library(reshape2)
library(foreach)
library(mgcv)

# Load estimator functions
source("code/estimator_functions.R")

# Load the data
d <- readRDS("intermediate/d.Rds") %>%
  filter(YEAR >= 2005)

# Calculate the point estimates
forwardLinkingWeight <- counterfactual_estimator(
  weight_name = "LNKFW1YWT", 
  data = d
)

# Save the result
saveRDS(forwardLinkingWeight, file = "intermediate/forwardLinkingWeight.Rds")

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
