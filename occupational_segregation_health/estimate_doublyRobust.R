
# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
sink("logs/estimate_doublyRobust.txt")

print("Produce doubly robust estimates")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

library(tidyverse)
library(reshape2)
library(foreach)
library(mgcv)
library(nnet)

# Load estimator functions
source("code/estimator_functions.R")

# Load the data
d <- readRDS("intermediate/d.Rds") %>%
  filter(YEAR >= 2005)

# Calculate the point estimates
doublyRobust <- counterfactual_estimator(
  treatment_formula = formula(OCC2010 ~ SEX + EDUC + foreign_born + 
                                AGE + YEAR + questionnaire_redesign +
                                factor(HEALTH))
)

# Save the result
saveRDS(doublyRobust, file = "intermediate/doublyRobust.Rds")

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))