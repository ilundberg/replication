
# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
sink("logs/estimate_from1988.txt")

print("Produce alternative specification for 1988-2020")

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
d <- readRDS("intermediate/d.Rds")
print("Sample size from 1988")
print(nrow(d))

# Calculate the point estimates
from1988 <- counterfactual_estimator(data = d,
                                     outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                                                                 s(OCC2010, bs = "re") + 
                                                                 SEX + EDUC + s(AGE) + s(YEAR, k = 8) + questionnaire_redesign))

# Save the result
saveRDS(from1988, file = "intermediate/from1988.Rds")

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))