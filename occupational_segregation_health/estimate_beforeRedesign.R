
# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
sink("logs/estimate_beforeRedesign.txt")

print("Produce alternative specification before questionnaire redesign")

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
d_alt_all <- readRDS("intermediate/d.Rds") %>%
  filter(YEAR >= 2005) %>%
  filter(YEAR <= 2013) %>% 
  mutate(to_keep = !questionnaire_redesign & !lag_questionnaire_redesign) %>% 
  # Rescale the weights to keep the same sum when we restrict
  group_by(YEAR) %>% 
  mutate(ASECWT = ASECWT * sum(ASECWT) / sum(ASECWT * to_keep)) %>%
  group_by() %>%
  filter(to_keep) %>%
  group_by(OCC2010) %>%
  mutate(in_support = n_distinct(RACE) == 4) %>%
  group_by() %>%
  mutate(ASECWT = ASECWT / mean(ASECWT))

# Make and note common support restriction
d_alt <- d_alt_all %>% 
  filter(in_support)
print("Sample size:")
print(nrow(d_alt))
print("Number of occupations")
print(data.frame(d_alt_all %>%
                   group_by(OCC2010) %>%
                   filter(1:n() == 1) %>%
                   group_by() %>%
                   summarize(total = n(),
                             on_support = sum(in_support),
                             off_support = sum(!in_support))))
print("Proportion of weight on common support")
print(data.frame(d_alt_all %>%
                   summarize(in_support = weighted.mean(in_support, w = ASECWT))))

# Calculate the point estimates
beforeRedesign <- counterfactual_estimator(data = d_alt)

# Save the result
saveRDS(beforeRedesign, file = "intermediate/beforeRedesign.Rds")

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
