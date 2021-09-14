
# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.
# Note: This file was run locally after the AWS code had executed.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
sink("logs/change_confidence_intervals.txt")

print("Produce counterfactual estimates")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load the packages
library(tidyverse)

counterfactual_results <- readRDS("intermediate/counterfactual_results.Rds")

print("Confidence intervals for factual-to-counterfactual additive change in each gap")

print("Confidence intervals for factual-to-counterfactual proportional change in each gap")
print(data.frame(counterfactual_results$counterfactual_point %>%
                   filter(estimand %in% c("counterfactual_within_educ","factual")) %>%
                   pivot_wider(names_from = "RACE", values_from = "estimate") %>%
                   mutate(gap_BlackWhite = `Non-Hispanic Black` - `Non-Hispanic White`,
                          gap_HispanicWhite = Hispanic - `Non-Hispanic White`,
                          gap_OtherWhite = Other - `Non-Hispanic White`) %>%
                   select(estimand, starts_with("gap")) %>%
                   pivot_longer(cols = starts_with("gap"), names_to = "gap", values_to = "value") %>%
                   pivot_wider(names_from = "estimand", values_from = "value") %>%
                   mutate(estimate = 1 - counterfactual_within_educ / factual) %>%
                   select(gap, estimate) %>%
                   # Now add standard errors
                   left_join(counterfactual_results$counterfactual_reps %>%
                               filter(estimand %in% c("counterfactual_within_educ","factual")) %>%
                               pivot_wider(names_from = "RACE", values_from = "estimate") %>%
                               mutate(gap_BlackWhite = `Non-Hispanic Black` - `Non-Hispanic White`,
                                      gap_HispanicWhite = Hispanic - `Non-Hispanic White`,
                                      gap_OtherWhite = Other - `Non-Hispanic White`) %>%
                               select(estimand, replicate, starts_with("gap")) %>%
                               pivot_longer(cols = starts_with("gap"), names_to = "gap", values_to = "value") %>%
                               pivot_wider(names_from = "estimand", values_from = "value") %>%
                               mutate(prop_change = 1 - counterfactual_within_educ / factual) %>%
                               group_by(gap) %>%
                               summarize(se = sd(prop_change)),
                             by = "gap") %>%
                   # Create confidence intervals
                   mutate(ci.min = estimate - qnorm(.975) * se,
                          ci.max = estimate + qnorm(.975) * se)))

print(data.frame(counterfactual_results$counterfactual_point %>%
                   filter(estimand %in% c("counterfactual_within_educ","factual")) %>%
                   pivot_wider(names_from = "RACE", values_from = "estimate") %>%
                   mutate(gap_BlackWhite = `Non-Hispanic Black` - `Non-Hispanic White`,
                          gap_HispanicWhite = Hispanic - `Non-Hispanic White`,
                          gap_OtherWhite = Other - `Non-Hispanic White`) %>%
                   select(estimand, starts_with("gap")) %>%
                   pivot_longer(cols = starts_with("gap"), names_to = "gap", values_to = "value") %>%
                   pivot_wider(names_from = "estimand", values_from = "value") %>%
                   mutate(estimate = counterfactual_within_educ - factual) %>%
                   select(gap, estimate) %>%
                   # Now add standard errors
                   left_join(counterfactual_results$counterfactual_reps %>%
                               filter(estimand %in% c("counterfactual_within_educ","factual")) %>%
                               pivot_wider(names_from = "RACE", values_from = "estimate") %>%
                               mutate(gap_BlackWhite = `Non-Hispanic Black` - `Non-Hispanic White`,
                                      gap_HispanicWhite = Hispanic - `Non-Hispanic White`,
                                      gap_OtherWhite = Other - `Non-Hispanic White`) %>%
                               select(estimand, replicate, starts_with("gap")) %>%
                               pivot_longer(cols = starts_with("gap"), names_to = "gap", values_to = "value") %>%
                               pivot_wider(names_from = "estimand", values_from = "value") %>%
                               mutate(c_minus_f = counterfactual_within_educ - factual) %>%
                               group_by(gap) %>%
                               summarize(se = sd(c_minus_f)),
                             by = "gap") %>%
                   # Create confidence intervals
                   mutate(ci.min = estimate - qnorm(.975) * se,
                          ci.max = estimate + qnorm(.975) * se)))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))