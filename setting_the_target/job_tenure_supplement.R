
# Code file 1 of 2 for 
# "Setting the Target: Precise Estimands and the Gap Between Theory and Empirics"
# by Ian Lundberg, Rebecca Johnson, and Brandon Stewart

# Code by Ian Lundberg, ilundberg@princeton.edu

# This code file produces all estimates involving
# the proportion of U.S. civilians ages 25--45
# to hold the same job in January 2017 and January 2018

setwd("/Users/iandl/Dropbox/estimands")
sink("results/job_tenure_supplement.txt")
library(tidyverse)
library(reshape2)
library(gridExtra)
library(scales)
library(ipumsr)
library(foreach)
ddi <- read_ipums_ddi("data/cps_00034.xml")
data <- read_ipums_micro(ddi)

for_estimation <- data %>%
  # Restrict to age range
  filter(AGE >= 25 & AGE <= 45) %>%
  # Restrict to civilians
  filter(WTFINL > 0) %>%
  # Create employed indicator
  mutate(employed = EMPSTAT %in% c(10,12)) %>%
  # Create the variables of interest
  mutate(same_job = case_when(!employed ~ F,
                              JTYEARS >= 1 & JTYEARS < 99 ~ T, # worked more than 1 year at current job
                              JTYEARS < 1 ~ F)) %>%
  mutate(prop_responded = mean(!is.na(same_job))) %>%
  filter(!is.na(WTFINL)) %>%
  mutate(num_in_weights = n(),
         weighted_total = sum(WTFINL)) %>%
  filter(!is.na(same_job)) %>%
  mutate(num_analytic = n(),
         weighted_analytic = sum(WTFINL))

print("Unweighted sample restrictions")
for_estimation[1,] %>%
  select(starts_with("num")) %>%
  mutate(proportion = num_analytic / num_in_weights)

print("Weighted sample restrictions")
for_estimation[1,] %>%
  select(starts_with("weighted")) %>%
  mutate(proportion = weighted_analytic / weighted_total)

print("Estimated proportion in same job, for opening of paper:")
for_estimation %>%
  summarize(prop_same_job = weighted.mean(same_job, w = WTFINL))

# Function to estimate the MSE at a given proportion winsorized (trimmed)
# The take_every argument is a way to reduce the sample size
get_mse_hat <- function(prop_winsorized, take_every) {
  # Take the average over every possible systematic sample
  foreach(i = 0:(take_every - 1), .combine = "rbind") %do% {
    for_estimation %>%
      arrange(-WTFINL) %>%
      filter(((1:n()) %% take_every) == i) %>%
      mutate(winsorized_weight = case_when(WTFINL < quantile(WTFINL, prop_winsorized) ~ quantile(WTFINL, prop_winsorized),
                                           WTFINL > quantile(WTFINL, 1 - prop_winsorized) ~ quantile(WTFINL, 1 - prop_winsorized),
                                           T ~ WTFINL)) %>%
      summarize(prop_same_job = weighted.mean(same_job, w = winsorized_weight),
                var_prop = (1 / sum(winsorized_weight)) ^ 2 * sum(winsorized_weight ^ 2 * var(same_job))) %>%
      transmute(prop_winsorized = prop_winsorized,
                take_every = take_every,
                estimate = prop_same_job,
                estimate_var = var_prop)
  } %>%
    group_by(prop_winsorized, take_every) %>%
    summarize_all(.funs = mean) %>%
    group_by()
}

# Look at chosen approach at different sample sizes
set.seed(08544)
results <- foreach(prop_winsorized = seq(0,.5,.1), .combine = "rbind") %do% {
  foreach(take_every = c(1,10,50), .combine = "rbind") %do% {
    get_mse_hat(prop_winsorized, take_every)
  }
}
for (case in unique(results$take_every)) {
  results %>%
    left_join(results %>%
                filter(prop_winsorized == 0) %>%
                rename(truth = estimate) %>%
                select(take_every, truth),
              by = c("take_every")) %>%
    filter(take_every == case) %>%
    mutate(bias_sq = (estimate - truth) ^ 2,
           mse = bias_sq + estimate_var) %>%
    select(-truth, -estimate, -take_every) %>%
    melt(id = c("prop_winsorized")) %>%
    group_by(variable) %>%
    mutate(best = value == min(value) & variable == "mse") %>%
    group_by() %>%
    mutate(variable = case_when(variable == "bias_sq" ~ "1) Bias (squared)\nof estimator",
                                variable == "estimate_var" ~ "2) Variance\nof estimator",
                                variable == "mse" ~ "3) Mean squared error\nof estimator")) %>%
    ggplot(aes(x = prop_winsorized, y = value)) +
    geom_line(color = "darkgray") +
    geom_point(aes(color = best, shape = best, size = best), size = 3) +
    facet_wrap(~variable, scales = "free", nrow = 1) +
    scale_color_manual(values = c("darkgray","blue")) +
    scale_shape_manual(values = c(16,8)) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_x_continuous(name = "Proportion trimmed at lower and upper end of weight distribution") +
    scale_y_continuous(name = element_blank(),
                       labels = function(x) scientific(x,digits = 1)) +
    ggsave(paste0("figures/weight_trimming_",case,".pdf"),
           height = 2, width = 6.5)
}

sink()
