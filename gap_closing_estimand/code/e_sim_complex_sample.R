# Author: Ian Lundberg (ianlundberg@ucla.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Forthcoming in Sociological Methods and Research

# See README.md for guidance to the replication package.

# This file simulates data to show performance with sampling weights
# in a survey sample selected from the population with unequal probabilities.

# Initialize sink file to hold printed output
my_sink <- file("logs/e_sim_complex_sample.txt", open = "wt")
sink(my_sink ,type = "output")
sink(my_sink, type = "message")

t0 <- Sys.time()
print("Time began:")
print(t0)

# Set seed
set.seed(08544)

# Load packages
source("code/prepare_environment.R")

# Print the computing environment information
print(sessionInfo())

# Prepare for parallel computing
cl <- makeCluster(4)
registerDoParallel(cl)

# Set the number of simulations
r <- 1000

# Set the sample size
n <- 500

######################################
# Define the data generating process #
######################################

make_sample <- function(n) {
  data.frame(x = rbinom(n,1,.5)) %>%
    mutate(l = ifelse(x == 1, runif(n,0,1), runif(n,-1,0)),
           m = plogis(-2 + 4 * (l ^ 2)),
           d = rbinom(n,1,m),
           g = 4 + 4*l + 4*d - 2*d*l,
           y = rnorm(n, g, sd = 1))
}

############################################
# Store the truth to which we will compare #
############################################

counterfactual_truth <- 2

###############################
# Simulate estimation results #
###############################

# Draw a giant sample (i.e. a population) from which all individual samples will be drawn
giant_sample <- make_sample(100000)

draw_simulations <- function(treatment_formula_case, outcome_formula_case) {
  foreach(rep = 1:r, .combine = "rbind", .packages = c("gapclosing","tidyverse"), .export = c("giant_sample","n")) %dorng% {
    this_sample <- giant_sample %>%
      sample_n(n, replace = T, weight = m) %>%
      mutate(sample_weight = 1 / m)
    weighted <- gapclosing(
      data = this_sample,
      outcome_formula = outcome_formula_case,
      treatment_formula = treatment_formula_case,
      category_name = "x",
      counterfactual_assignments = 1,
      sample_split = "single_sample",
      treatment_algorithm = "glm",
      outcome_algorithm = "lm",
      weight_name = "sample_weight"
    )
    unweighted <- gapclosing(
      data = this_sample,
      outcome_formula = outcome_formula_case,
      treatment_formula = treatment_formula_case,
      category_name = "x",
      counterfactual_assignments = 1,
      sample_split = "single_sample",
      treatment_algorithm = "glm",
      outcome_algorithm = "lm"
    )
    result_weighted <- as.data.frame(weighted) %>%
      select(-primary) %>%
      filter(estimator %in% c("outcome_modeling","treatment_modeling","doubly_robust")) %>%
      filter(x == "1 - 0") %>%
      filter(estimand == "counterfactual_disparities") %>%
      select(estimator, estimate) %>%
      mutate(weighted = "weighted")
    result_unweighted <- as.data.frame(unweighted) %>%
      select(-primary) %>%
      filter(estimator %in% c("outcome_modeling","treatment_modeling","doubly_robust")) %>%
      filter(x == "1 - 0") %>%
      filter(estimand == "counterfactual_disparities") %>%
      select(estimator, estimate) %>%
      mutate(weighted = "unweighted")
    rownames(result_weighted) <- rownames(result_unweighted) <- NULL
    return(result_weighted %>%
             bind_rows(result_unweighted))
  }
}
sims_both_correct <- draw_simulations(treatment_formula_case = formula(d ~ poly(l,2)),
                                      outcome_formula_case = formula(y ~ l*d))
sims_outcome_correct <- draw_simulations(treatment_formula = formula(d ~ l),
                                         outcome_formula = formula(y ~ l*d))
sims_treatment_correct <- draw_simulations(treatment_formula = formula(d ~ poly(l,2)),
                                           outcome_formula = formula(y ~ l + d))

# Combine all simulations into one data frame
sim_complex_sample <- sims_both_correct %>%
  mutate(fun_form = "Both Prediction\nFunctions Correct") %>%
  bind_rows(sims_outcome_correct %>%
              mutate(fun_form = "Treatment Prediction\nFunction Incorrect")) %>%
  bind_rows(sims_treatment_correct %>%
              mutate(fun_form = "Outcome Prediction\nFunction Incorrect")) %>%
  mutate(truth = counterfactual_truth) %>%
  mutate(error = estimate - truth,
         estimator = fct_relevel(estimator,"outcome_modeling","treatment_modeling","doubly_robust"),
         fun_form = fct_rev(fun_form))
saveRDS(sim_complex_sample , file = "intermediate/sim_complex_sample.Rds")

# Make variable label vectors to improve ggplot2 output
estimator_labels <- c("Estimation by\nPredicted Outcomes", "Estimation by\nPredicted Treatment Probabilities", "Doubly Robust Estimation")
names(estimator_labels) <- c("outcome_modeling","treatment_modeling","doubly_robust")

# Plot the densities
for (weighted_case in unique(sim_complex_sample$weighted)) {
  p <- sim_complex_sample %>%
    filter(weighted == weighted_case) %>%
    ggplot(aes(x = error, y = fun_form)) +
    geom_density_ridges(color = NA, scale = .9, fill = "gray", bandwidth = .07) +
    geom_vline(xintercept = 0, size = .2) +
    facet_wrap(~ estimator,
               labeller = as_labeller(estimator_labels)) +
    scale_x_continuous(name = "Error Distribution Estimates Across Simulations",
                       limits = range(sim_complex_sample$error)) +
    scale_y_discrete(name = "Estimation Setting",
                     expand = expansion(mult = c(0.1,.5))) +
    theme_bw() +
    theme(axis.text.y = element_text(vjust = -.2, size = 8),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank())
  ggsave(plot = p,
         file = paste0("figures/sim_complex_sample_",weighted_case,".pdf"),
         height = 2, width = 8)
}

stopCluster(cl)

# Close the sink
print(Sys.time())
print("Time spent:")
print(difftime(Sys.time(),t0))
sink()
