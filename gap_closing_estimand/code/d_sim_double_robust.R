# Author: Ian Lundberg (ianlundberg@ucla.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Forthcoming in Sociological Methods and Research

# See README.md for guidance to the replication package.

# This file simulates data to show the double robustness of the estimator with parametric models.

# Initialize sink file to hold printed output
my_sink <- file("logs/d_sim_double_robust.txt", open = "wt")
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

counterfactual_truth <- data.frame(setting = "factual",
                                   category = c("1","0","1 - 0"),
                                   truth = c(9,7,2))

# Draw a giant sample to know some true values
giant_sample <- make_sample(100000)
factual_truth <- giant_sample %>%
  group_by(x) %>%
  summarize(y = mean(y),
            .groups = "drop") %>%
  spread(key = x, value = y) %>%
  mutate(`1 - 0` = `1` - `0`) %>%
  melt(id = NULL,
       variable.name = "category",
       value.name = "truth")

# Determine the true proportional change in each
truth <- counterfactual_truth %>%
  rename(counterfactual = truth) %>%
  select(category, counterfactual) %>%
  left_join(factual_truth %>%
              rename(factual = truth) %>%
              select(category, factual),
            by = "category") %>%
  mutate(change = factual - counterfactual,
         prop_change = (factual - counterfactual) / factual) %>%
  melt(id = "category", variable.name = "setting", value.name = "truth")

########################################
# Illustrate how models would go wrong #
########################################

wrong_outcome <- lm(y ~ l + d, data = giant_sample)
wrong_treatment <- glm(d ~ l, data = giant_sample, family = binomial)
simulation_wrong_g <- giant_sample %>%
  mutate(ghat = predict(wrong_outcome)) %>%
  sample_frac(.01) %>%
  mutate(Category = ifelse(x == 1, "X = 1", "X = 0"),
         Treatment = ifelse(d == 1, "T = 1", "T = 0")) %>%
  select(Category, Treatment, l, g, ghat) %>%
  melt(id = c("Category","Treatment","l"),
       variable.name = "true_or_estimated",
       value.name = "g") %>%
  mutate(true_or_estimated = factor(case_when(true_or_estimated == "g" ~ 1,
                                              true_or_estimated == "ghat" ~ 2),
                                    labels = c("True Data Generating Process",
                                               "Incorrect Model Specification"))) %>%
  ggplot(aes(x = l, y = g, color = Treatment, linetype = Category)) +
  geom_line() +
  xlab("Confounding Variable L") +
  scale_y_continuous(name = ("Expected Potential Outcome"),
                     breaks = seq(0,10,2)) +
  facet_wrap(~true_or_estimated, ncol = 2)
ggsave(plot = simulation_wrong_g,
       file = "figures/simulation_wrong_g.pdf",
       height = 3, width = 6.5)

simulation_wrong_m <- giant_sample %>%
  mutate(mhat = predict(wrong_treatment, type = "response")) %>%
  sample_frac(.01) %>%
  sample_frac(.1) %>%
  mutate(Category = ifelse(x == 1, "X = 1", "X = 0")) %>%
  select(Category, l, m, mhat) %>%
  melt(id = c("Category","l"),
       variable.name = "true_or_estimated",
       value.name = "m") %>%
  mutate(true_or_estimated = factor(case_when(true_or_estimated == "m" ~ 1,
                                       true_or_estimated == "mhat" ~ 2),
                                    labels = c("True Data Generating Process",
                                               "Incorrect Model Specification"))) %>%
  ggplot(aes(x = l, y = m, linetype = Category)) +
  geom_line() +
  xlab("Confounding Variable L") +
  ylab("Probability of Treatment") +
  facet_wrap(~true_or_estimated, ncol = 2)
ggsave(plot = simulation_wrong_m,
       file = "figures/simulation_wrong_m.pdf",
       height = 3, width = 6.43)

###############################
# Simulate estimation results #
###############################

draw_simulations <- function(treatment_formula_case, outcome_formula_case) {
  foreach(rep = 1:r, .combine = "rbind", .packages = c("gapclosing","tidyverse"), .export = c("make_sample","n")) %dorng% {
    gapclosing.out <- gapclosing(
      data = make_sample(n),
      outcome_formula = outcome_formula_case,
      treatment_formula = treatment_formula_case,
      category_name = "x",
      counterfactual_assignments = 1,
      sample_split = "single_sample",
      treatment_algorithm = "glm",
      outcome_algorithm = "lm"
    )
    result <- as.data.frame(gapclosing.out) %>%
      select(-primary) %>%
      filter(estimator %in% c("outcome_modeling","treatment_modeling","doubly_robust")) %>%
      filter(x == "1 - 0") %>%
      filter(estimand == "counterfactual_disparities") %>%
      select(estimator, estimate)
    rownames(result) <- NULL
    return(result)
  }
}
sims_both_correct <- draw_simulations(treatment_formula_case = formula(d ~ poly(l,2)),
                                      outcome_formula_case = formula(y ~ l*d))
sims_outcome_correct <- draw_simulations(treatment_formula = formula(d ~ l),
                                         outcome_formula = formula(y ~ l*d))
sims_treatment_correct <- draw_simulations(treatment_formula = formula(d ~ poly(l,2)),
                                           outcome_formula = formula(y ~ l + d))

# Combine all simulations into one data frame
sims_combined <- sims_both_correct %>%
  mutate(fun_form = "Both Prediction\nFunctions Correct") %>%
  bind_rows(sims_outcome_correct %>%
              mutate(fun_form = "Treatment Prediction\nFunction Incorrect")) %>%
  bind_rows(sims_treatment_correct %>%
              mutate(fun_form = "Outcome Prediction\nFunction Incorrect")) %>%
  mutate(truth = truth$truth[truth$category == "1 - 0" & truth$setting == "counterfactual"]) %>%
  mutate(error = estimate - truth,
         estimator = fct_relevel(estimator,"outcome_modeling","treatment_modeling","doubly_robust"),
         fun_form = fct_rev(fun_form))
saveRDS(sims_combined, file = "intermediate/sims_combined_parametric.Rds")

# Make variable label vectors to improve ggplot2 output
estimator_labels <- c("Estimation by\nPredicted Outcomes", "Estimation by\nPredicted Treatment Probabilities", "Doubly Robust Estimation")
names(estimator_labels) <- c("outcome_modeling","treatment_modeling","doubly_robust")

# Plot the densities
sim_densities <- sims_combined %>%
  ggplot(aes(x = error, y = fun_form, alpha = fun_form)) +
  geom_density_ridges(color = NA, scale = .9, fill = "gray", bandwidth = .06) +
  geom_vline(xintercept = 0, size = .2) +
  facet_wrap(~ estimator,
             labeller = as_labeller(estimator_labels)) +
  scale_x_continuous(name = "Error Distribution Estimates Across Simulations",
                     limits = range(sims_combined$error)) +
  scale_y_discrete(name = "Estimation Setting",
                   expand = expansion(mult = c(0.1,.5))) +
  scale_alpha_manual(values = c(1,1,1)) +
  theme_bw() +
  theme(axis.text.y = element_text(vjust = -.2, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")
saveRDS(sim_densities, file = "intermediate/sim_densities.Rds")
ggsave(sim_densities,
       file = "figures/sim_x1mx0.pdf",
       height = 2, width = 8)

stopCluster(cl)

# Close the sink
print(Sys.time())
print("Time spent:")
print(difftime(Sys.time(),t0))
sink()
