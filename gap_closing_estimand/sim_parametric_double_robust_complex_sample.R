
# Author: Ian Lundberg (ilundberg@princeton.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Paper is a working draft. Code will be posted on the Harvard Dataverse when the paper is finalized for publication.

# See run_all.R to set up the working environment.
# This file simulates the double robustness property with GLM estimation.

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

###############################
# Simulate estimation results #
###############################

draw_simulations <- function(treatment_formula_case, outcome_formula_case) {
  foreach(rep = 1:r, .combine = "rbind", .packages = c("gapclosing","tidyverse"), .export = c("giant_sample","n")) %dopar% {
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
    result_weighted <- do.call(rbind,weighted$all_estimates)
    result_unweighted <- do.call(rbind,unweighted$all_estimates)
    result_weighted$method <- result_unweighted$method <- gsub("[.].*","",rownames(result_weighted))
    rownames(result_weighted) <- rownames(result_unweighted) <- NULL
    return(result_weighted %>%
             mutate(weighted = "Weighted") %>%
             bind_rows(result_unweighted %>%
                         mutate(weighted = "Unweighted")) %>%
             select(setting, method, category, weighted, estimate))
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
  left_join(truth, by = c("category","setting")) %>%
  mutate(error = estimate - truth,
         method = fct_relevel(method,"outcome_modeling","treatment_modeling","doubly_robust"),
         category = fct_relevel(category, "1", "0", "1 - 0"),
         fun_form = fct_rev(fun_form))
save(sims_combined, file = "intermediate/sims_combined_parametric_complex_sample.Rdata")

# Make variable label vectors to improve ggplot2 output
method_labels <- c("Estimation by\nPredicted Outcomes", "Estimation by\nPredicted Treatment Probabilities", "Doubly Robust Estimation")
names(method_labels) <- c("outcome_modeling","treatment_modeling","doubly_robust")

# Plot the densities
for (category_case in unique(sims_combined$category)) {
  for (weighted_case in unique(sims_combined$weighted)) {
    sims_combined %>%
      filter(category == category_case & weighted == weighted_case & setting == "counterfactual") %>%
      ggplot(aes(x = error, y = fun_form)) +
      geom_density_ridges(color = NA, scale = .9, fill = "gray", bandwidth = .07) +
      geom_vline(xintercept = 0, size = .2) +
      facet_wrap(~ method,
                 labeller = as_labeller(method_labels)) +
      scale_x_continuous(name = "Error Distribution Estimates Across Simulations",
                         limits = range(sims_combined$error)) +
      scale_y_discrete(name = "Estimation Setting",
                       expand = expansion(mult = c(0.1,.5))) +
      theme_bw() +
      theme(axis.text.y = element_text(vjust = -.2, size = 8),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank()) +
      ggsave(case_when(category_case == "1" ~ paste0("figures/sim_x1_complex_sample_",weighted_case,".pdf"),
                       category_case == "0" ~ paste0("figures/sim_x0_complex_sample_",weighted_case,".pdf"),
                       category_case == "1 - 0" ~ paste0("figures/sim_x1mx0_complex_sample_",weighted_case,".pdf")),
             height = 2, width = 8)
  }
}

stopCluster(cl)
