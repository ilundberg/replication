
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

########################################
# Illustrate how models would go wrong #
########################################

wrong_outcome <- lm(y ~ l + d, data = giant_sample)
wrong_treatment <- glm(d ~ l, data = giant_sample, family = binomial)
giant_sample %>%
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
  scale_y_continuous(name = ("Potential Outcome Y(t)"),
                     breaks = seq(0,10,2)) +
  facet_wrap(~true_or_estimated, ncol = 2) +
  ggsave("figures/simulation_wrong_g.pdf",
         height = 3, width = 6.5)
giant_sample %>%
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
  facet_wrap(~true_or_estimated, ncol = 2) +
  ggsave("figures/simulation_wrong_m.pdf",
         height = 3, width = 6.43)



###############################
# Simulate estimation results #
###############################

draw_simulations <- function(treatment_formula_case, outcome_formula_case) {
  foreach(rep = 1:r, .combine = "rbind", .packages = c("gapclosing","tidyverse"), .export = c("make_sample","n")) %dopar% {
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
    result <- do.call(rbind,gapclosing.out$all_estimates)
    result$method <- gsub("[.].*","",rownames(result))
    rownames(result) <- NULL
    return(result %>%
             select(setting, method, category, estimate))
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
  mutate(fun_form = "Both Models\nCorrect") %>%
  bind_rows(sims_outcome_correct %>%
              mutate(fun_form = "Treatment Model\nIncorrect")) %>%
  bind_rows(sims_treatment_correct %>%
              mutate(fun_form = "Outcome Model\nIncorrect")) %>%
  left_join(truth, by = c("category","setting")) %>%
  mutate(error = estimate - truth,
         method = fct_relevel(method,"outcome_modeling","treatment_modeling","doubly_robust"),
         category = fct_relevel(category, "1", "0", "1 - 0"),
         fun_form = fct_rev(fun_form))
save(sims_combined, file = "intermediate/sims_combined_parametric.Rdata")

# Make variable label vectors to improve ggplot2 output
method_labels <- c("Outcome Modeling", "Treatment Modeling", "Doubly Robust")
names(method_labels) <- c("outcome_modeling","treatment_modeling","doubly_robust")

# Plot the densities
for (category_case in unique(sims_combined$category)) {
  sims_combined %>%
    filter(category == category_case & setting == "counterfactual") %>%
    ggplot(aes(x = error, y = fun_form)) +
    geom_density_ridges(color = NA, scale = .9, fill = "gray", bandwidth = .05) +
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
    ggsave(case_when(category_case == "1" ~ "figures/sim_x1.pdf",
                     category_case == "0" ~ "figures/sim_x0.pdf",
                     category_case == "1 - 0" ~ "figures/sim_x1mx0.pdf"),
           height = 1.8, width = 6.5)
}

# Animated slide versions of the gap-closing estimand plot
for (slide_number in 0:3) {
  sims_combined %>%
    filter(category == "1 - 0" & setting == "counterfactual") %>%
    ggplot(aes(x = error, y = fun_form, alpha = fun_form)) +
    geom_density_ridges(color = NA, scale = .9, fill = "gray", bandwidth = .05) +
    geom_vline(xintercept = 0, size = .2) +
    facet_wrap(~ method,
               labeller = as_labeller(method_labels)) +
    scale_x_continuous(name = "Error Distribution Estimates Across Simulations",
                       limits = range(sims_combined$error)) +
    scale_y_discrete(name = "Estimation Setting",
                     expand = expansion(mult = c(0.1,.5))) +
    scale_alpha_manual(values = c(rep(0,3 - slide_number),rep(1,slide_number))) +
    theme_bw() +
    theme(axis.text.y = element_text(vjust = -.2, size = 8),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none") +
    ggsave(paste0("figures/sim_x1mx0_",slide_number,".pdf"),
           height = 1.8, width = 6.5)
}

stopCluster(cl)