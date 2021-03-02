
# Author: Ian Lundberg (ilundberg@princeton.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Paper is a working draft. Code will be posted on the Harvard Dataverse when the paper is finalized for publication.

# See run_all.R to set up the working environment.
# This code illustrates the benefits of cross-fitting.
# It uses cross-fitting random forest estimation and
# compares to estimation with correct treatment and outcome models.

cl <- makeCluster(4)
registerDoParallel(cl)

# Set the number of simulations to draw for each setting
r <- 1000

# Set the sequence of sample sizes for simulations
n_vals <- c(250,500,750,1000,1250,1500,2000,3000,5000,10000)

# Define the data generating process
make_sample <- function(n) {
  # Generate 10 uniform confounding variables l1,...,l10
  L <- matrix(runif(10*n,-1,1), nrow = n, ncol = 10)
  colnames(L) <- paste0("l",1:10)
  # Put those in a data frame
  data.frame(L) %>%
    # The category will be independent of all covariates
    mutate(x = sample(rep(0:1, n / 2)),
           # Treatment probabilities: True propensity score is a
           # logit with coefficient 0.3 on each confounder
           m = plogis(.3*rowSums(L)),
           # Treatment assignment
           d = rbinom(n,1,m),
           # Outcome function: Truth is OLS with a coefficient 1
           # on each confounder, plus a treatment effect that is
           # 1 in category X = 1 and -1 in category X = 0
           g = rowSums(L) + ifelse(x == 1, d, -d),
           # Outcome is a normal with high variance.
           # SD of noise here equals 10 times the treatment effect,
           # which may be a realistic setting
           y = rnorm(n, g, sd = 10),
           # I am not simulating a complex sample, so all units
           # have the same sampling weight.
           weight = 1)
}

# Define the true values of the post-intervention means and gap-closing estimand
true_values <- data.frame(truth = c(1, -1, 2),
                          category = c("1","0","1 - 0"))

# Define labels to be used in plots
category_labels <- c("Post-Intervention Mean\nin Category 0","Post-Intervention Mean\nin Category 1")
names(category_labels) <- c("0","1")#,"1 - 0")
summary_labels <- c("Absolute Bias of Estimator","Root Mean Squared Error of Estimator")
names(summary_labels) <- c("absolute_bias","rmse")

####################################
# Simulation: With random forests, #
# cross-fitting helps              #
# correctly-specified LM and GLM   #
####################################

# Note: This simulation takes several hours

sim_cross_fitting_helps <- foreach(n = n_vals,.combine = "rbind") %do% {
  print(paste("Starting n",n))
  foreach(rep = 1:r, .combine = "rbind", .packages = c("tidyverse","gapclosing","foreach")) %dopar% {
    sim_data <- make_sample(n)
    foreach(sample_split_case = c("single_sample","cross_fit"), .combine = "rbind") %do% {
      gapclosing.out <- gapclosing(
        counterfactual_assignments = 1,
        data = sim_data,
        outcome_formula = formula(y ~ x + l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9 + l10),
        treatment_formula = formula(d ~ x + l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9 + l10),
        category_name = "x",
        treatment_algorithm = "ranger",
        outcome_algorithm = "ranger",
        sample_split = sample_split_case
      )
      result <- do.call(rbind,gapclosing.out$all_estimates)
      result$method <- gsub("[.].*","",rownames(result))
      rownames(result) <- NULL
      result$sample_split <- sample_split_case
      return(result)
    }
  } %>% mutate(n = n)
}
save(sim_cross_fitting_helps, file = "intermediate/sim_cross_fitting_helps.Rdata")

# Produce results in terms of bias and MSE
results <- sim_cross_fitting_helps %>%
  filter(setting == "counterfactual") %>%
  filter(method == "doubly_robust" | sample_split == "single_sample") %>%
  select(sample_split, method, category, n, estimate) %>%
  group_by(sample_split, method, category, n) %>%
  left_join(true_values, by = "category") %>%
  summarize(variance = var(estimate),
            bias = mean(estimate - truth),
            bias_se = sd(estimate) / sqrt(n()),
            mse = mean((estimate  - truth) ^ 2),
            mse_se = sd((estimate - truth) ^ 2) / sqrt(n()),
            .groups = "drop") %>%
  mutate(category = case_when(category == "0" ~ "Post-Intervention Mean\nin Category 0",
                              category == "1" ~ "Post-Intervention Mean\nin Category 1",
                              category == "1 - 0" ~ "Gap-Closing Estimand\nCategory 1 - Category 0"),
         category = fct_rev(category),
         method_label = case_when(method == "doubly_robust" & sample_split == "cross_fit" ~ "Doubly robust\n+ cross fitting",
                                  method == "doubly_robust" & sample_split == "single_sample" ~ "Doubly robust",
                                  method == "outcome_modeling" ~ "Outcome modeling",
                                  method == "treatment_modeling" ~ "Treatment modeling"),
         method_label = fct_relevel(method_label,"Outcome modeling","Treatment modeling","Doubly robust","Doubly robust\n+ cross fitting"),
         sample_split_label = case_when(sample_split == "cross_fit" ~ "Cross Fitting",
                                        sample_split == "single_sample" ~ "Single Sample"))
save(results, file = "intermediate/sim_crossfit_results.Rdata")
# Most simplified version of the figure for slides
# Compare to outcome and DR in terms of RMSE
for (i in 0:3) {
  results %>%
    filter(method_label %in% c("Outcome modeling","Doubly robust","Doubly robust\n+ cross fitting") &
             category == "Gap-Closing Estimand\nCategory 1 - Category 0") %>%
    ggplot(aes(x = n, y = sqrt(mse),
               color = method_label, shape = method_label,
               alpha = method_label)) +
    geom_hline(yintercept = 0) +
    geom_point() +
    geom_line() +
    scale_y_continuous(name = "Root Mean Squared Error") +
    scale_x_continuous(name = "Sample Size",
                       breaks = c(1000,5000,9000)) +
    scale_alpha_manual(values = c(rep(1,i),rep(0,3 - i))) + 
    guides(alpha = F) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.key.height = unit(36,"pt"),
          legend.text = element_text(size = 10)) +
    ggsave(paste0("figures/sim_cross_fitting_slide_",i,".pdf"),
           height = 3, width = 6.5)
}
# More complex RMSE figure
results %>%
  filter(method == "doubly_robust") %>%
  filter(n < 10000) %>%
  ggplot(aes(x = n, y = sqrt(mse),
             color = method_label, shape = method_label)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_line() +
  facet_wrap(~category) +
  scale_y_continuous(name = "Root Mean Squared Error") +
  scale_x_continuous(name = "Sample Size",
                     breaks = c(1000,3000,5000)) +
  theme(legend.title = element_blank(),
        legend.key.height = unit(36,"pt"),
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ggsave("figures/sim_cross_fitting_helps_rmse.pdf",
         height = 3, width = 6.5)
# Bias figure
results %>%
  filter(method == "doubly_robust") %>%
  filter(n < 10000) %>%
  ggplot(aes(x = n, y = bias,
             color = method_label, shape = method_label)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_line() +
  facet_wrap(~category) +
  scale_y_continuous(name = "Bias") +
  scale_x_continuous(name = "Sample Size",
                     breaks = c(1000,3000,5000)) +
  theme(legend.title = element_blank(),
        legend.key.height = unit(36,"pt"),
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ggsave("figures/sim_cross_fitting_helps_bias.pdf",
         height = 3, width = 6.5)

# Show that cross-fitting does not really affect variance
results %>%
  filter(method == "doubly_robust") %>%
  filter(n < 10000) %>%
  ggplot(aes(x = n, y = variance,
             color = method_label, shape = method_label)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_line() +
  facet_wrap(~category) +
  scale_y_continuous(name = "Variance") +
  scale_x_continuous(name = "Sample Size",
                     breaks = c(1000,3000,5000)) +
  theme(legend.title = element_blank(),
        legend.key.height = unit(36,"pt"),
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ggsave("figures/sim_cross_fitting_not_help_variance.pdf",
         height = 3, width = 6.5)

# Compare to all non-robust alternatives in terms of RMSE
results %>%
  ggplot(aes(x = n, y = sqrt(mse),
             color = method_label, shape = method_label)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_line() +
  facet_wrap(~category) +
  scale_y_continuous(name = "Root Mean Squared Error") +
  scale_x_continuous(name = "Sample Size",
                     breaks = c(1000,5000,9000)) +
  theme(legend.title = element_blank(),
        legend.key.height = unit(36,"pt"),
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ggsave("figures/sim_cross_fitting_helps_rmse_all_models.pdf",
         height = 3, width = 6.5)

# Compare to the non-robust alternatives in terms of bias
results %>%
  ggplot(aes(x = n, y = bias,
             color = method_label, shape = method_label)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_line() +
  facet_wrap(~category) +
  scale_y_continuous(name = "Bias") +
  scale_x_continuous(name = "Sample Size",
                     breaks = c(1000,3000,5000)) +
  theme(legend.title = element_blank(),
        legend.key.height = unit(36,"pt"),
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ggsave("figures/sim_cross_fitting_helps_bias_all_models.pdf",
         height = 3, width = 6.5)

#####################################
# Show that cross-fit random forest #
# is as good as correctly-specified #
# parametric models in this case    #
#####################################

# Conduct simulations with parametric GLM estimation
sim_parametric_correct <- foreach(n = n_vals,.combine = "rbind") %do% {
  print(paste("Starting n",n))
  foreach(rep = 1:r, .combine = "rbind", .packages = c("tidyverse","gapclosing")) %dopar% {
    gapclosing.out <- gapclosing(
      data = make_sample(n),
      outcome_formula = formula(y ~ d*x + l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9 + l10),
      treatment_formula = formula(d ~ x + l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9 + l10),
      category_name = "x",
      counterfactual_assignments = 1,
      sample_split = "single_sample",
      treatment_algorithm = "glm",
      outcome_algorithm = "lm"
    )
    result <- do.call(rbind,gapclosing.out$all_estimates)
    result$method <- gsub("[.].*","",rownames(result))
    rownames(result) <- NULL
    result$sample_split <- "single_sample"
    return(result)
  } %>% mutate(n = n)
}
save(sim_parametric_correct, file = "intermediate/sim_parametric_correct.Rdata")

# Aggregate to parametric results
parametric_results <- sim_parametric_correct %>%
  filter(setting == "counterfactual") %>%
  select(sample_split, method, category, n, estimate) %>%
  group_by(sample_split, method, category, n) %>%
  left_join(true_values, by = "category") %>%
  summarize(variance = var(estimate),
            bias = mean(estimate - truth),
            bias_se = sd(estimate) / sqrt(n()),
            mse = mean((estimate  - truth) ^ 2),
            mse_se = sd((estimate - truth) ^ 2) / sqrt(n()),
            .groups = "drop") %>%
  mutate(category = case_when(category == "0" ~ "Post-Intervention Mean\nin Category 0",
                              category == "1" ~ "Post-Intervention Mean\nin Category 1",
                              category == "1 - 0" ~ "Gap-Closing Estimand\nCategory 1 - Category 0"),
         method_label = case_when(method == "doubly_robust" ~ "Doubly robust\n(correct GLMs)",
                                  method == "outcome_modeling" ~ "Outcome modeling\n(correct OLS)",
                                  method == "treatment_modeling" ~ "Treatment modeling\n(correct logit)"))


parametric_results %>%
  mutate(fun_form = "Correct GLM") %>%
  bind_rows(results %>%
              mutate(fun_form = ifelse(sample_split == "single_sample", "Random forest", "Random forest\n+ cross fitting"))) %>%
  mutate(category = fct_rev(category),
         method_label = factor(case_when(method == "outcome_modeling" ~ 1,
                                         method == "treatment_modeling" ~ 2,
                                         method == "doubly_robust" ~ 3),
                               labels = c("Outcome Modeling","Treatment Modeling","Doubly Robust"))) %>%
  ggplot(aes(x = n, y = sqrt(mse),
             color = fun_form, shape = fun_form)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_line() +
  facet_grid(category ~ method_label) +
  scale_y_continuous(name = "Root Mean Squared Error") +
  scale_x_continuous(name = "Sample Size",
                     breaks = c(1000,5000,9000)) +
  theme(legend.title = element_blank(),
        legend.key.height = unit(36,"pt"),
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ggsave("figures/correct_vs_dml_rmse.pdf",
         height = 5, width = 6.5)

# Note that the cross-fit RF actually performs better than the correct parametric models
# at large sample sizes

results %>%
  filter(n %in% c(1000,3000,5000,10000)) %>%
  filter(category != "Gap-Closing Estimand\nCategory 1 - Category 0") %>%
  mutate(ci.min = mse - qnorm(.975) * mse_se,
         ci.max = mse + qnorm(.975) * mse_se,
         method_label = factor(case_when(method_label == "Outcome modeling" ~ 1,
                                         method_label == "Treatment modeling" ~ 2,
                                         method_label == "Doubly robust" ~ 3,
                                         method_label == "Doubly robust\n+ cross fitting" ~ 4),
                               labels = c("Outcome\nmodeling","Treatment\nmodeling","Doubly\nrobust (DR)","DR + cross\nfitting")),
         n_label = paste0("Sample Size\nn = ",n),
         n_label = fct_reorder(n_label,n)) %>%
  ggplot(aes(x = method_label, y = sqrt(mse),
             ymin = sqrt(mse - qnorm(.975) * mse_se),
             ymax = sqrt(mse + qnorm(.975) * mse_se))) +
  geom_errorbar(width = .2, size = .3) +
  geom_point() +
  facet_grid(n_label ~ category, scales = "free_y") +
  ylab("RMSE") +
  xlab("Method") +
  theme(axis.text.x = element_text(size = 8),
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  ggsave("figures/rmse_large_sample_ci.pdf",
         height = 6, width = 6.5)



