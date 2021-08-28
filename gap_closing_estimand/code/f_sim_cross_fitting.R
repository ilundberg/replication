# Author: Ian Lundberg (ianlundberg@ucla.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Forthcoming in Sociological Methods and Research

# See README.md for guidance to the replication package.

# This file illustrates the benefits of cross-fitting in a simulation with a random forest estimator.

# Initialize sink file to hold printed output
my_sink <- file("logs/f_sim_cross_fitting.txt", open = "wt")
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

# Set the number of simulations to draw for each setting
r <- 1000

# Set the sequence of sample sizes for simulations
n_vals <- c(250,500,1000,1500,2000,3000,5000,10000)

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

# Define the true value of the gap-closing estimand in this simulation
truth <- 2

####################################
# Simulation: With random forests, #
# cross-fitting helps              #
####################################

# Note: This simulation takes several hours

sim_cross_fitting <- foreach(n = n_vals,.combine = "rbind") %do% {
  print(paste("Starting n",n))
  foreach(rep = 1:r, .combine = "rbind", .packages = c("tidyverse","gapclosing","foreach")) %dorng% {
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
      result <- as.data.frame(gapclosing.out) %>%
        select(-primary) %>%
        filter(estimator %in% c("outcome_modeling","treatment_modeling","doubly_robust")) %>%
        filter(x == "1 - 0") %>%
        filter(estimand == "counterfactual_disparities") %>%
        select(estimator, estimate) %>%
        mutate(sample_split = sample_split_case)
      rownames(result) <- NULL
      return(result)
    }
  } %>% mutate(n = n)
}
saveRDS(sim_cross_fitting, file = "intermediate/sim_cross_fitting_helps.Rds")

# Produce results in terms of bias and MSE
sim_cross_fitting_aggregated <- sim_cross_fitting %>%
  group_by(sample_split, estimator, n) %>%
  summarize(variance = var(estimate),
            bias = mean(estimate - truth),
            bias_se = sd(estimate) / sqrt(n()),
            mse = mean((estimate  - truth) ^ 2),
            mse_se = sd((estimate - truth) ^ 2) / sqrt(n()),
            .groups = "drop") %>%
  # Drop the singly-robust sample split estimators because no one would choose those estimators
  filter(estimator == "doubly_robust" | sample_split == "single_sample") %>%
  mutate(estimator_label = case_when(estimator == "doubly_robust" & sample_split == "cross_fit" ~ "Doubly robust estimator\n+ cross fitting",
                                     estimator == "doubly_robust" & sample_split == "single_sample" ~ "Doubly robust estimator",
                                     estimator == "outcome_modeling" & sample_split == "single_sample" ~ "Estimator with\npredicted outcomes",
                                     estimator == "treatment_modeling" & sample_split == "single_sample" ~ "Estimator with\npredicted treatment\nprobabilities"),
         estimator_label = fct_relevel(estimator_label,"Estimator with\npredicted outcomes","Estimator with\npredicted treatment\nprobabilities","Doubly robust estimator","Doubly robust estimator\n+ cross fitting"))
saveRDS(sim_cross_fitting_aggregated, file = "intermediate/sim_cross_fitting_aggregated.Rds")

####################
# Produce the plot #
####################

sim_cross_fitting_convergence <- sim_cross_fitting_aggregated %>%
  filter(n %in% c(250,500,1000,1500,2000,3000,5000,10000)) %>%
  ggplot(aes(x = n, y = sqrt(mse),
             color = estimator_label, shape = estimator_label,
             alpha = estimator_label)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_line() +
  scale_y_continuous(name = "Root Mean Squared Error") +
  scale_x_continuous(name = "Sample Size") +
  scale_alpha_manual(values = rep(1,4)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key.height = unit(36,"pt"),
        legend.text = element_text(size = 10))
saveRDS(sim_cross_fitting_convergence,
        file = "intermediate/sim_cross_fitting_convergence.Rds")
ggsave(plot = sim_cross_fitting_convergence,
       file = "figures/sim_cross_fitting_convergence.pdf",
       height = 3, width = 6.5)

# Close the sink
print(Sys.time())
print("Time spent:")
print(difftime(Sys.time(),t0))
sink()
