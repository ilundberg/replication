

# Ian Lundberg (ilundberg@princeton.edu)
# Simulation for "A causal approach to study interventions that close gaps across social categories"

library(tidyverse)
library(reshape2)
library(foreach)

setwd("/Users/iandl/Dropbox/dissertation/gap_closing_estimands")

n <- 500000
data <- data.frame(L = rnorm(n)) %>%
  mutate(X = rbinom(n(), 1, prob = plogis(L)) == 1, # creates dependence of X and L
         p_t = plogis(X - .5 + L), # centered at 0.5
         marginal_prob = mean(p_t),
         treated = rbinom(n(), 1, prob = p_t) == 1,
         m = ifelse(treated, p_t, 1 - p_t),
         y0 = X - .5 + L,
         y1 = L + 1,
         outcome = ifelse(treated, y1, y0))
# Estimate the conditional probablity since this is complicated
data$conditional_prob <- predict(glm(treated ~ L, data = data, family = binomial), type = "response")

# POPULATION AVEREAGES
data %>%
  summarize(under1_estimate = mean(1 / m * outcome * treated),
            under0_estimate = mean(1 / m * outcome * !treated),
            marginal_estimate = mean(marginal_prob / m * outcome * treated + 
                                       (1 - marginal_prob) / m * outcome * !treated),
            conditional_estimate = mean(conditional_prob / m * outcome * treated + 
                                          (1 - conditional_prob) / m * outcome * !treated),
            under1_truth = mean(y1),
            under0_truth = mean(y0),
            marginal_truth = mean(y1 * marginal_prob + y0 * (1 - marginal_prob)),
            conditional_truth = mean(y1 * conditional_prob + y0 * (1 - conditional_prob))) %>%
  melt(id = NULL) %>%
  separate(variable, into = c("estimand","quantity")) %>%
  spread(key = quantity, value = value) %>%
  mutate(estimate = round(estimate,2),
         truth = round(truth,2))

# GAPS
truth <- data %>%
  group_by(X) %>%
  summarize(under1_estimate = mean(1 / m * outcome * treated),
            under0_estimate = mean(1 / m * outcome * !treated),
            marginal_estimate = mean(marginal_prob / m * outcome * treated + 
                                       (1 - marginal_prob) / m * outcome * !treated),
            conditional_estimate = mean(conditional_prob / m * outcome * treated + 
                                          (1 - conditional_prob) / m * outcome * !treated),
            under1_truth = mean(y1),
            under0_truth = mean(y0),
            marginal_truth = mean(y1 * marginal_prob + y0 * (1 - marginal_prob)),
            conditional_truth = mean(y1 * conditional_prob + y0 * (1 - conditional_prob))) %>%
  melt(id = "X") %>%
  spread(key = "X", value = value) %>%
  mutate(value = `TRUE` - `FALSE`) %>%
  select(-`TRUE`,-`FALSE`) %>%
  separate(variable, into = c("estimand","quantity")) %>%
  spread(key = quantity, value = value) %>%
  mutate(estimate = round(estimate,2),
         truth = round(truth,2))
truth


# CHECKS ON MY UNDERSTANDING

# Notes about toal weights
data %>%
  group_by(treated) %>%
  summarize(num_observed = n(),
            sum_weight = sum(1 / m))

data %>%
  summarize(total_marginal_weight = sum(marginal_prob / m * treated + (1 - marginal_prob) / m * !treated),
            total_conditional_weight = sum(conditional_prob / m * treated + (1 - conditional_prob) / m * !treated))

# Note the conditional probabilities
data %>%
  group_by(X) %>%
  summarize(factual_prob = mean(p_t),
            conditional_prob = mean(conditional_prob),
            marginal_prob = mean(marginal_prob))

# Note that unadjusted is wrong
data %>%
  group_by(treated) %>%
  summarize(unadjusted = mean(outcome))

####################
# LEARN IN SAMPLES #
####################

draws <- foreach(i = 1:1000, .combine = "rbind") %do% {
  data_sample <- data %>%
    sample_n(100)
  
  m_fit <- glm(treated ~ X + L, data = data_sample, family = binomial)
  m_noGroup_fit <- glm(treated ~ L, data = data_sample, family = binomial)
  
  data_sample %>%
    mutate(p_treated_hat = predict(m_fit, type = "response"),
           mhat = ifelse(treated, p_treated_hat, 1 - p_treated_hat),
           conditional_prob_hat = predict(m_noGroup_fit, type = "response"),
           marginal_prob_hat = mean(treated)) %>%
    group_by(X) %>%
    # Use a Hajek estimator that normalizes the weights
    summarize(under1 = weighted.mean(outcome, w = 1 / mhat * treated),
              under0 = weighted.mean(outcome, w = 1 / mhat * !treated),
              marginal = weighted.mean(outcome, w = marginal_prob_hat / mhat * treated + 
                                         (1 - marginal_prob_hat) / m * !treated),
              conditional = weighted.mean(outcome, w = conditional_prob_hat / mhat * treated + 
                                            (1 - conditional_prob_hat) / m * !treated)) %>%
    melt(id = "X", variable.name = "estimand") %>%
    spread(key = "X", value = value) %>%
    mutate(estimate = `TRUE` - `FALSE`) %>%
    select(-`TRUE`,-`FALSE`)
} %>%
  left_join(truth %>% select(estimand,truth),
            by = "estimand") %>%
  mutate(error = estimate - truth)

# Summarize bias with confidence intervals that can be as tight as needed by increasing sims
draws %>%
  group_by(estimand) %>%
  summarize(bias = mean(error),
            se = sd(error) / sqrt(n())) %>%
  mutate(estimand = fct_relevel(estimand,"under1","under0","marginal","conditional")) %>%
  ggplot(aes(x = estimand, y = bias,
             ymin = bias - qnorm(.975) * se,
             ymax = bias + qnorm(.975) * se)) +
  geom_point() +
  geom_errorbar() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()

# Summarize squared bias, variance, and MSE. Bias is swamped by variance
draws %>%
  group_by(estimand) %>%
  summarize(bias_sq = mean(error) ^ 2,
            variance = var(estimate),
            mse = mean(error ^ 2)) %>%
  mutate(estimand = fct_relevel(estimand,"under1","under0","marginal","conditional")) %>%
  melt(id = "estimand", variable.name = "metric") %>%
  ggplot(aes(x = estimand, y = value)) +
  geom_point() +
  facet_wrap(~metric, nrow = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()


#############################
# LEARN IN WEIGHTED SAMPLES #
#############################

draws <- foreach(i = 1:1000, .combine = "rbind") %do% {
  data_sample <- data %>%
    mutate(p_sampled = plogis(L),
           eta = 1 / p_sampled) %>%
    sample_n(100, weight = p_sampled, replace = T)
  
  m_fit <- glm(treated ~ X + L, data = data_sample, family = binomial, weights = eta)
  m_noGroup_fit <- glm(treated ~ L, data = data_sample, family = binomial, weights = eta)
  
  data_sample %>%
    mutate(p_treated_hat = predict(m_fit, type = "response"),
           mhat = ifelse(treated, p_treated_hat, 1 - p_treated_hat),
           conditional_prob_hat = predict(m_noGroup_fit, type = "response"),
           marginal_prob_hat = weighted.mean(treated, w = eta)) %>%
    group_by(X) %>%
    summarize(under1 = weighted.mean(outcome, w = eta / mhat * treated),
              under0 = weighted.mean(outcome, w = eta / mhat * !treated),
              marginal = weighted.mean(outcome, 
                                       w = eta * marginal_prob_hat / mhat * treated + 
                                         eta * (1 - marginal_prob_hat) / mhat * !treated),
              conditional = weighted.mean(outcome,
                                          w = eta * conditional_prob_hat / mhat * treated + 
                                            eta * (1 - conditional_prob_hat) / mhat * !treated)) %>%
    melt(id = "X", variable.name = "estimand") %>%
    spread(key = "X", value = value) %>%
    mutate(estimate = `TRUE` - `FALSE`) %>%
    select(-`TRUE`,-`FALSE`)
} %>%
  left_join(truth %>% select(estimand,truth),
            by = "estimand") %>%
  mutate(error = estimate - truth)

# Summarize estimates
pop_gap <- mean(data$outcome[data$X]) - mean(data$outcome[!data$X])
draws %>%
  group_by(estimand) %>%
  summarize(ci.min = quantile(estimate, .025),
            ci.max = quantile(estimate, .975),
            estimate = mean(estimate)) %>%
  group_by() %>%
  mutate(quantity = "Mean of\nsample estimates") %>%
  bind_rows(truth %>% 
              select(estimand, truth) %>% 
              rename(estimate = truth) %>% 
              mutate(ci.min = NA, 
                     ci.max = NA,
                     quantity = "Truth in\npopulation")) %>%
  mutate(quantity = fct_rev(quantity),
         estimand = factor(case_when(estimand == "under0" ~ 1,
                              estimand == "under1" ~ 2,
                              estimand == "marginal" ~ 3,
                              estimand == "conditional" ~ 4),
                           labels = c("Equalized\nat T = 0",
                                      "Equalized\nat T = 1",
                                      "Marginal\nequalization",
                                      "Conditional\nequalization"))) %>%
  ggplot(aes(x = estimand, y = estimate,
             ymin = ci.min,
             ymax = ci.max, shape = quantity)) +
  geom_point() +
  geom_errorbar() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = pop_gap,
             linetype = "dotted") +
  annotate(geom = "text", x = -.4, y = pop_gap,
           label = "Unadjusted gap", size = 3, vjust = -1, hjust = 0) +
  scale_shape_manual(values = c(17,16)) +
  scale_x_discrete(name = "Estimand", expand = expand_scale(add = c(1.5,1))) +
  #scale_x_discrete(name = "Estimand", limits = c(0,5)) +
  ylab("Estimate") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.key.height = unit(1,"cm")) +
  ggsave("figures/simple_simulation_estimates.pdf",
         height = 3, width = 6.5)

# Summarize bias with confidence intervals that can be as tight as needed by increasing sims
draws %>%
  group_by(estimand) %>%
  summarize(bias = mean(error),
            se = sd(error) / sqrt(n())) %>%
  mutate(estimand = fct_relevel(estimand,"under1","under0","marginal","conditional")) %>%
  ggplot(aes(x = estimand, y = bias,
             ymin = bias - qnorm(.975) * se,
             ymax = bias + qnorm(.975) * se)) +
  geom_point() +
  geom_errorbar() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("Estimand") +
  ylab("Bias") +
  ggsave("figures/simple_simulation_bias.pdf",
         height = 3, width = 6.5)

# Summarize bias, variance, and MSE
draws %>%
  group_by(estimand) %>%
  summarize(bias_sq = mean(error) ^ 2,
            variance = var(estimate),
            mse = mean(error ^ 2)) %>%
  mutate(estimand = factor(case_when(estimand == "under0" ~ 1,
                                     estimand == "under1" ~ 2,
                                     estimand == "marginal" ~ 3,
                                     estimand == "conditional" ~ 4),
                           labels = c("Equalized\nat T = 0",
                                      "Equalized\nat T = 1",
                                      "Marginal\nequalization",
                                      "Conditional\nequalization"))) %>%
  melt(id = "estimand", variable.name = "metric") %>%
  mutate(metric = factor(case_when(metric == "bias_sq" ~ 1,
                                   metric == "variance" ~ 2,
                                   metric == "mse" ~ 3),
                         labels = c("Bias Squared","Variance","Mean Squared Error"))) %>%
  ggplot(aes(x = estimand, y = value)) +
  geom_point() +
  facet_wrap(~metric, nrow = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  xlab("Estimand") +
  ylab("Performance of Estimator") +
  ggsave("figures/simple_simulation_mse.pdf",
         height = 3, width = 9.5)



