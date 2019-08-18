

# Replication code for:
# Does Opportunity Skip Generations?
# Reassessing Evidence from Sibling and Cousin Correlations
# Code by Ian Lundberg
# ilundberg@princeton.edu

# run_all.R sources all code files in order

# This file conducts a comparison with frequentist results

## File to conduct frequentist comparison
library(tidyverse)
library(reshape2)
library(nlme)

setwd("C:/Users/iandl/Documents/Empirical")

bayes_results <- estimates_permanentLogIncome <- read_csv("intermediate_files/estimates_permanentLogIncome.csv")
load("intermediate_files/d.Rdata")

fit <- lme(log_familyIncome ~ factor(age),
           random = ~ 1 | cousinset/siblingset/person,
           data = d)

# theta_hat is the log standard deviation of person, sibling, and cousin intercepts
theta_hat <- attr(fit$apVar,"Pars")[3:1] # reverse order to match my analytical derivation
sigma_hat <- fit$apVar[3:1,3:1]

# Function to get cousin correlation
f <- function(theta) {
  exp(theta[1]) ^ 2 / (exp(theta[1]) ^ 2 + exp(theta[2]) ^ 2 + exp(theta[3]) ^ 2)
}


# Function to get sibling correlation
g <- function(theta) {
  (exp(2*theta[1]) + exp(2*theta[2])) / (exp(2*theta[1]) + exp(2*theta[2]) + exp(2*theta[3]))
}

# Function to get test statistic
h <- function(theta) {
  f(theta) - g(theta) ^ 2
}

# Gradient functions
grad_f <- function(theta) {
  denominator <- (exp(2*theta[1]) + exp(2*theta[2]) + exp(2*theta[3])) ^ 2
  c(2*exp(2*theta[1] + 2*theta[2]) + 2*exp(2*theta[1] + 2*theta[3]),
    -2*exp(2*theta[1] + 2*theta[2]),
    -2*exp(2*theta[1] + 2*theta[3])) /
    denominator
}
grad_g <- function(theta) {
  denominator <- (exp(2*theta[1]) + exp(2*theta[2]) + exp(2*theta[3])) ^ 2
  c(2*exp(2*theta[1] + 2*theta[3]),
    2*exp(2*theta[2] + 2*theta[3]),
    -2*exp(2*theta[1] + 2*theta[3]) - 2*exp(2*theta[2] + 2*theta[3])) /
    denominator
}
grad_g_squared <- function(theta) {
  2 * g(theta) *grad_g(theta)
}
grad_h <- function(theta) {
  c(grad_f(theta)[1] - 2*g(theta)*grad_g(theta)[1],
    grad_f(theta)[2] - 2*g(theta)*grad_g(theta)[2],
    grad_f(theta)[3] - 2*g(theta)*grad_g(theta)[3])
}

frequentist_results <- data.frame(
  quantity = "Cousin",
  estimate = f(theta_hat),
  variance = t(grad_f(theta_hat)) %*% sigma_hat %*% grad_f(theta_hat)
) %>%
  bind_rows(data.frame(
    quantity = "Sibling",
    estimate = g(theta_hat),
    variance = t(grad_g(theta_hat)) %*% sigma_hat %*% grad_g(theta_hat)
  )) %>%
  bind_rows(data.frame(
    quantity = "SiblingSq",
    estimate = g(theta_hat) ^ 2,
    variance = t(grad_g_squared(theta_hat)) %*% sigma_hat %*% grad_g_squared(theta_hat)
  )) %>%
  bind_rows(data.frame(
    quantity = "tau",
    estimate = h(theta_hat),
    variance = t(grad_h(theta_hat)) %*% sigma_hat %*% grad_h(theta_hat)
  )) %>%
  mutate(ci.min = estimate - qnorm(.975)*sqrt(variance),
         ci.max = estimate + qnorm(.975)*sqrt(variance),
         Approach = "Frequentist")

bayes_results %>%
  mutate(tau = Cousin - Sibling ^ 2,
         SiblingSq = Sibling ^ 2) %>%
  melt(id = NULL, variable.name = "quantity") %>%
  group_by(quantity) %>%
  summarize(estimate = mean(value),
            variance = var(value),
            ci.min = quantile(value, .025),
            ci.max = quantile(value, .975)) %>%
  mutate(Approach = "Bayesian") %>%
  bind_rows(frequentist_results) %>%
  mutate(quantity = case_when(quantity == "Sibling" ~ "1. Sibling\ncorrelation",
                              quantity == "SiblingSq" ~ "2. Squared sibling\ncorrelation",
                              quantity == "Cousin" ~ "3. Cousin\ncorrelation",
                              quantity == "tau" ~ "4. Test statistic:\n(3) - (2)")) %>%
  ggplot(aes(x = quantity, y = estimate, ymin = ci.min, ymax = ci.max,
             label = format(round(estimate,2), digits = 2),
             color = Approach)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .5, position = position_dodge(width = .8)) +
  geom_label(label.padding = unit(.1, "lines"), show.legend = F,
             position = position_dodge(width = .8)) +
  ylab("Estimate") + 
  xlab("Estimand") +
  scale_x_discrete(name = element_blank()) +
  scale_color_manual(values = c("blue","seagreen4")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggsave("output/frequentist_comparison.pdf",
         height = 3, width = 6.5)