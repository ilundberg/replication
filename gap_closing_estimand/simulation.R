# Ian Lundberg (ilundberg@princeton.edu)

t0 <- Sys.time()

library(tidyverse)
library(reshape2)
library(foreach)
library(doParallel)

setwd("/Users/iandl/Dropbox/dissertation/gap_closing_estimands")

# Define the number of simulations to draw
num_reps <- 1000
# Define the size of the samples
sample_size <- 10000
# For determining the truth by simulation
# and for use in the weighted sample illustration,
# define a very large population size that we will
# view as though it is infinite.
population_size <- 1000000

# Prepare for parallel computing
cl <- makeCluster(3)
registerDoParallel(cl)

# Code to run the estimator within the simulation
# This function assumes a binary treatment "treated"
# I assume data contains a variable "pi" for the unit-specific treatment assignment probability under the intervention
gap_closing_estimator <- function(
  m_formula = formula(treated ~ X + L),
  g_formula = formula(outcome ~ (X + L)*treated),
  g_family = "gaussian",
  data
) {
  m_fit <- glm(m_formula, data = data, family = binomial, weights = weights)
  g_fit <- glm(g_formula, data = data, family = g_family, weights = weights)
  data %>%
    mutate(mhat = predict(m_fit, type = "response"),
           ghat0 = predict(g_fit, newdata = data %>% mutate(treated = 0), type = "response"),
           ghat1 = predict(g_fit, newdata = data %>% mutate(treated = 1), type = "response")) %>%
    group_by(X) %>%
    summarize(outcomeModel = weighted.mean(ghat1 * pi + ghat0 * (1 - pi), w = weights),
              treatmentModel = weighted.mean(outcome, w = weights * ifelse(treated, pi, 1 - pi) / ifelse(treated, mhat, 1 - mhat)),
              augmentation = weighted.mean(ifelse(treated,ghat1,ghat0) - outcome, w = weights * ifelse(treated, pi, 1 - pi) / ifelse(treated, mhat, 1 - mhat))) %>%
    mutate(aipw = outcomeModel - augmentation) %>%
    select(-augmentation) %>%
    mutate(X = ifelse(X, "X1", "X0")) %>%
    melt(id = "X", variable.name = "method") %>%
    spread(key = "X", value = value) %>%
    mutate(gap = X1 - X0)
}

# Simulation: Treatment closes gap
draw_sim <- function(n) {
  data.frame(L = rnorm(n)) %>%
    mutate(X = rbinom(n(), 1, prob = plogis(L)),
           p_t = plogis(X + L + X*L),
           marginal_prob = mean(p_t),
           treated = rbinom(n(), 1, prob = p_t),
           m = ifelse(treated == 1, p_t, 1 - p_t),
           epsilon = rnorm(n),
           y0 = rnorm(n,X + L,1),
           y1 = rnorm(n,0,1),
           outcome = ifelse(treated ==  1, y1, y0),
           weights = 1)
}

true_gap <- (draw_sim(population_size) %>%
               group_by(X) %>%
               summarize(post_intervention_mean = mean(y0)) %>%
               spread(key = X, value = post_intervention_mean) %>%
               mutate(gap = `1` - `0`))$gap

sim_estimates <- foreach(i = 1:num_reps, .combine = "rbind", .packages = c("tidyverse","reshape2")) %dopar% {
  sample <- draw_sim(sample_size)
  both_correct <- gap_closing_estimator(m_formula = formula(treated ~ X + L + X*L),
                                        g_formula = formula(outcome ~ (X + L)*treated),
                                        g_family = "gaussian",
                                        data = sample %>%
                                          mutate(pi = 0)) %>%
    mutate(setting = "Both\nmodels\ncorrect")
  wrong_treatment <- gap_closing_estimator(m_formula = formula(treated ~ X + L),
                                           g_formula = formula(outcome ~ (X + L)*treated),
                                           g_family = "gaussian",
                                           data = sample %>%
                                             mutate(pi = 0)) %>%
    mutate(setting = "Wrong\ntreatment\nmodel")
  wrong_outcome <- gap_closing_estimator(m_formula = formula(treated ~ X + L + X*L),
                                         g_formula = formula(outcome ~ X + L),
                                         g_family = "gaussian",
                                         data = sample %>%
                                           mutate(pi = 0)) %>%
    mutate(setting = "Wrong\noutcome\nmodel")
  return(both_correct %>%
           bind_rows(wrong_treatment) %>%
           bind_rows(wrong_outcome))
}

# Plots
sim_estimates %>%
  filter(method == "outcomeModel") %>%
  ggplot(aes(x = gap)) +
  facet_wrap(~setting, nrow = 1, strip.position = "bottom") +
  geom_density(fill = "gray") +
  geom_vline(xintercept = true_gap, linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(breaks = NULL, limits = c(.4,3),
                     name = "Gap-closing estimand") +
  scale_y_continuous(breaks = NULL, name = NULL) +
  ggtitle("Outcome modeling estimates") +
  coord_flip() +
  theme(axis.title.y = element_text(color = "white",size = 10),
        plot.title = element_text(hjust = .5, size = 10)) +
  ggsave("figures/simulated_outcomeModel.pdf",
         height = 2, width = 2.15)

sim_estimates %>%
  filter(method == "treatmentModel") %>%
  ggplot(aes(x = gap)) +
  facet_wrap(~setting, nrow = 1, strip.position = "bottom") +
  geom_density(fill = "gray") +
  geom_vline(xintercept = true_gap, linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(breaks = NULL, limits = c(.4,3),
                     name = "Gap-closing estimand") +
  scale_y_continuous(breaks = NULL, name = NULL) +
  ggtitle("Treatment modeling estimates") +
  coord_flip() +
  theme(axis.title.y = element_text(color = "white",size = 10),
        plot.title = element_text(hjust = .5, size = 10)) +
  ggsave("figures/simulated_treatmentModel.pdf",
         height = 2, width = 2.15)

sim_estimates %>%
  filter(method == "aipw") %>%
  ggplot(aes(x = gap)) +
  facet_wrap(~setting, nrow = 1, strip.position = "bottom") +
  geom_density(fill = "gray") +
  geom_vline(xintercept = true_gap, linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(breaks = NULL, limits = c(.4,3),
                     name = "Gap-closing estimand") +
  scale_y_continuous(breaks = NULL, name = NULL) +
  ggtitle("Doubly-robust estimates") +
  coord_flip() +
  theme(plot.title = element_text(hjust = .5, size = 10),
        axis.title.y = element_text(size = 10)) +
  ggsave("figures/simulated_aipw.pdf",
         height = 2, width = 2.15)

########################################################################
# Repeat the simulation with unequal probabilities of sample selection #
########################################################################

population <- draw_sim(population_size) %>%
  mutate(weights = plogis(L),
         p_sampled = 1 / weights)

sim_complexSample_estimates <- foreach(i = 1:num_reps, .combine = "rbind", .packages = c("tidyverse","reshape2")) %dopar% {
  sample <- population %>%
    sample_n(sample_size,
             weight = p_sampled,
             replace = T)
  both_correct <- gap_closing_estimator(m_formula = formula(treated ~ X + L + X*L),
                                        g_formula = formula(outcome ~ (X + L)*treated),
                                        g_family = "gaussian",
                                        data = sample %>%
                                          mutate(pi = 0)) %>%
    mutate(setting = "Both\nmodels\ncorrect")
  wrong_treatment <- gap_closing_estimator(m_formula = formula(treated ~ X + L),
                                           g_formula = formula(outcome ~ (X + L)*treated),
                                           g_family = "gaussian",
                                           data = sample %>%
                                             mutate(pi = 0)) %>%
    mutate(setting = "Wrong\ntreatment\nmodel")
  wrong_outcome <- gap_closing_estimator(m_formula = formula(treated ~ X + L + X*L),
                                         g_formula = formula(outcome ~ X + L),
                                         g_family = "gaussian",
                                         data = sample %>%
                                           mutate(pi = 0)) %>%
    mutate(setting = "Wrong\noutcome\nmodel")
  
  return(both_correct %>%
           bind_rows(wrong_treatment) %>%
           bind_rows(wrong_outcome))
}

# Plots
sim_complexSample_estimates %>%
  filter(method == "outcomeModel") %>%
  ggplot(aes(x = gap)) +
  facet_wrap(~setting, nrow = 1, strip.position = "bottom") +
  geom_density(fill = "gray") +
  geom_vline(xintercept = true_gap, linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(breaks = NULL, limits = c(.4,3),
                     name = "Gap-closing estimand") +
  scale_y_continuous(breaks = NULL, name = NULL) +
  ggtitle("Outcome modeling estimates") +
  coord_flip() +
  theme(axis.title.y = element_text(color = "white",size = 10),
        plot.title = element_text(hjust = .5, size = 10)) +
  ggsave("figures/simulated_complexSample_outcomeModel.pdf",
         height = 2, width = 2.15)

sim_complexSample_estimates %>%
  filter(method == "treatmentModel") %>%
  ggplot(aes(x = gap)) +
  facet_wrap(~setting, nrow = 1, strip.position = "bottom") +
  geom_density(fill = "gray") +
  geom_vline(xintercept = true_gap, linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(breaks = NULL, limits = c(.4,3),
                     name = "Gap-closing estimand") +
  scale_y_continuous(breaks = NULL, name = NULL) +
  ggtitle("Treatment modeling estimates") +
  coord_flip() +
  theme(axis.title.y = element_text(color = "white",size = 10),
        plot.title = element_text(hjust = .5, size = 10)) +
  ggsave("figures/simulated_complexSample_treatmentModel.pdf",
         height = 2, width = 2.15)

sim_complexSample_estimates %>%
  filter(method == "aipw") %>%
  ggplot(aes(x = gap)) +
  facet_wrap(~setting, nrow = 1, strip.position = "bottom") +
  geom_density(fill = "gray") +
  geom_vline(xintercept = true_gap, linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(breaks = NULL, limits = c(.4,3),
                     name = "Gap-closing estimand") +
  scale_y_continuous(breaks = NULL, name = NULL) +
  ggtitle("Doubly-robust estimates") +
  coord_flip() +
  theme(plot.title = element_text(hjust = .5, size = 10),
        axis.title.y = element_text(size = 10)) +
  ggsave("figures/simulated_complexSample_aipw.pdf",
         height = 2, width = 2.15)
