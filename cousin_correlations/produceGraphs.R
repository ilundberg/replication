
# Replication code for:
# Does Opportunity Skip Generations?
# Reassessing Evidence from Sibling and Cousin Correlations
# Code by Ian Lundberg
# ilundberg@princeton.edu

# run_all.R sources all code files in order

# This file produces the graphs in the paper.

library(tidyverse)
library(reshape2)
library(doParallel)


setwd("C:/Users/iandl/Documents/Empirical")

## Make a plot of long-run persistence for the introduction
data.frame(
  Generation = 1:10
) %>%
  mutate(value = .5 ^ Generation) %>%
  ggplot(aes(x = Generation, y = value)) +
  geom_line() +
  geom_point() +
  ylab("Permanent income correlation") +
  xlab("Generations elapsed") +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_segment(data = data.frame(x = c(2.5,3.25,5),
                                 y = c(.45,.35,.25),
                                 xend = c(1.25,2.2,4.1),
                                 yend = c(.49,.26,.08)),
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_label(data = data.frame(x = c(2.5,3.25,4), y = c(.45,.35,.25),
                               label = c("Correlation(Parent, Offspring) = 0.50",
                                         "Correlation(Grandparent, Offspring) = 0.25",
                                         "Correlation(Great-great-grandparent, Offspring) = 0.06")),
             aes(x = x, y = y, label = label),
             size = 2,
             hjust = 0) +
  scale_x_continuous(breaks = c(1,4,7,10)) +
  ggsave("output/persistence.pdf",
         height = 2.5, width = 4)

# Plot prior distributions
data.frame(x = seq(0,3,.01)) %>% 
  mutate(half_cauchy = 2*dcauchy(x)) %>%
  ggplot(aes(x = x, ymax = half_cauchy, ymin = 0)) + 
  geom_ribbon(alpha = .4, fill = "blue") +
  xlab("Standard deviation of random intercepts") +
  ylab("Prior density") +
  theme_bw() +
  ggsave("output/half_cauchy.pdf",
         height = 2, width = 4)

# Plot estimated population-average age trajectory
read_csv("intermediate_files/beta_estimates.csv") %>%
  melt(id = c("draw","chain")) %>%
  mutate(variable = str_replace_all(variable,"beta|\\.",""),
         age = as.numeric(variable) + 24) %>%
  group_by(age) %>%
  summarize(estimate = mean(value),
            ci_min = quantile(value, .025),
            ci_max = quantile(value, .975)) %>%
  ggplot(aes(x = age, y = estimate,
             ymin = ci_min, ymax = ci_max)) +
  geom_ribbon(alpha = .4, fill = "blue") +
  geom_line(color = "blue") +
  xlab("Age") + ylab("Coefficient") +
  theme_bw() +
  ggsave("output/age_trend.pdf",
         height = 3, width = 4)

# Trace plot of standard deviation components
read_csv("intermediate_files/sigma_estimates.csv") %>%
  melt(id = c("draw","chain")) %>%
  mutate(variable = case_when(variable == "sigma_cousinset" ~ "A. SD(Cousin set component)",
                              variable == "sigma_siblingset" ~ "B. SD(Sibling set component)",
                              variable == "sigma_person" ~ "C. SD(Person component)",
                              variable == "sigma_observation" ~ "D. SD(Observation component)")) %>%
  ggplot(aes(x = draw, y = value, color = factor(chain))) +
  geom_line() +
  facet_wrap(~variable, ncol = 1, scales = "free") +
  theme_bw() +
  xlab("Draw") +
  ylab("Value") +
  scale_color_discrete(name = "Chain") +
  ggsave("output/trace.pdf",
         height = 4, width = 5)

# Report results for key quantities and test statistic
estimates_permanentLogIncome <- read_csv("intermediate_files/estimates_permanentLogIncome.csv")
empirical_results <- estimates_permanentLogIncome %>%
  mutate(BeckerTomes = Sibling ^ 2,
         Difference = Cousin - Sibling ^ 2) %>%
  melt(id = NULL) %>%
  mutate(variable = case_when(variable == "Sibling" ~ "1. Sibling\ncorrelation",
                              variable == "BeckerTomes" ~ "2. Squared sibling\ncorrelation",
                              variable == "Cousin" ~ "3. Cousin\ncorrelation",
                              variable == "Difference" ~ "4. Test statistic:\n(3) - (2)")) %>%
  group_by(variable) %>%
  summarize(posterior_mean = mean(value),
            ci.min = quantile(value, .025),
            ci.max = quantile(value, .975))

# Report in a table
write_csv(empirical_results, path = "output/empirical_results.csv")

# Report in a graph
empirical_results %>%
  ggplot(aes(x = variable, y = posterior_mean,
             label = format(round(posterior_mean,2),digits = 2),
             ymin = ci.min, ymax = ci.max)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar() +
  geom_label(label.padding = unit(.1, "lines")) +
  ylab("Estimate") + 
  xlab("Estimand") +
  scale_x_discrete(name = element_blank()) +
  theme_bw() +
  ggsave("output/comparison.pdf",
         height = 3, width = 6.5)

# SEO robustness check
read_csv("intermediate_files/estimates_seo.csv") %>%
  mutate(BeckerTomes = Sibling ^ 2,
         Difference = Cousin - Sibling ^ 2) %>%
  melt(id = NULL) %>%
  mutate(variable = case_when(variable == "Sibling" ~ "1. Sibling\ncorrelation",
                              variable == "BeckerTomes" ~ "2. Squared sibling\ncorrelation",
                              variable == "Cousin" ~ "3. Cousin\ncorrelation",
                              variable == "Difference" ~ "4. Test statistic:\n(3) - (2)")) %>%
  group_by(variable) %>%
  summarize(posterior_mean = mean(value),
            ci.min = quantile(value, .025),
            ci.max = quantile(value, .975)) %>%
  ggplot(aes(x = variable, y = posterior_mean,
             label = format(round(posterior_mean,2),digits = 2),
             ymin = ci.min, ymax = ci.max)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar() +
  geom_label(label.padding = unit(.1, "lines")) +
  ylab("Estimate") + 
  xlab("Estimand") +
  scale_x_discrete(name = element_blank()) +
  theme_bw() +
  ggsave("output/seo_comparison.pdf",
         height = 3, width = 6.5)

