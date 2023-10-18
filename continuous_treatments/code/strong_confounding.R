

library(tidyverse)
theme_set(theme_bw())
library(grid)
library(gridExtra)

n_samp <- 1000

# Standard confounding, binary
binary_standard <- data.frame(L = rep(0:1, each = n_samp / 2)) %>%
  mutate(A = rbinom(n(), size = 1, prob = plogis(2*L - 1))) %>%
  mutate(A = factor(A, labels = c("Control","Treated")),
         L = factor(L, labels = c("Disadvantaged","Advantaged"))) %>%
  ggplot(aes(x = A, fill = L)) +
  geom_bar(position = "dodge",
           alpha = .6,
           color = "black") +
  xlab("Treatment") +
  scale_y_continuous(name = "Treatment Probability Within\nPopulation Subgroup",
                     labels = function(x) paste0(round(100*(x / (n_samp / 2))),"%"),
                     limits = c(0,n_samp / 2)) +
  scale_fill_discrete(name = "Population\nSubgroup") +
  theme(legend.position = "none")

# Strong confounding, binary
binary_strong <- data.frame(L = rep(c(-Inf,Inf), each = n_samp / 2)) %>%
  mutate(A = rbinom(n(), size = 1, prob = plogis(2*L - 1))) %>%
  mutate(A = factor(A, labels = c("Control","Treated")),
         L = factor(L, labels = c("Disadvantaged","Advantaged"))) %>%
  ggplot(aes(x = A, fill = L)) +
  geom_bar(position = "dodge",
           alpha = .6,
           color = "black") +
  xlab("Treatment") +
  scale_y_continuous(name = "Treatment Probability Within\nPopulation Subgroup",
                     labels = function(x) paste0(round(100*(x / (n_samp / 2))),"%"),
                     limits = c(0,n_samp / 2)) +
  scale_fill_discrete(name = "Population\nSubgroup") +
  theme(legend.position = "none")

# Standard confounding, continuous
sd_for_standard <- 1
continuous_standard <- data.frame(L = rep(0:1, each = n_samp / 2)) %>%
  group_by(L) %>%
  mutate(mu = L - .5,
         quantile = (1:n()) / n(),
         A = qnorm(quantile, mean = mu, sd = sd_for_standard),
         pdf = dnorm(A, mean = mu, sd = sd_for_standard)) %>%
  # Truncate distribution and renormalize pdf
  filter(A >= -1 & A <= 1) %>%
  mutate(pdf = pdf / (pnorm(1,mu,sd_for_standard) - pnorm(-1,mu,sd_for_standard))) %>%
  ungroup() %>%
  mutate(L = factor(L, labels = c("Disadvantaged","Advantaged"))) %>%
  ggplot(aes(x = A, y = pdf, fill = L)) +
  geom_ribbon(aes(ymin = 0, ymax = pdf),
              alpha = .6) +
  geom_line() +
  xlab("Treatment") +
  scale_y_continuous(name = "Treatment Density Within\nPopulation Subgroup",
                     limits = c(0,1.3),
                     # This is to make the plot sizes match;
                     # labels will be invisible in theme
                     labels = function(x) paste0(round(100*x),"%")) +
  scale_fill_discrete(name = "Population\nSubgroup") +
  theme(legend.position = "none",
        axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_line(color = "white"))

# Strong confounding, continuous
sd_for_strong <- .35
continuous_strong <- data.frame(L = rep(0:1, each = n_samp / 2)) %>%
  group_by(L) %>%
  mutate(mu = L - .5,
         quantile = (1:n()) / n(),
         A = qnorm(quantile, mean = mu, sd = sd_for_strong),
         pdf = dnorm(A, mean = mu, sd = sd_for_strong)) %>%
  # Truncate distribution and renormalize pdf
  filter(A >= -1 & A <= 1) %>%
  mutate(pdf = pdf / (pnorm(1,mu,sd_for_strong) - pnorm(-1,mu,sd_for_strong))) %>%
  ungroup() %>%
  mutate(L = factor(L, labels = c("Disadvantaged","Advantaged"))) %>%
  ggplot(aes(x = A, y = pdf, fill = L)) +
  geom_ribbon(aes(ymin = 0, ymax = pdf),
              alpha = .6) +
  geom_line() +
  xlab("Treatment") +
  scale_y_continuous(name = "Treatment Density Within\nPopulation Subgroup",
                     limits = c(0,1.3),
                     # This is to make the plot sizes match;
                     # labels will be invisible in theme
                     labels = function(x) paste0(round(100*x),"%")) +
  scale_fill_discrete(name = "Population\nSubgroup") +
  theme(legend.position = "none",
        axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_line(color = "white"))

# Get the legend
legend_confounding <- ggpubr::as_ggplot(ggpubr::get_legend(
  binary_standard +
    theme(legend.position = "right")
))

# Save plots
for (p_name in c(
  "binary_standard","binary_strong",
  "continuous_standard","continuous_strong",
  "legend_confounding"
)) {
  ggsave(paste0("figures/",p_name,".pdf"),
         plot = get(p_name),
         height = 2.5, width = 3)
}

