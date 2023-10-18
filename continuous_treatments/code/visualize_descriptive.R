
library(tidyverse)

source("code/label_outcomes_treatments.R")

descriptive_smooths <- readRDS("intermediate/descriptive_smooths.RDS")
descriptive_bins <- readRDS("intermediate/descriptive_bins.RDS")

descriptive_smooths %>%
  filter(estimand == "yhat") %>%
  mutate(outcome = fct_rev(outcome)) %>%
  ggplot(aes(x = income, y = estimate,
             color = outcome,
             fill = outcome)) +
  geom_ribbon(aes(ymin = ci.min, ymax = ci.max),
              alpha = .2, color = NA) +
  geom_line() +
  geom_point(data = descriptive_bins) +
  ylab("Probability of Educational Milestone") +
  scale_x_continuous(name = "Parent Income",
                     labels = scales::label_dollar()) +
  scale_color_discrete(name = "Outcome",
                       labels = label_outcomes_treatments) +
  scale_fill_discrete(name = "Outcome",
                      labels = label_outcomes_treatments) +
  theme(legend.key.height = unit(.4,"in"))
ggsave("figures/descriptive_all_outcomes.pdf",
       height = 5, width = 6.5)

descriptive_smooths %>%
  filter(outcome == "enrolled_any") %>%
  filter(estimand == "yhat") %>%
  ggplot(aes(x = income, y = estimate)) +
  geom_hline(yintercept = c(0,1),
             color = "gray",
             linetype = "dashed") +
  geom_ribbon(aes(ymin = ci.min, ymax = ci.max),
              alpha = .2, color = NA) +
  geom_line() +
  geom_point(data = descriptive_bins %>%
               filter(outcome == "enrolled_any")) +
  ylab("Probability of College Enrollment") +
  scale_x_continuous(name = "Family Income at Age 17",
                     labels = scales::label_dollar()) +
  scale_color_discrete(name = "Outcome",
                       labels = label_outcomes_treatments) +
  scale_fill_discrete(name = "Outcome",
                      labels = label_outcomes_treatments) +
  theme(legend.key.height = unit(.4,"in"),
        plot.margin = unit(c(6,12,6,12), "pt"))
ggsave("figures/descriptive.pdf",
       height = 3, width = 4)

# Examine the average association with $10,000 extra
print(
  descriptive_smooths %>%
    filter(estimand == "delta_10000") %>%
    select(outcome, estimate, se, ci.min, ci.max) %>%
    mutate_if(is.numeric, round, digits = 3)
)
