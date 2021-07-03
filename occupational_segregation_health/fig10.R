# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))

# Record printed output in a text file
sink("logs/fig10.txt")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load packages
library(tidyverse)
library(reshape2)

# Load data for the figures
d_onset <- readRDS("intermediate/d_onset.Rds")

# Panel A
plot <- d_onset %>%
  group_by(YEAR, questionnaire_redesign) %>%
  summarize(onset = weighted.mean(y, w = ASECWT),
            .groups = "drop") %>%
  ggplot(aes(x = YEAR, y = onset, color = factor(questionnaire_redesign))) +
  geom_vline(xintercept = 2013, color = "gray") +
  geom_vline(xintercept = 2014.5, color = "gray") +
  geom_point() +
  theme_bw() +
  ylab("\nOnset of Work-Limiting Disability\nAmong the Employed") +
  ylim(c(0,.03)) +
  xlab("Year") +
  scale_color_manual(values = c("black","gray")) +
  geom_hline(yintercept = 0, color = "gray") +
  theme(legend.position = "none")
ggsave("figures/hazard_trend.pdf",
       height = 3, width = 6.5, plot = plot)

# Panel B
plot <- d_onset %>%
  filter(grepl("Non-Hispanic",RACE)) %>%
  mutate(RACE = gsub("-| ","",RACE)) %>%
  group_by(YEAR, RACE, questionnaire_redesign) %>%
  summarize(onset = weighted.mean(y, w = ASECWT),
            .groups = "drop") %>%
  spread(key = RACE, value = onset) %>%
  mutate(disparity = NonHispanicBlack - NonHispanicWhite) %>%
  ggplot(aes(x = YEAR, y = disparity, color = factor(questionnaire_redesign))) +
  geom_vline(xintercept = 2013, color = "gray") +
  geom_vline(xintercept = 2014.5, color = "gray") +
  geom_point() +
  theme_bw() +
  ylab("Black-White Disparity in\nOnset of Work-Limiting Disability\nAmong the Employed") +
  xlab("Year") +
  scale_color_manual(values = c("black","gray")) +
  theme(legend.position = "none")
ggsave("figures/hazard_disparity_trend.pdf",
       height = 3, width = 6.5, plot = plot)

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))