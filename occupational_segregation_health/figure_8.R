
sink("figures/log_figure_8.txt")
print(Sys.time())

d_onset <- readRDS("intermediate/d_onset.RDS")

d_onset %>%
  group_by(YEAR, questionnaire_redesign) %>%
  summarize(onset = weighted.mean(y, w = ASECWT)) %>%
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
       height = 3, width = 6.5)

d_onset %>%
  filter(grepl("Non-Hispanic",RACE)) %>%
  mutate(RACE = gsub("-| ","",RACE)) %>%
  group_by(YEAR, RACE, questionnaire_redesign) %>%
  summarize(onset = weighted.mean(y, w = ASECWT)) %>%
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
       height = 3, width = 6.5)

sink()