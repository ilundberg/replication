
sink("figures/log_figure_14.txt")
print(Sys.time())

readRDS("intermediate/full_population.RDS") %>%
  group_by(YEAR) %>%
  summarize(weight = sum(ASECWT)) %>%
  group_by() %>%
  mutate(weight = weight / sum(weight)) %>%
  ggplot(aes(x = YEAR, y = weight))  +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  ylab("Proportion of total weight")
ggsave("figures/weight_over_time.pdf",
       height = 3, width = 5)

sink()