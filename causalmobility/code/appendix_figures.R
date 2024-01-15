# Dependencies
library(here)
library(tidyverse)
theme_set(theme_bw())

sink(here("logs","appendix_figures.txt"))
print(Sys.time())

# Import analysis panel
nlsy_analysis <- tibble(readRDS(here("data/nlsy_panel.RDS")))

# Plot the age distribution of these folks to see how close we get to age 40
# in general.
nlsy_analysis |>
  group_by(resp_occ_age) |>
  summarise(N = n(), .groups = "drop") |>
  mutate(prop = N / sum(N)) |>
  ggplot(aes(x = resp_occ_age, y = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(100*prop),"%")),
            nudge_y = .02) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank()) +
  scale_x_continuous(name = "Age of Child at Occupation Measurement",
                     breaks = 35:45) +
  ylab("Number of Respondents")

ggsave(here("figures", "closest_to_40_age_dist.pdf"), width = 5, height = 3)

sessionInfo()
sink()