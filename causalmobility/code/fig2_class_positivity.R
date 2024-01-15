# Dependencies
library(here)
library(tidyverse)
theme_set(theme_bw())

sink(here("logs","fig2_class_positivity.txt"))
print(Sys.time())

# Import analysis panel
nlsy_analysis <- tibble(readRDS(here("data/nlsy_panel.RDS")))

nlsy_analysis |>
  group_by(parental_educ, parental_egp) |>
  summarize(weight = sum(weight), .groups = "drop_last") |>
  mutate(weight = weight / sum(weight)) |>
  ggplot(aes(y = parental_egp, x = weight)) +
  geom_bar(stat = "identity", fill = "steelblue3") +
  facet_wrap(~parental_educ, ncol = 4) +
  geom_text(aes(label = paste0(round(100*weight),"%")),
            hjust = -.15) +
  scale_x_continuous(
    name = "Probability of Parent Occupational Class\nGiven Parent Education",
    limits = c(0,1.1),
    breaks = c(0,.5,1),
    labels = function(x) paste0(round(100*x),"%")
  ) +
  scale_y_discrete(name = "Parent Occupational Class",
                   labels = function(x) gsub(" ","\n",x)) +
  theme(panel.spacing = unit(1, "lines"))
ggsave(here("figures/class_positivity.pdf"),
       height = 3, width = 7)

sessionInfo()
sink()