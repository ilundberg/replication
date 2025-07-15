
sink("figures/log_figure_6.txt")
print(Sys.time())

# FIGURE 6A: DISPARITY IN LEVELS

full_population <- readRDS("intermediate/full_population.RDS")

point <- full_population |>
  filter(YEAR >= 2005) |>
  group_by(RACE) |>
  summarize(point = weighted.mean(y, w = ASECWT))

estimate <- foreach(i = 1:160, .combine = "rbind") %do% {
  this_weight <- readRDS(paste0("intermediate/REPWTP",i,".Rds"))
  colnames(this_weight)[5] <- "weight"
  full_population |>
    left_join(
      this_weight, 
      by = join_by(YEAR, SERIAL, PERNUM)
    ) |>
    filter(YEAR >= 2005) |>
    group_by(RACE) |>
    summarize(y = weighted.mean(y, w = weight))
} |>
  left_join(point, by = join_by(RACE)) |>
  group_by(RACE) |>
  summarize(
    point = unique(point),
    se = sqrt(4 / 160 * sum((y - point) ^ 2))
  )

estimate |>
  mutate(RACE = fct_relevel(RACE,"Non-Hispanic Black","Non-Hispanic White","Hispanic","Other")) %>%
  ggplot(aes(x = RACE, y = point,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100 * point, 1), nsmall = 1),"%"))) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  geom_text(aes(y = 0, vjust = -.5,
                label = paste0(format(round(100 * point,1), nsmall = 1), "%")),
            color = "white", fontface = "bold", size = 6) +
  scale_y_continuous(name = 'Onset of Work-Limiting Disability') +
  xlab("Race / Ethnicity") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"))
ggsave("figures/disability_by_race.pdf",
       height = 3.5, width = 5)


# FIGURE 6B: DISPARITY IN ONSET
  
d_onset <- readRDS("intermediate/d_onset.RDS")

point <- d_onset |>
  filter(YEAR >= 2005) |>
  group_by(RACE) |>
  summarize(point = weighted.mean(y, w = ASECWT))

estimate <- foreach(i = 1:160, .combine = "rbind") %do% {
  this_weight <- readRDS(paste0("intermediate/REPWTP",i,".Rds"))
  colnames(this_weight)[5] <- "weight"
  d_onset |>
    filter(YEAR >= 2005) |>
    left_join(
      this_weight, 
      by = join_by(CPSIDP, YEAR)
    ) |>
    filter(!is.na(weight)) |>
    group_by(RACE) |>
    summarize(y = weighted.mean(y, w = weight))
} |>
  left_join(point, by = join_by(RACE)) |>
  group_by(RACE) |>
  summarize(
    point = unique(point),
    se = sqrt(4 / 160 * sum((y - point) ^ 2))
  )

estimate |>
  mutate(RACE = fct_relevel(RACE,"Non-Hispanic Black","Non-Hispanic White","Hispanic","Other")) %>%
  ggplot(aes(x = RACE, y = point,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100 * point, 1), nsmall = 1),"%"))) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  geom_text(aes(y = 0, vjust = -.5,
                label = paste0(format(round(100 * point,1), nsmall = 1), "%")),
            color = "white", fontface = "bold", size = 6) +
  scale_y_continuous(name = 'Onset of Work-Limiting Disability') +
  xlab("Race / Ethnicity") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"))
ggsave("figures/onset_by_race.pdf",
       height = 3.5, width = 5)

sink()