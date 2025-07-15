
sink("figures/log_figure_16.txt")
print(Sys.time())

d_onset <- readRDS("intermediate/d_onset.RDS")
source("code/estimator_functions.R")

# In the 2009 and later period, subset to those without any reported difficulties

print("Alternative specification: Filter on additional controls")
d_alt_all <- d_onset %>%
  filter(YEAR >= 2009 & !DIFFANY) %>%
  group_by(OCC2010, EDUC) %>%
  mutate(in_support = n_distinct(RACE) == 4) %>%
  group_by()
d_alt <- d_alt_all %>% filter(in_support)
print("Sample size:")
print(nrow(d_alt))
print("Number of occupations")
d_alt_all %>%
  group_by(OCC2010) %>%
  filter(1:n() == 1) %>%
  group_by() %>%
  summarize(total = n(),
            on_support = sum(in_support),
            off_support = sum(!in_support))
print("Proportion of weight on common support")
print(d_alt_all %>%
        summarize(in_support = weighted.mean(in_support, w = ASECWT)))
extra_controls <- counterfactual_estimator(data = d_alt)

# VISUALIZE

extra_controls %>%
  group_by(estimand) |>
  mutate(
    reference = unique(na.omit(case_when(RACE == "Non-Hispanic White" ~ estimate))),
    estimate = estimate - reference
  ) |>
  select(-reference) |>
  filter(estimate != 0) |>
  mutate(RACE = paste0(RACE,"\n- Non-Hispanic White")) |>
  group_by() %>%
  rename(Estimand = estimand) %>%
  mutate(Estimand = case_when(Estimand == "factual" ~ "Factual Disparity",
                              Estimand == "counterfactual_within_educ" ~ "Counterfactual Disparity"),
         Estimand = fct_rev(Estimand),
         RACE = gsub(" -","\n-",RACE)) |>
  filter(RACE != "Non-Hispanic White\n- Non-Hispanic White") %>%
  group_by(RACE) |>
  mutate(y = mean(estimate)) |>
  ungroup() |>
  mutate(RACE = fct_reorder(RACE, -y)) %>%
  ggplot(aes(x = RACE, y = estimate, color = Estimand, shape = Estimand,
             label = paste0(format(round(100*estimate,1),nsmall = 1),"%"))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_text(position = position_dodge(width = .1),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.15, -.15)),
            size = 4,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity Contrast") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold"))
ggsave("figures/disparity_more_controls.pdf",
       height = 3.75, width  = 6.5)

sink()
