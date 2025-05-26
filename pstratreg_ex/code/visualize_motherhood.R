
sink("../logs/visualize_motherhood.txt")

t0 <- Sys.time()
print(t0)

library(tidyverse)

estimates <- readRDS("../intermediate/estimates.RDS")

uclablue <- "#2774AE"

# VISUALIZE EMPLOYMENT EFFECT OVERALL AND IN SUBGROUPS: WOMEN
overall <- estimates |>
  filter(sex == "Women"& estimand == "effect_s" & !monotonicity & !mean_dominance) |>
  mutate(wage_baseline_categories = "All Mothers", facet = "Overall Estimate") |>
  select(wage_baseline_categories, facet, effect_s = estimate, ci.min, ci.max)
subgroups <- readRDS("../intermediate/motherhood_within_subgroups.RDS") |>
  mutate(facet = "Mothers Partitioned by Baseline Outcome\nOne Year Before a First Birth")
overall |>
  bind_rows(subgroups) |>
  mutate(wage_baseline_categories = fct_reorder(wage_baseline_categories, effect_s)) |>
  mutate(facet = fct_rev(facet)) |>
  ggplot(
    aes(
      x = wage_baseline_categories, 
      y = effect_s,
      ymin = ci.min,
      ymax = ci.max,
      label = scales::label_percent(accuracy = .1)(effect_s)
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .2) +
  geom_label(size = 3) +
  xlab("Population Subgroup") +
  scale_y_continuous(
    name = "Effect of Motherhood on Employment\nAmong Mothers (ATT)",
    labels = scales::label_percent(),
    limits = c(-.2,.06)
  ) +
  facet_wrap(~facet, scales = "free_x") +
  theme_bw()
ggsave("../figures/employment_effects_women.pdf", height = 3, width = 7.5)

# VISUALIZE EMPLOYMENT EFFECT OVERALL AND IN SUBGROUPS: MEN
overall <- estimates |>
  filter(sex == "Men"& estimand == "effect_s" & !monotonicity & !mean_dominance) |>
  mutate(wage_baseline_categories = "All Fathers", facet = "Overall Estimate") |>
  select(wage_baseline_categories, facet, effect_s = estimate, ci.min, ci.max)
subgroups <- readRDS("../intermediate/fatherhood_within_subgroups.RDS") |>
  mutate(facet = "Fathers Partitioned by Baseline Outcome\nOne Year Before a First Birth")
overall |>
  bind_rows(subgroups) |>
  mutate(wage_baseline_categories = fct_relevel(wage_baseline_categories, c("All Fathers","Not employed","Below $15","$15 to $20","Over $20"))) |>
  mutate(facet = fct_rev(facet)) |>
  ggplot(
    aes(
      x = wage_baseline_categories, 
      y = effect_s,
      ymin = ci.min,
      ymax = ci.max,
      label = scales::label_percent(accuracy = .1)(effect_s)
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .2) +
  geom_label(size = 3) +
  xlab("Population Subgroup") +
  scale_y_continuous(
    name = "Effect of Fatherhood on Employment\nAmong Fathers (ATT)",
    labels = scales::label_percent(),
    limits = c(-.2,.06)
  ) +
  facet_wrap(~facet, scales = "free_x") +
  theme_bw()
ggsave("../figures/employment_effects_men.pdf", height = 3, width = 7)

# Note proportions in those groups
d <- readRDS("../intermediate/motherhood.RDS")
d |>
  filter(!is.na(wage_baseline)) |>
  filter(employed_baseline) |>
  group_by(sex) |>
  summarize(below_15 = weighted.mean(wage_baseline < log(15), w = w),
            middle = weighted.mean(wage_baseline >= log(15) & wage_baseline < log(20), w = w),
            high = weighted.mean(wage_baseline >= log(20), w = w))

# VISUALIZE HOW BOUNDS TIGHTEN EFFECTS
estimates |>
  mutate(
    setting = factor(
      case_when(
        !monotonicity & !mean_dominance ~ 1,
        monotonicity & !mean_dominance ~ 2,
        !monotonicity & mean_dominance ~ 3,
        monotonicity & mean_dominance ~ 4
      ),
      labels = c(
        "No assumptions",
        "Under\nmonotonicity",
        "Under\nmean dominance",
        "Under\nmonotonicity and\nmean dominance"
      )
    )
  ) |>
  filter(estimand %in% c("effect_y_lower","effect_y_upper")) |>
  select(-se) |>
  pivot_longer(cols = c("estimate","ci.min","ci.max","ci.min.normal","ci.max.normal"), names_to = "statistic") |>
  pivot_wider(names_from = c("estimand","statistic"), values_from = "value") |>
  ggplot(aes(x = setting)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(
    aes(ymin = effect_y_lower_ci.min, ymax = effect_y_upper_ci.max),
    width = .2
  ) +
  geom_errorbar(
    aes(ymin = effect_y_lower_estimate, ymax = effect_y_upper_estimate),
    width = 0,
    size = 2
  ) +
  geom_text(
    aes(y = effect_y_lower_ci.min, 
        label = format(round(effect_y_lower_ci.min,2),nsmall = 2)),
    size = 3, vjust = 1.5
  ) +
  geom_text(
    aes(y = effect_y_upper_ci.max, 
        label = format(round(effect_y_upper_ci.max,2),nsmall = 2)),
    size = 3, vjust = -.5
  ) +
  theme_bw() +
  ylab("Average Effect of Parenthood on Log Hourly Pay\nAmong those Employed Regardless of Parenthood") +
  xlab("Assumptions") +
  facet_wrap(~sex)
ggsave("../figures/assumptions_tighten_bounds_quantileCI.pdf", height = 5, width = 10)



sessionInfo()

print("Finish time")
print(Sys.time())
print("Time spent")
difftime(Sys.time(),t0)

sink()