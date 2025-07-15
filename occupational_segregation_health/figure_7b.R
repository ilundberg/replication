
sink("figures/log_figure_7b.txt")
print(Sys.time())

# See run_all.R to see how this file is called

# This code produces the disparity figure

load("intermediate/counterfactual_results.Rdata")
counterfactual_estimate <- counterfactual_results$counterfactual_estimate

# Disparity figure
for_disparity_figure <- counterfactual_estimate %>%
  filter(grepl(" - ", RACE)) %>%
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
  mutate(RACE = fct_reorder(RACE, -y))
for_disparity_figure %>%
  ggplot(aes(x = RACE, y = estimate, color = Estimand, shape = Estimand,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se,
             label = paste0(format(round(100*estimate,1),nsmall = 1),"%"))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(position = position_dodge(width = .5),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .5)) +
  geom_text(position = position_dodge(width = .5),
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
ggsave("figures/disparity.pdf",
       height = 3.75, width  = 6.5)

# Note the percent reductions
print(
  counterfactual_estimate %>% 
    filter(grepl(" - ",RACE)) %>%
    select(RACE, estimand, estimate) %>%
    pivot_wider(names_from = "estimand", values_from = "estimate") %>%
    mutate(factor_change = counterfactual_within_educ / factual)
)

print("Significance of additive change in proportions")
counterfactual_results$counterfactual_point |>
  pivot_wider(names_from = "estimand", values_from = "estimate") |>
  mutate(estimate = counterfactual_within_educ - factual) |>
  select(RACE, estimate) |>
  left_join(
    counterfactual_results$counterfactual_reps |>
      pivot_wider(names_from = "estimand", values_from = "estimate") |>
      mutate(change = counterfactual_within_educ - factual) |>
      group_by(RACE) |>
      summarize(se = sd(change)),
    by = join_by(RACE)
  ) |>
  mutate(
    ci.min = estimate - qnorm(.975) * se,
    ci.max = estimate + qnorm(.975) * se,
    pval = 2 * pnorm(abs(estimate / se), lower.tail = F),
    significance = case_when(pval < .001 ~ "***",
                             pval < .01 ~ "**",
                             pval < .05 ~ "*",
                             T ~ "ns")
  )
print("Significance of ratio change in proportion")
counterfactual_results$counterfactual_point |>
  pivot_wider(names_from = "estimand", values_from = "estimate") |>
  mutate(estimate = counterfactual_within_educ / factual) |>
  select(RACE, estimate) |>
  left_join(
    counterfactual_results$counterfactual_reps |>
      pivot_wider(names_from = "estimand", values_from = "estimate") |>
      mutate(change = counterfactual_within_educ / factual) |>
      group_by(RACE) |>
      summarize(se = sd(change)),
    by = join_by(RACE)
  ) |>
  mutate(
    ci.min = estimate - qnorm(.975) * se,
    ci.max = estimate + qnorm(.975) * se,
    pval = 2 * pnorm(abs(estimate  - 1) / se, lower.tail = F),
    significance = case_when(pval < .001 ~ "***",
                             pval < .01 ~ "**",
                             pval < .05 ~ "*",
                             T ~ "ns")
  )

print("Significance of change in disparities vs white")
print(
  counterfactual_results$counterfactual_point |>
    group_by(estimand) |>
    mutate(reference = case_when(RACE == "Non-Hispanic White" ~ estimate)) |>
    mutate(reference = unique(na.omit(reference))) |>
    mutate(estimate = estimate - reference) |>
    select(RACE, estimand, estimate) |>
    pivot_wider(names_from = "estimand", values_from = "estimate") |>
    mutate(estimate = counterfactual_within_educ / factual) |>
    select(RACE, estimate) |>
    left_join(
      counterfactual_results$counterfactual_reps |>
        group_by(estimand, replicate) |>
        mutate(reference = case_when(RACE == "Non-Hispanic White" ~ estimate)) |>
        mutate(reference = unique(na.omit(reference))) |>
        mutate(estimate = estimate - reference) |>
        select(-reference) |>
        pivot_wider(names_from = "estimand", values_from = "estimate") |>
        mutate(estimate = counterfactual_within_educ / factual) |>
        group_by(RACE) |>
        select(RACE, estimate) |>
        summarize(se = sd(estimate)),
      by = join_by(RACE)
    ) |>
    mutate(
      ci.min = estimate - qnorm(.975) * se,
      ci.max = estimate + qnorm(.975) * se,
      pval = 2 * pnorm(abs(estimate  - 1) / se, lower.tail = F),
      significance = case_when(pval < .001 ~ "***",
                               pval < .01 ~ "**",
                               pval < .05 ~ "*",
                               T ~ "ns")
    )
)
sink()

