
sink("figures/log_figure_7a.txt")
print(Sys.time())

# See run_all.R to see how this file is called

load("intermediate/counterfactual_results.Rdata")
counterfactual_estimate <- counterfactual_results$counterfactual_estimate

# Proportion figure

counterfactual_estimate %>%
  filter(!grepl(" - ",RACE)) %>%
  group_by() %>%
  rename(Estimand = estimand) %>%
  mutate(Estimand = case_when(Estimand == "factual" ~ "Factual",
                              Estimand == "counterfactual_within_educ" ~ "Counterfactual"),
         Estimand = fct_rev(Estimand),
         RACE = gsub(" ","\n",RACE),
         RACE = fct_relevel(RACE,"Non-Hispanic\nBlack","Non-Hispanic\nWhite","Hispanic","Other")) %>%
  ggplot(aes(x = RACE, y = estimate, color = Estimand, shape = Estimand,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se,
             label = paste0(format(round(100*estimate,1),nsmall = 1),"%"))) +
  geom_errorbar(position = position_dodge(width = .5),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .5)) +
  geom_text(position = position_dodge(width = .5),
            aes(hjust = ifelse(Estimand == "Factual", 1.2, -.2)),
            size = 4,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold"))
ggsave("figures/proportions.pdf",
       height = 3.75, width  = 6.5)

significance <- counterfactual_estimate %>%
  filter(grepl(" - ",RACE)) %>%
  group_by() %>%
  mutate(pval = 2 * pnorm(abs(estimate / se), lower.tail = F),
         significance = case_when(pval < .001 ~ "***",
                                  pval < .01 ~ "**",
                                  pval < .05 ~ "*",
                                  T ~ "ns")) %>%
  arrange(estimand,RACE)
print(significance)
sink()