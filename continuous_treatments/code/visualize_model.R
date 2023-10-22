
source("code/shorten_race.R")

for_visualize_model <- readRDS("intermediate/for_visualize_model.RDS")

for (outcome_val in unique(for_visualize_model$outcome)) {
  for_visualize_model %>%
    filter(outcome == outcome_val) %>%
    mutate(wealth_label = factor(case_when(wealth == 25e3 ~ 1,
                                           wealth == 100e3 ~ 2,
                                           wealth == 200e3 ~ 3),
                                 labels = c("$25k","$100k","$200k")),
           wealth_label = fct_rev(wealth_label)) %>%
    mutate(educJoint = fct_rev(educJoint),
           race = shorten_race(race)) %>%
    ggplot(aes(x = income, y = estimate, color = wealth_label)) +
    geom_line() +
    facet_grid(race ~ educJoint) +
    scale_color_discrete(name = "Wealth") +
    scale_x_continuous(name = "Parent Income",
                       labels = function(x) paste0("$",round(x / 1e3),"k")) +
    ylab(case_when(outcome_val == "enrolled_any" ~ "College Enrollment by 21",
                   outcome_val == "enrolled_4yr" ~ "4-Year College Enrollment by 25",
                   outcome_val == "completed_25" ~ "BA Degree by Age 25",
                   outcome_val == "completed_30" ~ "BA Degree by Age 30")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(paste0("figures/vis_model_",outcome_val,".pdf"),
         height = 6, width = 6.5)
}
