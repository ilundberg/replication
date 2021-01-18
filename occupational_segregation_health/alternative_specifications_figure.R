
# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This code produces the figure of alternative specifications
load("intermediate/counterfactual_results.Rdata")
load("intermediate/extra_controls.Rdata")
load("intermediate/no_immigrants.Rdata")
load("intermediate/link_weight.Rdata")
load("intermediate/full_years.Rdata")
load("intermediate/old_questions.Rdata")
load("intermediate/questionnaire_redesigns.Rdata")
load("intermediate/additive.Rdata")

forplot <- counterfactual_results$counterfactual_point %>%
  mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
  filter(estimand != "counterfactual_marginal") %>%
  mutate(facet = "A) Original specification\nData in 2005-2020") %>%
  bind_rows(counterfactual_results$counterfactual_point %>%
              mutate(estimand = ifelse(estimand == "counterfactual_marginal","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_within_educ") %>%
              mutate(facet = "B) Under marginal occupational equalization\nData in 2005-2020")) %>%
  bind_rows(extra_controls %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "C) Restricted to those without other reported disabilities\nData in 2005--2020")) %>% 
  bind_rows(no_immigrants %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "D) Restricted to those who are not foreign born\nData in 2005--2020")) %>% 
  bind_rows(link_weight %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "E) Weights designed for linked sample\nData in 2005--2020")) %>% 
  bind_rows(full_years %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "F) Longer period with fewer control variables\nData in 1988-2020")) %>% 
  bind_rows(old_questions %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "G) Traditional question order and wording\nData in 2005-2014")) %>% 
  bind_rows(questionnaire_redesigns %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "H) With questionnaire changes\nData in 2014-2020")) %>% 
  bind_rows(additive_point %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "I) Additive model (no race interactions)\nData in 2005-2020")) %>% 
  mutate(RACE = gsub(" ","\n",RACE)) %>%
  mutate(RACE = fct_relevel(RACE,"Non-Hispanic\nBlack","Non-Hispanic\nWhite","Hispanic","Other")) %>%
  spread(key = estimand, value = estimate)
forplot %>%
  ggplot(aes(x = RACE, xend = RACE, y = factual, yend = counterfactual)) + 
  geom_segment(arrow = arrow(length = unit(.1,"in"))) + 
  facet_wrap(~facet, ncol = 3) +
  ylab("Onset of Work-Limiting Disability") +
  xlab("Race / Ethnicity") +
  theme_bw() +
  theme(strip.text = element_text(hjust = 0)) +
  ggsave("figures/alternative_specifications.pdf",
         height = 10, width = 11)

# For slides, make plots of each panel separately
for (specific_panel in LETTERS[2:9]) {
  forplot %>%
    filter(grepl("^A",facet) | grepl(paste0("^",specific_panel),facet)) %>%
    ggplot(aes(x = RACE, xend = RACE, y = factual, yend = counterfactual)) + 
    geom_segment(arrow = arrow(length = unit(.1,"in"))) + 
    facet_wrap(~facet, ncol = 3) +
    ylab("Onset of Work-Limiting Disability") +
    xlab("Race / Ethnicity") +
    theme_bw() +
    theme(strip.text = element_text(hjust = 0)) +
    ggsave(paste0("figures/alternative_specifications_",specific_panel,".pdf"),
           height = 3, width = 8)
}
