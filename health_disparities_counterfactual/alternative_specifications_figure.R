
# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This code produces the figure of alternative specifications
load("intermediate/link_weight.Rdata")
load("intermediate/old_questions.Rdata")
load("intermediate/new_questions.Rdata")
load("intermediate/extra_controls.Rdata")
load("intermediate/full_years.Rdata")
load("intermediate/counterfactual_results.Rdata")
load("intermediate/no_immigrants.Rdata")
load("intermediate/race_interactions.Rdata")

counterfactual_results$counterfactual_point %>%
  mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
  filter(estimand != "counterfactual_marginal") %>%
  mutate(facet = "A) Original specification\n(data for which the first year is in 2005-2019)") %>%
  bind_rows(counterfactual_results$counterfactual_point %>%
              mutate(estimand = ifelse(estimand == "counterfactual_marginal","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_within_educ") %>%
              mutate(facet = "B) Under marginal occupational equalization\n(data for which the first year is in 2005-2019)")) %>%
  bind_rows(extra_controls %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "C) Data for which the first year is in 2009-2019\nsubsetted to those without other reported disabilities")) %>% 
  bind_rows(no_immigrants %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "D) Data for which the first year is in 2005-2019\nsubsetted to those who are not foreign born")) %>% 
  bind_rows(link_weight %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "E) Weights designed for\nyear-to-year linked sample")) %>% 
  bind_rows(full_years %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "F) Data for which the first year is in 1988-2019\nwith fewer variables adjusted")) %>% 
  bind_rows(old_questions %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "G) Data for which the first year is in 2005-2013\nwith traditional question order and wording")) %>% 
  bind_rows(new_questions %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "H) Data for which the first year is in 2014-2019\nwith new question order outcomes measured in (2015-2020) and new wording (outcomes measured in 2016-2020)")) %>% 
  bind_rows(race_interactions %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand != "counterfactual_marginal") %>% 
              mutate(facet = "I) All covariates interacted with race\nData from 2005-2019")) %>% 
  mutate(RACE = gsub(" ","\n",RACE)) %>%
  mutate(RACE = fct_relevel(RACE,"Non-Hispanic\nBlack","Non-Hispanic\nWhite","Hispanic","Other")) %>%
  spread(key = estimand, value = estimate) %>%
  ggplot(aes(x = RACE, xend = RACE, y = factual, yend = counterfactual)) + 
  geom_segment(arrow = arrow(length = unit(.1,"in"))) + 
  facet_wrap(~facet, ncol = 3) +
  ylab("Onset of Work-Limiting Disability") +
  xlab("Race / Ethnicity") +
  theme_bw() +
  theme(strip.text = element_text(hjust = 0)) +
  ggsave("figures/alternative_specifications.pdf",
         height = 10, width = 11)
