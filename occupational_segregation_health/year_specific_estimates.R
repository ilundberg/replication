# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This code calculates and plots year-specific estimates

source("code/prepare_data.R")
source("code/estimator_functions_within_educ.R")

all_data <- prepare_data(target_years = 2005:2020)

cl <- makeCluster(16)
registerDoParallel(cl)
year_specific_point <- foreach(
  i = 2005:2019, 
  .combine = "rbind", 
  .packages = c("tidyverse","reshape2","mgcv","foreach")
) %dopar% {
  if (i == 2014) {
    this_formula <- formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                              s(OCC2010, bs = "re") + 
                              SEX + EDUC + RACE + foreign_born + s(AGE) + new_question +
                              factor(HEALTH))
  } else {
    this_formula <- formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                              s(OCC2010, bs = "re") + 
                              SEX + EDUC + RACE + foreign_born + s(AGE) +
                              factor(HEALTH))
  }
  counterfactual_estimator(weight_name = "ASECWT", 
                           data = all_data$d_onset %>%
                             filter(YEAR == i), 
                           outcome_formula = this_formula,
                           save_intermediate = F) %>%
    mutate(YEAR = i)
}
save(year_specific_point, file = "intermediate/year_specific_point.Rdata")
stopCluster(cl)

# Plot facetted by year
year_specific_point %>%
  spread(key = estimand, value = estimate) %>%
  select(-counterfactual_marginal) %>%
  mutate(RACE = gsub(" ","\n",RACE),
         RACE = fct_relevel(RACE,c("Non-Hispanic\nBlack","Non-Hispanic\nWhite","Hispanic","Other"))) %>%
  ggplot(aes(x = RACE, xend = RACE, y = factual, yend = counterfactual_within_educ)) +
  geom_segment(arrow = arrow(length = unit(.05,"in"))) +
  facet_wrap(~YEAR, ncol = 3) +
  theme_bw() +
  ylab("Onset of Work-Limiting Disability") +
  xlab("Race / Ethnicity") +
  ggsave("figures/year_specific_year_facets.pdf",
         height = 12, width = 11)

# Plot facetted by race / ethnicity
year_specific_point %>%
  spread(key = estimand, value = estimate) %>%
  select(-counterfactual_marginal) %>%
  mutate(RACE = fct_relevel(RACE,c("Non-Hispanic Black","Non-Hispanic White","Hispanic","Other"))) %>%
  ggplot(aes(x = YEAR, xend = YEAR, y = factual, yend = counterfactual_within_educ)) +
  geom_segment(arrow = arrow(length = unit(.05,"in"))) +
  facet_wrap(~RACE, ncol = 1) +
  theme_bw() +
  ylab("Onset of Work-Limiting Disability") +
  xlab("Year") +
  ggsave("figures/year_specific_race_facets.pdf",
         height = 6.5, width = 6.5)

# Plot the black-white factual and counterfactual disparity
year_specific_point %>%
  spread(key = estimand, value = estimate) %>%
  select(-counterfactual_marginal) %>%
  filter(grepl("Non-Hispanic",RACE)) %>%
  mutate(RACE = gsub("Non-Hispanic ","",RACE)) %>%
  melt(id = c("RACE","YEAR"), variable.name = "estimand") %>%
  spread(key = RACE, value = value) %>%
  mutate(disparity = Black - White) %>%
  select(-Black,-White) %>%
  spread(key = estimand, value = disparity) %>%
  ggplot(aes(x = YEAR, xend = YEAR, y = factual, yend = counterfactual_within_educ)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_segment(arrow = arrow(length = unit(.05,"in"))) +
  theme_bw() +
  ylab("Onset of Work-Limiting Disability") +
  xlab("Year") +
  ggsave("figures/year_specific_disparity_arrows.pdf",
         height = 3, width = 6.5)

