# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A gap-closing perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This file produces the main estimates of the paper.

sink("figures/estimation_output.txt")

cl <- makeCluster(15)
registerDoParallel(cl)

#################
# Load the data #
#################

source("code/prepare_data.R")
all_data <- prepare_data(target_years = 2005:2020)

# Pull the data out into separate objects
full_population <- all_data$full_population
linked <- all_data$linked
d_onset <- all_data$d_onset
rm(all_data)

print("Full population size")
print(nrow(full_population))
print("From March basic CPS")
print(sum(full_population$CPSIDP != 0))
print("Not linked (person-years)")
print(sum(full_population$CPSIDP != 0) - 2 * nrow(linked))
print("Linked (persons)")
print(nrow(linked))
print("Sample that could recover from disability")
print(sum(linked$lag))
print("Sample that could experience disability onset")
print(sum(linked$employed & !linked$lag))
print("Analytic sample")
print(nrow(d_onset))

########################################
# Load the functions to make estimates #
########################################
source("code/estimator_functions.R")

#######################
# CALCULATE ESTIMATES #
#######################

# Estimate the descriptive disparity

disparity_point <- disparity_estimator("ASECWT")
disparity_reps <- foreach(
  i = 1:160, 
  .combine = "rbind", 
  .packages = c("tidyverse")
) %dopar% {
  disparity_estimator(paste0("REPWTP",i)) %>%
    mutate(replicate = i)
}
disparity_estimate <- disparity_reps %>%
  rename(estimate_star = estimate) %>%
  left_join(disparity_point,
            by = c("RACE","estimand")) %>%
  mutate(target = "proportion") %>%
  # Append the disparity vs. white
  bind_rows(disparity_reps %>%
              rename(estimate_star = estimate) %>%
              left_join(disparity_point,
                        by = c("RACE","estimand")) %>%
              group_by() %>%
              melt(id = c("RACE","replicate","estimand"), variable.name = "quantity") %>%
              group_by(replicate) %>%
              mutate(reference = mean(case_when(RACE == "Non-Hispanic White" ~ value), na.rm = T)) %>%
              filter(RACE != "Non-Hispanic White") %>%
              group_by() %>%
              mutate(value = value - reference, 
                     target = "disparity_vs_white") %>%
              select(-reference) %>%
              spread(key = "quantity", value = "value")) %>%
  # Get the pooled estimate and standard error
  group_by(RACE, estimand, target) %>%
  summarize(point = mean(estimate),
            se = sqrt(4 / 160 * sum((estimate_star - estimate) ^ 2)))
disparity_results <- list(disparity_estimate = disparity_estimate,
                          disparity_point = disparity_point,
                          disparity_reps = disparity_reps)
save(disparity_results, file = "intermediate/disparity_results.Rdata")

# Estimates the counterfactual disparity

print("Beginning counterfactual disparity estimation")
t0 <- Sys.time()
print(t0)
counterfactual_point <- counterfactual_estimator(weight_name = "ASECWT", 
                                                 data = d_onset,
                                                 outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                                                                             s(OCC2010, bs = "re") + 
                                                                             SEX + EDUC + RACE + foreign_born + s(AGE) + s(YEAR, k = 8) + new_question +
                                                                             factor(HEALTH)),
                                                 save_intermediate = T)
counterfactual_reps <- foreach(
  i = 1:160, 
  .combine = "rbind", 
  .packages = c("tidyverse","reshape2","foreach","mgcv")
) %dopar% {
  counterfactual_estimator(weight_name = paste0("REPWTP",i), 
                           data = d_onset,
                           outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                                                       s(OCC2010, bs = "re") + 
                                                       SEX + EDUC + RACE + foreign_born + s(AGE) + s(YEAR, k = 8) + new_question +
                                                       factor(HEALTH)),
                           save_intermediate = F) %>%
    mutate(replicate = i)
}
spent <- difftime(Sys.time(),t0)
counterfactual_estimate <- estimate_from_point_reps(counterfactual_point, counterfactual_reps) 
counterfactual_results <- list(counterfactual_estimate = counterfactual_estimate,
                               counterfactual_point = counterfactual_point,
                               counterfactual_reps = counterfactual_reps)
save(counterfactual_results, file = "intermediate/counterfactual_results.Rdata")
print("Finished counterfactual disparity estimation")
print("Spent")
print(difftime(Sys.time(),t0))
print(Sys.time())

#################################################
# Point estimates of alternative specifications #
#################################################

# In the 2009 and later period, subset to those without any reported difficulties
print("Alternative specification: Filter on additional controls")
print("Sample size:")
print(nrow(d_onset %>% filter(YEAR >= 2009 & !DIFFANY & !QUITSICK)))
extra_controls <- counterfactual_estimator(
  weight_name = "ASECWT", 
  data = d_onset %>% filter(YEAR >= 2009 & !DIFFANY & !QUITSICK), 
  outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                              s(OCC2010, bs = "re") + 
                              SEX + EDUC + RACE + foreign_born + s(AGE) + s(YEAR, k = 8) + new_question +
                              factor(HEALTH)),
  save_intermediate = F
)
save(extra_controls, file = "intermediate/extra_controls.Rdata")

# Estimate on the years with the new question 
print("Alternative specification: Only the new version of the question")
print("Sample size:")
print(nrow(d_onset %>% filter(new_question & lag_new_question)))
new_questions <- counterfactual_estimator(
  weight_name = "ASECWT", 
  data = d_onset %>% 
    filter(YEAR >= 2014) %>% 
    mutate(to_keep = new_question & lag_new_question) %>% 
    # Rescale the weights to keep the same sum when we restrict
    group_by(YEAR) %>% 
    mutate(ASECWT = ASECWT * sum(ASECWT) / sum(ASECWT * to_keep)) %>%
    group_by() %>%
    filter(to_keep) %>%
    mutate(ASECWT = ASECWT / mean(ASECWT)),
  outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                              s(OCC2010, bs = "re") + 
                              SEX + EDUC + RACE + foreign_born + s(AGE) + s(YEAR, k = 3) +
                              factor(HEALTH)),
  save_intermediate = F
)
save(new_questions, file = "intermediate/new_questions.Rdata")

# Estimate on the years with the old question
print("Alternative specification: Only the previous version of the question")
print("Sample size:")
print(nrow(d_onset %>% filter(!new_question & !lag_new_question)))
old_questions <- counterfactual_estimator(
  weight_name = "ASECWT", 
  data = d_onset %>% 
    filter(YEAR <= 2013) %>% 
    mutate(to_keep = !new_question & !lag_new_question) %>% 
    # Rescale the weights to keep the same sum when we restrict
    group_by(YEAR) %>% 
    mutate(ASECWT = ASECWT * sum(ASECWT) / sum(ASECWT * to_keep)) %>%
    group_by() %>%
    filter(to_keep) %>%
    mutate(ASECWT = ASECWT / mean(ASECWT)), 
  outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                              s(OCC2010, bs = "re") + 
                              SEX + EDUC + RACE + foreign_born + s(AGE) + s(YEAR, k = 8) +
                              factor(HEALTH)),
  save_intermediate = F
)
save(old_questions, file = "intermediate/old_questions.Rdata")

# Estimate with the forward-linking weight
print("Alternative specification: Using the forward-linking weight")
print("Sample size:")
print(nrow(d_onset %>% filter(YEAR >= 2005)))
link_weight <- counterfactual_estimator(
  weight_name = "LNKFW1YWT", 
  data = d_onset %>% filter(YEAR >= 2005), 
  outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                              s(OCC2010, bs = "re") + 
                              SEX + EDUC + RACE + foreign_born + s(AGE) + s(YEAR, k = 8) + new_question +
                              factor(HEALTH)),
  save_intermediate = F
)
save(link_weight, file = "intermediate/link_weight.Rdata")

# Estimate without immigrants
print("Alternative specification: No immigrants")
print("Sample size:")
print(nrow(d_onset %>% filter(YEAR >= 2005 & !foreign_born)))
no_immigrants <- counterfactual_estimator(
  weight_name = "LNKFW1YWT", 
  data = d_onset %>% filter(YEAR >= 2005 & !foreign_born), 
  outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                              s(OCC2010, bs = "re") + 
                              SEX + EDUC + RACE + s(AGE) + s(YEAR, k = 8) + new_question +
                              factor(HEALTH)),
  save_intermediate = F
)
save(no_immigrants, file = "intermediate/no_immigrants.Rdata")

# Estimate with race interacted with everything
print("Alternative specification: Race interacted with everything")
print("Sample size:")
print(nrow(d_onset %>% filter(YEAR >= 2005) %>% group_by(OCC2010) %>% filter(n_distinct(RACE) == 4)))
print("Weighted proportion lost due to common support restriction")
print(d_onset %>% 
        filter(YEAR >= 2005) %>% 
        group_by(OCC2010) %>% 
        mutate(has_all_four = n_distinct(RACE) == 4) %>%
        group_by() %>%
        summarize(included = sum(as.numeric(has_all_four) * ASECWT),
                  total = sum(ASECWT),
                  people = sum(has_all_four),
                  occupations = n_distinct(ifelse(has_all_four,OCC2010,"Not")) - 1) %>%
        mutate(prop_weight = included / total))
race_interactions <- counterfactual_estimator_race_interactions(
  weight_name = "LNKFW1YWT", 
  data = d_onset %>% filter(YEAR >= 2005),
  save_intermediate = F
)
save(race_interactions, file = "intermediate/race_interactions.Rdata")

# Go back all the way to 1988
all_data <- prepare_data(target_years = 1988:2020)
d_onset_19882020 <- all_data$d_onset
rm(all_data)
print("Alternative specification: Full period 1988-2019 for year 1")
print("Sample size:")
print(nrow(d_onset_19882020))
full_years <- counterfactual_estimator(
  weight_name = "ASECWT", 
  data = d_onset_19882020, 
  outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                              s(OCC2010, bs = "re") + 
                              SEX + EDUC + RACE + s(AGE) + s(YEAR, k = 8) + new_question),
  save_intermediate = F
)
save(full_years, file = "intermediate/full_years.Rdata")

stopCluster(cl)

# Run it separately by education


sink()