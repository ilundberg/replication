
sink("figures/estimation_output.txt")

cl <- makeCluster(10)
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
d <- all_data$d
rm(all_data)

print("Full population size")
print(nrow(full_population))
print("Oversamples")
print(sum(full_population$CPSIDP == 0))
print("From March basic CPS")
print(sum(full_population$CPSIDP != 0))
print("Not linked (person-years)")
print(sum(full_population$CPSIDP != 0) - 2 * nrow(linked))
print("Linked (persons)")
print(nrow(linked))
print("Risk of recovery")
print(sum(linked$lag))
print("History of health limitations")
print(sum(linked$QUITSICK & !linked$lag))
print("Not employed")
print(sum(!linked$employed & !linked$lag & !linked$QUITSICK))
print("Risk of onset")
print(nrow(d_onset))
print("Lack of common support")
print(nrow(d_onset) - nrow(d))
print("Occupation analyses")
print(nrow(d))

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
                                                 save_intermediate = T)
spent <- difftime(Sys.time(),t0)
t0 <- Sys.time()
print(t0)
counterfactual_reps <- foreach(
  i = 1:160, 
  .combine = "rbind", 
  .packages = c("tidyverse","reshape2","foreach","mgcv")
) %dopar% {
  counterfactual_estimator(weight_name = paste0("REPWTP",i)) %>%
    mutate(replicate = i)
}
spent <- difftime(Sys.time(),t0)
print("Time spent on replicates")
print(spent)
counterfactual_estimate <- estimate_from_point_reps(counterfactual_point, counterfactual_reps) 
counterfactual_results <- list(counterfactual_estimate = counterfactual_estimate,
                               counterfactual_point = counterfactual_point,
                               counterfactual_reps = counterfactual_reps)
save(counterfactual_results, file = "intermediate/counterfactual_results.Rdata")
print("Finished counterfactual disparity estimation")

#################################################
# Point estimates of alternative specifications #
#################################################

# In the 2009 and later period, subset to those without any reported difficulties
print("Alternative specification: Filter on additional controls")
d_alt_all <- d %>%
  filter(YEAR >= 2009 & !DIFFANY) %>%
  group_by(OCC2010) %>%
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
save(extra_controls, file = "intermediate/extra_controls.Rdata")

# Estimate on the years with the new question 
print("Alternative specification: After the questionnaire redesign")
d_alt_all <-  d %>% 
  filter(YEAR >= 2014) %>% 
  mutate(to_keep = questionnaire_redesign & lag_questionnaire_redesign) %>% 
  # Rescale the weights to keep the same sum when we restrict
  group_by(YEAR) %>% 
  mutate(ASECWT = ASECWT * sum(ASECWT) / sum(ASECWT * to_keep)) %>%
  group_by() %>%
  filter(to_keep) %>%
  # Make common support restriction
  group_by(OCC2010) %>%
  mutate(in_support = n_distinct(RACE) == 4) %>%
  group_by() %>%
  mutate(ASECWT = ASECWT / mean(ASECWT))
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
questionnaire_redesigns <- counterfactual_estimator(data = d_alt,
                                                    outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                                                                                s(OCC2010, bs = "re") + 
                                                                                SEX + EDUC + foreign_born + s(AGE) + s(YEAR, k = 3) + questionnaire_redesign +
                                                                                factor(HEALTH)))
save(questionnaire_redesigns, file = "intermediate/questionnaire_redesigns.Rdata")

# Estimate on the years with the old question
print("Alternative specification: Before the questionnaire redesign")
d_alt_all <-  d %>% 
  filter(YEAR <= 2013) %>% 
  mutate(to_keep = !questionnaire_redesign & !lag_questionnaire_redesign) %>% 
  # Rescale the weights to keep the same sum when we restrict
  group_by(YEAR) %>% 
  mutate(ASECWT = ASECWT * sum(ASECWT) / sum(ASECWT * to_keep)) %>%
  group_by() %>%
  filter(to_keep) %>%
  group_by(OCC2010) %>%
  mutate(in_support = n_distinct(RACE) == 4) %>%
  group_by() %>%
  mutate(ASECWT = ASECWT / mean(ASECWT))
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
old_questions <- counterfactual_estimator(data = d_alt)
save(old_questions, file = "intermediate/old_questions.Rdata")

# Estimate with the forward-linking weight
print("Alternative specification: Using the forward-linking weight")
print("Sample size (this is the same size as the main specification):")
print(nrow(d))
link_weight <- counterfactual_estimator(
  weight_name = "LNKFW1YWT", 
  data = d
)
save(link_weight, file = "intermediate/link_weight.Rdata")

# Estimate without immigrants
print("Alternative specification: No immigrants")
d_alt_all <-  d %>% 
  filter(!foreign_born) %>%
  group_by(OCC2010) %>%
  mutate(in_support = n_distinct(RACE) == 4) %>%
  group_by() %>%
  mutate(ASECWT = ASECWT / mean(ASECWT))
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
no_immigrants <- counterfactual_estimator(data = d_alt)
save(no_immigrants, file = "intermediate/no_immigrants.Rdata")

# Go back all the way to 1988
all_data_all_years <- prepare_data(target_years = 1988:2020)
print("Alternative specification: Full period 1988-2019 for year 1")
print("Sample size:")
print(nrow(all_data_all_years$d))
full_years <- counterfactual_estimator(data = all_data_all_years$d,
                                       outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                                                                   s(OCC2010, bs = "re") + 
                                                                   SEX + EDUC + s(AGE) + s(YEAR, k = 8) + questionnaire_redesign))
save(full_years, file = "intermediate/full_years.Rdata")

# Estimate without race interactions, including standard errors for this one
print("Alternative specification: No race interactions")
print("Sample size (same as original):")
print(nrow(d))
print("Beginning additive counterfactual disparity estimation")
additive_point <- counterfactual_estimator(data = d, interactions = "none",
                                           outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                                                                       s(OCC2010, bs = "re") + 
                                                                       RACE + SEX + EDUC + foreign_born + s(AGE) + s(YEAR, k = 8) + questionnaire_redesign +
                                                                       factor(HEALTH)),
                                           save_intermediate = F)
save(additive_point, file = "intermediate/additive.Rdata")
t0 <- Sys.time()
print(t0)
additive_reps <- foreach(
  i = 1:160, 
  .combine = "rbind", 
  .packages = c("tidyverse","reshape2","foreach","mgcv")
) %dopar% {
  counterfactual_estimator(data = d, interactions = "none",
                           outcome_formula = formula(y ~ s(prop_NonHispanicBlack) + s(prop_Hispanic) + s(prop_Other) + 
                                                       s(OCC2010, bs = "re") + 
                                                       RACE + SEX + EDUC + foreign_born + s(AGE) + s(YEAR, k = 8) + questionnaire_redesign +
                                                       factor(HEALTH)),
                           save_intermediate = F,
                           weight_name = paste0("REPWTP",i)) %>%
    mutate(replicate = i)
}
spent <- difftime(Sys.time(),t0)
print("Time spent on additive replicates")
print(spent)
additive_estimate <- estimate_from_point_reps(additive_point, additive_reps) 
additive_results <- list(additive_estimate = additive_estimate,
                         additive_point = additive_point,
                         additive_reps = additive_reps)
save(additive_results, file = "intermediate/additive_results.Rdata")
print("Finished additive counterfactual disparity estimation")

stopCluster(cl)


sink()
