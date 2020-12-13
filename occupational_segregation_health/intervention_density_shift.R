# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This code shows how the intervention shifts occupational risk


source("code/prepare_data.R")
all_data <- prepare_data(2005:2020)

load("intermediate/outcome_fit.Rdata")
X <- model.matrix(outcome_fit)
beta <- coef(outcome_fit)

# Make predictions for occupation with all other covariates at their means
all_occupation_numbers <- unique(all_data$d_onset$OCC2010)
occupation_columns <- grepl("prop|OCC2010",colnames(X))
for_predictions <- all_data$d_onset
load("intermediate/prop_NonHispanicBlack_fit.Rdata")
for_predictions$prop_NonHispanicBlack <- fitted(fit)
load("intermediate/prop_Hispanic_fit.Rdata")
for_predictions$prop_Hispanic <- fitted(fit)
load("intermediate/prop_Other_fit.Rdata")
for_predictions$prop_Other <- fitted(fit)
rm(fit)
# Calculate the weighted mean of the linear predictor given all other columns
weighted_mean_other_columns <- weighted.mean(X[,!occupation_columns] %*% beta[!occupation_columns], w = all_data$d_onset$ASECWT)
# Get the average potential outcome in each occupation
cl <- makeCluster(4)
registerDoParallel(cl)
occupation_estimates <- foreach(occ_case = all_occupation_numbers, .combine = "rbind", .packages = c("mgcv","tidyverse")) %dopar% {
  this_case <- for_predictions %>%
    filter(OCC2010 == occ_case)
  X_this_occ <- predict(outcome_fit, 
                        type = "lpmatrix", 
                        newdata = this_case)
  this_case %>%
    group_by(OCC2010) %>%
    summarize(size = sum(ASECWT), .groups = "drop_last") %>%
    mutate(average_potential_outcome = weighted_mean_other_columns + as.numeric(X_this_occ[1,occupation_columns] %*% beta[occupation_columns]))
}
stopCluster(cl)


# Densities of the danger in each occupation under the
# factual world and under the counterfactual assignment

factual_risks <- all_data$d_onset %>%
  left_join(occupation_estimates,
            by = "OCC2010") %>%
  select(RACE, EDUC, OCC2010, average_potential_outcome, ASECWT) %>%
  mutate(treatment = "factual")

marginal_equalized_risks <- foreach(race_case = unique(all_data$d_onset$RACE), .combine = "rbind") %do% {
  factual_risks %>%
    mutate(RACE = race_case,
           treatment = "marginal")
}

withinEduc_equalized_risks <- foreach(educ_case = unique(all_data$d_onset$EDUC), .combine = "rbind") %do% {
  foreach(race_case = unique(all_data$d_onset$RACE), .combine = "rbind") %do% {
    factual_risks %>%
      filter(EDUC == educ_case) %>%
      group_by() %>%
      # Get the total weight on this education category within this race category
      mutate(total_weight = sum(as.numeric(RACE == race_case) * ASECWT)) %>%
      # Now code everyone as this racial category since they will be assigned
      # to all of these occupations with their observed frequency
      mutate(RACE = race_case) %>%
      # Now adjust the weight to sum to the total weight
      mutate(ASECWT = ASECWT / sum(ASECWT) * total_weight) %>%
      select(-total_weight) %>%
      mutate(treatment = "withinEduc")
  }
}

factual_risks %>%
  bind_rows(withinEduc_equalized_risks) %>%
  mutate(treatment = ifelse(treatment == "factual","Factual\nOccupation\nAssignments","Counterfactual\nOccupation\nAssignments"),
         treatment = fct_rev(treatment)) %>%
  # Collapse redundant rows to speed figure construction
  group_by(OCC2010, treatment, RACE) %>%
  summarize(average_potential_outcome = mean(average_potential_outcome),
            ASECWT = sum(ASECWT)) %>%
  group_by() %>%
  mutate(RACE = fct_relevel(RACE, "Non-Hispanic Black","Non-Hispanic White","Hispanic","Other")) %>%
  ggplot(aes(x = average_potential_outcome, weight = ASECWT, fill = treatment)) +
  geom_density(alpha = .4, bw = .003) +
  xlab("Population Average Potential Outcome in One's Occupation") +
  ylab("Density") +
  scale_fill_manual(name = element_blank(), values = c("blue","gray")) +
  facet_wrap(~ RACE) +
  theme_bw() +
  theme(legend.key.height = unit(.5,"in")) +
  ggsave("figures/potential_outcomes_assignment_shift.pdf",
         height = 3, width = 6.5)

factual_risks %>%
  bind_rows(marginal_equalized_risks) %>%
  mutate(comparison = "Counterfactual assigns occupations\nby their marginal\nprevalence") %>%
  bind_rows(factual_risks %>%
              bind_rows(withinEduc_equalized_risks) %>%
              mutate(comparison = "Counterfactual assigns occupations\nby their prevalence\nwithin education categories")) %>%
  mutate(treatment = ifelse(treatment == "factual","Factual","Counterfactual")) %>%
  group_by(OCC2010, treatment, comparison, RACE) %>%
  # Collapse redundant rows to speed figure construction
  summarize(average_potential_outcome = mean(average_potential_outcome),
            ASECWT = sum(ASECWT)) %>%
  group_by() %>%
  mutate(comparison = fct_rev(comparison),
         RACE = paste0(gsub(" ","\n",RACE),"\nOccupation\nAssignments"),
         RACE = fct_relevel(RACE,
                            "Non-Hispanic\nBlack\nOccupation\nAssignments",
                            "Non-Hispanic\nWhite\nOccupation\nAssignments",
                            "Hispanic\nOccupation\nAssignments",
                            "Other\nOccupation\nAssignments")) %>%
  ggplot(aes(x = average_potential_outcome, weight = ASECWT, fill = treatment)) +
  geom_density(alpha = .4, bw = .003) +
  xlab("Population Average Potential Outcome in One's Occupation") +
  ylab("Density") +
  scale_fill_manual(name = element_blank(), values = c("blue","gray")) +
  facet_grid(RACE ~ comparison) +
  theme_bw() +
  theme(legend.key.height = unit(.5,"in"),
        strip.text.y = element_text(angle = 0),
        legend.position = "bottom") +
  ggsave("figures/potential_outcomes_assignment_shift_bothRules.pdf",
         height = 6, width = 6.5)



sink("figures/intervention_summaries.txt")
print("Summaries of occupational hazards:")
weighted.median <- function(x,w) {
  sorted.w <- w[order(x)]
  sorted.x <- x[order(x)]
  median.case <- min(which(cumsum(sorted.w) / sum(sorted.w) > .5))
  return(sorted.x[median.case])
}
print("Factual risks:")
print(factual_risks %>%
        group_by(RACE) %>%
        summarize(mean_hazard = weighted.mean(average_potential_outcome, w = ASECWT),
                  median_hazard = weighted.median(average_potential_outcome, w = ASECWT)))
print("After intervention within education:")
print(withinEduc_equalized_risks %>%
        group_by(RACE) %>%
        summarize(mean_hazard = weighted.mean(average_potential_outcome, w = ASECWT),
                  median_hazard = weighted.median(average_potential_outcome, w = ASECWT)))
print("After marginal intervention:")
print(marginal_equalized_risks %>%
        group_by(RACE) %>%
        summarize(mean_hazard = weighted.mean(average_potential_outcome, w = ASECWT),
                  median_hazard = weighted.median(average_potential_outcome, w = ASECWT)))
sink()

# Provide some example occupations to illustrate the intervention
sink("figures/intervention_occupation_examples.txt")
print(
  all_data$d_onset %>%
    group_by(EDUC, OCC2010) %>%
    summarize(weight = sum(ASECWT)) %>%
    group_by(EDUC) %>%
    mutate(prop = weight / sum(weight)) %>%
    # Keep only the occupations most common in at least one category
    arrange(-prop) %>%
    mutate(top_two = 1:n() <= 2) %>%
    group_by(OCC2010) %>%
    filter(any(top_two)) %>%
    group_by() %>%
    # Append the occupation titles
    left_join(all_data$full_population %>%
                group_by(OCC2010) %>%
                filter(1:n() == 1) %>%
                mutate(occ_title = as.character(as_factor(OCC2010))) %>%
                group_by() %>%
                mutate(OCC2010 = factor(OCC2010)) %>%
                select(OCC2010, occ_title),
              by = "OCC2010") %>%
    mutate(EDUC = fct_relevel(EDUC,"Less than HS","High school","Some college","College")) %>%
    arrange(EDUC, -prop) %>%
    # The square root of size is useful because it will be the diameter of the circle
    mutate(sqrt_prop = sqrt(prop)) %>%
    select(EDUC, prop, sqrt_prop, occ_title),
  n = 24
)
sink()

# Provide some example occupations to illustrate the intervention
examples <- all_data$d_onset %>%
  group_by(EDUC, OCC2010) %>%
  summarize(weight = sum(ASECWT),
            y = weighted.mean(y, w = ASECWT)) %>%
  group_by() %>%
  left_join(all_data$full_population %>%
              select(OCC2010) %>%
              mutate(occ_title = as.character(as_factor(OCC2010)))) %>%
  group_by(EDUC) %>%
  mutate(prop = weight / sum(weight)) %>%
  # Version 4: Most common and most dangerous (among those with at least 1% of people)
  arrange(prop) %>%
  mutate(most_common = 1:n() == n()) %>%
  group_by(OCC2010) %>%
  filter(any(most_common) | occ_title %in% c(""))


all_data$d_onset %>%
  group_by(EDUC, OCC2010) %>%
  summarize(weight = sum(ASECWT),
            y = weighted.mean(y, w = ASECWT)) %>%
  group_by(EDUC) %>%
  mutate(prop = weight / sum(weight)) %>%
  filter(prop > .01) %>%
  mutate(safest = y == min(y),
         dangerous = y == max(y)) %>%
  filter(safest | dangerous) %>%
  left_join(all_data$full_population %>%
              group_by(OCC2010) %>%
              filter(1:n() == 1) %>%
              select(OCC2010) %>%
              mutate(occ_title = as.character(as_factor(OCC2010)),
                     OCC2010 = factor(OCC2010)),
            by = "OCC2010")

print(
  all_data$d_onset %>%
    filter(EDUC == "Less than HS") %>%
    group_by(OCC2010) %>%
    summarize(size = sum(ASECWT),
              prop_black = weighted.mean(RACE == "Non-Hispanic Black", w = ASECWT),
              prop_white = weighted.mean(RACE == "Non-Hispanic White", w = ASECWT),
              safety = weighted.mean(y, w = ASECWT)) %>%
    filter(prop_black < .03 & prop_white > .8) %>%
    arrange(-size) %>%
    left_join(all_data$full_population %>%
                group_by(OCC2010) %>%
                filter(1:n() == 1) %>%
                select(OCC2010) %>%
                mutate(occ_title = as.character(as_factor(OCC2010)),
                       OCC2010 = factor(OCC2010)),
              by = "OCC2010"),
  n = 30
)

  


