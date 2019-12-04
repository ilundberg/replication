
# Replication code for estimation section of
# "Setting the Target: Precise Estimands and the Gap Between Theory and Empirics"
# Ian Lundberg, Rebecca Johnson, and Brandon Stewart

# Code by Ian Lundberg
# ilundberg@princeton.edu

library(readstata13)
library(tidyverse)
library(reshape2)
library(foreach)
library(mgcv)
  
setwd("/Users/iandl/Dropbox/Estimands/pal2017")

## Prep the main data file on women ages 25-44
data <- read.dta13("cps_00040.dta",
                   convert.factors = F)

rep_weights <- data %>% select(pernum, starts_with("repwtp"))

d <- data %>%
  filter(age >= 25 & age <= 44 & sex == 2 & classwly > 14 &
           ## Drop those who report no incomes or for whom
           ## incomes are missing
           incwage != 9999999 & incwage != 9999998) %>%
  mutate(
    ## Weeks worked last year
    ## Use the intervalled value in years 1968-1975
    ## Make intervalled weeks worked last year into the midpoint
    wkswork2 = ifelse(wkswork2 == 5, 48.5,
                      ifelse(wkswork2 == 6, 51,
                             ifelse(wkswork2 == 4, 43.5,
                                    ifelse(wkswork2 == 3, 33,
                                           ifelse(wkswork2 == 2, 20,
                                                  ifelse(wkswork2 == 1, 7, NA)))))),
    ## For the other years, we can just use wkswork1, which is the continuous report
    wkswork1 = ifelse(year <= 1975, wkswork2,
                      ifelse(wkswork1 <= 0, NA, wkswork1)),
    ## Usual hours per week worked last year
    uhrsworkly = ifelse(year <= 1975,
                        ## For 1968-1975, use hours last week because usual hours worked last year is not available
                        ifelse(ahrsworkt != 999, ahrsworkt, NA),
                        ## Otherwise use the variable that directly measures this
                        ifelse(uhrsworkly != 999, uhrsworkly, NA)),
    ## Create hourly wages
    wage = incwage / (wkswork1 * uhrsworkly),
    # Truncate log wage at 1st and 99th percentile
    ln_wage = log(case_when(wage < quantile(wage, .01, na.rm = T) ~ quantile(wage, .01, na.rm = T),
                            wage > quantile(wage, .99, na.rm = T) ~ quantile(wage, .99, na.rm = T),
                            T ~ wage)),
    ## Create controls
    educ = factor(ifelse(educ == 1 | educ == 999, NA,
                         ## Less than high school
                         ifelse(educ <= 60 | educ == 71, 1,
                                ## HS degree (include diploma unclear 72)
                                ifelse(educ == 70 | educ == 72 | educ == 73, 2,
                                       ## Some college
                                       ifelse(educ < 110, 3,
                                              ## College or more
                                              4))))),
    ## Family status is whether married with spouse present
    married = ifelse(marst == 9, NA, marst == 1),
    race = factor(ifelse(race == 100, 1,
                         ifelse(race == 200, 2, 3)),
                  labels = c("White","Black","Other")),
    mother = (nchild > 0)
  ) %>%
  select(pernum, ln_wage, mother, age, educ, married, race, asecwt, starts_with("repwtp")) %>%
  filter(asecwt > 0) %>%
  ## Remove those with missing hourly wages
  filter(!is.na(ln_wage)) %>%
  ## Remove those with missing education
  filter(!is.na(educ))

###################################
# Note the common support problem #
###################################

support_data <- d %>%
  group_by(mother, age, educ, race, married) %>%
  mutate(num_in_group = n()) %>%
  group_by(age, educ, race, married) %>%
  # This stratum has support if mothers and non-mothers are observed
  mutate(has_support = n_distinct(mother) == 2) %>%
  group_by(mother) %>%
  mutate(prop_with_support = weighted.mean(has_support, w = asecwt))

with_support <- support_data %>%
  filter(has_support)

print(with_support %>%
        group_by(mother) %>%
        filter(1:n() == 1) %>%
        select(mother,starts_with("prop")))

print("These groups have no support")
print(data.frame(support_data %>%
                   filter(!has_support) %>%
                   select(mother, educ, married, race, age) %>%
                   arrange(mother, educ, married, race, age)))

# All remaining analyses will focus on those with support only

###################
# Descriptive gap #
###################

make_results <- function(weight_name) {
  d_case <- with_support
  d_case$weight <- d_case[[weight_name]]
  # Normalize weights
  d_case$weight <- d_case$weight / sum(d_case$weight)
  unadjusted <- d_case %>%
    group_by(mother) %>%
    summarize(Estimate = weighted.mean(ln_wage, w = weight)) %>%
    spread(key = mother, value = Estimate) %>%
    mutate(approach = "Unadjusted",
           Estimate = `TRUE` - `FALSE`) %>%
    select(approach, Estimate)
  
  nonparametric_adjusted <- d_case %>%
    group_by(mother, age, educ, race, married) %>%
    summarize(estimate = weighted.mean(ln_wage, w = weight),
              weight = sum(weight)) %>%
    group_by(age, educ, race, married) %>%
    # Give this stratum the weight of mothers in this stratum
    mutate(weight = sum(weight * mother)) %>%
    group_by(mother) %>%
    summarize(Estimate = weighted.mean(estimate, w = weight)) %>%
    spread(key = mother, value = Estimate) %>%
    mutate(approach = "Stratification",
           Estimate = `TRUE` - `FALSE`) %>%
    select(approach, Estimate)
  
  ols_additive_fit <- lm(ln_wage ~ mother + age + I(age ^ 2) +
                           educ + race + married,
                         data = d_case,
                         weights = weight)
  ols_interactive_fit <- lm(ln_wage ~ mother*(age + I(age ^ 2)) +
                              educ + race + married,
                            data = d_case,
                            weights = weight)
  ols_ageFactor_fit <- lm(ln_wage ~ mother*(factor(age)) +
                            educ + race + married,
                          data = d_case,
                          weights = weight)
  gam_interactive_fit <- gam(ln_wage ~ mother + s(age, by = mother) +
                               educ + race + married,
                             data = d_case %>%
                               group_by() %>%
                               mutate(mother = factor(mother)),
                             weight = d_case$weight)
  
  to_predict <- d_case %>%
    group_by() %>%
    filter(mother)
  
  ols_additive_adjusted <- data.frame(
    approach = "OLS (additive age)",
    Estimate = weighted.mean(predict(ols_additive_fit, newdata = to_predict %>% mutate(mother = T)) -
                               predict(ols_additive_fit, newdata = to_predict %>% mutate(mother = F)),
                             w = to_predict$weight)
  )
  
  ols_interactive_adjusted <- data.frame(
    approach = "OLS (interactive age)",
    Estimate = weighted.mean(predict(ols_interactive_fit, newdata = to_predict %>% mutate(mother = T)) -
                               predict(ols_interactive_fit, newdata = to_predict %>% mutate(mother = F)),
                             w = to_predict$weight)
  )

  ols_ageFactor_adjusted <- data.frame(
    approach = "OLS (age factor)",
    Estimate = weighted.mean(predict(ols_ageFactor_fit, newdata = to_predict %>% mutate(mother = T)) -
                               predict(ols_ageFactor_fit, newdata = to_predict %>% mutate(mother = F)),
                             w = to_predict$weight)
  )
  
  gam_interactive_adjusted <- data.frame(
    approach = "GAM (interactive age)",
    Estimate = weighted.mean(predict(gam_interactive_fit, newdata = to_predict %>% mutate(mother = T)) -
                               predict(gam_interactive_fit, newdata = to_predict %>% mutate(mother = F)),
                             w = to_predict$weight)
  )
  
  return(ols_additive_adjusted %>%
           bind_rows(ols_interactive_adjusted) %>%
           bind_rows(ols_ageFactor_adjusted) %>%
           bind_rows(gam_interactive_adjusted) %>%
           bind_rows(nonparametric_adjusted))
}

estimate_point <- make_results("asecwt")
replicates_aggregate <- foreach(i = 1:160, .combine = "rbind") %do% {
  make_results(paste0("repwtp",i))
}

# Note how replicate draws look
replicates_aggregate %>% 
  ggplot(aes(x = Estimate)) + 
  facet_wrap(~approach, ncol = 1) + 
  geom_density() +
  geom_vline(data = estimate_point,
             aes(xintercept = Estimate))

estimate_variance <- replicates_aggregate %>%
  rename(Estimate_star = Estimate) %>%
  left_join(estimate_point,
            by = "approach") %>%
  group_by(approach) %>%
  summarize(variance = 4 / 160 * sum((Estimate_star - Estimate) ^ 2))

aggregate_results <- estimate_point %>%
  left_join(estimate_variance, by = "approach") %>%
  mutate(approach = factor(case_when(approach == "Stratification" ~ 1,
                                     approach == "OLS (age factor)" ~ 2,
                                     approach == "GAM (interactive age)" ~ 3,
                                     approach == "OLS (interactive age)" ~ 4,
                                     approach == "OLS (additive age)" ~ 5),
                           labels = c("Stratification:\nNo assumptions",
                                      "+ assume only interaction\nis motherhood x age",
                                      "+ assume smooth\nage form (GAM)",
                                      "+ assume quadratic\nage form",
                                      "+ assume no interactions")))

aggregate_results %>%
  ggplot(aes(x = approach, y = Estimate,
             ymin = Estimate - qnorm(.975) * sqrt(variance),
             ymax = Estimate + qnorm(.975) * sqrt(variance))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(width = .5) +
  theme_bw() +
  scale_x_discrete(name = "Approach") +
  facet_wrap(~approach, scales = "free_x", nrow = 1) +
  ylab("Estimated Log Wage Gap\n(Mothers - Non-mothers)") +
  theme(axis.text.x = element_blank()) +
  ggsave("aggregate_gap.pdf",
         height = 2.5, width = 9)

################################
# NOW DO IT FOR SUBGROUPS: AGE #
################################

make_results <- function(weight_name) {
  d_case <- with_support
  d_case$weight <- d_case[[weight_name]]
  #if (grepl("repwt",weight_name)) {
  #  d_case$weight <- d_case$weight * d_case$asecwt
  #}
  # Normalize weights
  d_case$weight <- d_case$weight / sum(d_case$weight)
  unadjusted <- d_case %>%
    group_by(age,mother) %>%
    summarize(Estimate = weighted.mean(ln_wage, w = weight)) %>%
    spread(key = mother, value = Estimate) %>%
    mutate(approach = "Unadjusted",
           Estimate = `TRUE` - `FALSE`) %>%
    select(age, approach, Estimate)
  
  nonparametric_adjusted <- d_case %>%
    group_by(mother, age, educ, race, married) %>%
    summarize(estimate = weighted.mean(ln_wage, w = weight),
              weight = sum(weight)) %>%
    group_by(age, educ, race, married) %>%
    # Give this stratum the weight of mothers in this stratum
    mutate(weight = sum(weight * mother)) %>%
    group_by(age, mother) %>%
    summarize(Estimate = weighted.mean(estimate, w = weight)) %>%
    spread(key = mother, value = Estimate) %>%
    mutate(approach = "Stratification",
           Estimate = `TRUE` - `FALSE`) %>%
    select(age, approach, Estimate)
  
  ols_additive_fit <- lm(ln_wage ~ mother + age + I(age ^ 2) +
                           educ + race + married,
                         data = d_case,
                         weights = weight)
  ols_interactive_fit <- lm(ln_wage ~ mother*(age + I(age ^ 2)) +
                              educ + race + married,
                            data = d_case,
                            weights = weight)
  ols_ageFactor_fit <- lm(ln_wage ~ mother*(factor(age)) +
                               educ + race + married,
                             data = d_case,
                             weights = weight)
  gam_interactive_fit <- gam(ln_wage ~ mother + s(age, by = mother) +
                               educ + race + married,
                             data = d_case %>%
                               group_by() %>%
                               mutate(mother = factor(mother)),
                             weight = d_case$weight)
  
  to_predict <- d_case %>%
    group_by(age) %>%
    filter(1:n() == 1) %>%
    group_by()
  
  ols_additive_adjusted <- to_predict %>%
    mutate(Estimate = predict(ols_additive_fit, newdata = to_predict %>% mutate(mother = T)) -
             predict(ols_additive_fit, newdata = to_predict %>% mutate(mother = F)),
           approach = "OLS (additive age)") %>%
    select(age, approach, Estimate)
  ols_interactive_adjusted <- to_predict %>%
    mutate(Estimate = predict(ols_interactive_fit, newdata = to_predict %>% mutate(mother = T)) -
             predict(ols_interactive_fit, newdata = to_predict %>% mutate(mother = F)),
           approach = "OLS (interactive age)") %>%
    select(age, approach, Estimate)
  ols_ageFactor_adjusted <- to_predict %>%
    mutate(Estimate = predict(ols_ageFactor_fit, newdata = to_predict %>% mutate(mother = T)) -
             predict(ols_ageFactor_fit, newdata = to_predict %>% mutate(mother = F)),
           approach = "OLS (age factor)") %>%
    select(age, approach, Estimate)
  gam_interactive_adjusted <- to_predict %>%
    mutate(Estimate = predict(gam_interactive_fit, newdata = to_predict %>% mutate(mother = T)) -
             predict(gam_interactive_fit, newdata = to_predict %>% mutate(mother = F)),
           approach = "GAM (interactive age)") %>%
    select(age, approach, Estimate)
  
  return(data.frame(unadjusted) %>%
           bind_rows(nonparametric_adjusted) %>%
           bind_rows(ols_additive_adjusted) %>%
           bind_rows(ols_interactive_adjusted) %>%
           bind_rows(ols_ageFactor_adjusted) %>%
           bind_rows(gam_interactive_adjusted))
}

estimate_point <- make_results("asecwt")
replicates_age <- foreach(i = 1:160, .combine = "rbind") %do% {
  make_results(paste0("repwtp",i))
}

estimate_variance <- replicates_age %>%
  rename(Estimate_star = Estimate) %>%
  left_join(estimate_point,
            by = c("age","approach")) %>%
  group_by(age,approach) %>%
  summarize(variance = 4 / 160 * sum((Estimate_star - Estimate) ^ 2))

subgroup_results <- estimate_point %>%
  filter(approach != "Unadjusted") %>%
  left_join(estimate_variance, by = c("age","approach")) %>%
  mutate(approach = factor(case_when(approach == "Stratification" ~ 1,
                                     approach == "OLS (age factor)" ~ 2,
                                     approach == "GAM (interactive age)" ~ 3,
                                     approach == "OLS (interactive age)" ~ 4,
                                     approach == "OLS (additive age)" ~ 5),
                           labels = c("Stratification:\nNo assumptions",
                                      "+ assume only interaction\nis motherhood x age",
                                      "+ assume smooth\nage form (GAM)",
                                      "+ assume quadratic\nage form",
                                      "+ assume no interactions")))

subgroup_results %>% 
  ggplot(aes(x = age, y = Estimate, #color = approach,
             ymin = Estimate - qnorm(.975) * sqrt(variance),
             ymax = Estimate + qnorm(.975) * sqrt(variance))) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  #theme(strip.text = element_text(hjust = 0)) +
  facet_wrap(~approach, nrow = 1) +
  xlab("Age") +
  ylab("Estimated Log Wage Gap\n(Mothers - Non-mothers)") +
  #scale_color_manual(name = "Estimator",
  #                   values = c("blue","seagreen4","purple","black")) +
  theme(axis.text.x = element_text(size = 8)) +
  ggsave("gap_by_age.pdf",
         height = 3, width = 9)

####################################
# WHICH LEARNER PREDICTS THE BEST? #
####################################


make_cv_results <- function(weight_name) {
  
  folded <- with_support
  folded$weight <- folded[[weight_name]]
  
  folded <- folded %>%
    group_by() %>%
    arrange(weight) %>%
    mutate(fold = rep(1:5, ceiling(n() / 5))[1:n()])
  
  foreach(f = 1:5, .combine = "rbind") %do% {
    train <- folded %>%
      filter(fold != f)
    test <- folded %>%
      filter(fold == f)
    ols_additive_fit <- lm(ln_wage ~ mother + age + I(age ^ 2) +
                             educ + race + married,
                           data = train,
                           weights = weight)
    ols_interactive_fit <- lm(ln_wage ~ mother*(age + I(age ^ 2)) +
                                educ + race + married,
                              data = train,
                              weights = weight)
    ols_ageFactor_fit <- lm(ln_wage ~ mother*(factor(age)) +
                              educ + race + married,
                            data = train,
                            weights = weight)
    gam_interactive_fit <- gam(ln_wage ~ mother + s(age, by = mother) +
                                 educ + race + married,
                               data = train %>%
                                 group_by() %>%
                                 mutate(mother = factor(mother)),
                               weight = train$weight)
    
    stratification_result <- train %>%
      group_by(mother, age, educ, race, married) %>%
      summarize(yhat = weighted.mean(ln_wage, w = weight)) %>%
      inner_join(test, by = c("mother","age","educ","race","married")) %>%
      mutate(squared_error = (ln_wage - yhat) ^ 2) %>%
      group_by() %>%
      mutate(approach = "Stratification") %>%
      select(approach, squared_error, weight)
    
    model_squared_errors <- test %>%
      select(ln_wage, weight) %>%
      mutate(ols_additive = predict(ols_additive_fit, newdata = test),
             ols_interactive = predict(ols_interactive_fit, newdata = test),
             ols_ageFactor = predict(ols_ageFactor_fit, newdata = test),
             gam_interactive = predict(gam_interactive_fit, newdata = test)) %>%
      melt(id = c("ln_wage","weight"), variable.name = "approach", value.name = "yhat") %>%
      mutate(squared_error = (ln_wage - yhat) ^ 2)
    
    stratification_result %>%
      bind_rows(model_squared_errors)
  } %>%
    group_by(approach) %>%
    summarize(mse = weighted.mean(squared_error, w = weight))
}

cv_point <- make_cv_results("asecwt")
replicates <- foreach(i = 1:160, .combine = "rbind") %do% {
  make_cv_results(paste0("repwtp",i))
}

cv_variance <- replicates %>%
  rename(mse_star = mse) %>%
  left_join(cv_point,
            by = c("approach")) %>%
  group_by(approach) %>%
  summarize(variance = 4 / 160 * sum((mse_star - mse) ^ 2))

cv_results <- cv_point %>%
  left_join(cv_variance, by = "approach") %>%
  mutate(approach = factor(case_when(approach == "Stratification" ~ 1,
                                     approach == "ols_ageFactor" ~ 2,
                                     approach == "gam_interactive" ~ 3,
                                     approach == "ols_interactive" ~ 4,
                                     approach == "ols_additive" ~ 5),
                           labels = c("Stratification:\nNo assumptions",
                                      "+ assume only interaction\nis motherhood x age",
                                      "+ assume smooth\nage form (GAM)",
                                      "+ assume quadratic\nage form",
                                      "+ assume no interactions")))
cv_results %>%
  mutate(approach = fct_rev(approach),
         best = mse == min(mse))

# GAP ESTIMATES PLOT
for_aggregate_plot <- aggregate_results %>%
  mutate(estimand = "Aggregate\ngap",
         age = 35) %>%
  bind_rows(subgroup_results %>%
              mutate(estimand = "Age-specific\ngap")) %>%
  mutate(estimand = fct_relevel(estimand,
                                "Aggregate\ngap",
                                "Age-specific\ngap"))
for_aggregate_plot %>%
  ggplot(aes(x = age, y = Estimate,
             ymin = Estimate - qnorm(.975) * sqrt(variance),
             ymax = Estimate + qnorm(.975) * sqrt(variance))) +
  geom_hline(yintercept = 0, color = "red") +
  geom_errorbar(width = .5) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "Difference in log hourly wage\n(Mother - Non-mothers)",
                     limits = c(-.25,.25)) +
  xlab("Age") +
  facet_grid(estimand ~ approach) +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  # Annotate within facets
  geom_text(data = for_aggregate_plot %>%
              filter(estimand == "Aggregate\ngap") %>%
              mutate(age = ifelse(approach == "Stratification:\nNo assumptions",25,45),
                     Estimate = .25,
                     label = case_when(
                       approach == "Stratification:\nNo assumptions" ~ "Weakest assumptions\n(most credible)",
                       approach == "+ assume no interactions" ~ "Strongest assumptions\n(least credible)"
                     )),
            aes(label = label, hjust = ifelse(approach == "Stratification:\nNo assumptions",0,1)),
            vjust = 1,
            size = 3) +
  geom_text(data = for_aggregate_plot %>%
              filter(estimand == "Aggregate\ngap" &
                       approach == "+ assume smooth\nage form (GAM)") %>%
              mutate(Estimate = .25),
            label = "All estimation strategies\nestimate a similar\naggregate gap",
            size = 3, vjust = 1) +
  geom_text(data = for_aggregate_plot %>%
              filter(estimand == "Aggregate\ngap" & approach == "+ assume no interactions") %>%
              mutate(age = 45,
                     Estimate = -.25),
            label = "This panel parallels the estimand and\n specification in Pal and Waldfogel (2016)",
            vjust = 0, hjust = 1,
            size = 2) +
  geom_segment(data = for_aggregate_plot %>%
                 filter(estimand == "Aggregate\ngap") %>%
                 mutate(Estimate = .19,
                        age = case_when(approach == "+ assume smooth\nage form (GAM)" ~ 27),
                        xend = 25),
               aes(xend = xend, yend = Estimate),
               arrow = arrow(length = unit(.15,"cm"))) +
  geom_segment(data = for_aggregate_plot %>%
                 filter(estimand == "Aggregate\ngap") %>%
                 mutate(Estimate = .19,
                        age = case_when(approach == "+ assume smooth\nage form (GAM)" ~ 43),
                        xend = 45),
               aes(xend = xend, yend = Estimate),
               arrow = arrow(length = unit(.15,"cm"))) +
  geom_text(data = for_aggregate_plot %>%
              filter(estimand == "Age-specific\ngap" & age == 25) %>%
              mutate(Estimate = .25,
                     label = case_when(
                       approach == "Stratification:\nNo assumptions" ~ "Uselessly uncertain\nage-specific\ngap",
                       approach == "+ assume only interaction\nis motherhood x age" ~ "Uselessly uncertain\nage-specific\ngap",
                       approach == "+ assume smooth\nage form (GAM)" ~ "With this assumption,\nevidence shows a gap\nat young ages only",
                       approach == "+ assume no interactions" ~ "Additive OLS assumes\na constant effect.\nA gap exists at all ages\nonly by assumption."
                     )),
            aes(label = label),
            size = 3, hjust = 0, vjust = 1) +
  ggsave("all_gap_estimates.pdf",
         height = 5, width = 10)

  

