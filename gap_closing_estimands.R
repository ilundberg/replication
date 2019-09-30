
library(tidyverse)
library(reshape2)
library(survey)

# Author: Ian Lundberg (ilundberg@princeton.edu)
# Title: Gap-closing estimators to study categorical inequality that persists under a local intervention to equalize a treatment
# Paper is a working draft. Code will be posted on the Harvard Dataverse when the paper is finalized for publication.

setwd("/Users/iandl/Dropbox/Dissertation/gap_closing_estimands")

# Load data
source("data/class_ceiling_with_sample/GSS.r")
crosswalk <- read_csv("data/occ10-to-egp-class-crosswalk.csv")

set.seed(08544)

d <- GSS %>%
  left_join(crosswalk %>%
              select(occ10,egp10_10) %>%
              rename(OCC10 = occ10,
                     egp_r = egp10_10), by = "OCC10") %>%
  left_join(crosswalk %>%
              select(occ10,egp10_10) %>%
              rename(PAOCC10 = occ10,
                     egp_pa = egp10_10), by = "PAOCC10") %>%
  mutate(treated = egp_r == 1,
         subgroup = egp_pa == 1) %>%
  filter(!(SAMPLE %in% c(4,5,7)) & YEAR >= 1975) %>%
  mutate(n_raw = n()) %>%
  filter(AGE >= 30 & AGE <= 45) %>%
  mutate(n_ages = n()) %>%
  filter(PAOCC10 != 0) %>%
  mutate(n_parent_occ = n()) %>%
  filter(OCC10 != 0) %>%
  mutate(n_r_occ = n()) %>%
  filter(!is.na(egp_pa)) %>%
  filter(!is.na(egp_r)) %>%
  mutate(n_egp_occ = n()) %>%
  # For common support
  filter(DEGREE != 0) %>%
  filter(!(DEGREE %in% c(0,8,9))) %>%
  mutate(n_atLeastHS = n()) %>%
  filter(REALRINC > 0) %>%
  mutate(n_analytic = n()) %>%
  mutate(log_income = log(REALRINC)) %>%
  mutate(parent_class1 = egp_pa == 1) %>%
  # Adjust to give equal total weight to each year.
  group_by(YEAR) %>%
  mutate(WTSSALL = WTSSALL / sum(WTSSALL)) %>%
  group_by() %>%
  mutate(id = 1:n()) %>%
  arrange(subgroup, treated, RACE, DEGREE, SEX, AGE, WTSSALL) %>%
  mutate(fold = do.call(c, lapply(1:ceiling(n() / 5), function(x) sample(1:5)))[1:n()]) %>%
  group_by() %>%
  mutate(RACE = factor(RACE), DEGREE = factor(DEGREE))

# Note sample sizes
d %>%
  filter((1:n()) == 1) %>%
  select(starts_with("n_"))

# Note some class 1 occupations
unique(crosswalk$title[crosswalk$egp10_10 == 1])

library(foreach)
library(ranger)
run_estimators <- function(data, bs = F) {
  if (bs) {
    data <- data %>%
      # Putting race and degree in the group_by to avoid trouble with new factor levels
      group_by(subgroup, treated, RACE, DEGREE) %>%
      sample_frac(1, replace = T) %>%
      group_by() %>%
      arrange(subgroup, treated, RACE, DEGREE, SEX, AGE, WTSSALL) %>%
      mutate(fold = do.call(c, lapply(1:ceiling(n() / 5), function(x) sample(1:5)))[1:n()])
  }
  unadjusted <- data %>%
    group_by()
  with_prediction_rf <- foreach(x = unique(data$subgroup), .combine = "rbind") %do% {
    foreach(f = unique(data$fold), .combine = "rbind") %do% {
      to_predict <- data %>% filter(fold == f & subgroup == x)
      fit_m <- ranger(as.numeric(treated) ~ RACE + SEX + AGE + DEGREE,
                      data = data %>% filter(subgroup == x & fold != f))
      fit_g <- ranger(log_income ~ RACE + SEX + AGE + DEGREE,
                      data = data %>% filter(subgroup == x & treated & fold != f))
      to_predict$mhat <- predict(fit_m, data = to_predict)$predictions
      to_predict$ghat <- predict(fit_g, data = to_predict)$predictions
      return(to_predict %>%
               select(subgroup, treated, WTSSALL, mhat, ghat, log_income))
    }
  }
  with_prediction_ols <- foreach(x = unique(data$subgroup), .combine = "rbind") %do% {
    foreach(f = unique(data$fold), .combine = "rbind") %do% {
      to_predict <- data %>% filter(fold == f & subgroup == x)
      fit_m <- glm(treated ~ RACE + SEX + AGE + DEGREE,
                   data = data %>% filter(subgroup == x & fold != f),
                   family = binomial(link = "logit"))
      fit_g <- lm(log_income ~ RACE + SEX + AGE + DEGREE,
                  data = data %>% filter(subgroup == x & treated & fold != f))
      to_predict$mhat <- predict(fit_m, newdata = to_predict)
      to_predict$ghat <- predict(fit_g, newdata = to_predict)
      return(to_predict %>%
               select(subgroup, treated, WTSSALL, mhat, ghat, log_income))
    }
  }
  results <- with_prediction_rf %>%
    mutate(method = "Random forest") %>%
    bind_rows(with_prediction_ols %>%
                mutate(method = "OLS")) %>%
    group_by(method, subgroup) %>%
    summarize(estimate = weighted.mean(ghat - ifelse(treated, (ghat - log_income) / mhat, 0),
                                       w = WTSSALL)) %>%
    bind_rows(data %>%
                group_by(subgroup) %>%
                summarize(estimate = weighted.mean(log_income, w = WTSSALL)) %>%
                mutate(method = "Unadjusted") %>%
                group_by() %>%
                select(method, subgroup, estimate)) %>%
    bind_rows(data %>%
                filter(treated) %>%
                group_by(subgroup) %>%
                summarize(estimate = weighted.mean(log_income, w = WTSSALL)) %>%
                mutate(method = "Unadjusted_Treated") %>%
                group_by() %>%
                select(method, subgroup, estimate)) %>%
    group_by() %>%
    spread(key = subgroup, value = estimate) %>%
    rename(Group0 = `FALSE`, Group1 = `TRUE`) %>%
    mutate(gap =  Group1 - Group0)
    
  return(results)
}
tau_hat <- run_estimators(d, bs = F)
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)
tau_hat_draws <- foreach(i = 1:100, .combine = "rbind", .packages = c("tidyverse","foreach","ranger")) %dopar% {
  run_estimators(d, bs = T)
}
stopCluster(cl)

for_plot <- tau_hat_draws %>%
  melt(id = "method") %>%
  group_by(method, variable) %>%
  summarize(se = sd(value)) %>%
  left_join(tau_hat %>% melt(id = "method", value.name = "estimate"),
            by = c("method", "variable")) %>%
  group_by() %>%
  mutate(panel = ifelse(variable == "gap", "B. Difference: Gap that remains", "A. Group-specific potential outcome"),
         variable = case_when(variable == "Group0" ~ "Low class origin",
                              variable == "Group1" ~ "High class origin",
                              variable == "gap" ~ "Gap: High - Low"))

for_plot %>%
  filter(method == "OLS" | method == "Random forest") %>%
  ggplot(aes(x = variable, y = estimate,
             ymin = estimate - qnorm(.975)*se,
             ymax = estimate + qnorm(.975)*se,
             color = method)) +
  geom_point(position = position_dodge(width = 1)) +
  geom_errorbar(position = position_dodge(width = 1),
                width = .5) +
  facet_wrap(~panel, scales = "free") +
  theme_bw() +
  scale_color_manual(values = c("blue","seagreen4"),
                     name = "Method") +
  scale_x_discrete(name = element_blank()) +
  ylab("Estimate") +
  ggsave("figures/dml_comparison.pdf",
         height = 3, width = 6.5)


for_plot %>%
  mutate(method = case_when(method == "Unadjusted_Treated" ~ "If observed in high-class destination",
                            method == "Unadjusted" ~ "Overall",
                            T ~ paste0("If assigned to high-class destination: ",method)),
         method = fct_relevel(method, "Overall","If observed in high-class destination",
                              "If assigned to high-class destination: OLS",
                              "If assigned to high-class destination: Random forest")) %>%
  ggplot(aes(x = variable, y = estimate,
             ymin = estimate - qnorm(.975)*se,
             ymax = estimate + qnorm(.975)*se,
             color = method)) +
  geom_point(position = position_dodge(width = .7)) +
  geom_errorbar(position = position_dodge(width = .7),
                width = .5) +
  facet_wrap(~panel, scales = "free") +
  theme_bw() +
  scale_color_manual(values = c("black","blue","seagreen4","purple"),
                     name = "Method") +
  scale_x_discrete(name = element_blank()) +
  ylab("Estimate")

simple_fit <- lm(log_income ~ treated + subgroup + RACE + SEX + AGE + DEGREE,
                 data = d,
                 weights = WTSSALL)
for_plot %>%
  filter(variable == "Gap: High - Low") %>%
  filter(method != "OLS") %>%
  select(method, estimate, se) %>%
  bind_rows(data.frame(method = "Coefficient",
                       estimate = coef(simple_fit)["subgroupTRUE"],
                       se = sqrt(diag(vcov(simple_fit))["subgroupTRUE"]))) %>%
  mutate(method = case_when(method == "Unadjusted_Treated" ~ "B. Descriptive gap given\nhigh class destination",
                            method == "Unadjusted" ~ "A. Descriptive gap",
                            method == "Random forest" ~ "C. Gap-closing estimand:\nExpected gap if assigned\nto high class destination")) %>%
  filter(!is.na(method)) %>%
  ggplot(aes(x = method, y = estimate,
             label = format(round(estimate,2),nsmall = 2),
             ymin = estimate - qnorm(.975)*se,
             ymax = estimate + qnorm(.975)*se)) +
  geom_errorbar() +
  geom_label() +
  xlab("Estimand") +
  ylab("Estimate: Gap in log\nincome by class origin") +
  theme_bw() +
  ggsave("figures/gap_closing_rf.pdf",
         height = 2.5, width = 6.5)

fit <- lm(log_income ~ subgroup + factor(RACE) + SEX + AGE + factor(DEGREE),
          data = d %>% filter(treated),
          weights = (d %>% filter(treated))$WTSSALL)

for_plot %>%
  filter(variable == "Gap: High - Low") %>%
  filter(method != "OLS") %>%
  select(method, estimate, se) %>%
  bind_rows(data.frame(method = "Coefficient",
                       estimate = coef(simple_fit)["subgroupTRUE"],
                       se = sqrt(diag(vcov(simple_fit))["subgroupTRUE"]))) %>%
  mutate(method = case_when(method == "Unadjusted_Treated" ~ "B. Descriptive gap given\nhigh class destination",
                            method == "Unadjusted" ~ "A. Descriptive gap")) %>%
  filter(!is.na(method)) %>%
  bind_rows(data.frame(method = "C. Coefficient on class origin\ngiven all predictors",
                       estimate = coef(fit)["subgroupTRUE"],
                       se = sqrt(diag(vcov(fit))["subgroupTRUE"]))) %>%
  ggplot(aes(x = method, y = estimate,
             label = format(round(estimate,2),nsmall = 2),
             ymin = estimate - qnorm(.975)*se,
             ymax = estimate + qnorm(.975)*se)) +
  geom_errorbar() +
  geom_label() +
  xlab("Estimand") +
  ylab("Estimate: Gap in log\nincome by class origin") +
  theme_bw() +
  ggsave("figures/gap_closing_coefficient_comparison.pdf",
         height = 2.5, width = 6.5)


