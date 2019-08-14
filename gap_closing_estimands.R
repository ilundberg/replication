
library(tidyverse)
library(reshape2)
library(survey)

# Author: Ian Lundberg (ilundberg@princeton.edu)
# Title: Gap-closing estimands: Gaps that remain after an intervention
# Paper is a working draft. Code will be posted on the Harvard Dataverse when the paper is finalized for publication.

setwd("/Users/iandl/Dropbox/Dissertation/gap_closing_estimands")

# Load data
source("data/class_ceiling_with_sample/GSS.r")
crosswalk <- read_csv("data/occ10-to-egp-class-crosswalk.csv")

d <- GSS %>%
  left_join(crosswalk %>%
              select(occ10,egp10_10) %>%
              rename(OCC10 = occ10,
                     egp_r = egp10_10), by = "OCC10") %>%
  left_join(crosswalk %>%
              select(occ10,egp10_10) %>%
              rename(PAOCC10 = occ10,
                     egp_pa = egp10_10), by = "PAOCC10") %>%
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
  group_by(YEAR) %>%
  mutate(WTSSALL = WTSSALL / sum(WTSSALL)) %>%
  group_by() %>%
  mutate(id = 1:n())

# Note sample sizes
d %>%
  filter((1:n()) == 1) %>%
  select(starts_with("n_"))

# Note class 1 occupations
unique(crosswalk$title[crosswalk$egp10_10 == 1])

# Store survey design
# Taken from https://gssdataexplorer.norc.org/pages/show?page=gss%2Fstandard_error
design <- svydesign(ids = ~ VPSU,
                    weights = ~ WTSSALL,
                    strata = ~VSTRAT,
                    data = d,
                    nest = T)

d_given_M <- d %>% filter(egp_r == 1)
design_given_M <- svydesign(ids = ~ VPSU,
                            weights = ~ WTSSALL,
                            strata = ~VSTRAT,
                            data = d_given_M,
                            nest = T)

options(survey.lonely.psu = "adjust")

# Fit models
fit_gap <- svyglm(log_income ~ parent_class1,
                  design = design)
fit_gap_given_M <- svyglm(log_income ~ parent_class1,
                          data = d_given_M,
                          design = design_given_M)
fit_gap_given_ML_additive <- svyglm(log_income ~ parent_class1 + factor(RACE) + SEX + AGE + factor(DEGREE),
                                    data = d_given_M,
                                    design = design_given_M)
fit_gap_given_ML <- svyglm(log_income ~ parent_class1*(factor(RACE) + SEX + AGE + factor(DEGREE)),
                           data = d_given_M,
                           design = design_given_M)

# Get covariate means at which to predict gaps
Xbar_subgroup1 <- apply(
  model.matrix(formula(fit_gap_given_ML),
               data = d %>%
                 filter(parent_class1)),
  2, function(x) weighted.mean(x, w = d$WTSSALL[d$parent_class1])
)
Xbar_subgroup0 <- apply(
  model.matrix(formula(fit_gap_given_ML),
               data = d %>%
                 filter(!parent_class1)),
  2, function(x) weighted.mean(x, w = d$WTSSALL[!d$parent_class1])
)
Xbar_difference <- Xbar_subgroup1 - Xbar_subgroup0

# Predict gaps
gaps <- data.frame(
  gap_estimand = "A. Descriptive gap",
  gap_estimate = coef(fit_gap)["parent_class1TRUE"],
  gap_variance = diag(vcov(fit_gap))["parent_class1TRUE"]
) %>%
  bind_rows(data.frame(
    gap_estimand = "B. Descriptive gap\ngiven high class destination",
    gap_estimate = coef(fit_gap_given_M)["parent_class1TRUE"],
    gap_variance = diag(vcov(fit_gap_given_M))["parent_class1TRUE"]
  )) %>%
  bind_rows(data.frame(
    gap_estimand = "C. Gap-closing estimand:\nExpected gap if assigned\nto high class destination",
    gap_estimate = Xbar_difference %*% coef(fit_gap_given_ML),
    gap_variance = Xbar_difference %*% vcov(fit_gap_given_ML) %*% Xbar_difference
  )) %>%
  bind_rows(data.frame(
    gap_estimand = "C. Coefficient on class origin\ngiven all predictors",
    gap_estimate = coef(fit_gap_given_ML_additive)["parent_class1TRUE"],
    gap_variance = diag(vcov(fit_gap_given_ML_additive))["parent_class1TRUE"]
  ))

gaps %>%
  filter(gap_estimand != "C. Coefficient on class origin\ngiven all predictors") %>%
  ggplot(aes(x = gap_estimand, y = gap_estimate,
             ymin = gap_estimate - qnorm(.975)*sqrt(gap_variance),
             ymax = gap_estimate + qnorm(.975)*sqrt(gap_variance),
             label = format(round(gap_estimate,2),digits = 2))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .5) +
  geom_label() +
  theme_bw() +
  ylab("Estimated log income gap\nby social origins") +
  xlab("Estimand") +
  theme_bw() +
  ggsave("figures/class_gap_gss.pdf",
         height = 3, width = 6.5)

gaps %>%
  filter(gap_estimand != "C. Gap-closing estimand:\nExpected gap if assigned\nto high class destination") %>%
  ggplot(aes(x = gap_estimand, y = gap_estimate,
             ymin = gap_estimate - qnorm(.975)*sqrt(gap_variance),
             ymax = gap_estimate + qnorm(.975)*sqrt(gap_variance),
             label = format(round(gap_estimate,2),digits = 2))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .5) +
  geom_label() +
  theme_bw() +
  ylab("Estimated log income gap\nby social origins") +
  xlab("Estimand") +
  theme_bw() +
  ggsave("figures/class_gap_gss_misleading.pdf",
         height = 3, width = 6.5)

