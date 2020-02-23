
# Author: Ian Lundberg (ilundberg@princeton.edu)
# Title: A causal approach to study interventions that close gaps across social categories
# Paper is a working draft. Code will be posted on the Harvard Dataverse when the paper is finalized for publication.

library(tidyverse)
library(reshape2)
library(survey)
library(haven)
library(foreach)
library(doParallel)
library(doRNG)
library(survey)
library(ranger)
library(Amelia)
library(xtable)

setwd("C:/Users/iandl/Documents/gap_closing_estimands")
#setwd("/Users/iandl/Dropbox/Dissertation/gap_closing_estimands")
set.seed(08544)

# Load data
crosswalk <- read_csv("data/occ10-to-egp-class-crosswalk.csv")
source("data/class_ceiling_with_sample/GSS.r")

normalize <- function(x) x / sum(x)

# Note that lone PSUs are very rare in the sample
GSS %>%
  group_by(VSTRAT) %>%
  mutate(lone_psu = n_distinct(VPSU) == 1) %>%
  group_by() %>%
  summarize(num_lone_psu = sum(lone_psu),
            mean_lone_psu = weighted.mean(lone_psu, w = WTSSALL))

# Remove the lone PSUs, merge in crosswalk, and define some variables
GSS <- GSS  %>%
  group_by(VSTRAT) %>%
  mutate(lone_psu = n_distinct(VPSU) == 1) %>%
  group_by() %>%
  filter(!lone_psu) %>%
  select(-lone_psu) %>%
  left_join(crosswalk %>%
              select(occ10,egp10_10) %>%
              rename(OCC10 = occ10,
                     egp_r = egp10_10), by = "OCC10") %>%
  left_join(crosswalk %>%
              select(occ10,egp10_10) %>%
              rename(PAOCC10 = occ10,
                     egp_pa = egp10_10), by = "PAOCC10") %>%
  # Define treatment t and group-identifying variable g
  mutate(treated = egp_r == 1,
         X = egp_pa == 1)
save(GSS, file = "intermediate/GSS_clean.Rdata")

sample_restrictions <- function(raw_data) {
  print("Number in age range:")
  print(sum(raw_data$AGE >= 30 & raw_data$AGE <= 45))
  print("Number not NA on parent occupation:")
  print(sum(raw_data$AGE >= 30 & raw_data$AGE <= 45 & raw_data$PAOCC10 > 0))
  print("Number not NA on own occupation:")
  print(sum(raw_data$AGE >= 30 & raw_data$AGE <= 45 & raw_data$PAOCC10 > 0 & raw_data$OCC10 > 0))
  print("Number reporting positive incomes:")
  print(sum((raw_data$AGE >= 30 & raw_data$AGE <= 45 & raw_data$PAOCC10 > 0 & raw_data$OCC10 > 0 & raw_data$REALRINC > 0)))
  to_impute <- raw_data %>%
    filter(AGE >= 30 & AGE <= 45) %>%
    filter(PAOCC10 > 0) %>%
    filter(OCC10 > 0) %>%
    filter(REALRINC > 0) %>%
    mutate(outcome = log(REALRINC),
           DEGREE = case_when(!(DEGREE %in% 8:9) ~ DEGREE),
           RACE = factor(RACE), 
           DEGREE = factor(DEGREE),
           id = 1:n()) %>%
    group_by(YEAR) %>%
    mutate(weight = normalize(WTSSALL)) %>%
    group_by() %>%
    select(id, VSTRAT, weight, outcome, treated, X, RACE, SEX, DEGREE, AGE)
  
  # Note prevalence of missingness
  print("Weighted proportion missing")
  print(to_impute %>%
          summarize(outcome = weighted.mean(is.na(outcome), w = weight), 
                    treated = weighted.mean(is.na(treated), w = weight), 
                    X = weighted.mean(is.na(X), w = weight),
                    RACE = weighted.mean(is.na(RACE), w = weight), 
                    DEGREE = weighted.mean(is.na(DEGREE), w = weight), 
                    SEX = weighted.mean(is.na(SEX), w = weight),
                    AGE = weighted.mean(is.na(AGE), w = weight))
  )
  
  # Singly impute since uncertainty will come from BRR instead
  imputed <- amelia(data.frame(to_impute),
                    noms = c("RACE","SEX","DEGREE","treated","X"),
                    idvars = c("id","VSTRAT"),
                    boot.type = "none", m = 1)$imputations$imp1
  return(imputed)
}

d <- sample_restrictions(GSS)

# Repeat that several times to average over randomness, and produce descriptive statistics
set.seed(08544)
many_imputations <- foreach(i = 1:10, .combine = "rbind") %do% sample_restrictions(GSS)

# Note prevalence of group and of treatment
print("Proportion upper class")
print(many_imputations %>%
        summarize(X = weighted.mean(X, w = weight),
                  treated = weighted.mean(treated, w = weight)))

# Make table of descriptive statistics by X
matrix_of_variables <- model.matrix(~-1 + outcome + treated + RACE + DEGREE + SEX + AGE,
                                    data = many_imputations)
descriptives_upperOrigin <- apply(matrix_of_variables[many_imputations$X,], 2, weighted.mean, w = many_imputations$weight[many_imputations$X])
descriptives_lowerOrigin <- apply(matrix_of_variables[!many_imputations$X,], 2, weighted.mean, w = many_imputations$weight[!many_imputations$X])
descriptives <- cbind(upperOrigin = descriptives_upperOrigin,
                      lowerOrigin = descriptives_lowerOrigin)
# Add the omitted category of treatments
descriptives <- rbind(descriptives[1:3,], RACE1 = 1 - colSums(descriptives[4:5,]), descriptives[4:11,])
# Make sex 0 = male, 1 = female instead of 1 and 2
descriptives["SEX",] <- descriptives["SEX",] - 1
# Update names for the table
descriptives <- rbind(descriptives[1:6,], DEGREE0 = 1 - colSums(descriptives[7:10,]), descriptives[7:12,])
# Remove the redundant lower class occupation
descriptives <- descriptives[-2,]
rownames(descriptives) <- sapply(rownames(descriptives), function(x) {
  case_when(x == "outcome" ~ "Log income",
            x == "treatedTRUE" ~ "Upper class occupation",
            x == "RACE1" ~ "Race: White",
            x == "RACE2" ~ "Race: Black",
            x == "RACE3" ~ "Race: Other",
            x == "DEGREE0" ~ "Degree: Less than high school",
            x == "DEGREE1" ~ "Degree: High school",
            x == "DEGREE2" ~ "Degree: Junior college",
            x == "DEGREE3" ~ "Degree: Bachelor's",
            x == "DEGREE4" ~ "Degree: Graduate",
            x == "SEX" ~ "Female",
            x == "AGE" ~ "Age")
})
print(xtable::xtable(descriptives))

# Note some class 1 occupations
unique(crosswalk$title[crosswalk$egp10_10 == 1])

# Load the code for analysis (this code will ultimately be part of a package)
source("code/gap_estimator.R")

# Calculate point estimates
point_estimates <- gap_estimator_allMethods(data = d,
                                            treatment_formula = formula(treated ~ X + RACE + DEGREE + SEX + AGE),
                                            outcome_formula = formula(outcome ~ treated*X + RACE + DEGREE + SEX + AGE),
                                            treatment_formula_marginal = formula(treated ~ RACE + DEGREE + SEX + AGE),
                                            stratum_id = "VSTRAT",
                                            num_folds = 2,
                                            learner = "glm") %>%
  mutate(learner = "glm") %>%
  bind_rows(gap_estimator_allMethods(data = d,
                                     treatment_formula = formula(treated ~ X + RACE + DEGREE + SEX + AGE),
                                     outcome_formula = formula(outcome ~ treated + X + RACE + DEGREE + SEX + AGE),
                                     treatment_formula_marginal = formula(treated ~ RACE + DEGREE + SEX + AGE),
                                     stratum_id = "VSTRAT",
                                     num_folds = 2,
                                     learner = "ranger") %>%
              mutate(learner = "ranger"))

# Use balanced repeated replicates to computationally simulate uncertainty.
set.seed(08544)
# Define the survey design in the survey package
gss_svydesign <- svydesign(ids = ~ cluster_id,
                           strata = ~ VSTRAT,
                           weights = ~ WTSSALL,
                           data = GSS %>%
                             mutate(cluster_id = paste(VSTRAT,VPSU, sep = "_")))
# Create balanced repeated replicate weights
# The advantage of a computational approach is that inference works for
# a wide variety of estimation algorithms, without any adaptation.
# This line takes a long time.
gss_bs <- as.svrepdesign(gss_svydesign, type = "BRR", compress = F)
save(gss_bs, file = "intermediate/gss_bs.Rdata")

cl <- makeCluster(20)
registerDoParallel(cl)
t0 <- Sys.time()
# With 20 cores, I expect about 40 minutes
bs_samps_glm <- foreach(
  i = 1:ncol(gss_bs$repweights),
  .combine = "rbind", 
  .packages = c("tidyverse","foreach","reshape2","Amelia")
) %dorng% {
  # Restrict to the chosen PSUs
  GSS_replicate <- GSS[gss_bs$repweights[,i] > 0,]
  d_replicate <- sample_restrictions(GSS_replicate)
  gap_estimator_allMethods(data = d_replicate,
                           treatment_formula = formula(treated ~ X + RACE + DEGREE + SEX + AGE),
                           outcome_formula = formula(outcome ~ treated*X + RACE + DEGREE + SEX + AGE),
                           treatment_formula_marginal = formula(treated ~ RACE + DEGREE + SEX + AGE),
                           stratum_id = "VSTRAT",
                           num_folds = 2,
                           learner = "glm") %>%
    mutate(learner = "glm")
}
spent <- difftime(t0, Sys.time())
stopCluster(cl)
save(bs_samps_glm, file = "intermediate/bs_samps_glm.Rdata")

cl <- makeCluster(2)
registerDoParallel(cl)
t0 <- Sys.time()
bs_samps_ranger <- foreach(
  i = 1:ncol(gss_bs$repweights),
  .combine = "rbind", 
  .packages = c("tidyverse","foreach","reshape2","ranger","Amelia")
) %dorng% {
  # Restrict to the chosen PSUs
  GSS_replicate <- GSS[gss_bs$repweights[,i] > 0,]
  d_replicate <- sample_restrictions(GSS_replicate)
  gap_estimator_allMethods(data = d_replicate,
                           treatment_formula = formula(treated ~ X + RACE + DEGREE + SEX + AGE),
                           outcome_formula = formula(outcome ~ treated + X + RACE + DEGREE + SEX + AGE),
                           treatment_formula_marginal = formula(treated ~ RACE + DEGREE + SEX + AGE),
                           stratum_id = "VSTRAT",
                           num_folds = 2,
                           learner = "ranger") %>%
    mutate(learner = "ranger")
}
spent <- difftime(t0, Sys.time())
stopCluster(cl)
save(bs_samps_ranger, file = "intermediate/bs_samps_ranger.Rdata")

results <- bs_samps_glm %>%
  bind_rows(bs_samps_ranger) %>%
  group_by(estimand,strategy,cross_fit,learner) %>%
  summarize_all(.funs = sd) %>%
  melt(id = c("estimand","strategy","cross_fit","learner"),
       variable.name = "group", value.name = "se") %>%
  left_join(point_estimates %>%
              melt(id = c("estimand","strategy","cross_fit","learner"),
                   variable.name = "group",
                   value.name = "estimate"),
            by = c("estimand","strategy","cross_fit","group","learner"))

# For comparison, get coefficient in conditional model
simple_fit <- coef(lm(outcome ~ X + treated + RACE + DEGREE + SEX + AGE,
                      data = d,# %>% filter(treated),
                      weights = weight))["XTRUE"]
cl <- makeCluster(20)
registerDoParallel(cl)
simple_fit_draws <- foreach(
  i = 1:ncol(gss_bs$repweights),
  .combine = "c", 
  .packages = c("tidyverse","foreach","reshape2","Amelia")
) %dorng% {
  # Restrict to the chosen PSUs
  GSS_replicate <- GSS[gss_bs$repweights[,i] > 0,]
  d_replicate <- sample_restrictions(GSS_replicate)
  fit <- lm(outcome ~ X + treated + RACE + DEGREE + SEX + AGE,
            data = d_replicate,
            weights = weight)
  return(coef(fit)["XTRUE"])
}
stopCluster(cl)
simple_fit_result <- data.frame(coefficient = simple_fit,
                                se = sd(simple_fit_draws)) %>%
  mutate(ci.min = coefficient - qnorm(.975) * se,
         ci.max = coefficient + qnorm(.975) * se)
print("Coefficient when sample is restricted to T = 1")
coef(lm(outcome ~ X + RACE + DEGREE + SEX + AGE,
        data = d %>% filter(treated),
        weights = weight))["XTRUE"]

results %>%
  filter(strategy == "AIPW" & !cross_fit & learner == "glm") %>%
  mutate(estimand = factor(case_when(estimand == "unadjusted" ~ 1,
                                     estimand == "equalize1" ~ 2,
                                     estimand == "equalize0" ~ 3,
                                     estimand == "marginal" ~ 4,
                                     estimand == "conditional" ~ 5),
                           labels = c("Unadjusted",
                                      "Equalized at\nT = 1",
                                      "Equalized at\nT = 0",
                                      "Marginal\nequalization",
                                      "Conditional\nequalization"))) %>%
  ggplot(aes(x = estimand, y = estimate,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se,
             label = format(round(estimate, 2), nsmall = 2))) +
  geom_errorbar() +
  geom_label() +
  theme_bw() +
  xlab("Estimand") +
  ylab("Gap in log annual income by class origin\n(Upper - Lower)") +
  geom_hline(yintercept = simple_fit_result$coefficient, linetype = "dashed") +
  geom_text(data = data.frame(estimand = "Equalized at\nT = 0",
                              estimate = simple_fit_result$coefficient,
                              se = NA),
            label = paste0("Dashed comparison: OLS coefficient on class origin, which is not a post-intervention gap. Estimate: ",
                           format(round(simple_fit_result$coefficient,2),nsmall = 2)," (CI: ",
                           paste(format(round(c(simple_fit_result$ci.min,simple_fit_result$ci.max),2),nsmall = 2), collapse = ", "),")"),
            size = 2.5, vjust = -1) +
  ggsave("figures/class_gap_aipw.pdf",
         height = 3.5, width = 6.5)

results %>%
  filter((!cross_fit  & learner != "ranger") | (cross_fit & strategy == "AIPW" & learner == "ranger")) %>%
  # Keep only one unadjusted estimate
  filter(!(estimand == "unadjusted" & strategy != "IPW")) %>%
  mutate(estimand = factor(case_when(estimand == "unadjusted" ~ 1,
                                     estimand == "equalize1" ~ 2,
                                     estimand == "equalize0" ~ 3,
                                     estimand == "marginal" ~ 4,
                                     estimand == "conditional" ~ 5),
                           labels = c("Unadjusted",
                                      "Equalized at\nT = 1",
                                      "Equalized at\nT = 0",
                                      "Marginal\nequalization",
                                      "Conditional\nequalization")),
         strategy = factor(case_when(strategy == "IPW" & estimand == "Unadjusted" ~ 1,
                                     strategy == "Imputation" ~ 2,
                                     strategy == "IPW" ~ 3,
                                     strategy == "AIPW" & !cross_fit ~ 4,
                                     strategy == "AIPW" & cross_fit ~ 5),
                           labels = c("Mean difference",
                                      "Outcome\nModeling",
                                      "Inverse\nProbability Weighting",
                                      "Augmented Inverse\nProbability Weighting\n(main text specification)",
                                      "Double Machine\nLearning"))) %>%
  ggplot(aes(x = estimand, y = estimate,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se,
             color = strategy, shape = strategy)) +
  geom_point(position = position_dodge(width = .8)) +
  geom_errorbar(position = position_dodge(width = .8),
                width = .5) +
  theme_bw() +
  xlab("Estimand") +
  ylab("Gap in log annual income by class origin\n(Upper - Lower)") +
  scale_shape_discrete(name = "Estimation\nStrategy") +
  scale_color_manual(name = "Estimation\nStrategy",
                     values = c("black","blue","gray","seagreen4","purple")) +
  theme(legend.key.height = unit(1,"cm")) +
  ggsave("figures/class_gap_all_estimators.pdf",
         height = 3.5, width = 6.5)

# Print parametric fits of g and m
get_fits <- function(data) {
  m_fit <- glm(treated ~ X + RACE + DEGREE + SEX + AGE,
               data = data,
               family = binomial(link = "logit"),
               weights = weight)
  m_fit_noGroup <- glm(treated ~ RACE + DEGREE + SEX + AGE,
                       data = data,
                       family = binomial(link = "logit"),
                       weights = weight)
  g_fit <- glm(outcome ~ treated*X + RACE + DEGREE + SEX + AGE,
               data = data,
               weights = weight)
  standard_fit <- glm(outcome ~ treated + X + RACE + DEGREE + SEX + AGE,
                      data = data,
                      weights = weight)
  data.frame(variable = names(coef(m_fit)),
             fit = "m",
             coefficient = coef(m_fit)) %>%
    bind_rows(data.frame(variable = names(coef(g_fit)),
                         fit = "g",
                         coefficient = coef(g_fit))) %>%
    bind_rows(data.frame(variable = names(coef(standard_fit)),
                         fit = "standard",
                         coefficient = coef(standard_fit))) %>%
    bind_rows(data.frame(variable = names(coef(m_fit_noGroup)),
                         fit = "m_noGroup",
                         coefficient = coef(m_fit_noGroup)))
}
point <- get_fits(d)
cl <- makeCluster(30)
registerDoParallel(cl)
replicates <- foreach(
  i = 1:ncol(gss_bs$repweights),
  .combine = "rbind", 
  .packages = c("tidyverse","foreach","reshape2","ranger","Amelia")
) %dorng% {
  # Restrict to the chosen PSUs
  GSS_replicate <- GSS[gss_bs$repweights[,i] > 0,]
  d_replicate <- sample_restrictions(GSS_replicate)
  get_fits(d_replicate)
}
stopCluster(cl)

print(xtable(
  replicates %>%
    group_by(variable, fit) %>%
    summarize(se = sd(coefficient)) %>%
    left_join(point, by = c("variable","fit")) %>%
    melt(id = c("variable","fit"), variable.name = "quantity") %>%
    mutate(value = format(round(value, 2), nsmall = 2),
           value = ifelse(quantity == "se", paste0("(",value,")"), value)) %>%
    spread(key = fit, value = value) %>%
    select(variable, quantity, standard, g, m, m_noGroup) %>%
    mutate(quantity = fct_rev(quantity)) %>%
    arrange(variable, quantity) %>%
    mutate(variable = ifelse(quantity == "coefficient", variable, "")) %>%
    select(-quantity)
), include.rownames = F)

