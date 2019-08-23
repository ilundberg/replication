
# Housing assistance protects against eviction
# Ian Lundberg, Sarah Gold, Louis Donnelly, Jeanne Brooks-Gunn, and Sara McLanahan
# Last updated: 2019-05-19
# Code by Ian Lundberg (ilundberg@princeton.edu)

setwd("/Users/iandl/Dropbox/FF_housing_assistance")

library(tidyverse)
library(reshape2)
library(haven)
library(foreach)
library(doParallel)
library(doRNG)
library(Amelia)
library(survey)
library(mitools)
library(grf)
library(ggridges)
library(xtable)
library(mvtnorm)

# Set the number of samples for simulation-based uncertainty
bs.samps <- 100

################
## Clean data ##
################

# Write a function to clean factor variables
# by marking missing as NA and keeping value labels
clean_factor <- function(x) {
  missing <- as.numeric(x) < 0
  x <- as_factor(x)
  x <- case_when(!missing ~ x)
  x = fct_drop(x)
  return(x)
}

d0 <- read_dta("data/ff_pub_merge2.dta") %>%
  left_join(read_dta("data/ff_y9_pub1.dta"), by = "idnum") %>%
  left_join(read_dta("data/FF_Y15_pub.dta"), by = "idnum") %>%
  left_join(read_dta("data/ff_y9_pubweights082013STATA.dta"))
save(d0, file = "intermediate/d0.Rdata")

d1 <- d0 %>%
  # Restrict to those with mother as caregiver at age 9
  # who are in the national sample
  filter(m5a2 %in% c(1,2) & !is.na(m1natwt)) %>%
  transmute(
    idnum = idnum,
    # Define the caregiver at wave 6 (age 15, outcome wave)
    caregiver.6 = case_when(cp6pcgrel == 1 ~ "m",
                            cp6pcgrel == 2 ~ "f",
                            cp6pcgrel > 0 ~ "other",
                            cp6pcgrel == -9 ~ "noninterview"),
    
    # Participation of mom at each wave
    interviewed.2 = m2a3 != -9,
    interviewed.3 = m3a2 != -9,
    interviewed.4 = m4a2 != -9,
    interviewed.5 = m5a2 != -9,
    interviewed.6 = cp6pcgrel == 1,
    
    # Treatment variable and lagged treatments: Housing assistance in all waves
    # This gets collapsed below
    housing.2 = case_when(m2h5 == 1 ~ "public",
                          m2h7 == 1 ~ "assistance",
                          # Including -6 below because if you aren't paying rent
                          # that implies you aren't getting assistance
                          # If you refused to answer, though, you get NA
                          m2h5 %in% c(-6,2) & m2h7 %in% c(-6,2) ~ "no_help"),
    housing.3 = case_when(m3i5 == 1 ~ "public",
                          m3i6 == 1 ~ "assistance",
                          # Including -6 below because if you aren't paying rent
                          # that implies you aren't getting assistance
                          # If you refused to answer, though, you get NA
                          m3i5 %in% c(-6,2) & m3i6 %in% c(-6,2) ~ "no_help"),
    housing.4 = case_when(m4i1 == 2 ~ housing.3,
                          m4i5 == 1 ~ "public",
                          m4i6 == 1 ~ "assistance",
                          # Including -6 below because if you aren't paying rent
                          # that implies you aren't getting assistance
                          # If you refused to answer, though, you get NA
                          m4i5 %in% c(-6,2) & m4i6 %in% c(-6,2) ~ "no_help"),
    housing.5 = case_when(m5f1 == 2 ~ housing.4,
                          m5f5 == 1 ~ "public",
                          m5f6 == 1 ~ "assistance",
                          # Including -6 below because if you aren't paying rent
                          # that implies you aren't getting assistance
                          # If you refused to answer, though, you get NA
                          m5f5 %in% c(-6,2) & m5f6 %in% c(-6,2) ~ "no_help"),
    
    # Outcome and lagged outcomes: Eviction report in each wave
    evicted.2 = case_when(m2h19e > 0 ~ m2h19e == 1),
    evicted.3 = case_when(m3i23c > 0 ~ m3i23c == 1),
    evicted.4 = case_when(m4i23e > 0 ~ m4i23e == 1),
    evicted.5 = case_when(m5f23d > 0 ~ m5f23d == 1),
    evicted.6 = case_when(caregiver.6 == "m" & (p6j40 == 1 | p6j51 == 1) ~ T,
                          caregiver.6 == "m" & (p6j40 == 2 & p6j51 == 2) ~ F),
    evicted.history = evicted.2 | evicted.3 | evicted.4,
    
    # Nonpayment of rent or mortgage
    nonpayment.2 = case_when(m2h19d > 0 ~ m2h19d == 1),
    nonpayment.3 = case_when(m3i23b > 0 ~ m3i23b == 1),
    nonpayment.4 = case_when(m4i23d > 0 ~ m4i23d == 1),
    nonpayment.5 = case_when(m5f23c > 0 ~ m5f23c == 1),
    # Wave 6 of this may be useful for imputing missing eviction
    nonpayment.6 = case_when(caregiver.6 == "m" & (p6j39 == 1 | p6j50 == 1) ~ T,
                             caregiver.6 == "m" & (p6j39 == 2 & p6j50 == 2) ~ F),
    
    # Baseline controls: Married, race, and education
    married = case_when(cm1relf > 0 ~ cm1relf == 1),
    race = factor(case_when(cm1ethrace %in% c(1,4) ~ "White/other",
                            cm1ethrace == 2 ~ "Black",
                            cm1ethrace == 3 ~ "Hispanic")),
    education = clean_factor(cm1edu),
    impulsivity = 1/6 * (ifelse(m3j44a > 0, m3j44a, NA) + ifelse(m3j44b > 0, m3j44b, NA) +
                           ifelse(m3j44c > 0, m3j44c, NA) + ifelse(m3j44d > 0, m3j44d, NA) +
                           ifelse(m3j44e > 0, m3j44e, NA) + ifelse(m3j44f > 0, m3j44f, NA)),
    cognitive = ifelse(cm3cogsc > 0, cm3cogsc, NA),
    
    # Home ownership
    own.1 = case_when(m1f2 == 2 ~ F), # we only know if they rent, not whether they personally own
    own.2 = case_when(m2h1 == 1 & m2h2 == 4 ~ T,
                      m2h1 == 1 & m2h2 > 0 & m2h2 != 4 ~ F,
                      m2h1 == 2 ~ own.1),
    own.3 = case_when(m3i1 == 1 & m3i2 == 4 ~ T,
                      m3i1 == 1 & m3i2 > 0 & m3i2 != 4 ~ F,
                      m3i1 == 2 ~ own.2),
    own.4 = case_when(m4i1 == 1 & m4i2 == 4 ~ T,
                      m4i1 == 1 & m4i2 > 0 & m4i2 != 4 ~ F,
                      m4i1 == 2 ~ own.3),
    own.5 = case_when(m5f1 == 1 & m5f2 == 4 ~ T,
                      m5f1 == 1 & m5f2 > 0 & m5f2 != 4 ~ F,
                      m5f1 == 2 ~ own.4),
    
    #################################################
    # Wave-specific controls: We control for wave 5 #
    # but include the other waves to aid imputation #
    #################################################
    
    # Household income / poverty line
    income.1 = case_when(cm1inpov >= 0 ~ cm1inpov),
    income.2 = case_when(cm2povco >= 0 ~ cm2povco),
    income.3 = case_when(cm3povco >= 0 ~ cm3povco),
    income.4 = case_when(cm4povco >= 0 ~ cm4povco),
    income.5 = case_when(cm5povco >= 0 ~ cm5povco),
    # Top-code those at 5
    income.1 = ifelse(income.1 > 5, 5, income.1),
    income.2 = ifelse(income.2 > 5, 5, income.2),
    income.3 = ifelse(income.3 > 5, 5, income.3),
    income.4 = ifelse(income.4 > 5, 5, income.4),
    income.5 = ifelse(income.5 > 5, 5, income.5),
    
    # Disability: Health limits type or amount of work you can do
    disability.2 = case_when(m2j2 > 0 ~ m2j2 == 1),
    disability.3 = case_when(m3j2 > 0 ~ m3j2 == 1),
    disability.4 = case_when(m4j2 > 0 ~ m4j2 == 1),
    disability.5 = case_when(m5g2 > 0 ~ m5g2 == 1),
    
    # Convicted of crime
    convicted.3 = case_when(m3i25 == 2 ~ F,
                            m3i27 == 2 ~ F,
                            m3i27 == 1 ~ T),
    convicted.4 = case_when(m4i25 == 2 ~ F,
                            m4i27 == 2 ~ F,
                            m4i27 == 1 ~ T),
    convicted.5 = case_when(m5f25 == 2 ~ F,
                            m5f27 == 2 ~ F,
                            m5f27 == 1 ~ T),
    convicted = convicted.3 | convicted.4 | convicted.5,
    m5natwt = m5natwt
  ) %>%
  select(-own.1,-own.2,-own.3,-own.4)

d2 <- d1 %>%
  mutate(num_motherCaregiver = n()) %>%
  filter(!own.5 | is.na(own.5)) %>%
  mutate(num_nonowners = n()) %>%
  filter(income.5 < 2 | is.na(income.5)) %>%
  mutate(num_income.range = n()) %>%
  filter(!is.na(housing.5)) %>%
  mutate(num_validTreatment = n(),
         num_changed_caregiver = sum(caregiver.6 != "m"))
save(d2, file = "intermediate/d2.Rdata")

d2 %>%
  select(starts_with("num")) %>%
  filter((1:n()) == 1)

# Multiply impute
set.seed(08544)
filled_raw <- amelia(data.frame(d2) %>%
                       select(-starts_with("num"),-own.5) %>%
                       mutate(education = as.numeric(education)),
                     idvars = c("idnum","interviewed.2","interviewed.3",
                                "interviewed.4","interviewed.5","interviewed.6",
                                "caregiver.6","m5natwt"),
                     ords = c("education"),
                     noms = c("race",
                              "housing.2","housing.3","housing.4",
                              "housing.5"),
                     m = 5)
filled <- transform(filled_raw,
                    convicted = convicted.3 | convicted.4 | convicted.5,
                    evicted.history = evicted.2 | evicted.3 | evicted.4,
                    nonpayment.history = nonpayment.2 | nonpayment.3 | nonpayment.4,
                    income.history = (income.2 + income.3 + income.4) / 3,
                    education = factor(education))
save(filled,file = "intermediate/filled.Rdata")


# Unadjusted result
unadjusted.unweighted <- foreach(imp = filled$imputations, .combine = "rbind") %do% {
  imp %>%
    left_join(d0 %>% select(idnum, m5natwt_rep1), by = "idnum") %>%
    filter(!is.na(m5natwt_rep1)) %>%
    group_by(housing.5) %>%
    summarize(SampleSize = n(),
              NumberEvicted = sum(evicted.6),
              ProportionEvicted = mean(evicted.6))
} %>%
  group_by(housing.5) %>%
  summarize_all(.funs = mean)

# Make CI on weighted proportion
unadjusted.by.imp <- foreach(imp = filled$imputations) %do% {
  
  imp_restricted <- imp %>%
    left_join(d0 %>%
                select(idnum, starts_with("m5natwt_rep")),
              by = "idnum") %>%
    filter(!is.na(m5natwt_rep1))
  
  my_design <- svrepdesign(repweights = imp_restricted %>% select(starts_with("m5natwt_rep")),
                           weights = imp_restricted$m5natwt,
                           data = imp_restricted)
  
  svy_result <- svyglm(evicted.6 ~ -1 + housing.5, my_design, family = "gaussian")
  
  return(svy_result)
}

unadjusted.combined <- MIcombine(results = lapply(unadjusted.by.imp, coef),
                                 variances = lapply(unadjusted.by.imp, vcov))
unadjusted.weighted <- data.frame(
  housing.5 = gsub("housing.5","",names(coef(unadjusted.combined))),
  estimate = coef(unadjusted.combined),
  ci.min = coef(unadjusted.combined) - qnorm(.975)*sqrt(diag(vcov(unadjusted.combined))),
  ci.max = coef(unadjusted.combined) + qnorm(.975)*sqrt(diag(vcov(unadjusted.combined)))
)
A <- rbind(c(1,-1,0),
           c(0,-1,1))
difference_estimates <- A %*% coef(unadjusted.combined)
difference_se <- sqrt(diag(A %*% vcov(unadjusted.combined) %*% t(A)))
unadjusted.weighted.differences <- data.frame(
  housing.5 = gsub("housing.5","",names(coef(unadjusted.combined))[c(1,3)]),
  estimate = difference_estimates,
  ci.min = difference_estimates - qnorm(.975)*difference_se,
  ci.max = difference_estimates + qnorm(.975)*difference_se
)

unadjusted.weighted %>%
  left_join(unadjusted.unweighted, by = "housing.5") %>%
  mutate(housing.5 = case_when(housing.5 == "no_help" ~ "A. No assistance",
                               housing.5 == "public" ~ "B. Public housing",
                               housing.5 == "assistance" ~ "C. Other assistance")) %>%
  mutate(housing.5 = paste0(housing.5,
                            "\n\nSample size: ",prettyNum(round(SampleSize),big.mark = ","),
                            "\nNumber evicted: ", prettyNum(round(NumberEvicted), big.mark = ","))) %>%
  ggplot(aes(x = housing.5, y = estimate, 
             label = format(round(estimate,3),digits = 3),
             ymin = ci.min, ymax = ci.max)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(width = .5) +
  geom_label(size = 3) +
  ylab("Proportion Evicted\n(weighted)") +
  scale_x_discrete(name = element_blank()) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0)) +
  ggtitle("A) Proportion evicted, by housing assistance") +
  ggsave("figures/unadjusted_weighted.pdf",
         height = 3, width = 6.5)
  
##########################
# Model-based adjustment #
##########################
ols_formula <- formula(evicted.6 ~ married + race + education + 
                         impulsivity + cognitive + convicted +
                         disability.5 + income.5 + 
                         income.history +
                         nonpayment.5 + evicted.5 + 
                         evicted.history + nonpayment.history)

# Note missingness
d2 %>%
  mutate(convicted = convicted.3 | convicted.4 | convicted.5,
         income.history = 1 / 3 * (income.2 + income.3 + income.4),
         evicted.history = evicted.2 | evicted.3 | evicted.4,
         nonpayment.history = nonpayment.2 | nonpayment.3 | nonpayment.4) %>%
  select(evicted.6, married, race, education, impulsivity, cognitive, disability.5,income.5, nonpayment.5, evicted.5,
         convicted, income.history, evicted.history, nonpayment.history) %>%
  summarize_all(.funs = function(x) mean(is.na(x))) %>%
  melt(id = NULL, value.name = "prop_missing") %>%
  arrange(-prop_missing)

fits_y0 <- lapply(filled$imputations, function(imp) {
  lm(ols_formula,
     data = imp %>%
       filter(housing.5 == "no_help"))
})

# Make point estimates
point.estimates <- foreach(i = 1:length(filled$imputations), .combine = "rbind") %do% {
  X <- model.matrix(ols_formula, data = filled$imputations[[i]])
  filled$imputations[[i]] %>%
    mutate(yhat0 = as.vector(X %*% coef(fits_y0[[i]]))) %>%
    filter(housing.5 != "no_help" & !is.na(m5natwt)) %>%
    group_by(housing.5) %>%
    summarize(ybar1 = weighted.mean(evicted.6, w = m5natwt),
              ybar0 = weighted.mean(yhat0, w = m5natwt),
              patt = weighted.mean(evicted.6 - yhat0, w = m5natwt))
} %>%
  group_by(housing.5) %>%
  summarize_all(.funs = mean)

# Make confidence intervals

# 1. Estimate P(Y | No assistance)
# 2. Sample from the posterior
# 3. For each sample, calculate the replicate-weighted estimate and variance
# 4. Sample from the posterior of (3)
# 5. Pool samples
# Step 1
cl <- makeCluster(4)
registerDoParallel(cl)
t0 <- Sys.time()
samples <- foreach(i = 1:length(filled$imputations), .combine = "cbind", .packages = c("mvtnorm")) %dorng% {
  X <- model.matrix(ols_formula, data = filled$imputations[[i]])
  beta <- rmvnorm(ceiling(bs.samps / length(filled$imputations)), mean = coef(fits_y0[[i]]), sigma = vcov(fits_y0[[i]]))
  effect_draws <- apply(X %*% t(beta),2,function(yhat) filled$imputations[[i]]$evicted.6 - yhat)
  return(effect_draws)
}
# Step 3
adjusted_pooled <- foreach(col_num = 1:ncol(samples), .combine = "rbind", .packages = c("survey","mvtnorm","tidyverse")) %dorng% {
  my_design <- svrepdesign(repweights = d2 %>% 
                             select(idnum) %>% 
                             left_join(d0 %>% 
                                         select(idnum, starts_with("m5natwt_rep")),
                                       by = "idnum") %>%
                             select(-idnum),
                           weights = d2$m5natwt,
                           data = data.frame(housing.5 = d2$housing.5, estimate = samples[,col_num]))
  svy_result <- svyglm(estimate ~ -1 + housing.5, my_design, family = "gaussian")
  return(rmvnorm(1, mean = coef(svy_result), sigma = vcov(svy_result)))
}
spent <- difftime(Sys.time(),t0)
stopCluster(cl)

data.frame(adjusted_pooled) %>%
  melt(id = NULL, variable.name = "housing.5") %>%
  mutate(housing.5 = gsub("housing.5","",housing.5)) %>%
  group_by(housing.5) %>%
  summarize(se = sd(value)) %>%
  filter(housing.5 != "no_help") %>%
  left_join(point.estimates, by = "housing.5") %>%
  mutate(ci.min = patt - qnorm(.975)*se,
         ci.max = patt + qnorm(.975)*se,
         Estimator = "B. Adjusted") %>%
  select(housing.5, patt, ci.min, ci.max, Estimator) %>%
  bind_rows(unadjusted.weighted.differences %>%
              rename(patt = estimate) %>%
              mutate(Estimator = "A. Unadjusted")) %>%
  mutate(housing.5 = case_when(housing.5 == "public" ~ "A. Public housing",
                               housing.5 == "assistance" ~ "B. Other assistance")) %>%
  ggplot(aes(x = housing.5, y = patt, 
             label = format(round(patt, 3), digits = 3),
             ymin = ci.min, max = ci.max, 
             color = Estimator)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .7), width = .5) +
  geom_label(position = position_dodge(width = .7),
             show.legend = F, size = 3, fontface = "bold") +
  ylab("Effect on P(Eviction)\ncompared with no assistance") +
  scale_x_discrete(name = element_blank()) +
  scale_color_manual(values = c("blue","seagreen4")) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0)) +
  ggtitle("B) Effect of housing assistance on eviction") +
  ggsave("figures/difference_estimators.pdf",
         height = 3, width = 6.5)



##########################################
# Appendix: Report coefficient estimates #
##########################################

combined <- MIcombine(results = lapply(fits_y0,coef),
                      variances = lapply(fits_y0,vcov))

fit_results <- data.frame(variable = names(combined$coefficients),
                          coef = combined$coefficients,
                          se = sqrt(diag(combined$variance)),
                          df = combined$df)

ols_coefficients <- as.matrix(
  fit_results %>%
    mutate(pval = 2*pt(abs(coef) / se, df = df, lower.tail = F),
           variable = factor(
             case_when(
               variable == "evicted.5" ~ 1,
               variable == "evicted.historyTRUE" ~ 2,
               variable == "nonpayment.5" ~ 3,
               variable == "nonpayment.historyTRUE" ~ 4,
               variable == "income.5" ~ 5,
               variable == "income.history" ~ 6,
               variable == "disability.5" ~ 7,
               variable == "convictedTRUE" ~ 8,
               variable == "education2" ~ 9,
               variable == "education3" ~ 10,
               variable == "education4" ~ 11,
               variable == "married" ~ 12,
               variable == "raceHispanic" ~ 13,
               variable == "raceWhite/other" ~ 14,
               variable == "cognitive" ~ 15,
               variable == "impulsivity" ~ 16,
               variable == "(Intercept)" ~ 17
             ),
             labels = c(
               "Evicted in past 12 months (age 9)",
               "Evicted in past 12 months at age 1, 3, or 5",
               "Nonpayment in past 12 months",
               "Nonpayment at age 1, 3, or 5",
               "Income / poverty threshold in past 12 months",
               "Income / poverty threshold averaged over ages 1, 3, and 5",
               "Disability",
               "Conviction",
               "Education: High school",
               "Education: Some college",
               "Education: College",
               "Parents married at birth",
               "Race: Hispanic",
               "Race: White/other (black omitted)",
               "WAIS-R cognitive score",
               "Impulsivity (Dickman 1990)",
               "Intercept"
             )
           ),
           coef = paste0(format(round(coef,2),digits = 2),ifelse(pval < .001, "***",
                                                                 ifelse(pval < .01, "**",
                                                                        ifelse(pval < .05, "*", "")))),
           se = paste0("(",format(round(se,2),digits = 2),")")) %>%
    select(variable, coef, se) %>%
    melt(id = c("variable"),
         variable.name = "quantity") %>%
    arrange(variable,quantity) %>%
    select(-quantity)
)

ols_coefficients[2*(1:((nrow(ols_coefficients) - 1)/2)),"variable"] <- ""
print(xtable(ols_coefficients,
             caption = paste0("OLS coefficients for model of eviction as a function of pre-treatment variables, among those receiving no assistance (N = ",sum(d2$housing.5 == "no_help"),")."),
             label = "tbl:coefs_evicted"),
      include.rownames = F)


## Supplemental: Check subsequent moves
for_table <- d2 %>%
  left_join(d0 %>%
              transmute(idnum = idnum,
                        housing.6 = case_when(p6j14 == 1 & cp6pcgrel == 1  ~ "public",
                                              p6j15 == 1 & cp6pcgrel == 1  ~ "assistance",
                                              p6j14 == 2 & p6j15 == 2 & cp6pcgrel == 1 ~ "no_help")),
            by = "idnum")

table(age15 = for_table$housing.6, age9 = for_table$housing.5, useNA = "ifany")[c(3,1,2,4),c(3,1,2)]

round(prop.table(table(for_table$housing.6, for_table$housing.5, useNA = "ifany"),
                 margin = 2),2)[c(3,1,2,4),c(3,1,2)]

## Supplemental: Check prevalence of housing assistance by city
restricted <- readstata13::read.dta13("data/FF_allwaves_res_2019.dta", convert.factors = F)

city_names <- readstata13::read.dta13("data/FF_allwaves_res_2019.dta") %>%
  select(idnum, m1city)

restricted_clean <- restricted %>%
  filter(m5a2 %in% c(1,2)) %>%
  transmute(idnum = idnum,
            housing.2 = case_when(m2h5 == 1 ~ "public",
                                  m2h7 == 1 ~ "assistance",
                                  # Including -6 below because if you aren't paying rent
                                  # that implies you aren't getting assistance
                                  # If you refused to answer, though, you get NA
                                  m2h5 %in% c(-6,2) & m2h7 %in% c(-6,2) ~ "no_help"),
            housing.3 = case_when(m3i5 == 1 ~ "public",
                                  m3i6 == 1 ~ "assistance",
                                  # Including -6 below because if you aren't paying rent
                                  # that implies you aren't getting assistance
                                  # If you refused to answer, though, you get NA
                                  m3i5 %in% c(-6,2) & m3i6 %in% c(-6,2) ~ "no_help"),
            housing.4 = case_when(m4i1 == 2 ~ housing.3,
                                  m4i5 == 1 ~ "public",
                                  m4i6 == 1 ~ "assistance",
                                  # Including -6 below because if you aren't paying rent
                                  # that implies you aren't getting assistance
                                  # If you refused to answer, though, you get NA
                                  m4i5 %in% c(-6,2) & m4i6 %in% c(-6,2) ~ "no_help"),
            housing.5 = case_when(m5f1 == 2 ~ housing.4,
                                  m5f5 == 1 ~ "public",
                                  m5f6 == 1 ~ "assistance",
                                  # Including -6 below because if you aren't paying rent
                                  # that implies you aren't getting assistance
                                  # If you refused to answer, though, you get NA
                                  m5f5 %in% c(-6,2) & m5f6 %in% c(-6,2) ~ "no_help")) %>%
  left_join(city_names, by = "idnum")


for (treatment_value in na.omit(unique(restricted_clean$housing.5))) {
  restricted_clean %>%
    group_by(m1city, housing.5) %>%
    summarize(number = n()) %>%
    group_by(m1city) %>%
    mutate(proportion = number / sum(number)) %>%
    group_by() %>%
    filter(housing.5 == treatment_value) %>%
    mutate(m1city = fct_reorder(m1city, proportion)) %>%
    ggplot(aes(x = m1city, y = proportion)) +
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle(paste0("Group: ",treatment_value,"\nNote: Figure does not yet make our analytic sample restrictions.")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab(paste("Unweighted proportion of city\nsample in group:",treatment_value)) +
    xlab("City of birth") +
    ggsave(paste0("figures/treatment_by_city_",treatment_value,".pdf"),
           height = 4, width = 6.5)
}

restricted_clean %>%
  group_by(m1city, housing.5) %>%
  summarize(number = n()) %>%
  group_by(m1city) %>%
  mutate(proportion = number / sum(number)) %>%
  group_by() %>%
  select(m1city, housing.5, proportion) %>%
  spread(key = housing.5, value = proportion) %>%
  mutate(m1city = fct_reorder(m1city, public)) %>%
  melt(id = "m1city", variable.name = "housing.5", value.name = "proportion") %>%
  ggplot(aes(x = m1city, y = proportion)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~housing.5, sacles)

