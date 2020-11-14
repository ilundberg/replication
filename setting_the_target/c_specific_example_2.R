
# Replication code for:
# What is Your Estimand? Defining the Target Quantity Connects Statistical Evidence to Theory
# Ian Lundberg, Rebecca Johnson, and Brandon Stewart
# Email: ilundberg@princeton.edu

# This file conducts the analysis for Specific Example 2:
# Causal Estimands Facilitate Interpretable Effect Sizes and Clarify Claims to Mediation

# The data for this example are available from the Fragile Families and Child Wellbeing Study.
# The study website is: https://fragilefamilies.princeton.edu/
# The data are available at: https://www.opr.princeton.edu/archive/
# 1. Register for an account
# 2. Download ff_res_merge4.dta and place it in the data subdirectory

library(tidyverse)
library(reshape2)
library(haven)
library(foreach)
library(doParallel)
library(doRNG)
library(mvtnorm)
library(Amelia)

sink("output/WildemanEtAl_output.txt")
print("Output from replication of Wildeman et al.")

# Wildeman replication for estimand paper

# This code produces the following results:
# 1. Replicated coefficient for total effect
# 2. Exponentiated replicated coefficient
# 3. Estimate of sample average treatment effect on probability
# 4. 9 panels of mediation figure

################
# Prepare data #
################

# NOTE: Documentation for the data files is available at
# http://www.fragilefamilies.princeton.edu/documentation.asp
# Most variables come from the 3- and 5-year mother interview codebooks.

# Read data from baseline through 5 year interviews
data <- read_dta("data/ff_res_merge4.dta") %>%
  mutate(num_rawData = n()) %>%
  # Restrict to those with non-zero weight
  filter(!is.na(m4citywt)) %>%
  mutate(num_withWeight = n()) %>%
  # Mom completed both year 3 and 5 interviews
  filter(m3intyr != -9 & m4intyr != -9) %>%
  mutate(num_completedInterviews = n()) %>%
  # Not missing data on either DV (depression and life satisfaction) at year 5
  filter(cm4md_case_lib >= 0 & m4j0 >= 0) %>%
  mutate(num_validOutcomes = n()) %>%
  transmute(num_rawData = num_rawData,
            num_withWeight = num_withWeight,
            num_completedInterviews = num_completedInterviews,
            num_validOutcomes = num_validOutcomes,
            # City of birth is the one restricted variable not available in the public file
            city = case_when(m1city != -9 ~ as_factor(m1city)),
            city = fct_drop(city),
            # Maternal depression
            depressed.m.y5 = cm4md_case_lib == 1,
            # Lagged maternal depression (measured at year 3)
            depressed.m.y3 = case_when(cm3md_case_lib >= 0 ~ cm3md_case_lib == 1),
            
            # Recent paternal incarceration: Father incarcerated between age 3 and 5 interviews
            pat.inc.rec = case_when(cmf4finjail == 1 ~ T,
                                    m4c36 == 5 | m4c37 %in% c(1,3) ~ T, # this ignores missingness onf m4c37 and m4c36
                                    cmf4finjail == 0 ~ F),
            pat.inc.dist = case_when(cmf3fevjail %in% 0:1 ~ cmf3fevjail == 1),
            
            # COVARIATES
            # Race: White, black, Hispanic, other
            race.m = fct_drop(case_when(cm1ethrace > 0 ~ as_factor(cm1ethrace))),
            race.f = fct_drop(case_when(cf1ethrace > 0 ~ as_factor(cf1ethrace))),
            # Born outside the US
            foreign.born.m = case_when(m1h2 %in% 1:2 ~ m1h2 == 2),
            foreign.born.f = case_when(f1h2 %in% 1:2 ~ f1h2 == 2),
            # Age in years at baseline survey
            age.m = case_when(cm1age >= 0 ~ cm1age),
            age.f = case_when(cf1age >= 0 ~ cf1age),
            # Education: <HS, HS or GED, postsecondary education, at baseline
            educ.m = fct_drop(case_when(cm1edu > 0 ~ as_factor(cm1edu))),
            educ.f = fct_drop(case_when(cf1edu > 0 ~ as_factor(cf1edu))),
            # Parents' relationship at 3-year: married, cohabiting, in a nonresidential romantic relationship, and no longer romantically involved
            rel.yr3 = case_when(cm3relf==1 ~ "Married",
                                cm3relf == 2 ~ "Cohabiting",
                                cm3relf %in% 3:4 ~ "Nonresidential romantic relationship",
                                cm3relf %in% 5:8 ~ "No longer romantically involved"),
            # Whether mother reported romantic relationship with new partner at 3-year. MATCHES
            newpartner.yr3 = case_when(m3e2d %in% c(-6,0) ~ F,
                                       m3e2d == 1 ~ T),
            # Whether either of the mother’s biological parents experienced a two-week period of feeling depressed, down in the dumps, or blue
            # If has no knowledge of father (-14), I am coding as missing.
            mom.par.depressed = case_when(m3j45 >= 0 & m3j50 >= 0 ~ m3j45 == 1 | m3j50 == 1,
                                          m3j45 == 1 ~ T,
                                          m3j50 == 1 ~ T),
            # Maternal reports of income-to-poverty ratio
            incratio.y3 = case_when(cm3povco >= 0 ~ cm3povco),
            # Material hardship: Sum of events that could have happened because not enough money
            # The authors write that there are 12 questions. I think that's a typo.
            # I am only using the 8 items asked in all cities
            mat.hard.y3 = 1 / 8 * (
              case_when(m3i23a %in% 1:2 ~ m3i23a == 1) +
                case_when(m3i23b %in% 1:2 ~ m3i23b == 1) +
                case_when(m3i23c %in% 1:2 ~ m3i23c == 1) +
                case_when(m3i23d %in% 1:2 ~ m3i23d == 1) +
                case_when(m3i23e %in% 1:2 ~ m3i23e == 1) +
                case_when(m3i23f %in% 1:2 ~ m3i23f == 1) +
                case_when(m3i23g %in% 1:2 ~ m3i23g == 1) +
                case_when(m3i23h %in% 1:2 ~ m3i23h == 1)
            ),
            # Relationship quality with the child’s father: 1 = poor to 5 = excellent
            relat.qual.y3 = case_when(m3d0 == 2 ~ 1, # poor if skipped because of no relationship with father
                                      m3d4 %in% 1:5 ~ 6 - as.numeric(m3d4)), 
            # Number of children in the household
            num.children = case_when(cm3kids >= 0 ~ cm3kids),
            # Co-parenting as in Carlson, McLanahan, and Brooks-Gunn (2008),
            # though distinction not at all clear on page 468 of that article
            # Shared responsibility
            shared.responsibility.y3 = 1 / 4 * (
              case_when(m3c7a == -6 ~ 4,
                        m3c7a %in% 1:4 ~ 5 - as.numeric(m3c7a)) +
                case_when(m3c7b == -6 ~ 4,
                          m3c7b %in% 1:4 ~ 5 - as.numeric(m3c7b)) +
                case_when(m3c7c == -6 ~ 4,
                          m3c7c %in% 1:4 ~ 5 - as.numeric(m3c7c)) +
                case_when(m3c7d == -6 ~ 4,
                          m3c7d %in% 1:4 ~ 5 - as.numeric(m3c7d))
            ),
            # Cooperation in parenting
            cooperation.y3 = 1 / 6 * (
              case_when(m3d1a == -6 ~ 4,
                        m3d1a %in% 1:4 ~ 5 - as.numeric(m3d1a)) +
                case_when(m3d1b == -6 ~ 4,
                          m3d1b %in% 1:4 ~ 5 - as.numeric(m3d1b)) +
                case_when(m3d1c == -6 ~ 4,
                          m3d1c %in% 1:4 ~ 5 - as.numeric(m3d1c)) +
                case_when(m3d1d == -6 ~ 4,
                          m3d1d %in% 1:4 ~ 5 - as.numeric(m3d1d)) +
                case_when(m3d1e == -6 ~ 4,
                          m3d1e %in% 1:4 ~ 5 - as.numeric(m3d1e)) +
                case_when(m3d1f == -6 ~ 4,
                          m3d1f %in% 1:4 ~ 5 - as.numeric(m3d1f))
            ),
            # Paternal engagement: average (0 to 7) of days per week the mother reported the father
            # did activities with the focal child, such as sing songs,
            # read stories, or hug or show physical affection (m3c3a-m3c3m)
            pat.engage.y3 = 1 / 13 * (
              case_when(m3c3a == -6 ~ 0,
                        m3c3a >= 0 ~ as.numeric(m3c3a)) +
                case_when(m3c3b == -6 ~ 0,
                          m3c3b >= 0 ~ as.numeric(m3c3b)) +
                case_when(m3c3c == -6 ~ 0,
                          m3c3c >= 0 ~ as.numeric(m3c3c)) +
                case_when(m3c3d == -6 ~ 0,
                          m3c3d >= 0 ~ as.numeric(m3c3d)) +
                case_when(m3c3e == -6 ~ 0,
                          m3c3e >= 0 ~ as.numeric(m3c3e)) +
                case_when(m3c3f == -6 ~ 0,
                          m3c3f >= 0 ~ as.numeric(m3c3f)) +
                case_when(m3c3g == -6 ~ 0,
                          m3c3g >= 0 ~ as.numeric(m3c3g)) +
                case_when(m3c3h == -6 ~ 0,
                          m3c3h >= 0 ~ as.numeric(m3c3h)) +
                case_when(m3c3i == -6 ~ 0,
                          m3c3i >= 0 ~ as.numeric(m3c3i)) +
                case_when(m3c3j == -6 ~ 0,
                          m3c3j >= 0 ~ as.numeric(m3c3j)) +
                case_when(m3c3k == -6 ~ 0,
                          m3c3k >= 0 ~ as.numeric(m3c3k)) +
                case_when(m3c3l == -6 ~ 0,
                          m3c3l >= 0 ~ as.numeric(m3c3l)) +
                case_when(m3c3m == -6 ~ 0,
                          m3c3m >= 0 ~ as.numeric(m3c3m))
            ),
            # Parenting stress: average of responses to the following (1 = strongly disagree to 4 = strongly agree):
            # Being a parent is harder than I thought it would be;
            # I feel trapped by my responsibilities as a parent; 
            # Taking care of my children is much more work than pleasure;
            # I often feel tired, worn out, or exhausted from raising a family.
            parenting.stress.y3 = 1 / 4 * (
              case_when(m3b6a %in% 1:4 ~ 5 - as.numeric(m3b6a)) +
                case_when(m3b6b %in% 1:4 ~ 5 - as.numeric(m3b6b)) +
                case_when(m3b6c %in% 1:4 ~ 5 - as.numeric(m3b6c)) +
                case_when(m3b6d %in% 1:4 ~ 5 - as.numeric(m3b6d))
            ),
            # Father impulsivity at baseline: Dickman’s (1990) impulsivity scale, 
            # based on an average of six questions 
            # (recoded to 1 = strongly disagree to 4 = strongly agree)
            impulsivity = 1 / 6 * (
              case_when(f2j21 %in% 1:4 ~ 5 - as.numeric(f2j21)) +
                case_when(f2j22 %in% 1:4 ~ 5 - as.numeric(f2j22)) +
                case_when(f2j23 %in% 1:4 ~ 5 - as.numeric(f2j23)) +
                case_when(f2j24 %in% 1:4 ~ 5 - as.numeric(f2j24)) +
                case_when(f2j25 %in% 1:4 ~ 5 - as.numeric(f2j25)) +
                case_when(f2j26 %in% 1:4 ~ 5 - as.numeric(f2j26))
            ),
            # Domestic violence: mother’s report that the father hit, slapped, or kicked her at any point up to and including the three-year interview.
            dom.violence.yb13 = case_when(m1b7b %in% 1:2 | m1b13b %in% 1:2 |
                                            m2d6h %in% 1:2 | m2d6i %in% 1:2 |
                                            m3d7m == 1 | m3d7o == 1 ~ T,
                                          m1b7b %in% c(3,-6) & m1b13b %in% c(3,-6) &
                                            m2d6h %in% c(3,-6) & m2d6i %in% c(3,-6) &
                                            m3d7m %in% c(2,-6) & m3d7o %in% c(2,-6) ~ F),
            # Drug and alcohol abuse at year 3 interview, interfering with work. Use mother's report only to avoid complex missingness.
            drug.y3 = case_when(m3c44 == 1 ~ T,
                                m3c44 %in% c(2,-6) ~ F),

            # Relationship type (5-year interview)
            # Romantic partner other than the father at year 5 in the household most of the time
            newpartner.y5 = case_when(m4e1 == 1 ~ F,
                                      m4e2 == 2 ~ F,
                                      m4e2d == 2 ~ F,
                                      m4e2d == 1 ~ T),
            weight = m4citywt) %>%
  # Restrict to those with distal parental incarceration
  filter(pat.inc.dist == 1) %>%
  mutate(num_distalIncarceration = n())

print("Sample restrictions:")
print(data %>%
        filter(1:n() == 1) %>%
        select(starts_with("num_")))

print("Proportion missing on each covariate")
print("High rate of missingness for father variables because he is often not interviewed")
print("Especially high missingness for father impulsivity because it was not asked in 2 cities at age 1")
print(as.matrix(round(colMeans(is.na(data)),2)))

print("Distribution of treatment")
print(round(prop.table(table(data$pat.inc.rec, useNA = "ifany")),2))
print("Distribution of the outcome")
print(round(prop.table(table(data$depressed.m.y5, useNA = "ifany")),2))


# Function to impute and get one set of simulated estimates
get_estimates <- function(num.sims = 100) {
  d <- amelia(data.frame(data %>%
                           sample_frac(replace = T) %>%
                           select(-starts_with("num_"), -contains("change"), -pat.inc.dist) %>%
                           # Top-code to reduce extreme noisiness in imputations
                           mutate(incratio.y3 = ifelse(incratio.y3 > 5, 5, incratio.y3),
                                  num.children = factor(ifelse(num.children <= 3, num.children, 3)),
                                  relat.qual.y3 = factor(relat.qual.y3))),
              noms = c("city","depressed.m.y3", "depressed.m.y5", "pat.inc.rec", 
                       "race.m", "foreign.born.m", "educ.m", 
                       "mom.par.depressed", "rel.yr3", "newpartner.yr3", 
                       "race.f", "foreign.born.f", "educ.f", 
                       "dom.violence.yb13", "drug.y3", "newpartner.y5",
                       "relat.qual.y3","num.children"),
              idvars = "weight",
              m = 1, boot.type = "none")$imputations$imp1 %>%
    mutate(weight = 1) # Do everything unweighted
  fit_marginal <- glm(depressed.m.y5 ~ pat.inc.rec,
                      family = binomial(link="logit"),
                      data = d)
  fit_total <- glm(depressed.m.y5 ~ pat.inc.rec + (race.m + foreign.born.m + age.m + educ.m + mom.par.depressed + incratio.y3 +
                                                     mat.hard.y3 + rel.yr3 +
                                                     newpartner.yr3 + relat.qual.y3 + num.children + parenting.stress.y3 +
                                                     shared.responsibility.y3 + cooperation.y3 + pat.engage.y3 + race.f +
                                                     foreign.born.f + age.f + educ.f + impulsivity +
                                                     dom.violence.yb13 + drug.y3 + depressed.m.y3),
                   family = binomial(link="logit"),
                   data = d)
  fit_direct <- glm(depressed.m.y5 ~ pat.inc.rec*newpartner.y5 + (race.m + foreign.born.m + age.m + educ.m + mom.par.depressed + incratio.y3 +
                                                               mat.hard.y3 + rel.yr3 +
                                                               newpartner.yr3 + relat.qual.y3 + num.children + parenting.stress.y3 +
                                                               shared.responsibility.y3 + cooperation.y3 + pat.engage.y3 + race.f +
                                                               foreign.born.f + age.f + educ.f + impulsivity +
                                                               dom.violence.yb13 + drug.y3 + depressed.m.y3),
                    family = binomial(link="logit"),
                    data = d)
  
  # Set values to predict for total effect model
  X1n <- X0n <- model.matrix(fit_total)
  X1n[,"pat.inc.recTRUE"] <- 1
  X0n[,"pat.inc.recTRUE"] <- 0
  
  # Set values to predict for direct effect model
  X_direct <- model.matrix(fit_direct)
  set_values <- function(X, treatment_value, mediator_value) {
    if (!is.na(treatment_value)) {
      X[,"pat.inc.recTRUE"] <- treatment_value
    }
    if (!is.na(mediator_value)) {
      X[,"newpartner.y5TRUE"] <- mediator_value
    }
    X[,"pat.inc.recTRUE:newpartner.y5TRUE"] <- X[,"pat.inc.recTRUE"] * X[,"newpartner.y5TRUE"]
    return(X)
  }
  Xn0 <- set_values(X_direct,NA,0)
  Xn1 <- set_values(X_direct,NA,1)
  X00 <- set_values(X_direct,0,0)
  X01 <- set_values(X_direct,0,1)
  X10 <- set_values(X_direct,1,0)
  X11 <- set_values(X_direct,1,1)
  
  # Simulate coefficients from the likelihood distribution
  beta.marginal.star <- rmvnorm(num.sims, mean = coef(fit_marginal), sigma = vcov(fit_marginal))
  beta.total.star <- rmvnorm(num.sims, mean = coef(fit_total), sigma = vcov(fit_total))
  beta.direct.star <- rmvnorm(num.sims, mean = coef(fit_direct), sigma = vcov(fit_direct))
  
  # Convert to simulated point estimates
  point.star <- data.frame(
    factual.0.n = as.vector(plogis(c(1,0) %*% t(beta.marginal.star))),
    factual.1.n = as.vector(plogis(c(1,1) %*% t(beta.marginal.star))),
    potential.0.n = apply(plogis(X0n %*% t(beta.total.star)),2,function(x) weighted.mean(x,w = d$weight)),
    potential.1.n = apply(plogis(X1n %*% t(beta.total.star)),2,function(x) weighted.mean(x,w = d$weight)),
    potential.n.1 = apply(plogis(Xn1 %*% t(beta.direct.star)),2,function(x) weighted.mean(x,w = d$weight)),
    potential.n.0 = apply(plogis(Xn0 %*% t(beta.direct.star)),2,function(x) weighted.mean(x,w = d$weight)),
    potential.0.0 = apply(plogis(X00 %*% t(beta.direct.star)),2,function(x) weighted.mean(x,w = d$weight)),
    potential.0.1 = apply(plogis(X01 %*% t(beta.direct.star)),2,function(x) weighted.mean(x,w = d$weight)),
    potential.1.0 = apply(plogis(X10 %*% t(beta.direct.star)),2,function(x) weighted.mean(x,w = d$weight)),
    potential.1.1 = apply(plogis(X11 %*% t(beta.direct.star)),2,function(x) weighted.mean(x,w = d$weight)),
    coefficient_total = beta.total.star[,"pat.inc.recTRUE"],
    coefficient_direct = beta.direct.star[,"pat.inc.recTRUE"],
    newpartner_new_incarcerated = weighted.mean(d$newpartner.y5[!d$newpartner.yr3 & d$pat.inc.rec], w = d$weight[!d$newpartner.yr3 & d$pat.inc.rec]),
    newpartner_new_notIncarcerated = weighted.mean(d$newpartner.y5[!d$newpartner.yr3 & !d$pat.inc.rec], w = d$weight[!d$newpartner.yr3 & !d$pat.inc.rec])
  )
  return(point.star)
}

t0 <- Sys.time()
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(08544)
n.imps <- 100
draws <- foreach(i = 1:n.imps, .packages = c("tidyverse","Amelia","mvtnorm","reshape2"), .combine = "rbind") %dorng% {
  get_estimates() %>%
    mutate(draw = i)
}
stopCluster(cl)


# Note the estimates for the text
print("Descriptive difference across groups")
print(draws %>%
        select(factual.1.n, factual.0.n) %>%
        mutate(difference = factual.1.n - factual.0.n) %>%
        melt(id = NULL, variable.name = "estimand") %>%
        group_by(estimand) %>%
        summarize(estimate = mean(value),
                  ci.min = quantile(value, .025),
                  ci.max = quantile(value, .975)))
print("Coefficient-based estimates:")
coef_based_estimates <- draws %>%
  select(coefficient_total, coefficient_direct) %>%
  melt(id = NULL, variable.name = "estimand") %>%
  group_by(estimand) %>%
  summarize(estimate = mean(value),
            ci.min = quantile(value, .025),
            ci.max = quantile(value, .975))
print("Beta")
print(coef_based_estimates)
print("Odds ratio")
print(coef_based_estimates %>% 
        mutate_if(is.numeric,exp))
print("Treatment effect on P(Depressed)")
print(draws %>%
        select(potential.1.n, potential.0.n) %>%
        mutate(effect = potential.1.n - potential.0.n) %>%
        melt(id = NULL, variable.name = "estimand") %>%
        group_by(estimand) %>%
        summarize(estimate = mean(value),
                  ci.min = quantile(value, .025),
                  ci.max = quantile(value, .975)))
print("Motivation for mediation:")
print("Proportion repartnering by age 5 among those not repartered at age 3, by treatment")
print(draws %>%
        select(starts_with("newpartner")) %>%
        summarize_all(.funs = mean))
print("Mean outcome under all treatment and mediator assignments")
print(draws %>%
        select(potential.0.0, potential.0.1, potential.1.0, potential.1.1) %>%
        melt(id = NULL, variable.name = "estimand") %>%
        group_by(estimand) %>%
        summarize(estimate = mean(value),
                  ci.min = quantile(value, .025),
                  ci.max = quantile(value, .975)))
print("Mediator effect on P(Depressed)")
print(draws %>%
        select(potential.n.1, potential.n.0) %>%
        mutate(effect = potential.n.1 - potential.n.0) %>%
        melt(id = NULL, variable.name = "estimand") %>%
        group_by(estimand) %>%
        summarize(estimate = mean(value),
                  ci.min = quantile(value, .025),
                  ci.max = quantile(value, .975)))
print("Causal interaction mediator effect on P(Depressed)")
print(draws %>%
        select(potential.1.1, potential.1.0, potential.0.1, potential.0.0) %>%
        transmute(effect.1 = potential.1.1 - potential.1.0,
                  effect.0 = potential.0.1 - potential.0.0) %>%
        melt(id = NULL, variable.name = "estimand") %>%
        group_by(estimand) %>%
        summarize(estimate = mean(value),
                  ci.min = quantile(value, .025),
                  ci.max = quantile(value, .975)))
print("Controlled direct effect on P(Depressed)")
print(draws %>%
        select(potential.1.1, potential.1.0, potential.0.1, potential.0.0) %>%
        transmute(effect.1 = potential.1.1 - potential.0.1,
                  effect.0 = potential.1.0 - potential.0.0) %>%
        melt(id = NULL, variable.name = "estimand") %>%
        group_by(estimand) %>%
        summarize(estimate = mean(value),
                  ci.min = quantile(value, .025),
                  ci.max = quantile(value, .975)))


# Make plots to combine in latex
for_plots <- draws %>%
  select(potential.1.1, potential.1.0, potential.0.1, potential.0.0) %>%
  mutate(cde.d.1 = potential.1.1 - potential.0.1,
         cde.d.0 = potential.1.0 - potential.0.0,
         mediator.1.d = potential.1.1 - potential.1.0,
         mediator.0.d = potential.0.1 - potential.0.0,
         interaction.d.d = mediator.1.d - mediator.0.d) %>%
  melt(id = NULL,
       variable.name = "estimand") %>%
  group_by(estimand) %>%
  summarize(estimate = mean(value),
            ci.min = quantile(value, .025),
            ci.max = quantile(value, .975))
for (estimand_case in unique(for_plots$estimand)) {
  for_plots %>%
    filter(estimand == estimand_case) %>%
    ggplot(aes(x = 0, y = estimate,
               ymin = ci.min,
               ymax = ci.max)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(width = .2) +
    geom_point() +
    #geom_label(aes(label = format(round(estimate,2),nsmall = 2)),
    #           size = 1.5) +
    theme_bw() +
    scale_x_continuous(limits = c(-1,1),
                       name = element_blank()) +
    scale_y_continuous(limits = c(min(for_plots$ci.min) - .02, max(for_plots$ci.max) + .02),
                       name = case_when(grepl("potential",estimand_case) ~ "\nP(Depression)",
                                        grepl("mediator",estimand_case) ~ "Difference in\nP(Depression)",
                                        grepl("cde",estimand_case) ~ "Difference in\nP(Depression)",
                                        grepl("interaction",estimand_case) ~ "Difference in\ndifference")) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()) + 
    ggsave(paste0("output/subplot_",gsub("[.]","_",estimand_case),".pdf"),
           height = 1.5, width = 2)
}

sink()
