
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
# Helper functions
# and constants 
################

#' Code individual items into a scale
#' 
#' @param data Dataframe that contains the columns
#' @param prefix_string String with prefix for scale or gen regex pattern
#' @param verbose whether to print name of items
#' @param FUN  what to code as yes = true, no = false, and NA
#' @return vector of length nrow(data) that has scale values (sum of yes on 
#' indiv items and then averaging over n of items)
#' One note is that, as structured, if a person is missing any
#' scale items they are missing for the entire scale
#' Can change by setting na.rm = TRUE in rowsums and adjusting
#' the denominator on the scale to be person-specific 
code_agg_scales <- function(data, prefix_string,
                            verbose = FALSE,
                            FUN){
  
  ## get all cols with that prefix
  cols_withprefix = grep(sprintf("^%s", prefix_string),
                         colnames(data),
                         value = TRUE)
  if(verbose) print(sprintf("Coding scale based on: %s", paste(cols_withprefix, 
                                                        collapse = ";")))
  ## apply coding func to all cols with that pattern
  data[, cols_withprefix] = apply(data[, cols_withprefix], 2, FUN)
  ## sum those cols and average by length of cols considered 
  sum_across = rowSums(data[, cols_withprefix])
  avg_across = (1/length(cols_withprefix)) * sum_across
  # return vector of averages 
  return(avg_across)
}

## Constants
READ_RAW_DATA = TRUE
RAW_DATA_NAME = "ff_res_merge4.dta"


################
# Prepare data #
################

# NOTE: Documentation for the data files is available at
# http://www.fragilefamilies.princeton.edu/documentation.asp
# Most variables come from the 3- and 5-year mother interview codebooks.


if(READ_RAW_DATA){
  # Read data from baseline through 5 year interviews
  raw_data <- read_dta(sprintf("data/%s", RAW_DATA_NAME))
  
  # Code and select variables
  df_init <- raw_data %>%
    # Filtering flags
    ## Flag for those with with non-zero weight
    ## Flag for mom completing y3 (wav3) and y5 (wave4) ints
    ## Flag for not missing either of the outcomes data (so observe both)
    mutate(filter_is_nonzero_cityweight = !is.na(m4citywt),
           filter_momcompletey3y5 = m3intyr != -9 & m4intyr != -9,
           filter_nonmissing_DV = cm4md_case_lib >= 0 & m4j0 >= 0) %>%
    ## Clean up other variables/construct scales
    transmute(filter_is_nonzero_cityweight,
              filter_momcompletey3y5,
              filter_nonmissing_DV,
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
              # Distant paternal incarceration - among non-missing for dad ever in jail, if ever in jail
              pat.inc.dist = case_when(cmf3fevjail %in% 0:1 ~ cmf3fevjail == 1),
              
              # COVARIATES
              # Race: White, black, Hispanic, other
              race.m = fct_drop(case_when(cm1ethrace > 0 ~ as_factor(cm1ethrace))),
              race.f = fct_drop(case_when(cf1ethrace > 0 ~ as_factor(cf1ethrace))),
              # Born outside the US
              ## rj note: i think follows same logic of -9 through -1 missing
              ## so changed just to make more consistent
              foreign.born.m = case_when(m1h2 > 0 ~ m1h2 == 2),
              foreign.born.f = case_when(f1h2 > 0 ~ f1h2 == 2),
              # Age in years at baseline survey
              age.m = case_when(cm1age >= 0 ~ cm1age),
              age.f = case_when(cf1age >= 0 ~ cf1age),
              # Education: <HS, HS or GED, postsecondary education, at baseline
              educ.m = fct_drop(case_when(cm1edu > 0 ~ as_factor(cm1edu))),
              educ.f = fct_drop(case_when(cf1edu > 0 ~ as_factor(cf1edu))),
              # Parents' relationship at 3-year: married, cohabiting, in a nonresidential romantic relationship, 
              # and no longer romantically involved; unnamed categories var < 0 become NA
              rel.yr3 = case_when(cm3relf==1 ~ "Married",
                                  cm3relf == 2 ~ "Cohabiting",
                                  cm3relf %in% 3:4 ~ "Nonresidential romantic relationship",
                                  cm3relf %in% 5:8 ~ "No longer romantically involved"),
              # Whether mother reported romantic relationship with new partner at 3-year. MATCHES
              newpartner.yr3 = case_when(m3e2d %in% c(-6,2) ~ F,
                                         m3e2d == 1 ~ T),
              # Romantic partner other than the father at year 5 who
              # lives in the household most of the time
              newpartner.y5 = case_when(m4e1 == 1 ~ F, # mother and father are living together most of the time
                                        m4e2 == 2 ~ F, # mother reports "no" on whether involved in romantic relate w/ other than father
                                        m4e2d == 2 ~ F, # even if yes to above (is involved in romantic relationship w/ non-f), reports no on living together
                                        m4e2d == 1 ~ T), # finally, only true if not filtered to False above and affirmatively reports living together
              
              # Whether either of the mother’s biological parents experienced a two-week period of feeling depressed, down in the dumps, or blue
              # If neither depressed (m3j[45|50] both equal 2), mom.par.depressed == FALSE
              # If has no knowledge of father (-14), I am coding as missing.
              mom.par.depressed = case_when(m3j45 >= 0 & m3j50 >= 0 ~ m3j45 == 1 | m3j50 == 1,
                                            m3j45 == 1 ~ T,
                                            m3j50 == 1 ~ T),
              # Maternal reports of income-to-poverty ratio
              incratio.y3 = case_when(cm3povco >= 0 ~ cm3povco),
              # Number of children in the household
              num.children = case_when(cm3kids >= 0 ~ cm3kids),
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
              weight = m4citywt,
              
              # Step 2: code scales  
              ## Material hardship: Sum of events that could have happened because not enough money
              ## The authors write that there are 12 questions. I think that's a typo.
              ## I am only using the 8 items asked in all cities (so excluding ij suffix 
              ## which here notes is only asked in 18-cities: https://fragilefamilies.princeton.edu/sites/fragilefamilies/files/year_3_guide.pdf)
              mat.hard.y3 = code_agg_scales(raw_data, prefix_string = "m3i23[a-h]",
                                            FUN = function(x){case_when(x %in% 1 ~ TRUE,
                                                                        x %in% 2 ~ FALSE,
                                                                        TRUE ~ NA)}),
              ## Relationship quality with the child’s father: 1 = poor to 5 = excellent
              relat.qual.y3 = case_when(m3d0 == 2 ~ 1, # poor if skipped because of no relationship with father
                                        m3d4 %in% 1:5 ~ 6 - as.numeric(m3d4)), 
              ## Co-parenting as in Carlson, McLanahan, and Brooks-Gunn (2008),
              ## though distinction not at all clear on page 468 of that article
              ## Shared responsibility (rev. code from 1 = often; 4 = never; those who skip are often)
              shared.responsibility.y3 = code_agg_scales(raw_data, 
                                                         prefix_string = "m3c7", 
                                                         FUN = function(x){case_when(x == -6 ~ 4,
                                                                                     x %in% 1:4 ~ 5 - as.numeric(x))}),
              ## Cooperation in parenting (same values and skip logic coding as above)
              cooperation.y3 = code_agg_scales(raw_data, 
                                               prefix_string = "m3d1[a-f]$", 
                                               FUN = function(x){case_when(x == -6 ~ 4,
                                                                           x %in% 1:4 ~ 5 - as.numeric(x))}),
              ## Paternal engagement: average (0 to 7) of days per week the mother reported the father
              ## did activities with the focal child, such as sing songs,
              ## read stories, or hug or show physical affection (m3c3a-m3c3m)
              pat.engage.y3 = code_agg_scales(raw_data, 
                                              prefix_string = "m3c3[a-m]$", 
                                              FUN = function(x){case_when(x == -6 ~ 0,
                                                                          x >=0  ~ as.numeric(x))}),
              ## Parenting stress: average of responses to the following (recoded to 
              ## 1 = strongly disagree to 4 = strongly agree)
              ## Being a parent is harder than I thought it would be;
              ## I feel trapped by my responsibilities as a parent; 
              ## Taking care of my children is much more work than pleasure;
              ## I often feel tired, worn out, or exhausted from raising a family
              parenting.stress.y3 = code_agg_scales(raw_data, 
                                                    prefix_string = "m3b6[a-d]$", 
                                                    FUN = function(x){case_when(
                                                      x %in% 1:4  ~5- as.numeric(x))}),
              # Father impulsivity at baseline: Dickman’s (1990) impulsivity scale, 
              # based on an average of six questions 
              # (recoded to 1 = strongly disagree to 4 = strongly agree)
              impulsivity = code_agg_scales(raw_data, 
                                            prefix_string = "f2j2[1-6]$", 
                                            FUN = function(x){case_when(
                                              x %in% 1:4  ~5- as.numeric(x))})) %>%
    mutate(filter_is_distalincarc = case_when(pat.inc.dist ~ TRUE,
                                              TRUE ~ FALSE))
  
  print("Sample restrictions from each filter applied separately:")
  all_filters = grep("^filter", colnames(df_init), value = TRUE)
  print(sprintf("Orig: %s", nrow(df_init)))
  for(i in 1:length(all_filters)){
    filt_use = all_filters[1:i]
    print(sprintf("Removing ppl who fail %s: %s", paste(gsub("filter\\_", "", filt_use), 
                                                        collapse = ";"), nrow(df_init[rowSums(df_init[, filt_use]) == length(filt_use), ])))
  }
  
  data <- df_init[rowSums(df_init[, all_filters]) == length(all_filters), ] %>%
    select(-contains("filter")) %>%
    filter(pat.inc.dist == TRUE) # explicitly restrict to those with affirmative pat incarc
  
  write.csv(data, "intermediate/w_ffs_preimpute.csv",
            row.names = FALSE)
} else{
  data <- read.csv("intermediate/w_ffs_preimpute.csv")
}

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
                           select(-pat.inc.dist) %>%
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
