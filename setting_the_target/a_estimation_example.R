
# Replication code for:
# What is Your Estimand? Defining the Target Quantity Connects Statistical Evidence to Theory
# Ian Lundberg, Rebecca Johnson, and Brandon Stewart
# Email: ilundberg@princeton.edu

# This file carries out the analysis for
# Concrete Estimation Example: The Family Gap in Pay

# The data for this example are available from IPUMS at
# https://cps.ipums.org/cps/
# 1. Register for an account
# 2. Put the following variables in your cart
# YEAR, SERIAL, MONTH, CPSID, ASECFLAG, ASECWTH,
# PERNUM, CPSIDP, ASECWT, RELATE, AGE, SEX, RACE,
# MARST, NCHILD, AHRSWORKT, EDUC, EARNWT, REPWTP,
# CLASSWLY, WKSWORK1, WKSWORK2, UHRSWORKLY, INCWAGE
# 3. Select the sample "IPUMS-CPS, ASEC 2019"
# 4. Select data format ".dta (Stata)"
# 5. Select cases on AGE (ages 25-44 only) and SEX (Female only)
# 6. Download the data and place it in the data subdirectory.
# You will need to rename to "cps_00040.dta" for the code below or 
# change the name of the RAW_DATA_NAME parameter

library(readstata13)
library(tidyverse)
library(reshape2)
library(foreach)
library(mgcv)


## Constants
OUTPUT_NAME= "PalWaldfogel_output.txt"

## Can either read raw data or read in prepared data
READ_RAW_DATA <- TRUE
RAW_DATA_NAME <- "cps_00040.dta"
RESTRICTIONS_DATA_NAME <- "pw_d_restrictions.csv"
FIT_DATA_NAME <- "pw_to_fit.csv"
PREDICT_DATA_NAME <- "pw_to_predict.csv"
RUN_VAR_ESTIMATION_AGGREGATE <- FALSE
RUN_VAR_ESTIMATE_SUBGROUP <- FALSE


# Function to open your default browser to check when
# coding cps vars
lookup_cpsvar_inbrowser <- function(varname){
  browseURL(sprintf("https://cps.ipums.org/cps-action/variables/%s#codes_section",
                    varname))
}


## Initiate output file
sink(sprintf("output/%s", OUTPUT_NAME))
print("Output from replication of Pal and Waldfogel")


####################################
# Prep the main data file on women #
# ages 25-44                       #
####################################

if(READ_RAW_DATA){
  
  ## Read in stata file
  data <- read.dta13(sprintf("data/%s", RAW_DATA_NAME),
                     convert.factors = F)
  
  d_all <- data %>%
    
    # Create different flags to filter sample
    
    ### Filter to 25-44 (inclusive) and sex == female (2)
    filter(age >= 25 & age <= 44 & sex == 2) %>%
    mutate(filter_inDemRange = 1) %>%
  
    ## Mark missing incomes
    ### classwly is job codes (https://cps.ipums.org/cps-action/variables/CLASSWLY#codes_section)
    ## and >14 are wage/salary jobs
    mutate(derived_incwage = case_when(classwly > 14 ~ incwage,
                                       TRUE ~ NA_integer_), 
           ### Even for those with jobs in those categories, set to missing if has
           ### missing codes for those wages
           derived_incwage = case_when(derived_incwage != 9999999 & derived_incwage != 9999998 ~ derived_incwage,
                                       TRUE ~ NA_integer_),
           
           ## Weeks worked last year
           ### For the other years, we can just use wkswork1, which is the continuous report
           derived_wkswork1 = ifelse(wkswork1 <= 0, NA, wkswork1),
           
           ### Usual hours per week worked last year
           derived_uhrsworkly = ifelse(uhrsworkly != 999, uhrsworkly, NA),
           
           ## Create hourly wages as total income/wage divided by # of weeks x usual weekly hours
           derived_wage = derived_incwage / (derived_wkswork1 * derived_uhrsworkly),
           
           ## Truncate log wage at 1st and 99th percentile
           derived_ln_wage = log(case_when(derived_wage < quantile(derived_wage, .01, na.rm = T) ~ quantile(derived_wage, .01, na.rm = T),
                                derived_wage > quantile(derived_wage, .99, na.rm = T) ~ quantile(derived_wage, .99, na.rm = T),
                                T ~ derived_wage)),
           
           ## Create conditioning set
           
           ### Code education into four levels 
           derived_educ = factor(ifelse(educ == 1 | educ == 999, NA,
                                        ## Less than high school
                                        ifelse(educ <= 60 | educ == 71, 1,
                                               ## HS degree (include diploma unclear 72)
                                               ifelse(educ == 70 | educ == 72 | educ == 73, 2,
                                                      ## Some college
                                                      ifelse(educ < 110, 3,
                                                             ## College or more
                                                             4))))),
           
           ### Family status is whether married with spouse present (TRUE),
           ### other categories (FALSE) or NA
           derived_married = ifelse(marst == 9, NA, marst == 1),
           
           ### Race is white black or other, with later including mixed race
           derived_race = factor(ifelse(race == 100, 1,
                                        ifelse(race == 200, 2, 3)),
                                 labels = c("White","Black","Other")),
           
           ## Mother is TRUE if nchild > 0, false otherwise
           derived_mother = (nchild > 0),
           
           ## For later analysis, create age^squared
           derived_agesq = age^2,
           
           # Note who is employed for later filtering
           filter_employed = !is.na(derived_ln_wage) & derived_ln_wage != -Inf) 
  
  ## Variables to include
  weights_identifiers = c("pernum", "serial", "asecwt",
                          grep("repwtp", colnames(d_all),
                               value = TRUE)) 
  outcome = "derived_ln_wage"
  treatment = "derived_mother"
  covariates = c(sprintf("derived_%s", 
                         c("educ", "married",
                           "race", "agesq")), "age")
  filters = grep("filter", colnames(d_all), value = TRUE)
  cols_keep = c(weights_identifiers, outcome, treatment, covariates, 
                filters)
  
  # Determine the strata with support: 
  # Both mothers and non-mothers observed with hourly wages
  supported_strata <- d_all %>%
    select(all_of(outcome),all_of(treatment),all_of(covariates)) %>%
    # Restrict to cases with an hourly wage
    filter(!is.na(derived_ln_wage)) %>%
    # Restrict to strata with both moms and non-moms observed
    group_by_at(vars(all_of(covariates))) %>%
    summarize(filter_onSupport = n_distinct(derived_mother) == 2) %>%
    filter(filter_onSupport) %>%
    group_by()
  
  # Create a dataset noting the restrictions
  d_restrictions <- d_all %>%
    left_join(supported_strata, by = covariates) %>%
    mutate(filter_onSupport = ifelse(is.na(filter_onSupport), 0, filter_onSupport),
           # Make the employed filter 0 for anyone who is already not on the support
           filter_employed = ifelse(!filter_onSupport, 0, filter_employed)) %>%
    select(derived_mother,starts_with("filter")) %>%
    group_by(derived_mother) %>%
    summarize_all(.funs = sum) %>%
    # Order the filters in order of application
    select(derived_mother, filter_inDemRange, filter_onSupport, filter_employed)
    
  # Make data frames to fit (employed women)
  # and to predict (all mothers)
  to_fit <- d_all %>%
    select(all_of(cols_keep)) %>%
    # Determine which rows have support
    left_join(supported_strata, by = covariates) %>%
    # Restrict to employed women in common support
    filter(filter_onSupport & filter_employed)
  
  to_predict <- d_all %>%
    select(all_of(cols_keep)) %>%
    # Determine which rows have support
    left_join(supported_strata, by = covariates) %>%
    # Restrict to mothers in common support
    filter(filter_onSupport & derived_mother)
  
  write.csv(d_restrictions, sprintf("intermediate/%s", RESTRICTIONS_DATA_NAME))
  write.csv(to_fit, sprintf("intermediate/%s", FIT_DATA_NAME))
  write.csv(to_predict, sprintf("intermediate/%s", PREDICT_DATA_NAME))
} else{
  d_restrictions <- read.csv(sprintf("intermediate/%s", RESTRICTIONS_DATA_NAME))
  to_fit <- read.csv(sprintf("intermediate/%s", FIT_DATA_NAME))
  to_predict <- read.csv(sprintf("intermediate/%s", PREDICT_DATA_NAME))
}

##################################
# Note the sample restrictions   #
# and the common support problem #
##################################

print("Sample restrictions for mothers and non-mothers")
print("The models are fit on employed mothers and non-mothers (right column)")
print("Predictions are made for all mothers on common support (bottom middle)")
print(d_restrictions)

print("Note proportion of mothers and non-mothers in the region of common support")
print(d_restrictions %>%
        transmute(proportion_on_support = filter_onSupport / filter_inDemRange))

##################################
# Estimate the family gap in pay #
# in aggregate and at each age   #
##################################

# Write a function to estimate the gap with a given weight variable.
# Later, we will apply this function with the main weights and with each set of replicate weights.

estimate_gap <- function(weight_name) {
  
  # Prepare a data frame d_fit for model fitting with this weight variable
  d_fit <- to_fit %>%
    # Assign a variable to have this weight
    rename_at(weight_name, .funs = function(x) "weight") %>%
    # Normalize weight for fitting (matters for gam)
    mutate(weight = weight / mean(weight)) %>%
    # Make mother a factor variable (needed for gam)
    mutate(derived_mother = factor(derived_mother))
    
  # Make a data frame d_predict with the covariate strata where we will predict, with this weight variable
  d_predict <- to_predict %>%
    rename_at(weight_name, .funs = function(x) "weight") %>%
    # Make one row per covariate stratum
    # since prediction is constant within a stratum
    group_by_at(vars(all_of(covariates))) %>%
    summarize(weight = sum(weight)) %>%
    group_by()
  
  # Make estimates with the nonparametric adjustment strategy
  nonparametric_strata <- d_fit %>%
    ## Group by strata defined by treatment varname and control variables
    group_by_at(vars(all_of(c(covariates,"derived_mother")))) %>%
    ## Estimate the weighted mean ine ach stratum
    summarize(estimate = weighted.mean(derived_ln_wage, w = weight)) %>%
    ## Calculate the gap between mothers and non-mothers in each stratum
    spread(key = "derived_mother", value = "estimate") %>%
    mutate(estimate = `TRUE` - `FALSE`) %>%
    select(-`TRUE`, -`FALSE`)
  
  # Get fitted values  in the data frame where we are making predictions
  nonparametric_fitted <- d_predict %>%
    # Append the predictions from nonparametric stratification
    left_join(nonparametric_strata, by = covariates) %>%
    # Aggregate estimates within each age group
    group_by(age) %>%
    summarize(estimate = weighted.mean(estimate, w = weight)) %>%
    mutate(estimand = "age_specific") %>%
    # Append an estiamte that aggregates across the age groups
    bind_rows(
      d_predict %>%
        left_join(nonparametric_strata, by = covariates) %>%
        summarize(estimate = weighted.mean(estimate, w = weight)) %>%
        mutate(estimand = "aggregate",
               age = NA)
    ) %>%
    # Prepare columns to match our output format
    mutate(model = "nonparametric") %>%
    select(model, estimand, age, estimate)
  
  # Fit a series of models that make different estimation assumptions to pool information
  model_fits <- list(
    # OLS model with all terms entered additively (no interactions)
    additive = lm(derived_ln_wage ~ derived_mother + age + derived_agesq +
                    derived_educ + derived_married + derived_race,
                  data = d_fit,
                  weights = weight),
    # OLS model that includes an interaction between age and motherhood
    interactive = lm(derived_ln_wage ~ derived_mother*(age + derived_agesq) +
                       derived_educ + derived_married + derived_race,
                     data = d_fit,
                     weights = weight),
    # GAM that allows a smooth age curve interacted with motherhood
    smooth = gam(derived_ln_wage ~ derived_mother + s(age, by = derived_mother) +
                   derived_educ + derived_married + derived_race,
                 data = d_fit,
                 weights = weight),
    # OLS with indicators for each age interacted with motherhood
    indicators = lm(derived_ln_wage ~ derived_mother*factor(age) +
                      derived_educ + derived_married + derived_race,
                    data = d_fit,
                    weights = weight)
  )
  
  # Get the fitted values in the data frame where we are making predictions
  models_fitted <- foreach(fit_name = names(model_fits), .combine = "rbind") %do% {
    # First, calculate age-specific estimates
    age_specific <- d_predict %>%
      # Make predictions with derived_mother set to TRUE and set to FALSE. Difference them.
      mutate(estimate = predict(model_fits[[fit_name]],
                                newdata = d_predict %>%
                                  mutate(derived_mother = "TRUE")) -
               predict(model_fits[[fit_name]],
                       newdata = d_predict %>%
                         mutate(derived_mother = "FALSE"))) %>%
      # Aggregate within age groups
      group_by(age) %>%
      summarize(estimate = weighted.mean(estimate, w = weight),
                weight = sum(weight))
    # Aggregate across age groups to get an aggregate estimate.
    aggregate <- age_specific %>%
      group_by() %>%
      summarize(estimate = weighted.mean(estimate, w = weight))
    
    # Return a data frame with both the age-specific and aggregate estimate
    age_specific %>%
      select(-weight) %>%
      mutate(estimand = "age_specific") %>%
      bind_rows(aggregate %>%
                  mutate(age = NA,
                         estimand = "aggregate")) %>%
      mutate(model = fit_name) %>%
      select(model, estimand, age, estimate)
  }
  return(models_fitted %>%
           bind_rows(nonparametric_fitted))
}

# Calculate the point estimate
point <- estimate_gap("asecwt")

# Calculate the replicate estimates
replicates <- foreach(i = 1:160, .combine = "rbind") %do% {
  estimate_gap(paste0("repwtp",i))
}

# Aggregate those into results that include a point estimate and standard error
results <- replicates %>%
  rename(estimate_star = estimate) %>%
  left_join(point,
            by = c("model","estimand","age")) %>%
  group_by(model,estimand,age) %>%
  summarize(point = mean(estimate), # this just aggreagtes a constant point estimate
            # Calculate the standard error as described in the survey documentation
            se = sqrt(4 / 160 * sum((estimate_star - estimate) ^ 2))) %>% 
  group_by() %>%
  # Add detail to the variable values for use in plotting
  mutate(model = factor(case_when(model == "nonparametric" ~ 1,
                                  model == "indicators" ~ 2,
                                  model == "smooth" ~ 3,
                                  model == "interactive" ~ 4,
                                  model == "additive" ~ 5),
                        labels = c("Stratification:\nNo estimation assumptions",
                                   "Education + Marital + Race +\n(Age indicators x Motherhood)",
                                   "Education + Marital + Race +\n(Age smooth x Motherhood)",
                                   "Education + Marital + Race +\n(Age quadratic x Motherhood)",
                                   "Education + Marital + Race +\nAge quadratic + Motherhood")),
         estimand = factor(case_when(estimand == "aggregate" ~ 1,
                                     estimand == "age_specific"~ 2),
                           labels = c("Aggregate\ngap","Age-specific\ngap")))

# Plot the estimated gap
results %>%
  mutate(age = ifelse(estimand == "Aggregate\ngap",35,age)) %>%
  ggplot(aes(x = age, y = point,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_errorbar(width = .5) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "Controlled direct effect of motherhood\non the log hourly wage that would\nbe realized if employed",
                     limits = c(-.27,.27)) +
  xlab("Age") +
  facet_grid(estimand ~ model) +
  theme(strip.text.y = element_text(angle = 0),
        strip.text.x = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  # Annotate within facets
  geom_text(data = results %>%
              filter(estimand == "Aggregate\ngap") %>%
              mutate(age = ifelse(model == "Stratification:\nNo estimation assumptions",25,45),
                     point = .27,
                     label = case_when(
                       model == "Stratification:\nNo estimation assumptions" ~ "Weakest assumptions\n(most credible)",
                       model == "Education + Marital + Race +\nAge quadratic + Motherhood" ~ "Strongest assumptions\n(least credible)")),
            aes(label = label, hjust = ifelse(model == "Stratification:\nNo estimation assumptions",0,1)),
            vjust = 1,
            size = 3) +
  geom_text(data = results %>%
              filter(estimand == "Age-specific\ngap" & age == 25) %>%
              mutate(point = .27,
                     label = case_when(
                       model == "Stratification:\nNo estimation assumptions" ~ "Very uncertain\ngaps within\nsubgroups",
                       model == "Education + Marital + Race +\n(Age smooth x Motherhood)" ~ "With this assumption,\nevidence shows a gap\nat young ages only",
                       model == "Education + Marital + Race +\nAge quadratic + Motherhood" ~ "Additive OLS assumes\na constant effect.\nClearly this model\nis only an approximation."
                     )),
            aes(label = label),
            size = 3, hjust = 0, vjust = 1) +
  ggsave("output/all_gap_estimates_new.pdf",
         height = 5, width = 10)

#######################
# Cross-validated MSE #
#######################

# This function will calculate cross-validated mean squared error by 5-fold cross-validation.
# We will apply it with the main weights for a point estimate
# and with each set of replicate weights to get a standard error.
make_cv_results  <- function(weight_name, nfolds = 5) {
  
  # Assign a variable to have this weight
  # and create folded data
  folded <- to_fit %>%
    rename_at(weight_name, .funs = function(x) "weight") %>%
    # Normalize weight for fitting (matters for gam)
    mutate(weight = weight / mean(weight)) %>%
    # Make mother a factor variable (needed for gam)
    mutate(derived_mother = factor(derived_mother)) %>%
    # Create folds for cross-validation systematically by sample weigth
    group_by() %>%
    arrange(weight) %>%
    mutate(fold = rep(1:nfolds, ceiling(n() / nfolds))[1:n()])
  
  # Iterate through folds as train and test set
  # For each fold, store all squared errors in the test set
  squared_errors <- foreach(f = 1:nfolds, .combine = "rbind") %do% {
    
    ## split into train and test
    train <- folded %>%
      filter(fold != f)
    test <- folded %>%
      filter(fold == f)
    
    # Fit the model-based approaches
    model_fits <- list(
      additive = lm(derived_ln_wage ~ derived_mother + age + derived_agesq +
                      derived_educ + derived_married + derived_race,
                    data = train,
                    weights = weight),
      interactive = lm(derived_ln_wage ~ derived_mother*(age + derived_agesq) +
                         derived_educ + derived_married + derived_race,
                       data = train,
                       weights = weight),
      smooth = gam(derived_ln_wage ~ derived_mother + s(age, by = derived_mother) +
                     derived_educ + derived_married + derived_race,
                   data = train,
                   weights = weight),
      indicators = lm(derived_ln_wage ~ derived_mother*factor(age) +
                        derived_educ + derived_married + derived_race,
                      data = train,
                      weights = weight)
    )
    
    # Get squared errors for the stratification approach
    stratification_result <- train %>%
      group_by_at(vars(all_of(c(covariates,"derived_mother")))) %>%
      summarize(yhat = weighted.mean(derived_ln_wage, w = weight)) %>%
      # Use right join so we retain all rows in the test, even if they have no predictions
      right_join(test, by = c(covariates,"derived_mother")) %>%
      mutate(squared_error = (derived_ln_wage - yhat) ^ 2) %>%
      group_by() %>%
      mutate(model = "nonparametric") %>%
      select(model, squared_error, weight)
    
    # Create one data frame of all squared errors for this fold
    squared_errors_this_fold <- foreach(fit_name = names(model_fits), .combine = "rbind") %do% {
      test %>%
        mutate(yhat = predict(model_fits[[fit_name]],
                              newdata = test)) %>%
        transmute(model = fit_name,
                  squared_error = (derived_ln_wage - yhat) ^ 2,
                  weight = weight)
    } %>%
      bind_rows(stratification_result)
    
    return(squared_errors_this_fold)
  }
  
  # Aggregate to an estimate of mean squared error
  mse <- squared_errors %>%
    group_by(model) %>%
    summarize(mse = weighted.mean(squared_error, w = weight, na.rm = T),
              prop_included = weighted.mean(!is.na(squared_error), w = weight))
  
  return(mse)
}
      
# Calculate the point estimate for cross-validated MSE
cv_point <- make_cv_results("asecwt") %>%
  arrange(mse)

# Calculate the replicate estimates for cross-validated MSE
cv_replicates <- foreach(i = 1:160, .combine = "rbind") %do% {
  make_cv_results(paste0("repwtp",i))
}

cv_results <- cv_replicates %>%
  rename(mse_star = mse) %>%
  left_join(cv_point,
            by = c("model")) %>%
  group_by(model) %>%
  summarize(point = mean(mse), # this just aggreagtes a constant point estimate
            se = sqrt(4 / 160 * sum((mse_star - mse) ^ 2))) %>% 
  group_by() %>%
  mutate(model = factor(case_when(model == "nonparametric" ~ 1,
                                  model == "indicators" ~ 2,
                                  model == "smooth" ~ 3,
                                  model == "interactive" ~ 4,
                                  model == "additive" ~ 5),
                        labels = c("Stratification:\nNo estimation assumptions",
                                   "Education + Marital + Race +\n(Age indicators x Motherhood)",
                                   "Education + Marital + Race +\n(Age smooth x Motherhood)",
                                   "Education + Marital + Race +\n(Age quadratic x Motherhood)",
                                   "Education + Marital + Race +\nAge quadratic + Motherhood")))

print("Ranking of approaches by predictive performance")
print(cv_results %>%
        arrange(point))

print("More decimal places on mean squared error, in the above order:")
print(sort(cv_results$point))

# Plot of the cross-validation results
cv_results %>%
  filter(model != "Stratification:\nNo estimation assumptions") %>%
  mutate(model = fct_reorder(model, point)) %>%
  ggplot(aes(x = model, y = point,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se)) +
  geom_point() +
  geom_errorbar(width = .2) +
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "Statistical Approach") +
  theme_bw() +
  ggsave("output/cv_results.pdf",
         height = 3, width = 10)

sink()
