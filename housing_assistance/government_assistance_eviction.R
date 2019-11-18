
# Government assistance protects low-income families from eviction
# Ian Lundberg, Sarah Gold, Louis Donnelly, Jeanne Brooks-Gunn, and Sara McLanahan
# Code by Ian Lundberg (ilundberg@princeton.edu)

setwd("C:/Users/iandl/Documents/FF_housing_assistance")
sink("results/text_output.txt")
set.seed(08544)

library(tidyverse)
library(haven)
library(reshape2)
library(foreach)
library(Amelia)
library(mitools)
library(xtable)
library(foreach)
library(doParallel)
library(doRNG)
library(survey)

# Set the number of imputations
n.imps <- 50

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

# Load data
d0 <- read_dta("data/ff_allwaves_res_2019.dta") %>%
  left_join(read_dta("data/ff_mecon_all_res2.dta") %>%
              mutate(same_msa = em5scmsa) %>%
              select(idnum, same_msa),
            by = "idnum")

# Prepare variables
d1 <- d0 %>%
  # Restrict to those with mother as caregiver at age 9
  # and in national sample at age 9
  filter(m5a2 %in% c(1,2) & !is.na(m5natwt)) %>%
  transmute(
    idnum = idnum,
    weight = m5natwt,
    #stratum = natstratum,
    #psu = natpsu,
    #weight = m5citywt,
    #stratum = citystratum,
    psu = citypsu,
    ###
    # Most mothers live in the same metropolitan statistical area where they gave birth.
    # Some have moved to a new MSA. To reduce the risk of re-identification,
    # the study does not reveal the new MSA for those who move.
    # The new MSAs would also be so sparsely populated as to be statistically useless.
    # For our purposes, we use city of birth as the geographic indicator
    # and lump all who we know have moved into a new category.
    m1city = m1city,
    city = factor(ifelse(same_msa == 0, 
                         21, # this is the new category, above the 20 sample cities
                         m1city)),
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
    
    # Drug use
    drugs.5 = case_when(m5g21a == 1 ~ T,
                        m5g21b == 1 ~ T,
                        m5g21c == 1 ~ T,
                        m5g21d == 1 ~ T,
                        m5g21e == 1 ~ T,
                        m5g21f == 1 ~ T,
                        m5g21g == 1 ~ T,
                        m5g21h == 1 ~ T,
                        m5g21i == 1 ~ T,
                        m5g21a == 2 & m5g21b == 2 & m5g21c == 2 & 
                          m5g21d == 2 & m5g21e == 2 & m5g21f == 2 & 
                          m5g21g == 2 & m5g21h == 2 & m5g21i == 2 ~ F),
    drugs.4 = case_when(m4j22a == 1 ~ T,
                        m4j22b == 1 ~ T,
                        m4j22c == 1 ~ T,
                        m4j22d == 1 ~ T,
                        m4j22e == 1 ~ T,
                        m4j22f == 1 ~ T,
                        m4j22g == 1 ~ T,
                        m4j22h == 1 ~ T,
                        m4j22i == 1 ~ T,
                        m4j22a == 2 & m4j22b == 2 & m4j22c == 2 & 
                          m4j22d == 2 & m4j22e == 2 & m4j22f == 2 & 
                          m4j22g == 2 & m4j22h == 2 & m4j22i == 2 ~ F),
    drugs.3 = case_when(m3j36a == 1 ~ T,
                        m3j36b == 1 ~ T,
                        m3j36c == 1 ~ T,
                        m3j36d == 1 ~ T,
                        m3j36e == 1 ~ T,
                        m3j36f == 1 ~ T,
                        m3j36g == 1 ~ T,
                        m3j36h == 1 ~ T,
                        m3j36i == 1 ~ T,
                        m3j36a == 2 & m3j36b == 2 & m3j36c == 2 & 
                          m3j36d == 2 & m3j36e == 2 & m3j36f == 2 & 
                          m3j36g == 2 & m3j36h == 2 & m3j36i == 2 ~ F),
    # Slightly different question at wave 2:
    # In the past month, did you smoke marijuana or pot?
    # In the past month, did you use cocaine, crack, speed, LSD, or heroin or any other kind of hard drug?
    # This is just useful for imputation
    drugs.2 = case_when(m2j7 > 0 & m2j7 == 1 ~ T,
                        m2j8 > 0 & m2j8 == 1 ~ T,
                        m2j7 > 0 & m2j8 > 0 ~ F),
    
    # Alcohol: More than 4 drinks at least once per month in past 12 months
    alcohol.5 = case_when(m5g19 %in% (0:1) ~ F,
                          m5g19a >= 0 ~ m5g19a %in% (1:4)),
    alcohol.4 = case_when(m4j20 %in% (0:1) ~ F,
                          m4j20a >= 0 ~ m4j20a %in% (1:4)),
    alcohol.3 = case_when(m3j28 %in% (0:1) ~ F,
                          m3j28a >= 0 ~ m3j28a %in% (1:4)),
    # Slightly different question at wave 2:
    # In the past month, did you drink any alcoholic beverages such as beer, wine, or liquor?
    # In the past month, how many days did you have five or more drinks in one day?
    # This is just useful for imputation
    alcohol.2 = case_when(m2j6 == 2 ~ F,
                          m2j6a >= 0 ~ m2j6a >= 1)
  ) %>%
  select(-own.1,-own.2,-own.3,-own.4)

# Count for sample restrictions
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

print("Sample restrictions:")
print(
  d2 %>%
    select(starts_with("num")) %>%
    filter((1:n()) == 1)
)
print("Counts in each treatment")
print(table(d2$housing.5))
print("Count of eviction")
print(table(d2$evicted.6, useNA = "ifany"))
print("Count of nonpayment")
print(table(d2$nonpayment.6, useNA = "ifany"))

# Note low proportion who we know live in a new MSA at age 9.
print("Proportion with mother known to have new MSA by age 9:")
print(prop.table(table(d2$city == 21)))
print("Weighted proportion with mother known to have new MSA by age 9:")
print(weighted.mean(d2$city == 21, w = d2$weight))

d3 <- data.frame(d2 %>%
                   select(-starts_with("num"),-starts_with("interviewed"),-own.5) %>%
                   mutate(education = as.numeric(education),
                          evicted.6 = as.numeric(evicted.6),
                          nonpayment.6 = as.numeric(nonpayment.6)))

get_all_estimates <- function(data) {
  filled <- amelia(data,
                   idvars = c("idnum","caregiver.6","psu"),
                   ords = c("education"),
                   noms = c("race",
                            "housing.2","housing.3","housing.4",
                            "housing.5","city"),
                   logs = "weight",
                   m = 50,
                   # Turn off print to screen
                   p2s = 0)
  
  # Do everything on each imputation
  imp_specific_estimates <- lapply(filled$imputation, function(imp) {
    # Add indicators for missingness (in some analyses these become the outcome)
    imp <- imp %>%
      mutate(convicted = convicted.3 | convicted.4 | convicted.5,
             evicted.history = evicted.2 | evicted.3 | evicted.4,
             nonpayment.history = nonpayment.2 | nonpayment.3 | nonpayment.4,
             income.history = (income.2 + income.3 + income.4) / 3,
             education = factor(education)) %>%
      select(-weight) %>%
      left_join(data %>%
                  mutate(missing_eviction = is.na(evicted.6),
                         missing_nonpayment = is.na(nonpayment.6)) %>%
                  select(idnum, missing_eviction, missing_nonpayment,weight),
                by = "idnum")
    
    # Get pre-treatment variable means by treatment
    individual_variables <- c("income.5","income.history",
                              "lessThanHS","HS","someCollege","college",
                              "black","hispanic","other","married",
                              "impulsivity","cognitive",
                              "convicted","drugs.5","alcohol.5","disability.5",
                              "nonpayment.5","nonpayment.history","evicted.5","evicted.history")
    variable_means <- imp %>%
      mutate(black = race == "Black",
             hispanic = race == "Hispanic",
             other = race == "White/other",
             lessThanHS = education == 1,
             HS = education == 2,
             someCollege = education == 3,
             college = education == 4) %>%
      select(housing.5,weight,one_of(individual_variables)) %>%
      melt(id = c("housing.5","weight")) %>%
      group_by(housing.5,variable) %>%
      summarize(value = weighted.mean(value, w = weight)) %>%
      unite("quantity",c("housing.5","variable")) %>%
      mutate(quantity = paste0("covMean_",quantity))
    variable_means_vector <- variable_means$value
    names(variable_means_vector) <- variable_means$quantity
    
    # Get raw outcomes overall and by treatment
    outcome_mean <- imp %>%
      group_by(housing.5) %>%
      summarize(evicted = weighted.mean(evicted.6, w = weight),
                nonpayment = weighted.mean(nonpayment.6, w = weight),
                missing_eviction = weighted.mean(missing_eviction, w = weight),
                missing_nonpayment = weighted.mean(missing_nonpayment, w = weight)) %>%
      bind_rows(imp %>%
                  mutate(housing.5 = "overall") %>%
                  group_by(housing.5) %>%
                  summarize(evicted = weighted.mean(evicted.6, w = weight),
                            nonpayment = weighted.mean(nonpayment.6, w = weight),
                            missing_eviction = weighted.mean(missing_eviction, w = weight),
                            missing_nonpayment = weighted.mean(missing_nonpayment, w = weight))) %>%
      group_by() %>%
      melt(id = "housing.5") %>%
      unite("quantity",c("housing.5","variable")) %>%
      mutate(quantity = paste0("outcomeMean_",quantity))
    outcome_mean_vector <- outcome_mean$value
    names(outcome_mean_vector) <- outcome_mean$quantity
    
    # Note imputed and missing values of outcome variables
    imputed_outcomes <- imp %>%
      group_by(housing.5, missing_eviction) %>%
      summarize(evicted = weighted.mean(evicted.6, w = weight)) %>%
      mutate(missing_eviction = ifelse(missing_eviction,"imputed_eviction","observed_eviction")) %>%
      spread(key = missing_eviction, value = evicted) %>%
      left_join(
        imp %>%
          group_by(housing.5, missing_nonpayment) %>%
          summarize(nonpayment = weighted.mean(nonpayment.6, w = weight)) %>%
          mutate(missing_nonpayment = ifelse(missing_nonpayment,"imputed_nonpayment","observed_nonpayment")) %>%
          spread(key = missing_nonpayment, value = nonpayment),
        by = "housing.5"
      ) %>%
      melt(id = "housing.5") %>%
      unite("quantity",c("housing.5","variable"))
    imputed_outcomes_vector <- imputed_outcomes$value
    names(imputed_outcomes_vector) <- imputed_outcomes$quantity
    
    # Fit the model on the untreated
    for (outcome_name in c("evicted.6","nonpayment.6","missing_eviction","missing_nonpayment")) {
      with_outcome <- imp
      with_outcome$outcome <- with_outcome[,outcome_name]
      
      # Get unadjusted gap
      unadjusted <- with_outcome %>%
        group_by(housing.5) %>%
        summarize(ybar = weighted.mean(outcome, w = weight)) %>%
        spread(key = housing.5, value = ybar) %>%
        transmute(assistance = assistance - no_help,
                  public = public - no_help) %>%
        melt(id = NULL)
      unadjusted_vector <- unadjusted$value
      names(unadjusted_vector) <- paste0("outcome_",outcome_name,"_unadjustedGap_",unadjusted$variable)
      assign(paste0(outcome_name,"_unadjusted_vector"),unadjusted_vector)
      
      fit_y0 <- lm(outcome ~ married + race + education + impulsivity + cognitive + 
                     convicted + drugs.5 + alcohol.5 + disability.5 + income.5 + 
                     income.history + nonpayment.5 + evicted.5 + evicted.history + 
                     nonpayment.history + city,
                   data = with_outcome %>% filter(housing.5 == "no_help"))
      
      # Get model coefficients
      coefficients_vector <- coef(fit_y0)[!grepl("city",names(coef(fit_y0)))]
      names(coefficients_vector) <- paste0(outcome_name,"_coef_",names(coefficients_vector))
      assign(paste0(outcome_name,"_coefficients_vector"),coefficients_vector)
      
      # Get effects
      effects <- with_outcome %>%
        mutate(yhat0 = predict(fit_y0, newdata = with_outcome)) %>%
        group_by(housing.5) %>%
        summarize(ybar = weighted.mean(outcome, w = weight),
                  ybar0 = weighted.mean(yhat0, w = weight)) %>%
        mutate(effect = ybar - ybar0,
               factor_higher = ybar0 / ybar,
               factor_reduction = - effect / ybar0) %>%
        melt(id = "housing.5", value.name = "estimate") %>%
        unite("quantity",c("housing.5","variable"))
      effects_vector <- effects$estimate
      names(effects_vector) <- paste0("outcome_",outcome_name,"_",effects$quantity)
      assign(paste0(outcome_name,"_effects_vector"),effects_vector)
    }
    
    # Return all estimates in one vector
    return(c(evicted.6_effects_vector, nonpayment.6_effects_vector,
             missing_eviction_effects_vector, missing_nonpayment_effects_vector,
             evicted.6_unadjusted_vector, nonpayment.6_unadjusted_vector,
             missing_eviction_unadjusted_vector, missing_nonpayment_unadjusted_vector,
             outcome_mean_vector, variable_means_vector, 
             imputed_outcomes_vector,
             evicted.6_coefficients_vector, nonpayment.6_coefficients_vector))
  })
  # Average over imputations to get the point estimate.
  # We do not produce the variance here because the variance will be calculated
  # over the jackknife-specific point estimates.
  combined <- colMeans(do.call(rbind,imp_specific_estimates))
  return(combined)
}

point <- get_all_estimates(d3)
save(point, file = "results/point.Rdata")

# Jackknife with the PSU as the city, ignoring stratification
mydesign <- svydesign(ids = ~ m1city,# + psu, # second level is ignored anyway
                      weights = ~ weight,
                      data = d3)
# Convert to Jackknife replicates
my_reps <- as.svrepdesign(design = mydesign,
                          type = "JK1",
                          compress = F)
# Point estimate on each jackknife replicate
point_star <- matrix(NA,
                     nrow = ncol(my_reps$repweights),
                     ncol = length(point))
colnames(point_star) <- names(point)
for(jackknife_rep in 1:ncol(my_reps$repweights)) {
  print(paste("Starting city",jackknife_rep))
  point_star[jackknife_rep,] <- get_all_estimates(data.frame(
    d3 %>%
      mutate(repweight = my_reps$repweights[,jackknife_rep]) %>%
      filter(repweight != 0) %>%
      mutate(weight = weight * repweight) %>%
      select(-repweight)
  ))
}
# Combine to a variance-covariance matrix
variance_hat <- svrVar(thetas = point_star, 
                       scale = my_reps$scale,
                       rscales = my_reps$rscales,
                       mse = T,
                       coef = point)
save(variance_hat,
     file = "results/variance_hat.Rdata")

all_point_ci <- data.frame(quantity = names(point),
                           point = point,
                           se = sqrt(diag(variance_hat))) %>%
  mutate(ci.min = point - qnorm(.975) * se,
         ci.max = point + qnorm(.975) * se)
write_csv(all_point_ci,
          path = "results/all_point_ci.csv")

#######################################################
# FIGURE 1. Effects of housing assistance on eviction #
#######################################################

# Panel A: Means
all_point_ci %>%
  filter(quantity %in% c(
    "outcomeMean_public_evicted",
    "outcomeMean_assistance_evicted",
    "outcomeMean_no_help_evicted"
  )) %>%
  mutate(quantity = case_when(quantity == "outcomeMean_public_evicted" ~ "Public housing",
                              quantity == "outcomeMean_assistance_evicted" ~ "Other assistance",
                              quantity == "outcomeMean_no_help_evicted" ~ "No assistance"),
         quantity = fct_relevel(quantity,"No assistance","Public housing", "Other assistance")) %>%
  ggplot(aes(x = quantity, y = point, 
             label = format(round(point,2),nsmall = 2),
             ymin = ci.min, ymax = ci.max)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(width = .5) +
  geom_label(size = 3) +
  ylab("Proportion evicted\n(weighted)") +
  scale_x_discrete(name = element_blank()) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0)) +
  ggsave("results/unadjusted_weighted_eviction.pdf",
         height = 2, width = 6.5)
# Panel B: Differences
all_point_ci %>%
  filter((grepl("effect",quantity) & grepl("outcome_evicted",quantity)) |
           (grepl("unadjusted",quantity) & grepl("outcome_evicted",quantity))) %>%
  filter(!grepl("no_help",quantity)) %>%
  mutate(treatment = case_when(grepl("assistance",quantity) ~ "Other assistance",
                               grepl("public",quantity) ~ "Public housing"),
         treatment = fct_relevel(treatment,"Public housing","Other assistance"),
         Estimator = case_when(grepl("unadjusted",quantity) ~ "Unadjusted",
                               grepl("effect",quantity) ~ "Adjusted"),
         Estimator = fct_relevel(Estimator,"Unadjusted","Adjusted")) %>%
  ggplot(aes(x = treatment, y = point, 
             ymin = ci.min, ymax = ci.max,
             label = format(round(point,2), nsmall = 2),
             color = Estimator)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .7), width = .5) +
  geom_label(position = position_dodge(width = .7),
             show.legend = F, size = 3, fontface = "bold") +
  ylab(paste0("Effect on P(Eviction)\ncompared with\nno assistance")) +
  scale_x_discrete(name = element_blank()) +
  scale_color_manual(values = c("blue","seagreen4")) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0)) +
  ggsave("results/difference_estimators_eviction.pdf",
         height = 2, width = 6.5)

#########################################################
# FIGURE 2. Effects of housing assistance on nonpayment #
#########################################################

# Panel A: Means
all_point_ci %>%
  filter(quantity %in% c(
    "outcomeMean_public_nonpayment",
    "outcomeMean_assistance_nonpayment",
    "outcomeMean_no_help_nonpayment"
  )) %>%
  mutate(quantity = case_when(quantity == "outcomeMean_public_nonpayment" ~ "Public housing",
                              quantity == "outcomeMean_assistance_nonpayment" ~ "Other assistance",
                              quantity == "outcomeMean_no_help_nonpayment" ~ "No assistance"),
         quantity = fct_relevel(quantity,"No assistance","Public housing", "Other assistance")) %>%
  ggplot(aes(x = quantity, y = point, 
             label = format(round(point,2),nsmall = 2),
             ymin = ci.min, ymax = ci.max)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(width = .5) +
  geom_label(size = 3) +
  ylab("Proportion with\nnonpayment (weighted)") +
  scale_x_discrete(name = element_blank()) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0)) +
  ggsave("results/unadjusted_weighted_nonpayment.pdf",
         height = 2, width = 6.5)
# Panel B: Differences
all_point_ci %>%
  filter((grepl("effect",quantity) & grepl("outcome_nonpayment",quantity)) |
           (grepl("unadjusted",quantity) & grepl("outcome_nonpayment",quantity))) %>%
  filter(!grepl("no_help",quantity)) %>%
  mutate(treatment = case_when(grepl("assistance",quantity) ~ "Other assistance",
                               grepl("public",quantity) ~ "Public housing"),
         treatment = fct_relevel(treatment,"Public housing","Other assistance"),
         Estimator = case_when(grepl("unadjusted",quantity) ~ "Unadjusted",
                               grepl("effect",quantity) ~ "Adjusted"),
         Estimator = fct_relevel(Estimator,"Unadjusted","Adjusted")) %>%
  ggplot(aes(x = treatment, y = point, 
             ymin = ci.min, ymax = ci.max,
             label = format(round(point,2), nsmall = 2),
             color = Estimator)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .7), width = .5) +
  geom_label(position = position_dodge(width = .7),
             show.legend = F, size = 3, fontface = "bold") +
  ylab(paste0("Effect on P(Nonpayment)\ncompared with\nno assistance")) +
  scale_x_discrete(name = element_blank()) +
  scale_color_manual(values = c("blue","seagreen4")) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0)) +
  ggsave("results/difference_estimators_nonpayment.pdf",
         height = 2, width = 6.5)

###############################################
# TABLE 1. Covariate means by treatment group #
###############################################

mu_hat <- all_point_ci %>%
  filter(grepl("covMean",quantity)) %>%
  select(quantity, point)
predictor_names <- (
  mu_hat %>% 
    # Restrict to one set of names (from a single treatment level)
    filter(grepl("covMean_assistance",quantity)) %>%
    mutate(predictor_name = gsub("covMean_assistance_","",quantity))
)$predictor_name
cov_means_table <- foreach(predictor = predictor_names, .combine = "rbind") %do% {
  these_means <- mu_hat %>%
    filter(grepl(paste0("_",predictor),quantity))
  these_variances <- variance_hat[these_means$quantity,these_means$quantity]
  # Make a 2 x 3 matrix that will return differences of interest
  A <- rbind(public = grepl("public",these_means$quantity) - grepl("no_help",these_means$quantity),
             assistance = grepl("assistance",these_means$quantity) - grepl("no_help",these_means$quantity))
  # Calculate differences, their variance, and significance of each
  differences <- A %*% these_means$point
  differences_var <- A %*% these_variances %*% t(A)
  pvals <- 2*pnorm(abs(differences) / sqrt(diag(differences_var)), lower.tail = F)
  
  return(
    these_means %>%
      mutate(predictor = predictor,
             quantity = case_when(grepl("assistance",quantity) ~ "assistance",
                                  grepl("public",quantity) ~ "public",
                                  grepl("no_help",quantity) ~ "no_help"),
             pval = case_when(quantity == "no_help" ~ 1,
                              quantity == "public" ~ pvals["public",],
                              quantity == "assistance" ~ pvals["assistance",]),
             point = paste0(format(round(point,2),nsmall = 2),
                            ifelse(pval < .001,"***",
                                   ifelse(pval < .01,"**",
                                          ifelse(pval < .05,"*",""))))) %>%
      select(predictor,quantity,point) %>%
      spread(key = quantity, value = point) %>%
      select(predictor, no_help, public, assistance)
  )
}
print(xtable(cov_means_table), include.rownames = F)


#################################
# FIGURE A1. Effects on missing #
#################################

all_point_ci %>%
  filter((grepl("effect",quantity) & grepl("outcome_missing",quantity)) |
           (grepl("unadjusted",quantity) & grepl("outcome_missing",quantity))) %>%
  filter(!grepl("no_help",quantity)) %>%
  mutate(treatment = case_when(grepl("assistance",quantity) ~ "Other assistance",
                               grepl("public",quantity) ~ "Public housing"),
         treatment = fct_relevel(treatment,"Public housing","Other assistance"),
         Estimator = case_when(grepl("unadjusted",quantity) ~ "Unadjusted",
                               grepl("effect",quantity) ~ "Adjusted"),
         Estimator = fct_relevel(Estimator,"Unadjusted","Adjusted"),
         outcome = case_when(grepl("eviction",quantity) ~ "Missing eviction",
                             grepl("nonpayment",quantity) ~ "Missing nonpayment")) %>%
  ggplot(aes(x = treatment, y = point, 
             ymin = ci.min, ymax = ci.max,
             label = format(round(point,2), nsmall = 2),
             color = Estimator)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .7), width = .5) +
  geom_label(position = position_dodge(width = .7),
             show.legend = F, size = 3, fontface = "bold") +
  ylab(paste0("Effect on P(Missing)\ncompared with no assistance")) +
  scale_x_discrete(name = element_blank()) +
  scale_color_manual(values = c("blue","seagreen4")) +
  theme_bw() +
  facet_wrap(~outcome) +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12)) +
  ggsave("results/effect_on_missing.pdf",
         height = 3, width = 8)



###########################################
# Table A1. Imputed and observed eviction #
###########################################

print(xtable(
  all_point_ci %>%
    filter(grepl("imputed",quantity) | grepl("observed",quantity)) %>%
    mutate(quantity = gsub("no_help","nohelp",quantity)) %>%
    separate(quantity, into = c("treatment","missing","outcome")) %>%
    mutate(treatment = case_when(treatment == "public" ~ "Public housing",
                                 treatment == "assistance" ~ "Other assistance",
                                 treatment == "nohelp" ~ "No assistance"),
           treatment = fct_relevel(treatment,"Public housing","Other assistance","No assistance"),
           missing = case_when(missing == "imputed" ~ "Imputed cases",
                               missing == "observed" ~ "Observed cases"),
           outcome = case_when(outcome == "eviction" ~ "Eviction",
                               outcome == "nonpayment" ~ "Nonpayment")) %>%
    select(-se,-ci.min,-ci.max) %>%
    spread(key = missing, value = point) %>%
    arrange(outcome,treatment) %>%
    select(outcome,treatment,`Imputed cases`,`Observed cases`)
), include.rownames = F)

##############################
# TABLE A2. OLS coefficients #
##############################

print(xtable(
  all_point_ci %>%
    filter(grepl("coef",quantity)) %>%
    mutate(pval = 2*pnorm(abs(point) / se, lower.tail = F),
           point = paste0(format(round(point,2), nsmall = 2),
                          ifelse(pval < .001, "***",
                                 ifelse(pval < .01, "**",
                                        ifelse(pval < .05, "*", "")))),
           se = paste0("(",format(round(se,2)),")"),
           quantity = gsub("_coef_","_",quantity)) %>%
    select(quantity,point,se) %>%
    melt(id = "quantity") %>%
    separate(quantity, into = c("outcome","predictor"), sep = "_") %>%
    spread(key = outcome, value = value) %>%
    mutate(predictor = factor(
      case_when(
        predictor == "evicted.5" ~ 1,
        predictor == "evicted.historyTRUE" ~ 2,
        predictor == "nonpayment.5" ~ 3,
        predictor == "nonpayment.historyTRUE" ~ 4,
        predictor == "income.5" ~ 5,
        predictor == "income.history" ~ 6,
        predictor == "disability.5" ~ 7,
        predictor == "convictedTRUE" ~ 8,
        predictor == "drugs.5" ~ 9,
        predictor == "alcohol.5" ~ 10,
        predictor == "education2" ~ 11,
        predictor == "education3" ~ 12,
        predictor == "education4" ~ 13,
        predictor == "married" ~ 14,
        predictor == "raceHispanic" ~ 15,
        predictor == "raceWhite/other" ~ 16,
        predictor == "cognitive" ~ 17,
        predictor == "impulsivity" ~ 18,
        predictor == "(Intercept)" ~ 19
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
        "Drug use",
        "Heavy alcohol use",
        "Education: High school",
        "Education: Some college",
        "Education: College",
        "Parents married at birth",
        "Race: Hispanic",
        "Race: White/other (black omitted)",
        "WAIS-R cognitive score",
        "Impulsivity (Dickman 1990)",
        "Intercept"))) %>%
    arrange(predictor,variable) %>%
    mutate(predictor = ifelse(variable == "se","",as.character(predictor))) %>%
    select(-variable),
  caption = paste0("OLS coefficients"),
  label = "tbl:coefs"
), include.rownames = F)


############################################
# TABLE A3. Housing assistance at 9 vs. 15 #
############################################

## Supplemental: Note about how the treatment at 9 does not necessarily remain constant through age 15

# Note that a substantial percent now own their home
print("A substantial proportion own their home by age 15")
print("Note: NA includes those with non-mother caregiver at 15")
print(prop.table(table(
  (d2 %>%
     select(idnum) %>%
     left_join(d0 %>% 
                 mutate(owns_home = case_when(cp6pcgrel == 1 & p6j6 > 0 ~ p6j6 == 4)) %>%
                 select(idnum, owns_home), 
               by = "idnum"))$owns_home, 
  useNA = "ifany"
)))

print("Proportion in each assistance category at age 15 by age 9 category:")
for_table <- d2 %>%
  left_join(d0 %>%
              transmute(idnum = idnum,
                        housing.6 = case_when(p6j14 == 1 & cp6pcgrel == 1  ~ "public",
                                              p6j15 == 1 & cp6pcgrel == 1  ~ "assistance",
                                              p6j14 %in% c(-6,2) & p6j15 %in% c(-6,2) & cp6pcgrel == 1 ~ "no_help")),
            by = "idnum")
print(round(prop.table(table(age15 = for_table$housing.6, age9 = for_table$housing.5, useNA = "ifany"),
                       margin = 2),2)[c(3,1,2,4),c(3,1,2)])


#################################################################
# FIGURE A2. Uncertainty check: Compare to model-based approach #
#################################################################

set.seed(08544)
filled <- amelia(d3,
                 idvars = c("idnum","caregiver.6","psu"),
                 ords = c("education"),
                 noms = c("race",
                          "housing.2","housing.3","housing.4",
                          "housing.5","city"),
                 logs = "weight",
                 m = 50,
                 # Turn off print to screen
                 p2s = 0)

# Do everything on each imputation
imp_specific_estimates <- lapply(filled$imputation, function(imp) {
  # Add indicators for missingness (in some analyses these become the outcome)
  imp <- imp %>%
    mutate(convicted = convicted.3 | convicted.4 | convicted.5,
           evicted.history = evicted.2 | evicted.3 | evicted.4,
           nonpayment.history = nonpayment.2 | nonpayment.3 | nonpayment.4,
           income.history = (income.2 + income.3 + income.4) / 3,
           education = factor(education)) %>%
    select(-weight) %>%
    left_join(d3 %>%
                mutate(missing_eviction = is.na(evicted.6),
                       missing_nonpayment = is.na(nonpayment.6)) %>%
                select(idnum, missing_eviction, missing_nonpayment,weight),
              by = "idnum")
  
  # Get causal effects
  results <- foreach(outcome_name = c("evicted.6","nonpayment.6")) %do% {
    with_outcome <- imp
    with_outcome$outcome <- with_outcome[,outcome_name]
    
    fit_y0 <- lm(outcome ~ married + race + education + impulsivity + cognitive + 
                   convicted + drugs.5 + alcohol.5 + disability.5 + income.5 + 
                   income.history + nonpayment.5 + evicted.5 + evicted.history + 
                   nonpayment.history + city,
                 data = with_outcome %>% filter(housing.5 == "no_help"))
    
    X <- model.matrix(formula(fit_y0), data = with_outcome)
    xbar <- rbind(public = apply(X[with_outcome$housing.5 == "public",], 2, weighted.mean, 
                                 w = with_outcome$weight[with_outcome$housing.5 == "public"]),
                  assistance = apply(X[with_outcome$housing.5 == "assistance",], 2, weighted.mean, 
                                     w = with_outcome$weight[with_outcome$housing.5 == "assistance"]))
    ybar0 <- xbar %*% coef(fit_y0)
    ybar0_vcov <- xbar %*% vcov(fit_y0) %*% t(xbar)
    
    observed_public <- lm(outcome ~ 1,
                          data = with_outcome %>% filter(housing.5 == "public"),
                          w = weight)
    observed_assistance <- lm(outcome ~ 1,
                              data = with_outcome %>% filter(housing.5 == "assistance"),
                              w = weight)
    ybar <- c(public = coef(observed_public),
              assistance = coef(observed_assistance))
    ybar_vcov <- diag(c(vcov(observed_public),vcov(observed_assistance)))
    return(list(result = ybar - ybar0,
                variance = ybar_vcov + ybar0_vcov))
  }
  names(results) <- c("eviction","nonpayment")
  return(results)
})

model_based_eviction <- MIcombine(results = lapply(imp_specific_estimates, function(estimate) as.vector(estimate$eviction$result)),
                                  variances = lapply(imp_specific_estimates, function(estimate) estimate$eviction$variance))
model_based_nonpayment <- MIcombine(results = lapply(imp_specific_estimates, function(estimate) as.vector(estimate$nonpayment$result)),
                                  variances = lapply(imp_specific_estimates, function(estimate) estimate$nonpayment$variance))

data.frame(outcome = "evicted.6",
           treatment = colnames(model_based_eviction$variance),
           effect = model_based_eviction$coefficients,
           se = sqrt(diag(model_based_eviction$variance))) %>%
  bind_rows(data.frame(outcome = "nonpayment.6",
                       treatment = colnames(model_based_nonpayment$variance),
                       effect = model_based_nonpayment$coefficients,
                       se = sqrt(diag(model_based_nonpayment$variance)))) %>%
  mutate(ci.min = effect - qnorm(.975) * se,
         ci.max = effect + qnorm(.975) * se) %>%
  select(-se) %>%
  mutate(approach = "Model-based\n(alternative)") %>%
  bind_rows(all_point_ci %>%
              filter(quantity %in% c("outcome_evicted.6_assistance_effect",
                                     "outcome_evicted.6_public_effect",
                                     "outcome_nonpayment.6_assistance_effect",
                                     "outcome_nonpayment.6_public_effect")) %>%
              select(quantity, point, ci.min, ci.max) %>%
              separate(quantity, sep = "_", into = c("filler1","outcome","treatment","filler2")) %>%
              select(-filler1,-filler2) %>%
              rename(effect = point) %>%
              mutate(approach = "Jackknife\n(main text)")) %>%
  mutate(outcome = case_when(outcome == "evicted.6" ~ "Effect on eviction",
                             outcome == "nonpayment.6" ~ "Effect on nonpayment"),
         treatment = case_when(treatment == "public" ~ "Public housing",
                               treatment == "assistance" ~ "Other assistance"),
         treatment = fct_relevel(treatment, "Public housing", "Other assistance")) %>%
  ggplot(aes(x = treatment, y = effect, ymin = ci.min, ymax = ci.max,
             color = approach, label = format(round(effect,2),nsmall = 2))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .7), width = .5) +
  geom_label(position = position_dodge(width = .7),
             show.legend = F, size = 3, fontface = "bold") +
  ylab(paste0("Effect on outcome\ncompared with\nno assistance")) +
  scale_x_discrete(name = element_blank()) +
  scale_color_manual(values = c("blue","seagreen4"),
                     name = "Variance\nestimator") +
  theme_bw() +
  facet_wrap(~outcome, ncol = 1) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        legend.key.height = unit(1,"cm")) +
  ggsave("results/variance_estimator_comparison.pdf",
         height = 6, width = 6.5)

sink()


