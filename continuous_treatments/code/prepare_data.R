
# Prepare environment
library(tidyverse)
library(foreach)

# Load a helper function
source("code/weighted.quantile.R")

#############
# LOAD DATA #
#############

# Consumer Price Index
# We use this to adjust family incomes for inflation
cpi <- read_csv("data/CPIAUCSL.csv") %>%
  mutate(year = lubridate::year(DATE)) %>%
  group_by(year) %>%
  summarize(cpi = mean(CPIAUCSL), .groups = "drop")

# NLSY97 data
setwd("data/nlsy97")
source("nlsy97.R")
raw <- new_data %>%
  rename(PUBID = PUBID_1997) %>%
  rename_all(.funs = function(x) gsub("_XRND","",x))
rm(varlabels,qnames,vallabels,vallabels_continuous,new_data)
setwd("..")
setwd("..")

#####################
# PREPARE VARIABLES #
#####################

# Sampling weight for full sample
weight <- raw %>%
  select(PUBID, SAMPLING_WEIGHT_CC_1997) %>%
  rename(weight = SAMPLING_WEIGHT_CC_1997)

# Date of birth in continuous months
dob_m <- raw %>%
  rename(age1997 = `CV_AGE(MONTHS)_INT_DATE_1997`,
         month1997 = CV_INTERVIEW_CMONTH_1997) %>%
  mutate(dob_m = month1997 - age1997) %>%
  select(PUBID, dob_m)

# Month of each interview
int_month <- raw %>%
  select(PUBID, starts_with("CV_INTERVIEW_CMONTH")) %>%
  pivot_longer(starts_with("CV_INTERVIEW_CMONTH"),
               values_to = "int_month", names_to = "year") %>%
  mutate(year = as.numeric(gsub("CV_INTERVIEW_CMONTH_","",year))) %>%
  mutate(wave = case_when(year <= 2011 ~ year - 1996,
                          year == 2013 ~ 16,
                          year == 2015 ~ 17,
                          year == 2017 ~ 18,
                          year == 2019 ~ 19)) %>%
  left_join(dob_m, by = "PUBID") %>%
  mutate(age = (int_month - dob_m) / 12) %>%
  select(PUBID, wave, year, int_month, age)

# ENROLLMENT: ENROLLED IN ANY OR 4-YEAR AT ANY POINT UP TO 21
enrollment <- raw %>%
  select(PUBID, starts_with("SCH_COLLEGE_STATUS")) %>%
  pivot_longer(cols = starts_with("SCH")) %>%
  mutate(name = str_replace_all(name,".*_",""),
         year = as.numeric(str_replace_all(name,"[.].*","")),
         month = as.numeric(str_replace_all(name,".*[.]",""))) %>%
  mutate(category = case_when(value == 1 ~ "not_enrolled",
                              value == 2 ~ "enrolled_2year",
                              value == 3 ~ "enrolled_4year",
                              value == 4 ~ "enrolled_graduate")) %>%
  mutate(value = 12 * (year - 1980) + month) %>%
  left_join(dob_m, by = "PUBID") %>%
  mutate(age = (value - dob_m) / 12) %>%
  group_by(PUBID) %>%
  summarize(
    enrolled_any = case_when(
      any(age <= 21 & category %in% c("enrolled_2year","enrolled_4year")) ~ T,
      # False if not True and has at least one valid report at ages 19-21
      any(age >= 19 & age <= 21 & !is.na(category)) ~ F
    ),
    enrolled_4yr = case_when(
      any(age <= 21 & category == "enrolled_4year") ~ T,
      # False if not True and has at least one valid report at ages 19-21
      any(age >= 19 & age <= 21 & !is.na(category)) ~ F
    ),
    .groups = "drop"
  )

# COMPLETION: 4-YEAR DEGREE BY AGE 25 AND BY AGE 30
last_interview_age <- raw %>%
  select(PUBID, CVC_RND) %>%
  rename(wave = CVC_RND) %>%
  left_join(int_month, by = c("PUBID","wave")) %>%
  rename(last_interview_age = age) %>%
  select(PUBID, last_interview_age)
completion <- raw %>%
  select(PUBID, CVC_BA_DEGREE) %>%
  left_join(dob_m, by = "PUBID") %>%
  # Denote completion age
  # those whose values are -3 (invalid missing) do not meet either
  # requirement below and get coded as NA
  mutate(completion_age = case_when(CVC_BA_DEGREE == -4 ~ Inf,
                                    CVC_BA_DEGREE > 0 ~ ((CVC_BA_DEGREE - dob_m) / 12))) %>%
  select(PUBID, completion_age) %>%
  left_join(last_interview_age, by = "PUBID") %>%
  mutate(completed_25 = case_when(completion_age < 25 ~ T,
                                     completion_age >= 25 & last_interview_age >= 25 ~ F),
         completed_30 = case_when(completion_age < 30 ~ T,
                                     completion_age >= 30 & last_interview_age >= 30 ~ F)) %>%
  select(PUBID, completed_25, completed_30)

# Treatment: Family income measured in 1997, in 2022 dollars
income <- raw %>%
  select(PUBID, CV_INCOME_GROSS_YR_1997, `CV_AGE(MONTHS)_INT_DATE_1997`) %>%
  rename(income = CV_INCOME_GROSS_YR_1997,
         age_income = `CV_AGE(MONTHS)_INT_DATE_1997`) %>%
  mutate(age_income = age_income / 12) %>%
  # mark missing values
  mutate(income = case_when(!(income %in% c(-1:-5)) ~ income)) %>%
  # adjust for inflation
  mutate(income = income * cpi$cpi[cpi$year == 2022] / cpi$cpi[cpi$year == 1997],
         # Bottom-code income
         income = case_when(income >= 10e3 ~ income,
                            income < 10e3 ~ 10e3))

# Covariates
covariates <- raw %>%
  transmute(PUBID = PUBID,
            female = KEY_SEX_1997 == 2,
            race = case_when(KEY_RACE_ETHNICITY_1997 == 1 ~ "Non-Hispanic Black",
                             KEY_RACE_ETHNICITY_1997 == 2 ~ "Hispanic",
                             KEY_RACE_ETHNICITY_1997 %in% 3:4 ~ "Non-Black Non-Hispanic"),
            race = as_factor(race),
            educMom = case_when(CV_HGC_RES_MOM_1997 == -4 ~ "No residential mom",
                                (CV_HGC_RES_MOM_1997 >= 0 & CV_HGC_RES_MOM_1997 < 12) | CV_HGC_RES_MOM_1997 == 95 ~ "Less than high school",
                                CV_HGC_RES_MOM_1997 == 12 ~ "High school",
                                CV_HGC_RES_MOM_1997 < 16 ~ "Some college",
                                CV_HGC_RES_MOM_1997 >= 16 ~ "College"),
            educDad = case_when(CV_HGC_RES_DAD_1997 == -4 ~ "No residential Dad",
                                (CV_HGC_RES_DAD_1997 >= 0 & CV_HGC_RES_DAD_1997 < 12) | CV_HGC_RES_DAD_1997 == 95 ~ "Less than high school",
                                CV_HGC_RES_DAD_1997 == 12 ~ "High school",
                                CV_HGC_RES_DAD_1997 < 16 ~ "Some college",
                                CV_HGC_RES_DAD_1997 >= 16 ~ "College"),
            # Create a coarsened joint version of mom's and dad's education which captures
            # the variation we think is conceptually important
            educJoint = factor(case_when(educMom == "College" & educDad == "College" ~ 1,
                                         educMom == "College" | educDad == "College" ~ 2,
                                         !is.na(educMom) & !is.na(educDad) ~ 3),
                                  labels = c("Two parents\nfinished college",
                                             "One parent\nfinished college",
                                             "No parent\nfinished college")),
            # Wealth. From documentation:
            # Respondents with household net worth values above $600,000 were topcoded to a value of $600,000.
            wealth = case_when(!(CV_HH_NET_WORTH_P_1997 %in% -1:-5) ~ as.numeric(CV_HH_NET_WORTH_P_1997)),
            # Inflation adjustment for wealth
            wealth = wealth * cpi$cpi[cpi$year == 2022] / cpi$cpi[cpi$year == 1997],
            # Bottom-code wealth
            wealth = case_when(wealth < 1e3 ~ 1e3,
                               wealth >= 1e3 ~ wealth)) %>%
  left_join(weight, by = "PUBID")
  
# Merge the data together
merged <- raw %>%
  select(PUBID) %>%
  # Treatment
  left_join(income, by = "PUBID") %>%
  # Outcomes
  left_join(enrollment, by = "PUBID") %>%
  left_join(completion, by = "PUBID") %>%
  # Covariates
  left_join(covariates, by = "PUBID")
saveRDS(merged, file = "intermediate/merged.RDS")

# Define cutoffs for income and wealth terciles
cutoffs <- merged %>%
  select(income, wealth, weight) %>%
  pivot_longer(cols = c("income","wealth"),
               names_to = "variable") %>%
  group_by(variable) %>%
  summarize(low = weighted.quantile(value, 1/3, w = weight),
            high = weighted.quantile(value, 2/3, w = weight))
saveRDS(cutoffs, file = "intermediate/cutoffs.RDS")


# Create data for each outcome

print_n <- function(data, comment = NULL) {
  print(paste(nrow(data),comment))
  return(data)
}

for (outcome in c("enrolled_any","enrolled_4yr","completed_25","completed_30")) {
  print(outcome)
  data_this_outcome <- merged %>%
    print_n("Total") %>%
    filter(!is.na(income)) %>%
    print_n("Valid treatment") %>%
    filter(income != max(income)) %>%
    print_n("Treatment not top-coded") %>%
    rename_with(.fn = function(x) str_replace(x,outcome,"y")) %>%
    filter(!is.na(y)) %>%
    print_n("Valid outcome") %>%
    select(PUBID, y, income, female, race, educJoint, wealth)
  
  write_lines(data_this_outcome$PUBID,
              file = paste0("intermediate/","ids_",outcome,".txt"))
  
  saveRDS(data_this_outcome,
          file = paste0("intermediate/","data_",outcome,".RDS"))
}

# After running this file, manually upload each ids_[outcome].txt here to make weights:
# https://www.nlsinfo.org/weights/nlsy97
# Place each weights_[outcome].dat in the intermediate folder

