
sink("logs/1_prepare.txt")

cat("This file prepares data.")

t0 <- Sys.time()
print(t0)
set.seed(90095)

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
source("data/nlsy97_220611a/nlsy97_220611a.R")
raw <- new_data %>%
  rename(PUBID = PUBID_1997) %>%
  rename_all(.funs = function(x) gsub("_XRND","",x))
rm(varlabels,qnames,vallabels,vallabels_continuous,new_data)

#####################
# PREPARE VARIABLES #
#####################

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
  
# Age at last interview round
age_last <- raw %>%
  rename(wave = CVC_RND) %>%
  select(PUBID, wave) %>%
  left_join(int_month, by = c("PUBID","wave")) %>%
  rename(age_last = age) %>%
  select(PUBID, age_last)

# BA completion by age 25
# Use created variable for age at BA degree,
# among those interviewed at an age 25 or older
completed <- raw %>%
  select(PUBID, CVC_BA_DEGREE) %>%
  left_join(age_last, by = "PUBID") %>%
  left_join(dob_m, by = "PUBID") %>%
  mutate(completed = case_when(age_last >= 25 & CVC_BA_DEGREE == -4 ~ F,
                              age_last >= 25 & CVC_BA_DEGREE > 0 & ((CVC_BA_DEGREE - dob_m) / 12) <= 25 ~ T,
                              age_last >= 25 & CVC_BA_DEGREE > 0 & ((CVC_BA_DEGREE - dob_m) / 12) > 25 ~ F)) %>%
  select(PUBID, completed)
         
# Enrollment by age 20
# Use created variable for enrollment status at each age.
# Eligible units are those with non-missing enrollment at some point
# before age 20, who are observed at age 19 or greater
enrolled <- raw %>%
  select(PUBID,starts_with("CV_ENROLLSTAT")) %>%
  pivot_longer(cols = starts_with("CV_ENROLLSTAT")) %>%
  mutate(year = as.numeric(gsub("CV_ENROLLSTAT_|CV_ENROLLSTAT_EDT_","",name))) %>%
  select(-name) %>%
  mutate(enrolled_college = case_when(value %in% 9:11 ~ T,
                                      value %in% 1:8 ~ F)) %>%
  select(PUBID, year, enrolled_college) %>%
  left_join(int_month %>% select(PUBID, year, age), by = c("PUBID","year")) %>%
  # Enforce eligibility: Observed between ages 19 and 20
  filter(age >= 19 & age <= 20 & !is.na(enrolled_college)) %>%
  group_by(PUBID) %>%
  summarize(enrolled = any(enrolled_college),
            max_age_enrollmentReport = max(age))

# Treatment: Family income measured in 1997, in 2022 dollars
income <- raw %>%
  select(PUBID, CV_INCOME_GROSS_YR_1997, `CV_AGE(MONTHS)_INT_DATE_1997`) %>%
  rename(income = CV_INCOME_GROSS_YR_1997,
         age_income = `CV_AGE(MONTHS)_INT_DATE_1997`) %>%
  mutate(age_income = age_income / 12) %>%
  filter(income >= 0) %>%
  mutate(income = income * cpi$cpi[cpi$year == 2022] / cpi$cpi[cpi$year == 1997])

# Covariates
covariates <- raw %>%
  transmute(PUBID = PUBID,
            female = KEY_SEX_1997 == 2,
            race = case_when(KEY_RACE_ETHNICITY_1997 == 1 ~ "Black",
                             KEY_RACE_ETHNICITY_1997 == 2 ~ "Hispanic",
                             KEY_RACE_ETHNICITY_1997 %in% 3:4 ~ "White / Other"),
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
                                  labels = c("Both parents\nfinished college",
                                             "One parent\nfinished college",
                                             "Neither parent\nfinished college")),
            # Wealth. From documtation:
            # Respondents with household net worth values above $600,000 were topcoded to a value of $600,000.
            wealth = case_when(!(CV_HH_NET_WORTH_P_1997 %in% -1:-5) ~ as.numeric(CV_HH_NET_WORTH_P_1997)),
            # Inflation adjustment for wealth
            wealth = wealth * cpi$cpi[cpi$year == 2022] / cpi$cpi[cpi$year == 1997])

# Merge the data together
merged <- raw %>%
  select(PUBID) %>%
  left_join(age_last, by = "PUBID") %>%
  # Treatment
  left_join(income, by = "PUBID") %>%
  # Outcomes
  left_join(enrolled, by = "PUBID") %>%
  left_join(completed, by = "PUBID") %>%
  # Covariates
  left_join(covariates, by = "PUBID")

# Make sample restrictions
n_print <- function(.data, note) {
  print(paste0(note,": ",nrow(.data)))
  return(.data)
}
forWeights <- merged %>%
  n_print(note = "Raw sample size") %>%
  # Keep those observed at age 25+
  filter(age_last >= 25) %>%
  n_print(note = "Observed at age 25+") %>%
  filter(!is.na(income)) %>%
  n_print(note = "With valid income") %>%
  # Drop the top-code cases (top 2%) because there income value
  # is the average of those income values which likely actually cover a wide range
  filter(income < max(income)) %>%
  n_print(note = "With income not top-coded") %>%
  filter(!is.na(wealth)) %>%
  n_print(note = "Non-missing wealth") %>%
  filter(!is.na(enrolled)) %>%
  n_print(note = "Valid enrollment outcome") %>%
  filter(!is.na(completed)) %>%
  n_print(note = "Valid completion outcome")

# Print id numbers to weight
sink("intermediate/ids_to_weight_97.txt")
cat(paste(forWeights$PUBID, collapse = "\n"))
sink()

# Merge in the weights for these ids from https://www.nlsinfo.org/weights/nlsy97
weighted <- forWeights %>%
  left_join(read_delim("intermediate/customweights.dat",
                       col_names = c("PUBID","w"),
                       delim = " ",
                       show_col_types = F),
            by = "PUBID") %>%
  # Normalize weights to sum to number of observations
  mutate(w = w / mean(w))

# Determine wealth tercile cutoffs
wealthTerciles <- sapply(
  c(1/3,2/3), 
  weighted.quantile,
  x = weighted$wealth,
  w = weighted$w
)
wealthTerciles_labels <- paste0("$",prettyNum(round(wealthTerciles),big.mark = ","))

# Create prepared data
prepared <- weighted %>%
  select(-starts_with("num")) %>%
  # Append subgroup information for future facetting
  mutate(wealthTercile = factor(case_when(wealth < wealthTerciles[1] ~ 1,
                                          wealth < wealthTerciles[2] ~ 2,
                                          wealth >= wealthTerciles[2] ~ 3),
                                labels = c(paste0("Low: Less than ",wealthTerciles_labels[1]),
                                           paste0("Middle: ",wealthTerciles_labels[1],"-",wealthTerciles_labels[2]),
                                           paste0("High: More than ",wealthTerciles_labels[2]))),
         wealthTercile = fct_rev(wealthTercile)) %>%
  # Restrict to well-populated subgroups
  group_by(educJoint, wealthTercile) %>%
  mutate(num_in_subgroup = n()) %>%
  ungroup() %>%
  ( function(.data) {
    print("Note: Subgroups that are too small")
    print(.data %>% 
            filter(num_in_subgroup < 50) %>% 
            group_by(educJoint, wealthTercile) %>%
            slice_head(n = 1) %>%
            select(educJoint, wealthTercile, num_in_subgroup))
    return(.data)
  } ) %>%
  #filter(num_in_subgroup >= 50) %>%
  ( function(.data) {
    print(paste("Num remaining after drop:",nrow(.data)))
    return(.data)
  }) %>%
  mutate(subgroup = interaction(educJoint, wealthTercile),
         subgroup = fct_drop(subgroup),
         subgroup = as.numeric(subgroup)) %>%
  # Truncate net worth at $1,000 on the bottom
  ( function(x){print(paste("Note: Bottom-coding wealth for",
                            sum(x$wealth < 1e3))); return(x)} ) %>%
  ( function(x){print(paste("Note: Wealth already top-coded at 600,000 in 1997 dollars for",
                            sum(x$wealth == max(x$wealth)))); return(x)} ) %>%
  mutate(wealth = case_when(wealth < 1e3 ~ 1e3,
                            wealth >= 1e3 ~ wealth)) %>%
  # Bottom-code incomes at $1000
  # NOTE: USED TO BE 10k
  ( function(x){print(paste("Note: Bottom-coding income for",
                            sum(x$income < 10e3))); return(x)} ) %>%
  mutate(income = case_when(income >= 10e3 ~ income,
                            income < 10e3 ~ 10e3))

saveRDS(prepared, file = "intermediate/prepared.RDS")

sessionInfo()

sink()
         