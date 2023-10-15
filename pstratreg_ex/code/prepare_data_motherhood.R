sink("../logs/prepare_motherhood.txt")

# Load packages
library(tidyverse)
theme_set(theme_bw())
library(foreach)
library(doParallel)
library(doRNG)
cl <- makeCluster(6)
registerDoParallel(cl)

# Load NLSY97 data
setwd("../data/motherhood97")
source("motherhood97.R")
setwd("..")
setwd("../code")
new_data <- new_data %>%
  rename(PUBID = PUBID_1997)

# CPI for inflation adjustment
cpi <- read_csv('../data/CPIAUCSL.csv') %>%
  mutate(year = as.numeric(str_replace(DATE,"-.*",""))) %>%
  group_by(year) %>%
  summarize(cpi = mean(CPIAUCSL), .groups = "drop") %>%
  mutate(year = as.character(year))
cpi$cpi <- cpi$cpi[cpi$year == 2022] / cpi$cpi

# Helper function: the NLSY records dates in continuous months since January 1980. Convert to year values.
continuous_month_to_year <- function(month) {
  # Month 1 is jan 1980. This will be coded 1980.0
  1980 + (month - 1) / 12
}
month_year_to_continuous <- function(month, year) {
  12 * (year - 1980) + (month - 1)
}

# Create data frame of demographic pre-treatment variables
demographics <- new_data %>% 
  mutate(sex = factor(KEY_SEX_1997, labels = c("Men","Women")),
         race = case_when(KEY_RACE_ETHNICITY_1997 == 1 ~ "Non-Hispanic Black",
                          KEY_RACE_ETHNICITY_1997 == 2 ~ "Hispanic",
                          T ~ "Non-Hispanic Non-Black"),
         birth_year = KEY_BDATE_Y_1997) %>%
  select(PUBID, sex, race, birth_year)

# Determine interview dates
interview_month <- new_data %>%
  select(PUBID, starts_with("CV_INTERVIEW_CMONTH")) %>%
  pivot_longer(cols = starts_with("CV"),
               names_to = "year",
               values_to = "interview_month") %>%
  mutate(year = str_replace_all(year,"CV_INTERVIEW_CMONTH_",""))

# Determine respondent date of birth
r_birth_month <- new_data %>%
  mutate(r_birth_month = month_year_to_continuous(
    month = KEY_BDATE_M_1997,
    year = KEY_BDATE_Y_1997
  )) %>%
  select(PUBID, r_birth_month)

# Determine interview ages
age_interview <- interview_month %>%
  left_join(r_birth_month, by = "PUBID") %>%
  mutate(age_interview = case_when(interview_month > 0 ~ (interview_month - r_birth_month) / 12))

# Define when the first birth
first_birth <- new_data %>%
  # Select variables from each wave showing the first child's birth month
  select(PUBID, starts_with("CV_CHILD_BIRTH_MONTH.01")) %>%
  # Merge in respondent birth date
  left_join(r_birth_month, by = "PUBID") %>%
  # Determine first child birth date
  pivot_longer(cols = starts_with("CV"),
               values_to = "c_birth_month") %>%
  group_by(PUBID) %>%
  # If no birth, mark as infinity
  # otherwise, calculate parent age at the birth in years
  mutate(age_at_birth = case_when(c_birth_month == -4 ~ Inf,
                                  c_birth_month > 0 ~ (c_birth_month - r_birth_month) / 12)) %>%
  # Drop people for whom the year is invalidly missing
  filter(!is.na(age_at_birth)) %>%
  # Take the lowest positive value: the first birth timing
  arrange(age_at_birth) %>%
  slice_head(n = 1) %>%
  select(PUBID, age_at_birth, c_birth_month)

# Visualize age at first birth
first_birth %>%
  left_join(demographics, by = "PUBID") %>%
  ggplot(aes(x = age_at_birth)) + 
  geom_histogram() +
  facet_wrap(~sex, ncol = 1) +
  xlim(c(15,40)) +
  xlab("Age at First Birth") +
  geom_vline(xintercept = 18, linetype = "dashed")
ggsave("../figures/parenthood_ages.pdf",
       height = 5, width = 6.5)

# Determine employment at every year,
# defined as any current employment
employed <- new_data %>%
  select(PUBID, starts_with("YEMP_CURFLAG")) %>%
  pivot_longer(cols = starts_with("YEMP"), values_to = "current") %>%
  mutate(year = str_replace(name,".*_","")) %>%
  group_by(PUBID, year) %>%
  filter(!all(current == -5)) %>%
  summarize(employed = any(current == 1),
            .groups = "drop")

# Wage in each year
wage <- new_data %>%
  select(PUBID, starts_with("YEMP_CURFLAG"), starts_with("CV_HRLY_COMPENSATION")) %>%
  pivot_longer(cols = -PUBID) %>%
  mutate(variable = ifelse(grepl("HRLY",name),"wage","employed"),
         job = str_replace_all(name, ".*[.]|_.*",""),
         year = str_replace_all(name,".*_","")) %>%
  select(PUBID, variable, job, year, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  group_by(PUBID, year) %>%
  filter(employed == 1) %>%
  select(-employed) %>%
  # take highest-paying among current jobs
  arrange(PUBID, year, -wage) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  filter(wage > 0) %>%
  left_join(cpi, by = "year") %>%
  mutate(wage = wage * cpi / 100,
         wage = case_when(wage < 5 ~ 5,
                          wage > 100 ~ 100,
                          T ~ wage),
         wage = log(wage)) %>%
  select(-cpi)

# Education in each year
education <- new_data %>%
  select(PUBID, starts_with("CV_HIGHEST_DEGREE"), 
         # choose only one of two forms of variable in years that have both
         -contains("EDT_2010"), -contains("EDT_2011"), -contains("EDT_2013")) %>%
  pivot_longer(cols = starts_with("CV")) %>%
  mutate(year = str_replace_all(name,".*_","")) %>%
  mutate(educ = factor(case_when(value == 0 ~ 1,
                                 value %in% 1:2 ~ 2,
                                 value == 3 ~ 3,
                                 value >= 4 ~ 4),
                       labels = c("Less than high school",
                                  "High school",
                                  "2-year degree",
                                  "4-year degree"))) %>%
  select(PUBID, year, educ)

# Marital status
marital <- new_data %>%
  select(PUBID, starts_with("CV_MARSTAT")) %>%
  pivot_longer(cols = -PUBID) %>%
  mutate(year = str_replace_all(name,".*_","")) %>%
  mutate(marital = case_when(
    # married only counts married with spouse present
    value == 3 ~ "married",
    value %in% c(1,5,7,9) ~ "cohabiting",
    value %in% c(2,4,6,8,10) ~ "no_partner"
  )) %>%
  select(PUBID, year, marital)

# Years of full-time work experience
# Note: This block takes minutes to run
experience <- new_data %>%
  select(PUBID, starts_with("EMP_HOURS")) %>%
  pivot_longer(cols = -PUBID) %>%
  mutate(week = str_replace_all(name, ".*[.]|_.*",""),
         year = str_replace_all(name,"EMP_HOURS_",""),
         year = str_replace_all(year,"[.].*","")) %>%
  mutate(fulltime = value >= 35) %>%
  group_by(PUBID) %>%
  arrange(PUBID,year,week) %>%
  # calculate cumulative years of full-time work
  # summed over full-time defined in each week
  mutate(experience = cumsum(fulltime) / 52) %>%
  # convert weeks to months (this is approximate but very close)
  mutate(month = 1 + trunc(as.numeric(week) / 4.345),
         # for a tiny number of cases, code as December
         # non-consequential since will take first week anyway
         month = ifelse(month == 13, 12, month)) %>%
  mutate(continuous_month = month_year_to_continuous(
    month = month,
    year = as.numeric(year)
  )) %>%
  # keep experience as of first week in the month
  group_by(PUBID, year, continuous_month)  %>%
  arrange(PUBID, year, continuous_month, week) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(PUBID, experience, continuous_month) %>%
  # keep the data at the interview months
  rename(interview_month = continuous_month) %>%
  right_join(interview_month, by = c("PUBID","interview_month")) %>%
  select(PUBID, experience, year)

# Working full time at current job
fulltime <- new_data %>%
  select(PUBID, starts_with("CV_HRS_PER_WEEK")) %>%
  pivot_longer(cols = -PUBID,
               values_to = "hours") %>%
  mutate(job = str_replace_all(name, ".*[.]|_.*",""),
         year = str_replace_all(name,".*_","")) %>%
  mutate(fulltime = case_when(hours >= 0 ~ hours >= 35)) %>%
  select(PUBID, job, year, fulltime) %>%
  # restrict to the focal job in the wage data
  right_join(wage, by = c("PUBID","job","year")) %>%
  select(PUBID, year, fulltime)

# Job tenure in each year
tenure <- new_data %>%
  select(PUBID, starts_with("CV_WKSWK_JOB_DLI")) %>%
  pivot_longer(cols = -PUBID,
               values_to = "tenure") %>%
  mutate(job = str_replace_all(name, ".*[.]|_.*",""),
         year = str_replace_all(name,".*_","")) %>%
  select(PUBID, job, year, tenure) %>%
  # restrict to the focal job in the wage data
  right_join(wage, by = c("PUBID","job","year")) %>%
  select(-wage) %>%
  # modify job tenure to years
  mutate(tenure = case_when(tenure >= 0 ~ tenure / 52)) %>%
  select(PUBID, year, tenure)

# DATA CHECK: Tenure is named with the suffix _DLI for since date of last interview
# but documentation says that it is total tenure (going beyond last interview).
# The code below confirms it is total tenure:
# the quantity (job tenure - time since last interview) is often substantially positive
tenure_minus_gap <- interview_month %>%
  group_by(PUBID) %>%
  arrange(PUBID, year) %>%
  mutate(lag_month = lag(interview_month,1),
         time_since_dli = (interview_month - lag_month) / 12) %>%
  left_join(tenure, by = c("PUBID","year")) %>%
  filter(!is.na(tenure)) %>%
  mutate(tenure_minus_gap = tenure - time_since_dli)
tenure_minus_gap %>%
  ungroup() %>%
  summarize(positive = mean(tenure_minus_gap > 0, na.rm = T),
            over_1_year = mean(tenure_minus_gap > 1, na.rm = T))
tenure_minus_gap %>%
  mutate(positive = tenure_minus_gap > 0) %>%
  filter(!is.na(positive)) %>%
  ggplot(aes(x = tenure_minus_gap)) +
  geom_histogram() +
  facet_wrap(~positive, scales = "free", ncol = 1)

# Define all eligible focal years for each unit
years_categorized <- age_interview %>%
  left_join(first_birth, by = "PUBID") %>%
  # Parent is observation at least 12 months after the birth
  mutate(category = case_when(age_interview >= (age_at_birth + 1) ~ "parent",
                              # never parent is never birth
                              age_at_birth == Inf ~ "never_parent",
                              # pre-parenthood is more than 9 months before birth
                              age_interview < (age_at_birth - .75) ~ "pre_parent"))

# Find first post-parenthood observation on parents
first_obs_parent <- years_categorized %>%
  filter(category == "parent") %>%
  group_by(PUBID) %>%
  arrange(age_interview) %>%
  slice_head(n = 1)
# Find last pre-parenthood observation on parents
last_obs_pre_parent <- years_categorized %>%
  filter(category == "pre_parent") %>%
  group_by(PUBID) %>%
  arrange(age_interview) %>%
  slice_tail(n = 1)

# Create data with year_1 and year_2 for parent observation pairs
parent_pairs <- last_obs_pre_parent %>%
  rename(year_1 = year, age_1 = age_interview) %>%
  select(PUBID, year_1, age_1) %>%
  left_join(
    first_obs_parent %>%
      rename(year_2 = year, age_2 = age_interview) %>%
      select(PUBID, year_2, age_2, age_at_birth),
    by = "PUBID"
  ) %>%
  mutate(treated = T) %>%
  filter(!is.na(year_2)) %>%
  ungroup()

# Find control pairs of observations separated by 1.75+ years
control_pairs <- years_categorized %>%
  filter(category %in% c("pre_parent","never_parent")) %>%
  group_by(PUBID) %>%
  arrange(PUBID, year) %>%
  mutate(year_2 = case_when(
    lead(age_interview,1) >= age_interview + 1.75 ~ lead(year,1),
    lead(age_interview,2) >= age_interview + 1.75 ~ lead(year,2)
  )) %>%
  mutate(age_2 = case_when(
    lead(age_interview,1) >= age_interview + 1.75 ~ lead(age_interview,1),
    lead(age_interview,2) >= age_interview + 1.75 ~ lead(age_interview,2)
  )) %>%
  rename(year_1 = year,
         age_1 = age_interview) %>%
  select(PUBID, year_1, age_1, year_2, age_2, age_at_birth) %>%
  mutate(treated = F) %>%
  filter(!is.na(year_2)) %>%
  ungroup()

# Merge all data together
all_data <- parent_pairs %>%
  bind_rows(control_pairs) %>%
  # Merge in the mediator and outcome
  left_join(employed %>% rename(year_2 = year),
            by = c("PUBID","year_2")) %>%
  left_join(wage %>% 
              rename(year_2 = year) %>% 
              select(-job),
            by = c("PUBID","year_2")) %>%
  # Merge in demographics
  left_join(demographics,
            by = "PUBID") %>%
  # Merge in time-varying controls at year 1
  left_join(employed %>% 
              rename(year_1 = year,
                     employed_baseline = employed),
            by = c("PUBID","year_1")) %>%
  left_join(wage %>% 
              rename(year_1 = year,
                     wage_baseline = wage) %>% 
              select(-job),
            by = c("PUBID","year_1")) %>%
  # for lag wage, replace NA from non-employment with minimum value
  # which is ok because employment and lag wage will both be confounders
  mutate(wage_baseline = case_when(!employed_baseline ~ min(wage_baseline, na.rm = T),
                                   T ~ wage_baseline)) %>%
  left_join(education %>% rename(year_1 = year),
            by = c("PUBID","year_1")) %>%
  left_join(marital %>% rename(year_1 = year),
            by = c("PUBID","year_1")) %>%
  left_join(fulltime %>% rename(year_1 = year),
            by = c("PUBID","year_1")) %>%
  left_join(tenure %>% rename(year_1 = year),
            by = c("PUBID","year_1")) %>%
  # if not employed, recode tenure and fulltime to 0
  mutate(tenure = ifelse(!employed_baseline, 0, tenure),
         fulltime = ifelse(!employed_baseline, F, fulltime)) %>%
  left_join(experience %>% rename(year_1 = year),
            by = c("PUBID","year_1"))

restricted <- all_data %>%
  (function(.data) {
    print(.data %>% group_by(sex, treated) %>% 
            summarize(obs = n(),
                      people = n_distinct(PUBID)) %>% 
            mutate(rule = "All data"))
    return(.data)
  }) %>%
  # keep only pairs where first age is at least 18
  filter(age_1 >= 18) %>%
  (function(.data) {
    print(.data %>% group_by(sex, treated) %>% 
            summarize(obs = n(),
                      people = n_distinct(PUBID)) %>% 
            mutate(rule = "Age 1 is at 18+"))
    return(.data)
  }) %>%
  # restrict to
  # parents observed within 3 years before and 3 years after birth
  # nonparents with a window of less than 6 years
  filter((treated & (age_1 >= age_at_birth - 3) & (age_2 <= age_at_birth + 3)) |
           (!treated & (age_2 <= age_1 + 6))) %>%
  (function(.data) {
    print(.data %>% 
            group_by(sex, treated) %>% 
            summarize(obs = n(),
                      people = n_distinct(PUBID)) %>% 
            mutate(rule = "Age 1 and 2 are each within 3 years of birth"))
    return(.data)
  }) %>%
  filter(!is.na(employed_baseline) & !is.na(employed)) %>%
  (function(.data) {
    print(.data %>% group_by(sex, treated) %>% 
            summarize(obs = n(),
                      people = n_distinct(PUBID)) %>% 
            mutate(rule = "Valid employment report before and after"))
    return(.data)
  }) %>%
  filter(!(employed & is.na(wage))) %>%
  (function(.data) {
    print(.data %>% group_by(sex, treated) %>% 
            summarize(obs = n(),
                      people = n_distinct(PUBID)) %>%
            mutate(rule = "Valid wage outcome if employed"))
    return(.data)
  })

write_lines(unique(restricted$PUBID),
            file = "../intermediate/motherhood_ids.txt")

# manually go to NLSY website and get weights for those ids
weighted <- restricted  %>%
  left_join(read_delim("../intermediate/motherhood_weights.dat",
                       col_names = F) %>%
              rename(PUBID = X1, w = X2) %>%
              mutate(w = w / mean(w)),
            by = "PUBID")

saveRDS(weighted,
        file = "../intermediate/motherhood.RDS")

sink()
