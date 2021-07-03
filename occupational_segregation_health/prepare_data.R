
# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# This file prepares the data and saves it in the intermediate folder.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
sink("logs/prepare_data.txt")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

library(ipumsr)
library(tidyverse)

# Load data on the full population
full_population <- read_ipums_micro(
  ddi = "data/cps_00050.xml", 
  data_file = "data/cps_00050.dat", 
  verbose = TRUE
) %>%
  # Remove the occupation coded in specific years
  # to avoid confusion with the harmonized OCC2010 that I will use.
  select(-OCC) %>%
  # Prepare variables
  mutate(y = case_when(DISABWRK %in% 1:2 ~ DISABWRK == 2),
         employed = EMPSTAT == 10,
         # HFLAG only exists in 2014. It codes those part of the 3/8 file
         # that contained the new questions.
         # In years before 2014, mark as 0 (not redesigned).
         # In years after 2014, mark as 1 (redesigned)
         # It flags a different format for the question to be asked.
         HFLAG = ifelse(is.na(HFLAG), 0, HFLAG),
         questionnaire_redesign = case_when(YEAR < 2014 ~ 0,
                                            YEAR == 2014 & !HFLAG ~ 0,
                                            YEAR == 2014 & HFLAG ~ 1,
                                            YEAR > 2014 ~ 1),
         # Adjustment variables
         RACE = case_when(HISPAN != 0 ~ "Hispanic",
                          RACE == 100 ~ "Non-Hispanic White",
                          RACE == 200 ~ "Non-Hispanic Black",
                          T ~ "Other"),
         foreign_born = BPL >= 15000,
         EDUC = factor(case_when(EDUC < 73 ~ "Less than HS",
                                 EDUC == 73 ~ "High school",
                                 EDUC < 111 ~ "Some college",
                                 EDUC < 999 ~ "College")),
         SEX = factor(case_when(SEX == 1 ~ "Men",
                                SEX == 2 ~ "Women")),
         HEALTH = factor(HEALTH),
         # Ever retired or left a job for health reasons
         QUITSICK = case_when(QUITSICK %in% 1:2 ~ QUITSICK == 2),
         # Hearing difficulty
         DIFFHEAR = case_when(DIFFHEAR %in% 1:2 ~ DIFFHEAR == 2),
         # Vision difficulty
         DIFFEYE = case_when(DIFFEYE %in% 1:2 ~ DIFFEYE == 2),
         # Difficulty remembering
         DIFFREM = case_when(DIFFREM %in% 1:2 ~ DIFFREM == 2),
         # Physical difficulty
         DIFFPHYS = case_when(DIFFPHYS %in% 1:2 ~ DIFFPHYS == 2),
         # Disability limiting mobility
         DIFFMOB = case_when(DIFFMOB %in% 1:2 ~ DIFFMOB == 2),
         # Personal care limitation
         DIFFCARE = case_when(DIFFCARE %in% 1:2 ~ DIFFCARE == 2),
         # Any difficulty
         DIFFANY = case_when(DIFFANY %in% 1:2 ~ DIFFANY == 2)) %>%
  # There are two replicate weights that are negative (I believe in error)
  # for 21 people in 2014. I am recoding those to 0.
  mutate(REPWTP146 = ifelse(REPWTP146 < 0, 0, REPWTP146),
         REPWTP156 = ifelse(REPWTP156 < 0, 0, REPWTP156))

# In 2014, a redesign of the income question was done
# so that the weight sums to twice the population (see HFLAG)
full_population[full_population$YEAR == 2014, c("ASECWT",paste0("REPWTP",1:160))] <- full_population[full_population$YEAR == 2014, c("ASECWT",paste0("REPWTP",1:160))] / 2

# Save the full population data

saveRDS(full_population, file = "intermediate/full_population.Rds")
# Save a version without replicate weights.
# This is useful if later you want to load it on a computer with less RAM
# because replicate weights are large (n x 160).
saveRDS(full_population %>% select(-starts_with("REPWT")), 
        file = "intermediate/full_population_noRepWeights.Rds")

# Make the linked file
linked <- full_population %>%
  # Record the number in the full_population file
  mutate(num_original = n()) %>%
  # CPSIDP identifies people over their entire panel period in the CPS
  group_by(CPSIDP) %>%
  # Keep only those with two observations (over two years)
  filter(n() == 2) %>%
  group_by() %>%
  mutate(num_linkable = n()) %>%
  # Keep only the first observation here
  group_by(CPSIDP) %>%
  arrange(YEAR) %>%
  filter(1:n() == 1) %>%
  group_by() %>%
  # Rename a few variables to clarify they are measured in the lag period
  rename(lag = y,
         lag_questionnaire_redesign = questionnaire_redesign) %>%
  # Append the variables from the second observation on each person
  left_join(full_population %>%
              group_by(CPSIDP) %>%
              arrange(YEAR) %>%
              filter(1:n() == 2 & n() == 2) %>%
              group_by() %>%
              rename(employed_next = employed) %>%
              select(CPSIDP, y, questionnaire_redesign, employed_next),
            by = "CPSIDP") %>%
  mutate(num_linked = n()) %>%
  filter(AGE >= 25 & AGE <= 60) %>%
  mutate(num_age_range = n()) %>%
  filter(!is.na(y)) %>%
  mutate(num_nonMissingY = n())
saveRDS(linked, file = "intermediate/linked.Rds")

# Make the file with those employed who did not report a disability last year and never quit a job for health
d_onset <- linked %>%
  mutate(num_linked = n()) %>%
  filter(!lag) %>%
  mutate(num_reportedNoYear1 = n()) %>%
  filter(!QUITSICK) %>%
  mutate(num_neverQuitSick = n()) %>%
  filter(employed) %>%
  mutate(num_employedYear1 = n()) %>%
  mutate(OCC2010 = factor(OCC2010))
saveRDS(d_onset, file = "intermediate/d_onset.Rds")

# For the model-based evidence, restrict to those employed in occupations
# in which all four racial categories are observed
d <- d_onset %>%
  # Restrict to the occupations observed in every race category
  group_by(OCC2010) %>%
  filter(n_distinct(RACE) == 4) %>%
  group_by()
saveRDS(d, file = "intermediate/d.Rds")

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
