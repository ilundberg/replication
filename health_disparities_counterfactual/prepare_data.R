# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This file contains a function to prepare data
# in particular years of the CPS

prepare_data <- function(target_years = 2005:2020) {
  ddi <- read_ipums_ddi("data/cps_00050.xml")
  full_population <- read_ipums_micro(ddi) %>%
    # Restrict to 2005 and later so we can use replicate weights
    filter(YEAR %in% target_years) %>%
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
           new_question = case_when(YEAR < 2014 ~ 0,
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
  if (2014 %in% target_years) {
    full_population[full_population$YEAR == 2014, c("ASECWT",paste0("REPWTP",1:160))] <- full_population[full_population$YEAR == 2014, c("ASECWT",paste0("REPWTP",1:160))] / 2
  }
  
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
           lag_new_question = new_question) %>%
    # Append the variables from the second observation on each person
    left_join(full_population %>%
                group_by(CPSIDP) %>%
                arrange(YEAR) %>%
                filter(1:n() == 2 & n() == 2) %>%
                group_by() %>%
                rename(employed_next = employed) %>%
                select(CPSIDP, y, new_question, employed_next),
              by = "CPSIDP") %>%
    mutate(num_linked = n()) %>%
    filter(AGE >= 25 & AGE <= 60) %>%
    mutate(num_age_range = n()) %>%
    filter(!is.na(y)) %>%
    mutate(num_nonMissingY = n())
    
  
  # Make the employed linked file without lagged disability
  d_onset <- linked %>%
    mutate(num_linked = n()) %>%
    filter(!lag) %>%
    mutate(num_reportedNoYear1 = n()) %>%
    filter(employed) %>%
    mutate(num_employedYear1 = n()) %>%
    # Restrict to those with a valid occupation in the first wave
    #filter(OCC != 0 & !is.na(OCC) & OCC != 9840) %>%
    filter(OCC2010 != 0 & !is.na(OCC2010) & OCC2010 < 9830) %>%
    mutate(OCC2010 = factor(OCC2010)) %>%
    mutate(num_withOccYear1 = n())
  
  return(list(full_population = full_population,
              linked = linked, 
              d_onset = d_onset))
}

