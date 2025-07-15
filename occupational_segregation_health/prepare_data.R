
sink("figures/log_prepare_data.txt")
print(Sys.time())

# See run_all.R to see how this file is called

# This file contains a function to prepare data other than replicate weights.
# The data are loaded using ipumsr::read_ipums_micro_chunked()
# That function runs callback_function on many chunks of the raw data
# to reduce memory pressure.

# The result of this code is a set of .Rds files stored in the intermediate folder.
# By preparing these files, I only work with the very large IPUMS data once (done here).

# This is the function to be applied to the raw data in chunks.
# x is a chunk of the data
# pos is never used but must be an argument for use with ipumsr
callback_function <- function(x, pos) {
  x %>%
    # Handle replicate weights separately
    select(-starts_with("REPWTP")) %>%
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
           DIFFANY = case_when(DIFFANY %in% 1:2 ~ DIFFANY == 2),
           # Modify weights since in 2014 there are two split samples for
           # questionnaire testing that independently sum to the population
           # but for this analysis we don't want to double-weight 2014
           ASECWT = ifelse(YEAR == 2014, ASECWT / 2, ASECWT))
}
# Convert that callback function into an ipumsr callback
callback <- IpumsDataFrameCallback$new(callback_function)

# Load data on the full population
full_population <- read_ipums_micro_chunked(
  ddi = "data/cps_00050.xml", 
  data_file = "data/cps_00050.dat", 
  verbose = TRUE,
  callback = callback, 
  chunk_size = 10000
)
saveRDS(full_population, file = "intermediate/full_population.Rds")

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
  group_by(OCC2010, EDUC) %>%
  filter(n_distinct(RACE) == 4) %>%
  group_by()
saveRDS(d, file = "intermediate/d.Rds")

# Remove those data files from memory because they are now saved on disk
rm(full_population, linked, d_onset, d)

# Extract replicate weights in batches of 40 at a time
replicate_sets <- list(1:40, 41:80, 81:120, 121:160)
for (i in 1:length(replicate_sets)) {
  replicate_set <- replicate_sets[[i]]
  print(paste("Beginning replicate",replicate_set[1]))
  weights <- read_ipums_micro_chunked(
    ddi = "data/cps_00050.xml", 
    data_file = "data/cps_00050.dat", 
    callback = IpumsDataFrameCallback$new(function(x, pos) {
      x %>%
        # There are no replicate weights before 2005, so restrict the data
        filter(YEAR >= 2005) %>%
        # Select only identifiers and the replicate weight
        select(CPSIDP, YEAR, SERIAL, PERNUM, all_of(paste0("REPWTP",replicate_set))) %>%
        # Make all variables non-zero, which affects a very small number of weights that seem likely to be data errors.
        mutate_all(.funs = function(x) ifelse(x < 0, 0, x))
    }), 
    chunk_size = 1e5,
    verbose = F
  )
  for (replicate_number in replicate_set) {
    this_weight <- weights %>%
      select(CPSIDP, YEAR, SERIAL, PERNUM, matches(paste0("^REPWTP",replicate_number,"$")))
    # Correct 2014 weights to be half their size, so that they sum to the 2014 population
    # (analogous to handling of ASECWT above)
    this_weight[this_weight$YEAR == 2014,5] <- this_weight[this_weight$YEAR == 2014,5] / 2
    saveRDS(this_weight,
            file = paste0("intermediate/REPWTP",replicate_number,".Rds"))
    rm(this_weight)
  }
  rm(weights)
}

sink()