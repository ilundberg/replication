#' This code gets every individual's occupation as close to age 40 as possible.
#' It then maps all occupations (individuals' and parents') to Duncan SEI score.
#' The resulting analysis dataframe is used in estimation of the causal effect
#' of parental occupation.

# Dependencies
library(dplyr)
library(ggplot2)
library(gt)
library(gtsummary)
library(haven)
library(here)
library(ipumsr)
library(lubridate)
library(readxl)
library(rvest)
library(tidyr)
library(readr)
library(stringr)
library(forcats)

sink(here("logs","prepare_nlsy.txt"))

# Import data
source(here("code", "occs.R"))

################################
# CREATE OCCUPATION CROSSWALKS #
################################

# Occupation strategy:
# - crosswalk from child occupation codes to 1990 basis
# - crosswalk from parent occupation codes to 1990 basis
# - crosswalk each EGP class to a set of 2010 occupation codes,
#   which we further crosswalk to the 1990 basis

# Crosswalk from 2002 codes to 2010 codes
occ_2002_to_2010 <- read_excel(
  path = here("data", "occ2002_occ2010.xls"),
  sheet = 3,
  skip = 2
)
occ_2002_to_2010 <- setNames(
  occ_2002_to_2010[, 1:6],
  paste0("var_", gsub(" ", "_", tolower(names(occ_2002_to_2010[, 1:6]))))
)
occ_2002_to_2010 <- occ_2002_to_2010 |>
  mutate(
    var_2010_census_code = case_when(
      var_2002_census_code == "0130" ~ "0136",
      var_2002_census_code == "0210" ~ "0205",
      var_2002_census_code == "0620" ~ "0630",
      var_2002_census_code == "0730" ~ "0740",
      var_2002_census_code == "1000" ~ "1005",
      var_2002_census_code == "1110" ~ "1106",
      var_2002_census_code == "1960" ~ "1965",
      var_2002_census_code == "3130" ~ "3255",
      var_2002_census_code == "3650" ~ "3645",
      var_2002_census_code == "4550" ~ "9415",
      var_2002_census_code == "8230" ~ "8256",
      var_2002_census_code == "9840" ~ "0136",
      TRUE ~ var_2010_census_code
    )
  ) |>
  rename(
    "occ" = "var_2002_census_code",
    "occ_2010" = "var_2010_census_code"
  ) |>
  select(occ, occ_2010) |>
  filter(!is.na(occ))

# Crosswalk from 2010 codes to 1990 codes
occ_2010_to_1990 <- read_dta(here("data", "occ2010_occ1990dd.dta")) |>
  haven::zap_labels() |>
  mutate(occ = sprintf("%04d", occ), occ1990dd = sprintf("%03d", occ1990dd)) |>
  rename("occ_2010_1990" = "occ1990dd")

# Crosswalk from 2002 codes to 1990 codes by the two above
occ_2002_to_1990 <- occ_2002_to_2010 |>
  left_join(occ_2010_to_1990, by = c("occ_2010" = "occ")) |>
  select(-occ_2010)

# Crosswalk from 2000 codes to 1990 codes
occ_2000_to_1990 <- read_dta(here("data", "occ2000_occ1990dd.dta")) |>
  mutate(across(.cols = everything(), .fns = \(x) sprintf("%03d", x))) |>
  rename("occ_2000_1990" = "occ1990dd")

# Crosswalk from 1970 codes to 1990 codes
occ_1970_to_1990 <- read_dta(here("data", "occ1970_occ1990dd.dta")) |>
  mutate(across(.cols = everything(), .fns = \(x) sprintf("%03d", x))) |>
  rename("occ_1970_1990" = "occ1990dd")

# Crosswalk from 1990 codes to coarsened EGP classes
occ_1990_to_egp <- tibble(read.csv(
    here("data/occ10_to_egp_class_crosswalk.csv")
  )) |>
  mutate(occ10 = formatC(occ10, width = 4, flag = "0")) |>
  left_join(occ_2010_to_1990, by = c("occ10" = "occ")) |>
  rename("occ_1990" = "occ_2010_1990", "egp" = "egp10_10") |>
  filter(!is.na(occ_1990)) |>
  select(-occ10) |>
  # create five-class EGP
  # note that when a 1990 occupation could map to multiple EGP classes,
  # this takes the highest status of those classes
  group_by(occ_1990) |>
  summarize(egp = case_when(any(egp_label %in% c("I","II")) ~ 1,
                            any(egp_label %in% c("IIIa","IIIb")) ~ 2,
                            # self-employed are coded into specific occupations,
                            # so no small proprietors
                            any(egp_label %in% c("V","VI")) ~ 3,
                            any(egp_label %in% c("VIIa")) ~ 4,
                            any(egp_label %in% c("IVc","VIIb")) ~ 5),
            .groups = "drop") |>
  mutate(
    egp = factor(
      egp,
      labels = c(
        "Professional",
        "Routine Nonmanual",
        "Skilled Manual",
        "Unskilled Manual",
        "Farmers"
      )
    ),
    egp = fct_rev(egp)
  )

# Labels for 1990 occupation codes
occ_1990_labels <- read_html("data/IPUMS_USA _1990_Occupation_Codes.html") |>
  #read_html("https://usa.ipums.org/usa/volii/occ1990.shtml") |>
  html_elements("table") |>
  (\(x) x[[2]])() |>
  html_table() |>
  mutate(valid = !is.na(as.numeric(OCC))) |>
  filter(valid) |>
  select(-valid)

# HWSEI index for 1990 occupation codes
ddi <- read_ipums_ddi(here("data", "usa_00001.xml"))
occ_1990_to_hwsei <- read_ipums_micro(ddi, verbose = FALSE) |>
  filter(!is.na(OCC1990), OCC1990 != 999) |>
  mutate(occ_1990 = sprintf("%03d", OCC1990),
         # Two implied decimal places on HWSEI so range is between 0 and 100
         HWSEI = HWSEI / 100) |>
  select(occ_1990, HWSEI) |>
  zap_labels() |>
  distinct(occ_1990, .keep_all = TRUE)

#############################################
# PREPARE NLSY CHILD OCCUPATION INFORMATION #
#############################################

#' Pivot occupation x year variables into long format and keep each
#' respondent's occupation that is closest to when they are 40.
nlsy_resp_occs <- nlsy |>
  select(CASEID_1979, starts_with(c("COWALL-EMP", "OCCALL-EMP", "Q1-3", "FAM-DAD", "FAM-MOM"))) |>
  pivot_longer(
    cols = starts_with(c("OCCALL-EMP", "COWALL-EMP")),
    names_pattern = "(OCCALL|COWALL)-EMP\\.(..)_(.*)",
    names_to = c(".value", "no", "year"),
    values_to = NULL
  ) |>
  rename(
    resp_occ_no = no,
    resp_occ_year = year,
    resp_occ = OCCALL,
    resp_occ_class = COWALL
  ) |>
  rename_with(.fn = \(x) gsub(".*~", "", x), .cols = starts_with("Q1-3")) |>
  rename_with(.fn = \(x) gsub(".*-", "", x), .cols = starts_with("FAM-")) |>
  mutate(
    across(
      .cols = c(M_1979, Y_1979, M_1981, Y_1981, starts_with("FAM-")),
      .fns = \(x) case_when(x <= 0 ~ NA, TRUE ~ x)
    ),
    M_1979 = coalesce(M_1979, M_1981),
    Y_1979 = coalesce(Y_1979, Y_1981),
    Y_1979 = case_when(is.na(Y_1979) ~ NA, TRUE ~ paste0("19", Y_1979)),
    across(
      .cols = c("DAD_Y_1987", "MOM_Y_1987"),
      .fns = \(x) case_when(is.na(x) ~ NA, TRUE ~ paste0("19", x))
    ),
    birth_date = my(paste(M_1979, Y_1979), quiet = TRUE),
    birth_date_father = my(paste(DAD_M_1987, DAD_Y_1987), quiet = TRUE),
    birth_date_mother = my(paste(MOM_M_1987, MOM_Y_1987), quiet = TRUE),
    resp_occ = case_when(resp_occ <= 0 ~ NA, TRUE ~ resp_occ),
    resp_occ_no = as.numeric(resp_occ_no),
    resp_occ_type = case_when(
      resp_occ_year %in% c(1998, 2000) ~ "1970 3-digit",
      resp_occ_year == 2002 ~ "2000 3-digit",
      TRUE ~ "2000 4-digit"
    ),
    resp_occ = case_when(
      is.na(resp_occ) ~ NA,
      endsWith(resp_occ_type, "3-digit") ~ sprintf("%03d", .data$resp_occ),
      endsWith(resp_occ_type, "4-digit") ~ sprintf("%04d", .data$resp_occ)
    ),
    resp_occ_class = case_when(
      is.na(resp_occ) ~ NA,
      resp_occ_class == 4 ~ "self-employed",
      TRUE ~ "other"
    ),
    resp_occ_year = as.numeric(resp_occ_year)
  ) |>
  select(-c(M_1979, Y_1979, M_1981, Y_1981, starts_with(c("DAD_", "MOM_"))))

# Create data frame with one chosen occupation report per respondent
# This data frame will exclude those who do not have an occupation report
# in the age range
nlsy_resp_occs_subset <- nlsy_resp_occs |>
  group_by(CASEID_1979, resp_occ_year) |>
  fill(resp_occ, resp_occ_class, .direction = "updown") |>
  ungroup() |>
  distinct(CASEID_1979, resp_occ_year, .keep_all = TRUE) |>
  group_by(CASEID_1979) |>
  mutate(
    no_occ = all(is.na(resp_occ)),
    resp_occ_age = resp_occ_year - year(birth_date),
    resp_occ_age_dist = case_when(
      !is.na(resp_occ) ~ abs(resp_occ_age - 40),
      TRUE ~ NA
    )
  ) |>
  ungroup() |>
  filter(!no_occ) |>
  group_by(CASEID_1979) |>
  mutate(
    min_resp_age_dist = min(resp_occ_age_dist, na.rm = TRUE)
  ) |>
  filter(resp_occ_age_dist == min_resp_age_dist) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(-c(no_occ, contains("resp_occ_age_"), min_resp_age_dist, resp_occ_no))

########################
# CONSTRUCT NLSY PANEL #
########################

nlsy_panel <- nlsy |>
  # Remove many respondent occupation variables, handled above instead
  select(-starts_with(c("OCCALL-EMP", "COWALL-EMP")), -starts_with("Q1-3")) |>
  # Create user-friendly names
  rename(
    mother_occ = `FAM-9A_1979`,
    father_occ = `FAM-11A_1979`,
    case_id = CASEID_1979,
    educ_mother = `HGC-MOTHER_1979`,
    educ_father = `HGC-FATHER_1979`,
    sample_id = SAMPLE_ID_1979,
    resp_race = SAMPLE_RACE_78SCRN,
    resp_sex = SAMPLE_SEX_1979
  ) |>
  # Code race categories
  mutate(
    resp_race = factor(resp_race, labels = c("Hispanic","Black","White"))
  ) |>
  # Code each parent's education into categories
  mutate(
    across(.cols = all_of(c("educ_mother", "educ_father")),
           .fns = \(x) factor(
             case_when(
               # x == -4 ~ 1,
               x %in% 0:11 ~ 2,
               x %in% 12 ~ 3,
               x %in% 13:15 ~ 4,
               x %in% 16:21 ~ 5
              ),
             labels = c(
               # "No parent",
               "Less than high school",
               "High school",
               "Some college",
               "College"
             )
           ))
  ) |>
  # Code missing values as NA
  mutate(
    across(
      .cols = where(function(x) !is.factor(x)),
      .fns = \(x) replace(x, x < 0, NA)
    ),
    across(
      .cols = c(mother_occ, father_occ),
      .fns = \(x) case_when(is.na(x) ~ NA, TRUE ~ sprintf("%03s", x))
    )
  ) |>
  # Merge in respondent occupation from above
  left_join(
    y = select(nlsy_resp_occs_subset, -birth_date),
    by = c("case_id" = "CASEID_1979")
  ) |>
  # 
  # Apply crosswalks to map respondent occupation to 1990 codes
  left_join(occ_1970_to_1990, by = c("resp_occ" = "occ")) |>
  left_join(occ_2000_to_1990, by = c("resp_occ" = "occ")) |>
  left_join(occ_2002_to_1990, by = c("resp_occ" = "occ")) |>
  mutate(
    resp_occ_1990 = case_when(
      resp_occ_type == "2000 4-digit" ~ occ_2010_1990,
      resp_occ_type == "2000 3-digit" ~ occ_2000_1990,
      resp_occ_type == "1970 3-digit" ~ occ_1970_1990,
      TRUE ~ NA
    )
  ) |>
  select(-(occ_1970_1990:occ_2010_1990)) |>
  #
  # Apply crosswalks to map parent occupations to 1990 codes
  left_join(occ_1970_to_1990, by = c("father_occ" = "occ")) |>
  rename("father_occ_1990" = "occ_1970_1990") |>
  left_join(occ_1970_to_1990, by = c("mother_occ" = "occ")) |>
  rename("mother_occ_1990" = "occ_1970_1990") |>
  #
  # Merge in occupation titles
  left_join(occ_1990_labels, by = c("resp_occ_1990" = "OCC")) |>
  rename("resp_occ_1990_title" = "Occupation") |>
  left_join(occ_1990_labels, by = c("father_occ_1990" = "OCC")) |>
  rename("father_occ_1990_title" = "Occupation") |>
  left_join(occ_1990_labels, by = c("mother_occ_1990" = "OCC")) |>
  rename("mother_occ_1990_title" = "Occupation") |>
  #
  # Merge on HW-SEI scores
  left_join(occ_1990_to_hwsei, by = c("resp_occ_1990" = "occ_1990")) |>
  rename("hwsei" = "HWSEI") |>
  left_join(occ_1990_to_hwsei, by = c("father_occ_1990" = "occ_1990")) |>
  rename("father_hwsei" = "HWSEI") |>
  left_join(occ_1990_to_hwsei, by = c("mother_occ_1990" = "occ_1990")) |>
  rename("mother_hwsei" = "HWSEI") |>
  # 
  # Merge on EGP classifications
  left_join(occ_1990_to_egp, 
            by = c("resp_occ_1990" = "occ_1990")) |>
  rename("egp" = "egp") |>
  left_join(occ_1990_to_egp |>
              rename(mother_egp = egp), 
            by = c("mother_occ_1990" = "occ_1990")) |>
  left_join(occ_1990_to_egp |>
              rename(father_egp = egp), 
            by = c("father_occ_1990" = "occ_1990")) |>
  # 
  # Aggregate mother and father variables to refer
  # to the parent with the higher HW-SEI score
  # Create a general parental occupation variable that depends on
  # - whichever parent exists, or if both who has higher SEI score
  mutate(which_parent_occ = case_when(
    is.na(father_occ_1990) ~ "Mother",
    is.na(mother_occ_1990) ~ "Father",
    is.na(father_hwsei) ~ "Mother",
    is.na(mother_hwsei) ~ "Father",
    mother_hwsei >= father_hwsei ~ "Mother",
    father_hwsei > mother_hwsei ~ "Father"
  )) |>
  mutate(
    parental_occ = case_when(
      which_parent_occ == "Mother" ~ mother_occ_1990,
      which_parent_occ == "Father" ~ father_occ_1990),
    parental_occ_title = case_when(
      which_parent_occ == "Mother" ~ mother_occ_1990_title,
      which_parent_occ == "Father" ~ father_occ_1990_title
    ),
    parental_occ_title = as_factor(parental_occ_title),
    parental_hwsei = case_when(
      which_parent_occ == "Mother" ~ mother_hwsei,
      which_parent_occ == "Father" ~ father_hwsei
    ),
    parental_egp = case_when(
      which_parent_occ == "Mother" ~ mother_egp,
      which_parent_occ == "Father" ~ father_egp
    ),
    parental_educ = case_when(
      which_parent_occ == "Father" ~ educ_father,
      which_parent_occ == "Mother" ~ educ_mother
    ),
    other_parent_educ = case_when(
      which_parent_occ == "Father" ~ educ_mother,
      which_parent_occ == "Mother" ~ educ_father
    )
  ) |>
  #
  # For sample restriction notes,
  # merge in whether observed at 35-45
  left_join(
    nlsy |>
      mutate(birth_year = 1900 + as.numeric(`Q1-3_A~Y_1979`)) |>
      select(CASEID_1979, birth_year, all_of(starts_with("OCCALL-EMP"))) |>
      pivot_longer(cols = starts_with("OCCALL-EMP")) |>
      mutate(year = as.numeric(str_replace_all(name,".*_",""))) |>
      mutate(age = year - birth_year) |>
      group_by(CASEID_1979) |>
      summarize(observed_35_45 = any(value != -5 & age %in% c(35:45)),
                birth_year = unique(birth_year),
                .groups = "drop"),
    by = c("case_id" = "CASEID_1979")
  )
  

# Create full subset for balance table
nlsy_panel_balance <- nlsy_panel

# Sample restrictions
cat(paste("All observations:",nrow(nlsy_panel),"\n"))
nlsy_panel <- nlsy_panel |>
  filter(!is.na(father_occ) | !is.na(mother_occ))
cat(paste("Has parent occupation:",nrow(nlsy_panel),"\n"))
nlsy_panel <- nlsy_panel |>
  filter(
    !is.na(parental_hwsei),
    !is.na(parental_egp),
    !is.na(parental_occ_title)
  )
cat(paste(
  "Parental occupation maps to title, HWSEI, and EGP:",
  nrow(nlsy_panel),
  "\n"
))

nlsy_panel <- nlsy_panel |>
  filter(!is.na(parental_educ) & parental_educ != "No parent") |>
  mutate(parental_educ = fct_drop(parental_educ))
cat(paste("Valid education report for that parent:",nrow(nlsy_panel),"\n"))
nlsy_panel <- nlsy_panel |>
  filter(observed_35_45)
cat(paste("Child observed at age 35-45:",nrow(nlsy_panel),"\n"))
nlsy_panel <- nlsy_panel |>
  filter(!is.na(resp_occ) & resp_occ_age >= 35 & resp_occ_age <= 45)
cat(paste("Has child occupation:",nrow(nlsy_panel),"\n"))
nlsy_panel <- nlsy_panel |>
  filter(!is.na(hwsei) & !is.na(egp) & !is.na(resp_occ_1990_title))
cat(paste(
  "Child occupation maps to title, HWSEI, and EGP:",
  n_nlsy <- nrow(nlsy_panel),
  "\n\n"
))
cat(
  "Number (%) of children that are self-employed:",
  paste0(
    n_selfemp <- sum(nlsy_panel$resp_occ_class == "self-employed"), 
    " (", 100 * round(n_selfemp/n_nlsy, 4), ")"
  )
)

# Notes about which occupations missed scores

# Occupations missing SEI scores for respondent and parent
no_sei_occs <- nlsy_panel_balance |>
  filter(is.na(hwsei) | is.na(parental_hwsei)) |>
  mutate(
    which_missing = case_when(
      is.na(hwsei) & is.na(parental_hwsei) ~ "Both",
      is.na(hwsei) ~ "Respondent occ.",
      is.na(parental_hwsei) ~ "Parent occ."
    )
  )
print("Occupations missing respondent HWSEI")
resp_sei_missing <- no_sei_occs |>
  filter(which_missing %in% c("Respondent occ.","Both")) |>
  summarise(N = n(), .by = resp_occ_1990_title) |>
  print()
print("Occupations missing parent HWSEI")
parent_sei_missing <- no_sei_occs |>
  filter(which_missing %in% c("Parent occ.","Both")) |>
  summarise(N = n(), .by = parental_occ_title) |>
  print()
# Occupations missing EGP for respondent and parent
no_egp_occs <- nlsy_panel_balance |>
  filter(is.na(egp) | is.na(parental_egp)) |>
  mutate(
    which_missing = case_when(
      is.na(egp) & is.na(parental_egp) ~ "Both",
      is.na(egp) ~ "Respondent occ.",
      is.na(parental_egp) ~ "Parent occ."
    )
  )
print("Occupations missing respondent EGP")
resp_egp_missing <- no_egp_occs |>
  filter(which_missing %in% c("Respondent occ.","Both")) |>
  summarise(N = n(), .by = resp_occ_1990_title) |>
  print()
print("Occupations missing parent EGP")
parent_egp_missing <- no_egp_occs |>
  filter(which_missing %in% c("Parent occ.","Both")) |>
  summarise(N = n(), .by = parental_occ_title) |>
  print()

# Create a balance table between respondents with/without occupational info
nlsy_balance_panel <- nlsy_panel_balance |>
  mutate(group = "Full") |>
  bind_rows(
    mutate(
      filter(
        nlsy_panel,
        !is.na(resp_occ),
        !is.na(parental_occ)
      ),
      group = "Analysis"
    )
  ) |>
  mutate(
    resp_sex = case_when(
      resp_sex == 1 ~ "Male",
      resp_sex == 2 ~ "Female",
      TRUE ~ NA
    ),
    resp_race = factor(resp_race, exclude = NULL),
    resp_sex = factor(resp_sex, exclude = NULL)
  )

balance_table <- nlsy_balance_panel |>
  mutate(
    educ_parent = case_when(
      which_parent_occ == "Father" ~ educ_father,
      TRUE ~ educ_mother
    )
  ) |>
  filter(group == "Analysis") |>
  tbl_summary(
    include = c(
      educ_parent,
      resp_race,
      resp_sex,
      birth_year
    ),
    # by = group,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    label = list(
      educ_parent ~ "Parent's Education",
      resp_race ~ "Respondent Race/Ethnicity",
      resp_sex ~ "Respondent Gender",
      birth_year ~ "Birth Year"
    )
  ) |>
  bold_labels()
balance_table |>
  as_gt() |>
  gtsave(filename = "figures/balance_table.tex")

# Output the case ids for getting custom weights
write_lines(nlsy_panel$case_id,file = "logs/case_id.txt")

# We create weights for those IDs here: https://www.nlsinfo.org/weights/nlsy79

# Use those weights
nlsy_panel <- nlsy_panel |>
  left_join(read_delim("data/customweight.dat",
                       col_names = c("case_id","weight")),
            by = "case_id") |>
  mutate(weight = weight / mean(weight))

# Output panel
saveRDS(nlsy_panel, here("data", "nlsy_panel.RDS"))
write.csv(nlsy_panel, here("data", "nlsy_panel.csv"), row.names = FALSE)

sessionInfo()
sink()