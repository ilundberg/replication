# Dependencies
library(here)
library(tidyverse)
theme_set(theme_bw())

sink(here("logs","text_notes.txt"))
print(Sys.time())

# Import analysis panel
nlsy_analysis <- tibble(readRDS(here("data/nlsy_panel.RDS")))

print("Prevalence of secretaries")
nlsy_analysis |>
  filter(parental_egp == "Routine Nonmanual") |>
  group_by(parental_educ, parental_occ_title) |>
  summarize(weight = sum(weight), .groups = "drop_last") |>
  mutate(weight = weight / sum(weight)) |>
  arrange(-weight) |>
  filter(parental_occ_title == "Secretaries") |>
  print()

print("Common occupations")
nlsy_analysis |>
  group_by(parental_egp, parental_educ, parental_occ_title) |>
  summarize(weight = sum(weight), .groups = "drop_last") |>
  mutate(weight = weight / sum(weight)) |>
  arrange(-weight) |>
  slice_head(n = 5) |>
  print(n = Inf)

print("Common occupations in population but which have with no college graduates")
nlsy_analysis |>
  group_by(parental_occ_title, parental_hwsei) |>
  summarize(prop_college = mean(parental_educ == "College"),
            prop_noHS = mean(parental_educ == "Less than high school"),
            total_weight = sum(weight),
            .groups = "drop") |>
  mutate(parental_hwsei = round(parental_hwsei)) |>
  filter(prop_college == 0) |>
  arrange(-total_weight) |>
  print()

print("Common occupations in population but which have with no less-than-high-school parents")
nlsy_analysis |>
  group_by(parental_occ_title, parental_hwsei) |>
  summarize(prop_college = mean(parental_educ == "College"),
            prop_noHS = mean(parental_educ == "Less than high school"),
            total_weight = sum(weight),
            .groups = "drop") |>
  mutate(parental_hwsei = round(parental_hwsei)) |>
  filter(prop_noHS == 0) |>
  arrange(-total_weight) |>
  print()

print("Occupation examples reported in text")

nlsy_analysis |>
  filter(parental_occ_title %in% c(
    "Secretaries",
    "Teachers' aides",
    "Physicians",
    "Welders and cutters",
    "Sales counter clerks",
    "Janitors and cleaner",
    "Housekeepers and butlers",
    "Mail carriers, postal service"
  )) |>
  group_by(parental_occ_title, parental_egp, parental_hwsei) |>
  summarize(prop_hs = weighted.mean(parental_educ == "High school")) |>
  distinct() |>
  arrange(parental_hwsei) |>
  print()

print("Joey and Fred")
nlsy_analysis |>
  filter(resp_race == "White") |>
  filter(resp_sex == 1) |>
  filter(parental_educ == "High school") |>
  filter(which_parent_occ == "Mother") |>
  filter(parental_occ_title %in% c("Secretaries","Housekeepers and butlers")) |>
  select(
    resp_race,
    resp_sex,
    parental_educ,
    which_parent_occ,
    parental_occ_title,
    parental_hwsei,
    parental_egp
  ) |>
  distinct()

print("Sarah")
nlsy_analysis |>
  filter(resp_race == "White") |>
  filter(resp_sex == 2) |>
  filter(parental_educ == "College") |>
  filter(which_parent_occ == "Mother") |>
  filter(parental_occ_title == "Teachers, elementary school") |>
  select(
    resp_race,
    resp_sex,
    parental_educ,
    which_parent_occ,
    parental_occ_title,
    parental_hwsei,
    parental_egp
  ) |>
  distinct()

sessionInfo()
sink()