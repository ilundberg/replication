# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A gap-closing perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# Describe changes in the safety of occupations

source("code/prepare_data.R")
all_data <- prepare_data(target_years = 1988:2020)

# Examine within- and between-occupation changes in the disparity hazard
# First, identify the occupations that are reported for every race in every year category for black and white respondents
# (this scope restriction allows nonparametric estimation)
consistent_occs <- unique((
  all_data$d_onset %>%
    mutate(YEAR = case_when(YEAR %in% 1988:1992 ~ 1990,
                            YEAR %in% 1993:1997 ~ 1995,
                            YEAR %in% 1998:2002 ~ 2000,
                            YEAR %in% 2003:2007 ~ 2005,
                            YEAR %in% 2008:2012 ~ 2010,
                            YEAR %in% 2008:2012 ~ 2010)) %>%
    # Intentionally exclude those after 2012 because they are exposed
    # to the questionnaire changes starting in 2013 (since the 2013 outcome
    # is measured in 2014)
    # This choices applies to this analysis because the focus
    # is on levels, not only disparities.
    filter(!is.na(YEAR)) %>%
    #YEAR %in% 2013:2019 ~ 2016)) %>%
    filter(grepl("Non-Hispanic",RACE)) %>%
    group_by(YEAR, RACE, OCC2010) %>%
    filter(1:n() == 1) %>%
    group_by(RACE, OCC2010) %>%
    filter(n_distinct(YEAR) == 5) %>%
    group_by(OCC2010) %>%
    filter(n_distinct(RACE) == 2)
)$OCC2010)

sink("figures/occupation_within_between_restrictions.txt")
print(paste0("The consistent set is ",length(unique(consistent_occs))," out of 451 occupations"))
print("The proportion of individuals in the consistent occupations is")
print(all_data$d_onset %>%
        group_by(RACE) %>%
        summarize(consistent_set = weighted.mean(OCC2010 %in% consistent_occs, w = ASECWT)))
sink()

# Within-occupation: Fix occupations at the 1988-1992 distribution and 
# look at change over time in the hazards in that aggregation of occupations
weights <- all_data$d_onset %>%
  filter(grepl("Non-Hispanic",RACE) & OCC2010 %in% consistent_occs) %>%
  mutate(YEAR = case_when(YEAR %in% 1988:1992 ~ 1990,
                          YEAR %in% 1993:1997 ~ 1995,
                          YEAR %in% 1998:2002 ~ 2000,
                          YEAR %in% 2003:2007 ~ 2005,
                          YEAR %in% 2008:2012 ~ 2010)) %>%
  filter(!is.na(YEAR)) %>%
  #YEAR %in% 2013:2019 ~ 2016)) %>%
  group_by(YEAR, RACE, OCC2010) %>%
  summarize(weight = sum(ASECWT)) %>%
  group_by(YEAR, RACE) %>%
  mutate(weight = weight / sum(weight)) %>%
  group_by()
hazards <- all_data$d_onset %>%
  filter(grepl("Non-Hispanic",RACE) & OCC2010 %in% consistent_occs) %>%
  mutate(YEAR = case_when(YEAR %in% 1988:1992 ~ 1990,
                          YEAR %in% 1993:1997 ~ 1995,
                          YEAR %in% 1998:2002 ~ 2000,
                          YEAR %in% 2003:2007 ~ 2005,
                          YEAR %in% 2008:2012 ~ 2010)) %>%
  filter(!is.na(YEAR)) %>%
                          #YEAR %in% 2013:2019 ~ 2016)) %>%
  group_by(YEAR, RACE, OCC2010) %>%
  summarize(hazard = weighted.mean(y, w = ASECWT)) %>%
  group_by(YEAR, RACE) %>%
  group_by()

within <- hazards %>%
  left_join(weights %>%
              filter(YEAR == 1990) %>%
              select(OCC2010, RACE, weight),
            by = c("OCC2010","RACE")) %>%
  group_by(YEAR, RACE) %>%
  summarize(estimate = weighted.mean(hazard, w = weight)) %>%
  mutate(estimand = "Within-Occupation Change")
between <- weights %>%
  left_join(hazards %>%
              filter(YEAR == 1990) %>%
              select(OCC2010, RACE, hazard),
            by = c("OCC2010","RACE")) %>%
  group_by(YEAR, RACE) %>%
  summarize(estimate = weighted.mean(hazard, w = weight)) %>%
  mutate(estimand = "Between-Occupation Change")


within %>%
  bind_rows(between) %>%
  mutate(estimand = fct_rev(estimand)) %>%
  ggplot(aes(x = YEAR, y = estimate, color = RACE, shape = RACE)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~estimand) +
  xlab("Year") +
  ylab("Onset of Work-Limiting Disability") +
  scale_color_discrete(name = "Race / Ethnicity") +
  scale_shape_discrete(name = "Race / Ethnicity") +
  theme_bw() +
  ggsave("figures/occupation_changes.pdf",
         height = 3, width = 6.5)

# Make slide versions of that
within %>%
  bind_rows(between) %>%
  mutate(estimand = fct_rev(estimand)) %>%
  ggplot(aes(x = YEAR, y = estimate, color = RACE)) +
  geom_point(aes(x = ifelse(YEAR == 1990, YEAR, NA))) +
  geom_point(aes(x = ifelse(YEAR != 1990, YEAR, NA)),
             alpha = 0, show.legend = F) +
  facet_wrap(~estimand) +
  xlab("Year") +
  ylab("Onset of Work-Limiting Disability") +
  scale_color_discrete(name = "Race / Ethnicity") +
  scale_shape_discrete(name = "Race / Ethnicity") +
  theme_bw() +
  ggsave("figures/occupation_changes_1.pdf",
         height = 3, width = 6.5)

within %>%
  bind_rows(between) %>%
  mutate(estimand = fct_rev(estimand)) %>%
  ggplot(aes(x = YEAR, y = estimate, color = RACE)) +
  geom_point(aes(x = ifelse(YEAR == 1990 | (RACE == "Non-Hispanic Black" & estimand == "Within-Occupation Change"), YEAR, NA))) +
  geom_smooth(aes(x = ifelse(YEAR == 1990 | (RACE == "Non-Hispanic Black" & estimand == "Within-Occupation Change"), YEAR, NA)),
              method = "lm", se = F) +
  geom_point(aes(x = ifelse(YEAR != 1990, YEAR, NA)),
             alpha = 0, show.legend = F) +
  facet_wrap(~estimand) +
  xlab("Year") +
  ylab("Onset of Work-Limiting Disability") +
  scale_color_discrete(name = "Race / Ethnicity") +
  scale_shape_discrete(name = "Race / Ethnicity") +
  theme_bw() +
  ggsave("figures/occupation_changes_2.pdf",
         height = 3, width = 6.5)

within %>%
  bind_rows(between) %>%
  mutate(estimand = fct_rev(estimand)) %>%
  ggplot(aes(x = YEAR, y = estimate, color = RACE)) +
  geom_point(aes(x = ifelse(YEAR == 1990 | estimand == "Within-Occupation Change", YEAR, NA))) +
  geom_smooth(aes(x = ifelse(YEAR == 1990 | estimand == "Within-Occupation Change", YEAR, NA)),
              method = "lm", se = F) +
  geom_point(aes(x = ifelse(YEAR != 1990, YEAR, NA)),
             alpha = 0, show.legend = F) +
  facet_wrap(~estimand) +
  xlab("Year") +
  ylab("Onset of Work-Limiting Disability") +
  scale_color_discrete(name = "Race / Ethnicity") +
  scale_shape_discrete(name = "Race / Ethnicity") +
  theme_bw() +
  ggsave("figures/occupation_changes_3.pdf",
         height = 3, width = 6.5)

within %>%
  bind_rows(between) %>%
  mutate(estimand = fct_rev(estimand)) %>%
  ggplot(aes(x = YEAR, y = estimate, color = RACE)) +
  geom_point(aes(x = ifelse(YEAR == 1990 | RACE == "Non-Hispanic Black" | estimand == "Within-Occupation Change", YEAR, NA))) +
  geom_smooth(aes(x = ifelse(YEAR == 1990 | RACE == "Non-Hispanic Black" | estimand == "Within-Occupation Change", YEAR, NA)),
              method = "lm", se = F) +
  geom_point(aes(x = ifelse(YEAR != 1990, YEAR, NA)),
             alpha = 0, show.legend = F) +
  facet_wrap(~estimand) +
  xlab("Year") +
  ylab("Onset of Work-Limiting Disability") +
  scale_color_discrete(name = "Race / Ethnicity") +
  scale_shape_discrete(name = "Race / Ethnicity") +
  theme_bw() +
  ggsave("figures/occupation_changes_4.pdf",
         height = 3, width = 6.5)

# The fifth figure of the sequence is the complete figure (created at the start of this sequence)



