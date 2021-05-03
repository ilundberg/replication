
setwd("/Users/iandl/Dropbox/Dissertation/gender_gap_occupations")
source("code/a_prepare_environment.R")

# Load the prepared data
load("intermediate/d.Rdata")

############
# Analysis #
############

# Total sex ratio in pay
total <- d %>%
  group_by(sex, year) %>%
  summarize(median = weighted.median(wage, w = asecwt),
            gmean = exp(weighted.mean(lnwage, w = asecwt))) %>%
  melt(id = c("sex","year"), variable.name = "estimand") %>%
  spread(key = sex, value = value) %>%
  mutate(ratio = women / men) %>%
  select(-women, -men)

# Sex ratio in pay within occupations
within_each_occupation <- d %>%
  group_by(sex, year, occ90ly) %>%
  summarize(median = weighted.median(wage, w = asecwt),
            meanlog = weighted.mean(lnwage, w = asecwt),
            weight = sum(asecwt)) %>%
  group_by(year, occ90ly) %>%
  mutate(weight = sum(weight)) %>%
  melt(id = c("sex","year","weight","occ90ly"), variable.name = "estimand") %>%
  spread(key = sex, value = value) %>%
  mutate(gap = case_when(estimand == "meanlog" ~ women - men,
                         estimand == "median" ~ women / men))
within_occupation_dropped <- within_each_occupation %>%
  group_by(year, estimand) %>%
  summarize(prop_dropped = weighted.mean(is.na(gap), w = weight))
print("Proportion of each year-specific sample weight dropped for occupations with only men or only women")
print(summary(within_occupation_dropped$prop_dropped))
within_occupation <- within_each_occupation %>%
  filter(!is.na(gap)) %>%
  select(year, occ90ly, estimand, gap, weight) %>%
  spread(key = estimand, value = gap) %>%
  group_by(year) %>%
  summarize(median = weighted.median(median, w = weight),
            gmean = exp(weighted.mean(meanlog, w = weight))) %>%
  melt(id = "year", variable.name = "estimand", value.name = "ratio")

# Sex ratio in pay within occupation x hours
within_occupationHours_beforeDropped <- d %>%
  # Use binned years
  mutate(year = year_bin) %>%
  group_by(sex, year, occ90ly, hours) %>%
  summarize(median = weighted.median(wage, w = asecwt),
            meanlog = weighted.mean(lnwage, w = asecwt),
            weight = sum(asecwt)) %>%
  group_by(year, occ90ly) %>%
  mutate(weight = sum(weight)) %>%
  melt(id = c("sex","year","weight","occ90ly","hours"), variable.name = "estimand") %>%
  spread(key = sex, value = value) %>%
  mutate(gap = case_when(estimand == "meanlog" ~ women - men,
                         estimand == "median" ~ women / men))
dropped_common_support_occupationHours <- within_occupationHours_beforeDropped %>%
  group_by(year, estimand) %>%
  summarize(prop_dropped = weighted.mean(is.na(gap), w = weight))
print("Proportion of each year-specific sample weight dropped for occupation-hours with only men or only women")
print(summary(dropped_common_support_occupationHours$prop_dropped))
within_occupationHours <- within_occupationHours_beforeDropped %>%
  filter(!is.na(gap)) %>%
  select(year, occ90ly, hours, estimand, gap, weight) %>%
  spread(key = estimand, value = gap) %>%
  group_by(year) %>%
  summarize(median = weighted.median(median, w = weight),
            gmean = exp(weighted.mean(meanlog, w = weight))) %>%
  melt(id = "year", variable.name = "estimand", value.name = "ratio")

# Sex ratio in pay between occupations
between_each_occupation <- d %>%
  group_by(sex, year, occ90ly) %>%
  summarize(weight = sum(asecwt)) %>%
  left_join(d %>%
              group_by(year, occ90ly) %>%
              summarize(median = weighted.median(wage, w = asecwt),
                        meanlog = weighted.mean(lnwage, w = asecwt)),
            by = c("year","occ90ly"))
between_occupation <- between_each_occupation %>%
  group_by(sex, year) %>%
  summarize(median = weighted.median(median, w = weight),
            gmean = exp(weighted.mean(meanlog, w = weight))) %>%
  melt(id = c("sex","year"), variable.name = "estimand") %>%
  spread(key = sex, value = value) %>%
  mutate(gap = women / men) %>%
  select(-women, -men) %>%
  spread(key = estimand, value = gap) %>%
  melt(id = "year", variable.name = "estimand", value.name = "ratio")

# Sex ration in pay between occupations x hours
between_occupationHours <- d %>%
  mutate(year = year_bin) %>%
  group_by(sex, year, occ90ly, hours) %>%
  summarize(weight = sum(asecwt)) %>%
  left_join(d %>%
              mutate(year = year_bin) %>%
              group_by(year, occ90ly, hours) %>%
              summarize(median = weighted.median(wage, w = asecwt),
                        meanlog = weighted.mean(lnwage, w = asecwt)),
            by = c("year","occ90ly","hours")) %>%
  group_by(sex, year) %>%
  summarize(median = weighted.median(median, w = weight),
            gmean = exp(weighted.mean(meanlog, w = weight))) %>%
  melt(id = c("sex","year"), variable.name = "estimand") %>%
  spread(key = sex, value = value) %>%
  mutate(gap = women / men) %>%
  select(-women, -men) %>%
  spread(key = estimand, value = gap) %>%
  melt(id = "year", variable.name = "estimand", value.name = "ratio")

# Regression version of the within-occupation claim
within_occupation_OLS <- foreach(y = unique(d$year), .combine = "rbind") %do% {
  if (y %% 10 == 0) {
    print(paste("Starting year",y))
  }
  lm_total <- lm(lnwage ~ sex,
                 data = d %>% filter(year == y),
                 weights = asecwt)
  lm_within <- lm(lnwage ~ sex + factor(occ90ly),
                  data = d %>% filter(year == y),
                  weights = asecwt)
  return(data.frame(year = y,
                    estimand = "gmean",
                    ratio = exp(c(coef(lm_total)["sexwomen"],
                                  coef(lm_within)["sexwomen"])),
                    quantity = c("total_OLS","within_occupation_OLS"),
                    quantity_label = c("Total\n(OLS coefficient)","Within Occupation\n(OLS coefficient)")))
}

# Make one big data frame of aggregate estimates
estimates <- total %>%
  mutate(quantity = "total",
         quantity_label = "Total") %>%
  bind_rows(within_occupation %>%
              mutate(quantity = "within_occupation",
                     quantity_label = "Within Occupation")) %>%
  bind_rows(within_occupationHours %>%
              mutate(quantity = "within_occupationHours",
                     quantity_label = "Within Occupation\nx Employment Hours")) %>%
  bind_rows(between_occupation %>%
              mutate(quantity = "between_occupation",
                     quantity_label = "Between Occupation")) %>%
  bind_rows(between_occupationHours %>%
              mutate(quantity = "between_occupationHours",
                     quantity_label = "Between Occupation\nx Employment Hours")) %>%
  bind_rows(within_occupation_OLS) %>%
  mutate(quantity_label = fct_relevel(quantity_label,
                                      "Between Occupation",
                                      "Between Occupation\nx Employment Hours",
                                      "Within Occupation\nx Employment Hours",
                                      "Within Occupation",
                                      "Within Occupation\n(OLS coefficient)",
                                      "Total\n(OLS coefficient)",
                                      "Total"))

save(estimates, file = "intermediate/estimates.Rdata")

# Also save the versions that are not aggregated over occupations.
save(within_each_occupation, file = "intermediate/within_each_occupation.Rdata")
save(between_each_occupation, file = "intermediate/between_each_occupation.Rdata")
