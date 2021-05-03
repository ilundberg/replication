
setwd("/Users/iandl/Dropbox/Dissertation/gender_gap_occupations")
source("code/a_prepare_environment.R")

# Load Consumer Price Index for inflation adjustment
cpi <- read_csv("CPIAUCSL.csv", col_types = "cn") %>%
  mutate(YEAR = as.numeric(str_replace(DATE,"-.*","")),
         # Shift year by 1 since incomes reported in year X will refer to year X - 1
         YEAR = YEAR + 1) %>%
  group_by(YEAR) %>%
  summarize(cpi = mean(CPIAUCSL)) %>%
  rename(year = YEAR)
# Make this in 2018 dollars (year 2019 for reporting)
cpi$cpi <- cpi$cpi / cpi$cpi[cpi$year == 2019]

# Load Current Population Survey Data
data_all <- read_dta("cps_00042.dta") %>%
  filter(uhrsworkly != 999 & wkswork1 > 0 & uhrsworkly > 0 & incwage > 0) %>%
  left_join(cpi, by = "year") %>%
  mutate(sex = ifelse(sex == 1, "men", "women"),
         wage = incwage / (uhrsworkly * wkswork1),
         wage = wage / cpi,
         # Create log wages with bottom and top codes to be comparable
         # to standard practice in past work.
         lnwage = log(case_when(wage > 1 & wage < 100 ~ wage,
                                wage <= 1 ~ 1,
                                wage >= 100 ~ 100))) %>%
  # Bin years for some analyses. Denote with the midpoint
  mutate(year_bin = case_when(year >= 1976 & year <= 1989 ~ 1982.5,
                              year >= 1990 & year <= 2004 ~ 1997,
                              year >= 2005 & year <= 2019 ~ 2012)) %>%
  # Bin hours for some analyses
  mutate(hours = uhrsworkly - uhrsworkly %% 5)

# Restrict the range of work hours.
# This avoids hourly wages that mayt be misreported
# as well as reducing overlap issues when examining gaps within hours.
d <- data_all %>%
  filter(uhrsworkly >= 20 & uhrsworkly < 70)
save(d, file = "intermediate/d.Rdata")

print("Population kept in the hours restriction")
print(sum(d$asecwt) / sum(data_all$asecwt))
