
sink("../logs/motherhood_histograms.txt")

t0 <- Sys.time()
print(t0)

library(tidyverse)
theme_set(theme_bw())
library(Amelia)
library(foreach)
library(doParallel)
library(doRNG)
cl <- makeCluster(6)
registerDoParallel(cl)

bs_reps <- 100

set.seed(14850)

# Load the prepared data
data <- readRDS("../intermediate/motherhood.RDS")

# Visualize age distribution
data %>%
  filter(treated) %>%
  ggplot(aes(x = age_at_birth)) +
  geom_histogram(alpha = .8) +
  facet_wrap(~sex, ncol = 1) +
  xlab("Age at Birth") +
  ylab("Count")
ggsave("../figures/motherhood_age_at_birth.pdf",
       height = 4, width = 3)

# Visualize age 2 distribution
data %>%
  mutate(set = case_when(sex == "Men" & treated ~ "Fathers",
                         sex == "Women" & treated ~ "Mothers",
                         sex == "Men" & !treated ~ "Childless Men",
                         sex == "Women" & !treated ~ "Childless Women"),
         set = fct_rev(set)) %>%
  ggplot(aes(x = age_2)) +
  geom_histogram(alpha = .8, bins = 30) +
  facet_wrap(~ set, scales = "free") +
  xlab("Age at Wage Measurement") +
  ylab("Count")
ggsave("../figures/motherhood_age_wage.pdf",
       height = 4, width = 5)

# Age distribution at birth and at wage measurement, for parents
data %>%
  filter(treated) %>%
  mutate(gap_before = age_1 - age_at_birth,
         gap_after = age_2 - age_at_birth) %>%
  select(sex, starts_with("gap_")) %>%
  pivot_longer(cols = -sex) %>%
  mutate(name = case_when(name == "gap_before" ~ "Pre-Observation\nto Birth",
                          name == "gap_after" ~ "Birth to\nPost-Observation"),
         name = fct_rev(name)) %>%
  ggplot(aes(x = value)) +
  facet_grid(sex ~ name, scales = "free_x") +
  geom_histogram(bins = 10) +
  xlab("Time in Years From Birth") +
  ylab("Count")
ggsave("../figures/motherhood_age_gaps.pdf",
       height = 4, width = 5)

# Visualize year distribution for wage
data %>%
  mutate(set = case_when(sex == "Men" & treated ~ "Fathers",
                         sex == "Women" & treated ~ "Mothers",
                         sex == "Men" & !treated ~ "Childless Men",
                         sex == "Women" & !treated ~ "Childless Women"),
         set = fct_rev(set)) %>%
  ggplot(aes(x = as.numeric(year_2))) +
  geom_histogram(alpha = .8, binwidth = 1) +
  facet_wrap(~ set, scales = "free_y") +
  xlab("Year at Wage Measurement") +
  ylab("Count") +
  theme(plot.margin = unit(c(.05,.1,.05,.05),"in"))
ggsave("../figures/motherhood_year_wage.pdf",
       height = 4, width = 5)

# Visualize wage distribution
data %>%
  mutate(set = case_when(sex == "Men" & treated ~ "Fathers",
                         sex == "Women" & treated ~ "Mothers",
                         sex == "Men" & !treated ~ "Childless Men",
                         sex == "Women" & !treated ~ "Childless Women"),
         set = fct_rev(set)) %>%
  ggplot(aes(x = wage)) +
  geom_histogram(alpha = .8, bins = 30) +
  facet_wrap(~ set, scales = "free_y") +
  xlab("Log Wage") +
  ylab("Count") +
  theme(plot.margin = unit(c(.05,.1,.05,.05),"in"))
ggsave("../figures/motherhood_log_wage.pdf",
       height = 4, width = 5)
data %>%
  mutate(set = case_when(sex == "Men" & treated ~ "Fathers",
                         sex == "Women" & treated ~ "Mothers",
                         sex == "Men" & !treated ~ "Childless Men",
                         sex == "Women" & !treated ~ "Childless Women"),
         set = fct_rev(set)) %>%
  ggplot(aes(x = exp(wage))) +
  geom_histogram(alpha = .8, bins = 30) +
  facet_wrap(~ set, scales = "free_y") +
  xlab("Wage") +
  ylab("Count") +
  theme(plot.margin = unit(c(.05,.1,.05,.05),"in"))
ggsave("../figures/motherhood_wage.pdf",
       height = 4, width = 5)

print("Finish time")
print(Sys.time())
print("Time spent")
difftime(Sys.time(),t0)


sink()