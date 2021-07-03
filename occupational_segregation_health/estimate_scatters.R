# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
sink("logs/estimate_scatters.txt")

print("Produce estimates for scatter plots")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load packages
library(tidyverse)
library(reshape2)
library(labelled)
library(foreach)
library(doParallel)
library(mgcv)

# Prepare for parallel computing
cl <- makeCluster(4)
registerDoParallel(cl)

# Load the data
full_population <- readRDS("intermediate/full_population.Rds") %>%
  filter(YEAR >= 2005)
d_onset <- readRDS("intermediate/d_onset.Rds") %>%
  filter(YEAR >= 2005)
d <- readRDS("intermediate/d.Rds") %>%
  filter(YEAR >= 2005)

# Extract the occupational titles to facilitate labeling points
occupation_titles <- full_population %>%
  select(OCC2010) %>%
  group_by(OCC2010) %>%
  filter(1:n() == 1) %>%
  group_by() %>%
  mutate(occ_title = as.character(to_factor(OCC2010)),
         OCC2010 = factor(OCC2010))

# I will fit models on d_onset
# I will predict for to_predict, which has one row per occupation
to_predict <- d_onset %>%
  group_by(OCC2010) %>%
  summarize(size = sum(ASECWT)) %>%
  group_by() %>%
  left_join(occupation_titles, by = "OCC2010")

# Fit the proportion in each category of race
# This model does partial pooling to improve precision of estimates
categories <- unique(d_onset$RACE)
category_proportions <- foreach(r = categories, .combine = "rbind", .packages = c("mgcv","tidyverse")) %dopar% {
  fit <- bam(as.numeric(RACE == r) ~ s(OCC2010, bs = "re"),
             data = d_onset,
             weights = ASECWT)
  return(to_predict %>%
           mutate(category = r,
                  proportion = predict(fit, newdata = to_predict)))
}
# Fit the outcome model
# This model does partial pooling to improve precision of estimates
fit_outcome <- bam(y ~ s(OCC2010, bs = "re"),
                   data = d_onset,
                   weights = ASECWT)
# Combine those in a dataset for the scatter
for_scatter <- category_proportions %>%
  left_join(to_predict %>%
              select(OCC2010) %>%
              mutate(y = predict(fit_outcome, newdata = to_predict)),
            by = "OCC2010")

# Get the slope of the best-fit line as a function of each category proportion
scatter_slopes <- foreach(r = categories, .combine = "c", .packages = "tidyverse") %dopar% {
  fit <- lm(y ~ proportion,
            weights = size,
            data = for_scatter %>%
              filter(category == r))
  return(coef(fit)[2])
}
scatter_slopes <- format(round(scatter_slopes,2),nsmall = 2)
names(scatter_slopes) <- categories

# Note specific occupations that appear in the main text
print(
  data.frame(for_scatter %>%
               filter(occ_title %in% c("Nursing, Psychiatric, and Home Health Aides",
                                       "Agricultural workers, nec",
                                       "Industrial Truck and Tractor Operators",
                                       "Personal Care Aides",
                                       "Roofers")) %>%
               select(occ_title, category, proportion) %>%
               spread(key = category, value = proportion))
)
print("Later example uses CEOs")
print(data.frame(for_scatter %>%
                   filter(occ_title == "Chief executives and legislators/public administration") %>%
                   select(occ_title, category, proportion, y)))

# Save the results to be used in figures
saveRDS(for_scatter, file = "intermediate/for_scatter.Rds")
saveRDS(scatter_slopes, file = "intermediate/scatter_slopes.Rds")

print("Time spent")
print(difftime(Sys.time(),t0))

# Stop the parallel computing cluster
stopCluster(cl)

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))

