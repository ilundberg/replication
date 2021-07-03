# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))

# Record printed output in a text file
sink("logs/sample_restrictions.txt")

print("Sample restrictions")
# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load packages
library(tidyverse)

# Load data. Restrict to years after 2005 for the main analyses in order to use replicate weights.
full_population <- readRDS("intermediate/full_population.Rds") %>%
  filter(YEAR >= 2005)
linked <- readRDS("intermediate/linked.Rds") %>%
  filter(YEAR >= 2005)
d_onset <- readRDS("intermediate/d_onset.Rds") %>%
  filter(YEAR >= 2005)
d <- readRDS("intermediate/d.Rds") %>%
  filter(YEAR >= 2005)

# Print sample restrictions
print("Full population size")
print(nrow(full_population))
print("Oversamples")
print(sum(full_population$CPSIDP == 0))
print("From March basic CPS")
print(sum(full_population$CPSIDP != 0))
print("Not linked (person-years)")
print(sum(full_population$CPSIDP != 0) - 2 * nrow(linked))
print("Linked (persons)")
print(nrow(linked))
print("Risk of recovery")
print(sum(linked$lag))
print("History of health limitations")
print(sum(linked$QUITSICK & !linked$lag))
print("Not employed")
print(sum(!linked$employed & !linked$lag & !linked$QUITSICK))
print("Risk of onset")
print(nrow(d_onset))
print("Lack of common support")
print(nrow(d_onset) - nrow(d))
print("Occupation analyses")
print(nrow(d))

print("Occupations that remain after common support restriction:")
print(length(unique(d$OCC2010)))

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))