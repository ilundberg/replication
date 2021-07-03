# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))

# Record printed output in a text file
sink("logs/fig06.txt")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load packages
library(tidyverse)
library(reshape2)

disparity_results <- readRDS("intermediate/disparity_results.Rds")

# Panel A: Full population
plot <- disparity_results$disparity_estimate %>%
  group_by() %>%
  filter(estimand == "prevalence" & target == "proportion") %>%
  mutate(RACE = gsub(" ","\n",RACE),
         RACE = fct_relevel(RACE,"Non-Hispanic\nBlack","Non-Hispanic\nWhite","Hispanic","Other")) %>%
  ggplot(aes(x = RACE, y = point, 
             ymin = point - qnorm(.975) * se, 
             ymax = point + qnorm(.975) * se)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  geom_text(aes(y = 0, vjust = -.5,
                label = paste0(format(round(100 * point,1), nsmall = 1), "%")),
            color = "white", fontface = "bold", size = 6) +
  ylab("Work-Limiting Disability") +
  xlab("Race / Ethnicity") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"))
ggsave("figures/disability_by_race.pdf",
       height = 3.5, width = 5, plot = plot)

# Panel B: Onset next year among those employed with no disability this year
plot <- disparity_results$disparity_estimate %>%
  group_by() %>%
  filter(estimand == "onset" & target == "proportion") %>%
  mutate(RACE = gsub(" ","\n",RACE),
         RACE = fct_relevel(RACE,"Non-Hispanic\nBlack","Non-Hispanic\nWhite","Hispanic","Other")) %>%
  ggplot(aes(x = RACE, y = point, 
             ymin = point - qnorm(.975) * se, 
             ymax = point + qnorm(.975) * se)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  geom_text(aes(y = 0, vjust = -.5,
                label = paste0(format(round(100 * point,1), nsmall = 1), "%")),
            color = "white", fontface = "bold", size = 6) +
  ylab("Work-Limiting Disability") +
  xlab("Race / Ethnicity") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"))
ggsave("figures/onset_by_race.pdf",
       height = 3.5, width = 5, plot = plot)

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))