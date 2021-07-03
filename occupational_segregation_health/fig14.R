# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))

# Load data
full_population <- readRDS("intermediate/full_population.Rds")

# Record printed output in a text file
sink("logs/fig14.txt")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load packages
library(tidyverse)
library(reshape2)

plot <- full_population %>%
  group_by(YEAR) %>%
  summarize(weight = sum(ASECWT),
            .groups = "drop") %>%
  group_by() %>%
  mutate(weight = weight / sum(weight)) %>%
  ggplot(aes(x = YEAR, y = weight))  +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  ylab("Proportion of total weight")
ggsave("figures/weight_over_time.pdf",
       height = 3, width = 5, plot = plot)

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))