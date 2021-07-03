# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))

# Record printed output in a text file
sink("logs/fig03.txt")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load packages
library(tidyverse)
library(reshape2)
library(labelled)

# Load data
d_onset <- readRDS("intermediate/d_onset.Rds") %>%
  filter(YEAR >= 2005)
full_population <- readRDS("intermediate/full_population.Rds") %>%
  filter(YEAR >= 2005)

occupation_titles <- full_population %>%
  select(OCC2010) %>%
  group_by(OCC2010) %>%
  filter(1:n() == 1) %>%
  group_by() %>%
  mutate(occ_title = as.character(to_factor(OCC2010)),
         OCC2010 = factor(OCC2010))

# Find occupations with very different proportions across education and across race
factual <- d_onset %>%
  group_by(OCC2010, EDUC, RACE) %>%
  summarize(weight = sum(ASECWT), 
            .groups = "drop") %>%
  group_by(EDUC, RACE) %>%
  mutate(factual = weight / sum(weight)) %>%
  group_by() %>%
  # Restrict to HS and college
  # and to black and white
  filter(EDUC %in% c("High school","College") & RACE %in% c("Non-Hispanic Black","Non-Hispanic White")) %>%
  # Find the occupations that are allocated very unequally by race
  group_by(OCC2010, EDUC) %>%
  mutate(variance_factual = var(factual)) %>%
  group_by(EDUC, RACE) %>%
  arrange(-variance_factual) %>%
  mutate(variance_index = 1:n()) %>%
  group_by() %>%
  left_join(occupation_titles, by = "OCC2010") %>%
  mutate(RACE = gsub(" ","\n",RACE))

# Find the assignment probabilities if assigned by education alone
counterfactual <- d_onset %>%
  group_by(OCC2010, EDUC) %>%
  summarize(weight = sum(ASECWT), 
            .groups = "drop") %>%
  group_by(EDUC) %>%
  mutate(counterfactual = weight / sum(weight)) %>%
  group_by() %>%
  select(OCC2010, EDUC, counterfactual)

# Look at some of those occupations
print(unique((factual %>%
                filter(variance_index <= 3))$occ_title))

# Produce a plot of the factual assignment rule
plot <- factual %>%
  filter(variance_index <= 3) %>%
  group_by(RACE) %>%
  mutate(factual = factual / sum(factual)) %>%
  mutate(EDUC = case_when(EDUC == "High school" ~ "Among those with exactly\na high school degree",
                          EDUC == "College" ~ "Among those with exactly\na college degree"),
         EDUC = fct_rev(EDUC)) %>%
  mutate(occ_title = gsub("and","and\n",occ_title),
         occ_title = gsub("of","\nof",occ_title)) %>%
  ggplot(aes(x = RACE, y = occ_title, size = factual)) +
  geom_point() +
  facet_wrap(~EDUC, nrow = 2, scales = "free") +
  ggtitle("Factual\nAssignment\nRule") +
  annotate(geom = "rect", xmin = .8, xmax = 1.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  annotate(geom = "rect", xmin = 1.8, xmax = 2.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, face = "bold"))

ggsave("figures/pi_bucket_factual.pdf",
       height = 5, width = 4, plot = plot)

plot <- factual %>%
  filter(variance_index <= 3) %>%
  left_join(counterfactual, by = c("OCC2010","EDUC")) %>%
  mutate(counterfactual = counterfactual / sum(counterfactual)) %>%
  mutate(EDUC = case_when(EDUC == "High school" ~ "Among those with exactly\na high school degree",
                          EDUC == "College" ~ "Among those with exactly\na college degree"),
         EDUC = fct_rev(EDUC)) %>%
  mutate(occ_title = gsub("and","and\n",occ_title),
         occ_title = gsub("of","\nof",occ_title)) %>%
  ggplot(aes(x = RACE, y = occ_title, size = counterfactual)) +
  geom_point() +
  facet_wrap(~EDUC, nrow = 2, scales = "free") +
  ggtitle("Counterfactual\nAssignment\nRule") +
  annotate(geom = "rect", xmin = .8, xmax = 1.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  annotate(geom = "rect", xmin = 1.8, xmax = 2.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, face = "bold"))
ggsave("figures/pi_bucket_counterfactual.pdf",
       height = 5, width = 4, plot = plot)

plot <- factual %>%
  filter(variance_index <= 3) %>%
  left_join(counterfactual, by = c("OCC2010","EDUC")) %>%
  mutate(counterfactual = counterfactual / sum(counterfactual)) %>%
  mutate(EDUC = case_when(EDUC == "High school" ~ "Among those with exactly\na high school degree",
                          EDUC == "College" ~ "Among those with exactly\na college degree"),
         EDUC = fct_rev(EDUC)) %>%
  mutate(occ_title = gsub("and","and\n",occ_title),
         occ_title = gsub("of","\nof",occ_title)) %>%
  ggplot(aes(x = RACE, y = occ_title, size = counterfactual)) +
  geom_point() +
  facet_wrap(~EDUC, nrow = 2, scales = "free") +
  ggtitle("Counterfactual\nAssignment\nRule") +
  annotate(geom = "rect", xmin = .8, xmax = 1.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  annotate(geom = "rect", xmin = 1.8, xmax = 2.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = .5, face = "bold"))
ggsave("figures/pi_bucket_counterfactual_noLabels.pdf",
       height = 5, width = 2, plot = plot)

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
