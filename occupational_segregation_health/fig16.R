# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))

# Record printed output in a text file
sink("logs/fig16.txt")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load packages
library(tidyverse)
library(reshape2)

# Make a big data frame with the main and alternate specification point estimates
forplot <- readRDS("intermediate/counterfactual_results.Rds")$counterfactual_estimate %>%
  mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
  filter(estimand %in% c("factual","counterfactual") & target == "proportion") %>% 
  mutate(facet = "A) Original specification\nData in 2005-2020") %>%
  # Under marginal equalization
  bind_rows(readRDS("intermediate/counterfactual_results.Rds")$counterfactual_estimate %>%
              mutate(estimand = ifelse(estimand == "counterfactual_marginal","counterfactual",as.character(estimand))) %>%
              filter(estimand %in% c("factual","counterfactual") & target == "proportion") %>% 
              mutate(facet = "B) Under marginal occupational equalization\nData in 2005-2020")) %>%
  rename(estimate = point) %>%
  group_by() %>%
  select(-se, -target) %>%
  # With additional controls
  bind_rows(readRDS("intermediate/additionalControls.Rds") %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand %in% c("factual","counterfactual")) %>% 
              mutate(facet = "C) Restricted to those without other reported disabilities\nData in 2009--2020")) %>% 
  # Without immigrants
  bind_rows(readRDS("intermediate/withoutImmigrants.Rds") %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand %in% c("factual","counterfactual")) %>% 
              mutate(facet = "D) Restricted to those who are not foreign born\nData in 2005--2020")) %>% 
  # Forward-linkig weight
  bind_rows(readRDS("intermediate/forwardLinkingWeight.Rds") %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand %in% c("factual","counterfactual")) %>% 
              mutate(facet = "E) Weights designed for linked sample\nData in 2005--2020")) %>% 
  # Full period starting in 1988
  bind_rows(readRDS("intermediate/from1988.Rds") %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand %in% c("factual","counterfactual")) %>% 
              mutate(facet = "F) Longer period with fewer control variables\nData in 1988-2020")) %>% 
  # Before questionnaire redesign
  bind_rows(readRDS("intermediate/beforeRedesign.Rds") %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand %in% c("factual","counterfactual")) %>% 
              mutate(facet = "G) Traditional question order and wording\nData in 2005-2014")) %>% 
  # After questionnaire redesign
  bind_rows(readRDS("intermediate/afterRedesign.Rds") %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand %in% c("factual","counterfactual")) %>% 
              mutate(facet = "H) With questionnaire changes\nData in 2014-2020")) %>% 
  # Doubly robust
  bind_rows(readRDS("intermediate/doublyRobust.Rds") %>%
              mutate(estimand = ifelse(estimand == "counterfactual_within_educ","counterfactual",as.character(estimand))) %>%
              filter(estimand %in% c("factual","counterfactual")) %>% 
              mutate(facet = "I) Doubly-robust estimates\nData in 2005-2020")) %>% 
  mutate(RACE = gsub(" ","\n",RACE)) %>%
  mutate(RACE = fct_relevel(RACE,"Non-Hispanic\nBlack","Non-Hispanic\nWhite","Hispanic","Other")) %>%
  spread(key = estimand, value = estimate)
plot <- forplot %>%
  ggplot(aes(x = RACE, xend = RACE, y = factual, yend = counterfactual)) + 
  geom_segment(arrow = arrow(length = unit(.1,"in"))) + 
  facet_wrap(~facet, ncol = 3) +
  ylab("Onset of Work-Limiting Disability") +
  xlab("Race / Ethnicity") +
  theme_bw() +
  theme(strip.text = element_text(hjust = 0))
ggsave("figures/alternative_specifications.pdf",
       height = 10, width = 11, plot = plot)

# For slides, make plots of each panel separately
for (specific_panel in LETTERS[2:9]) {
  plot <- forplot %>%
    filter(grepl("^A",facet) | grepl(paste0("^",specific_panel),facet)) %>%
    ggplot(aes(x = RACE, xend = RACE, y = factual, yend = counterfactual)) + 
    geom_segment(arrow = arrow(length = unit(.1,"in"))) + 
    facet_wrap(~facet, ncol = 3) +
    ylab("Onset of Work-Limiting Disability") +
    xlab("Race / Ethnicity") +
    theme_bw() +
    theme(strip.text = element_text(hjust = 0))
  ggsave(paste0("figures/alternative_specifications_",specific_panel,".pdf"),
         height = 3, width = 8, plot = plot)
}

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))