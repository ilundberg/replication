# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))

# Record printed output in a text file
sink("logs/fig15.txt")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load packages
library(tidyverse)
library(reshape2)
library(foreach)

d <- readRDS("intermediate/d.Rds")
categories <- unique(d$RACE)

# Make plot of the age trend to help people understand the GAM
age_curves <- foreach(r = categories, .combine = "rbind") %do% {
  this_case <- d %>% filter(RACE == r) %>% filter(YEAR >= 2005)
  r_short <- gsub("-| ","",r)
  fit <- readRDS(paste0("intermediate/outcome_",r_short,"_fit.Rds"))
  X <- model.matrix(fit)
  beta <- coef(fit)
  age_columns <- grepl("AGE",colnames(X))
  weighted_mean_other_columns <- weighted.mean(X[,!age_columns] %*% beta[!age_columns], w = this_case$ASECWT)
  data.frame(Age = this_case$AGE,
             yhat = weighted_mean_other_columns + X[,age_columns] %*% beta[age_columns]) %>%
    group_by(Age) %>%
    summarize(yhat = mean(yhat),
              .groups = "drop") %>%
    mutate(RACE = r)
}
plot <- age_curves %>%
  mutate(RACE = fct_relevel(RACE,"Non-Hispanic Black","Non-Hispanic White","Hispanic","Other")) %>%
  ggplot(aes(x = Age, y = yhat, color = RACE, linetype = RACE)) +
  geom_line() +
  ylab("Onset of Work-Limiting Disability") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "gray") +
  theme(legend.title = element_blank())
ggsave("figures/model_age.pdf",
       height = 3, width = 6.5, plot = plot)


# Note what proportion of predicted expected outcomes under the assignment rule are in [0,1]
counterfactual_predictions <- readRDS("intermediate/counterfactual_predictions_race_interactions.Rds")
print("Proportion of counterfactual predictions in [0,1]")
print(data.frame(counterfactual_predictions %>%
                   group_by() %>%
                   summarize(withinEduc_in_01 = weighted.mean(counterfactual_within_educ >= 0 & counterfactual_within_educ <= 1, w = weight),
                             marginal_in_01 = weighted.mean(counterfactual_marginal >= 0 & counterfactual_marginal <= 1, w = weight),
                             .groups = "drop")))

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))

