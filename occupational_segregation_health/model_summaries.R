# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This code produces summaries of the fitted outcome model

source("code/prepare_data.R")
all_data <- prepare_data(target_years = 2005:2020)
d <- all_data$d
categories <- unique(d$RACE)

# Make plot of the age trend to help people understand the GAM
age_curves <- foreach(r = categories, .combine = "rbind") %do% {
  this_case <- d %>% filter(RACE == r)
  r_short <- gsub("-| ","",r)
  load(paste0("intermediate/outcome_",r_short,"_fit.Rdata"))
  X <- model.matrix(fit)
  beta <- coef(fit)
  age_columns <- grepl("AGE",colnames(X))
  weighted_mean_other_columns <- weighted.mean(X[,!age_columns] %*% beta[!age_columns], w = this_case$ASECWT)
  data.frame(Age = this_case$AGE,
             yhat = weighted_mean_other_columns + X[,age_columns] %*% beta[age_columns]) %>%
    group_by(Age) %>%
    summarize(yhat = mean(yhat)) %>%
    mutate(RACE = r)
}
age_curves %>%
  mutate(RACE = fct_relevel(RACE,"Non-Hispanic Black","Non-Hispanic White","Hispanic","Other")) %>%
  ggplot(aes(x = Age, y = yhat, color = RACE, linetype = RACE)) +
  geom_line() +
  ylab("Onset of Work-Limiting Disability") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "gray") +
  theme(legend.title = element_blank()) +
  ggsave("figures/model_age.pdf",
         height = 3, width = 6.5)


# Note what proportion of predicted expected outcomes under the assignment rule are in [0,1]
load("intermediate/counterfactual_predictions_race_interactions.Rdata")
print("Proportion of counterfactual predictions in [0,1]")
print(counterfactual_predictions %>%
        group_by() %>%
        summarize(withinEduc_in_01 = weighted.mean(counterfactual_within_educ >= 0 & counterfactual_within_educ <= 1, w = weight),
                  marginal_in_01 = weighted.mean(counterfactual_marginal >= 0 & counterfactual_marginal <= 1, w = weight)))
