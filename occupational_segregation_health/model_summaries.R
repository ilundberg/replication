# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A gap-closing perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This code produces summaries of the fitted outcome model

source("code/prepare_data.R")
all_data <- prepare_data(target_years = 2005:2020)
load("intermediate/outcome_fit.Rdata")

X <- model.matrix(outcome_fit)
beta <- coef(outcome_fit)

# Make predictions as a function of the occupational composition variables
# Note: These figures vary the proportion in one group.
# Implicitly, increasing that proportion decreases the proportion non-Hispanic white,
# because that is the reference category. This only affects the intercept
# because of the additivity of the model. These are best thought of as
# only model summaries to capture the shape of the curve.
for (predictor in c("Hispanic","NonHispanicBlack","Other")) {
  load(paste0("intermediate/prop_",predictor,"_fit.Rdata"))
  predictor_values <- fitted(fit)
  these_columns <- which(grepl(paste0("prop_",predictor), colnames(X)))
  weighted_mean_other_columns <- weighted.mean(X[,!these_columns] %*% beta[!these_columns], w = all_data$d_onset$ASECWT)
  yhat <- weighted_mean_other_columns + X[,these_columns] %*% beta[these_columns]
  data.frame(predictor_values = predictor_values, 
             yhat = yhat) %>%
    sample_frac(.05) %>%
    ggplot(aes(x = predictor_values, y = yhat)) +
    geom_line() +
    theme_bw() +
    xlab(paste("Proportion",
               ifelse(predictor == "NonHispanicBlack", "Non-Hispanic Black",predictor),
               "in Occupation")) +
    ylab("Onset of Work-Limiting Disability") +
    ggsave(paste0("figures/model_",predictor,".pdf"),
           height = 3, width = 6.5)
}

# Make predictions as a function of age
these_columns <- grepl("AGE",colnames(X))
weighted_mean_other_columns <- weighted.mean(X[,!these_columns] %*% beta[!these_columns], w = all_data$d_onset$ASECWT)
data.frame(Age = all_data$d_onset$AGE,
           yhat = weighted_mean_other_columns + X[,these_columns] %*% beta[these_columns]) %>%
  group_by(Age) %>%
  summarize(yhat = mean(yhat)) %>%
  ggplot(aes(x = Age, y = yhat)) +
  geom_line() +
  ylab("Onset of Work-Limiting Disability") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "gray") +
  ggsave("figures/model_age.pdf",
         height = 3, width = 6.5)

# Make predictions as a function of year
these_columns <- grepl("YEAR|question",colnames(X))
weighted_mean_other_columns <- weighted.mean(X[,!these_columns] %*% beta[!these_columns], w = all_data$d_onset$ASECWT)
data.frame(Year = all_data$d_onset$YEAR,
           new_question = all_data$d_onset$new_question,
           yhat = weighted_mean_other_columns + X[,these_columns] %*% beta[these_columns]) %>%
  group_by(Year, new_question) %>%
  summarize(yhat = mean(yhat)) %>%
  ggplot(aes(x = Year, y = yhat, group = new_question)) +
  geom_vline(xintercept = 2013, color = "gray") +
  geom_line() +
  ylab("Onset of Work-Limiting Disability") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "gray") +
  ggsave("figures/model_year.pdf",
         height = 3, width = 6.5)

# Note what proportion of predicted expected outcomes under the assignment rule are in [0,1]
load("intermediate/counterfactual_predictions.Rdata")
print("Proportion of counterfactual predictions in [0,1]")
print(counterfactual_predictions %>%
        group_by() %>%
        summarize(withinEduc_in_01 = weighted.mean(counterfactual_within_educ >= 0 & counterfactual_within_educ <= 1, w = weight),
                  marginal_in_01 = weighted.mean(counterfactual_marginal >= 0 & counterfactual_marginal <= 1, w = weight)))
