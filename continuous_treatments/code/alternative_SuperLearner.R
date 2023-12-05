

library(SuperLearner)
library(grf)

source("code/shorten_race.R")

# First part of code is base learners

# Base learner: Additive logistic regression
SL.glm.additive <- function (Y, X, newX, family, obsWeights, model = TRUE, ...) 
{
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  fit.glm <- glm(y ~ log(income) + race + educJoint + log(wealth), 
                 data = X, 
                 family = family, 
                 weights = obsWeights, 
                 model = model)
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm.additive"
  out <- list(pred = pred, fit = fit)
  return(out)
}

# Base learner: GAM from main text
SL.gam <- function (Y, X, newX, family, obsWeights, model = TRUE, ...) 
{
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  fit.gam <- mgcv::gam(y ~ s(log(income)) + 
                         log(income)*educJoint +
                         log(income)*race +
                         log(income)*log(wealth) + 
                         s(log(wealth)),
                       data = X,
                       family = binomial, 
                       weights = obsWeights,
                       model = model)
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  pred <- predict(fit.gam, newdata = newX, type = "response")
  fit <- list(object = fit.gam)
  class(fit) <- "SL.gam"
  out <- list(pred = pred, fit = fit)
  return(out)
}
# Base learner: Random forest
SL.grf <- function (Y, X, newX, family, obsWeights, model = TRUE, ...) 
{
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  fit.grf <- regression_forest(X = model.matrix(~ log(income) + race + educJoint + log(wealth),
                                                data = X),
                               Y = Y,
                               sample.weights = obsWeights,
                               tune.parameters = "all")
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  pred <- predict(fit.grf, 
                  newdata =  model.matrix(~ log(income) + race + educJoint + log(wealth),
                                          data = newX), 
                  type = "response")$predictions
  fit <- list(object = fit.grf)
  class(fit) <- "SL.grf"
  out <- list(pred = pred, fit = fit)
  return(out)
}
# Base learner: Local logistic regression with local weighting from random forest
SL.grf.glm <- function (Y, X, newX, family, obsWeights, model = TRUE, ...) 
{
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  fit.grf <- regression_forest(X = model.matrix(~ log(income) + race + educJoint + log(wealth),
                                                data = X),
                               Y = Y,
                               sample.weights = obsWeights,
                               tune.parameters = "all")
  if (is.matrix(newX)) {
    newX = as.data.frame(newX)
  }
  
  # In weights, each row is a case in newX
  # each column is a case in X
  weights <- get_forest_weights(fit.grf, 
                                newdata = model.matrix(~ log(income) + race + educJoint + log(wealth),
                                                       data = newX))
  
  # Create local estimates from a locally-weighted logistic regression
  local_estimates <- foreach(
    i = 1:nrow(newX), 
    .combine = "c"
  ) %dopar% {
    fit <- glm(y ~ log(income) + educJoint + race + log(wealth), 
               data = X,
               weights = weights[i,] * obsWeights,
               family = binomial)
    estimate <- predict(fit, newdata = newX[i,], type = "response")
    return(estimate)
  }
  
  fit <- list(object = fit.grf)
  class(fit) <- "SL.grf.glm"
  out <- list(pred = local_estimates, fit = fit)
  return(out)
}

# Causal estimator function that internally cally SuperLearner
causal_estimator <- function(
    outcome = "enrolled_any", 
    delta = 10e3,
    n_folds = 5,
    learning_pubids = NULL
) {
  
  source("code/load_data.R")
  
  d <- load_data(outcome, impute = T, bs = F, learning_pubids = learning_pubids)
  
  # Create data frame for prediction
  to_predict <- d %>%
    bind_rows(d %>%
                mutate(income = income + delta))
  
  folded <- d %>%
    # Create folds within strata of categorical covariates
    group_by(educJoint, label_wealth, race) %>%
    # Randomly sample folds 1:5
    mutate(fold = sample(rep(1:n_folds, ceiling(n() / n_folds))[1:n()])) %>%
    ungroup()
  
  # Fit the Super Learner
  SL.out <- SuperLearner(Y = as.numeric(d$y),
                         X = d,
                         newX = to_predict,
                         family = binomial(),
                         SL.library = c("SL.glm.additive","SL.gam","SL.grf","SL.grf.glm"),
                         obsWeights = d$w,
                         cvControl = list(
                           V = n_folds,
                           validRows = lapply(1:n_folds, function(f) {
                             which(folded$fold == f)
                           })
                         ))
  
  # Effect estimates
  ensemble_effects <- d %>%
    mutate(effect = SL.out$SL.predict[(nrow(d) + 1):nrow(to_predict),] -
             SL.out$SL.predict[1:nrow(d)],)
  return(list(effects = ensemble_effects,
              ensemble_weights = SL.out$coef))
}

# Run the function to make SuperLearner estimates of the effect of a $10k increase
sl_10k <- causal_estimator(outcome = "enrolled_any", delta = 10e3)

# Load the original estimates for comparison
original <- readRDS("intermediate/causal_enrolled_any.RDS")

# Create a comparison data frame with both estimates
comparison <- sl_10k$effects %>%
  rename(SuperLearner = effect) %>%
  left_join(original$estimate %>% 
              filter(delta == 10e3) %>% 
              select(PUBID,effect) %>% 
              rename(MainText = effect), 
            by = "PUBID") %>%
  pivot_longer(cols = c("MainText","SuperLearner"),
               names_to = "Method")

# Visualize estimates from the two methods, by various subgroups

comparison %>%
  group_by(educJoint, Method) %>%
  summarize(effect = weighted.mean(value, w = w),
            .groups = "drop") %>%
  mutate(educJoint = fct_rev(educJoint),
         Method = case_when(Method == "MainText" ~ "Main Text (GAM)",
                            Method == "SuperLearner" ~ "Super Learner")) %>%
  ggplot(aes(x = educJoint, y = effect, color = Method, shape = Method)) +
  geom_point(position = position_dodge(width = .2)) +
  geom_text(aes(label = format(round(effect,3),nsmall=3)),
            nudge_x = c(-.25,.25),
            size = 3, fontface = "bold",
            show.legend = F) +
  ylab("Effect on College Enrollment") +
  xlab("Parent Education") +
  theme_bw() +
  scale_color_manual(values = c("darkgray","blue")) +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("figures/SuperLearner_comparison_education.pdf",
       height = 3, width = 6)

comparison %>%
  group_by(label_income, Method) %>%
  summarize(effect = weighted.mean(value, w = w),
            .groups = "drop") %>%
  mutate(Method = case_when(Method == "MainText" ~ "Main Text (GAM)",
                            Method == "SuperLearner" ~ "Super Learner")) %>%
  ggplot(aes(x = label_income, y = effect, color = Method, shape = Method)) +
  geom_point(position = position_dodge(width = .2)) +
  geom_text(aes(label = format(round(effect,3),nsmall=3)),
            nudge_x = c(-.25,.25),
            size = 3, fontface = "bold",
            show.legend = F) +
  ylab("Effect on College Enrollment") +
  xlab("Parental Income") +
  theme_bw() +
  scale_color_manual(values = c("darkgray","blue")) +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("figures/SuperLearner_comparison_income.pdf",
       height = 3, width = 6)

comparison %>%
  group_by(label_wealth, Method) %>%
  summarize(effect = weighted.mean(value, w = w),
            .groups = "drop") %>%
  mutate(Method = case_when(Method == "MainText" ~ "Main Text (GAM)",
                            Method == "SuperLearner" ~ "Super Learner")) %>%
  ggplot(aes(x = label_wealth, y = effect, color = Method, shape = Method)) +
  geom_point(position = position_dodge(width = .2)) +
  geom_text(aes(label = format(round(effect,3),nsmall=3)),
            nudge_x = c(-.25,.25),
            size = 3, fontface = "bold",
            show.legend = F) +
  ylab("Effect on College Enrollment") +
  xlab("Parental Wealth") +
  theme_bw() +
  scale_color_manual(values = c("darkgray","blue")) +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("figures/SuperLearner_comparison_wealth.pdf",
       height = 3, width = 6)

comparison %>%
  group_by(race, Method) %>%
  summarize(effect = weighted.mean(value, w = w),
            .groups = "drop") %>%
  mutate(Method = case_when(Method == "MainText" ~ "Main Text (GAM)",
                            Method == "SuperLearner" ~ "Super Learner"),
         race = shorten_race(race)) %>%
  ggplot(aes(x = race, y = effect, color = Method, shape = Method)) +
  geom_point(position = position_dodge(width = .2)) +
  geom_text(aes(label = format(round(effect,3),nsmall=3)),
            nudge_x = c(-.25,.25),
            size = 3, fontface = "bold",
            show.legend = F) +
  ylab("Effect on College Enrollment") +
  xlab("Race") +
  theme_bw() +
  scale_color_manual(values = c("darkgray","blue")) +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("figures/SuperLearner_comparison_race.pdf",
       height = 3, width = 6)

# Visualize the ensemble weights
data.frame(sl_10k$ensemble_weights) %>%
  rename_all(.funs = function(x) "weight") %>%
  rownames_to_column(var = "learner") %>%
  mutate(learner = factor(case_when(learner == "SL.glm.additive_All" ~ 1,
                                    learner == "SL.gam_All" ~ 2,
                                    learner == "SL.grf_All" ~ 3,
                                    learner == "SL.grf.glm_All" ~ 4),
                          labels = c("(1)\nAdditive Logistic\nRegression",
                                     "(2)\nMain Text Specification:\nLogistic Regression\nwith Selected Interactions\nand Nonlinear Smooths",
                                     "(3)\nRandom Forest",
                                     "(4)\nLocal Additive Logistic Regression\nwith Random Forest Weights"))) %>%
  ggplot(aes(x = learner, y = weight)) +
  geom_bar(stat = "identity") +
  geom_text(y = .02, vjust = 0, color = "white", fontface = "bold",
            aes(label = paste0(round(100*weight),"%"))) +
  xlab("Base Learner") +
  ylab("Weight in Super Learner") +
  theme_bw()
ggsave("figures/SuperLearner_weights.pdf",
       height = 3, width = 8)




