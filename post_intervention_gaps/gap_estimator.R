
# Ian Lundberg (ilundberg@princeton.edu)
# Estimation function for post-intervention gaps

# This function is called in class_gap_example.R

# I will assume data contains the following variables:
# X: group membership (binary)
# treated: binary treatment indicator
# outcome: outcome variable to be modeled by OLS
# weight: the sample weight variable
# id: unit identifier

gap_estimator <- function(train_sample, predict_sample, outcome_formula, treatment_formula, treatment_formula_marginal, learner = "glm") {
  # Fit models
  if (learner == "glm") {
    m_fit <- glm(treatment_formula,
                 data = train_sample,
                 family = binomial(link = "logit"),
                 weights = weight)
    m_fit_noGroup <- glm(treatment_formula_marginal,
                         data = train_sample,
                         family = binomial(link = "logit"),
                         weights = weight)
    g_fit <- glm(outcome_formula,
                 data = train_sample,
                 weights = weight)
    m_fitted_train_sample <- predict(m_fit, newdata = train_sample, type = "response")
    m_fitted_predict_sample <- predict(m_fit, newdata = predict_sample, type = "response")
    m_fitted_noGroup_train_sample <- predict(m_fit_noGroup, newdata = train_sample, type = "response")
    m_fitted_noGroup_predict_sample <- predict(m_fit_noGroup, newdata = predict_sample, type = "response")
    g_fitted_train_sample <- predict(g_fit, newdata = train_sample)
    g_fitted_predict_sample <- predict(g_fit, newdata = predict_sample)
    g0_fitted_predict_sample <- predict(g_fit, newdata = predict_sample %>% mutate(treated = F))
    g1_fitted_predict_sample <- predict(g_fit, newdata = predict_sample %>% mutate(treated = T))
  } else if (learner == "ranger") {
    m_fit <- ranger(treatment_formula,
                    data = train_sample %>%
                      mutate(treated = as.numeric(treated)),
                    case.weights = train_sample$weight)
    m_fit_noGroup <- ranger(treatment_formula_marginal,
                            data = train_sample %>%
                              mutate(treated = as.numeric(treated)),
                            case.weights = train_sample$weight)
    g_fit <- ranger(outcome_formula,
                    data = train_sample,
                    case.weights = train_sample$weight)
    m_fitted_train_sample <- predict(m_fit, data = train_sample)$predictions
    m_fitted_predict_sample <- predict(m_fit, data = predict_sample)$predictions
    m_fitted_noGroup_train_sample <- predict(m_fit_noGroup, data = train_sample)$predictions
    m_fitted_noGroup_predict_sample <- predict(m_fit_noGroup, data = predict_sample)$predictions
    g_fitted_train_sample <- predict(g_fit, data = train_sample)$predictions
    g_fitted_predict_sample <- predict(g_fit, data = predict_sample)$predictions
    g0_fitted_predict_sample <- predict(g_fit, data = predict_sample %>% mutate(treated = F))$predictions
    g1_fitted_predict_sample <- predict(g_fit, data = predict_sample %>% mutate(treated = T))$predictions
  } else {
    stop("ERROR: learner is not glm or ranger")
  }
  
  if (all(train_sample$id == predict_sample$id)) {
    print("Marginal distribution of treatment under each intervention:")
    print(predict_sample %>%
            group_by() %>%
            mutate(conditional = m_fitted_noGroup_predict_sample,
                   marginal = weighted.mean(train_sample$treated, w = train_sample$weight)) %>%
            group_by(X) %>%
            summarize(conditional = weighted.mean(conditional, w = weight),
                      marginal = weighted.mean(marginal, w = weight),
                      as_observed = weighted.mean(treated, w = weight)))
  }
  
  weights <- predict_sample %>%
    group_by() %>%
    mutate(p_treated_hat = m_fitted_predict_sample,
           mhat = ifelse(treated, p_treated_hat, 1 - p_treated_hat),
           conditional_prob_hat = m_fitted_noGroup_predict_sample,
           marginal_prob_hat = weighted.mean(treated, w = weight),
           eta = weight) %>%
    mutate(equalize1 = eta / mhat * treated,
           equalize0 = eta / mhat * !treated,
           marginal = eta * marginal_prob_hat / mhat * treated + 
             eta * (1 - marginal_prob_hat) / mhat * !treated,
           conditional = eta * conditional_prob_hat / mhat * treated + 
             eta * (1 - conditional_prob_hat) / mhat * !treated,
           unadjusted = eta) %>%
    select(id, X, outcome, equalize1, equalize0, marginal, conditional, unadjusted)
  
  ipw <- weights %>%
    melt(id = c("id","X","outcome"), variable.name = "estimand", value.name = "weight") %>%
    group_by(X, estimand) %>%
    summarize(estimate = weighted.mean(outcome, w = weight)) %>%
    spread(key = X, value = estimate) %>%
    mutate(IPW = `TRUE` - `FALSE`) %>%
    select(estimand, IPW)
  
  augmentation <- weights %>%
    mutate(error = g_fitted_predict_sample - outcome) %>%
    select(-outcome) %>%
    melt(id = c("id","X","error"), variable.name = "estimand", value.name = "weight") %>%
    group_by(X, estimand) %>%
    summarize(estimate = weighted.mean(error, w = weight)) %>%
    spread(key = X, value = estimate) %>%
    mutate(Augmentation = `TRUE` - `FALSE`) %>%
    select(estimand, Augmentation)
  
  imputation <- predict_sample %>%
    group_by() %>%
    mutate(yhat0 = g0_fitted_predict_sample,
           yhat1 = g1_fitted_predict_sample,
           mhat_conditional = m_fitted_noGroup_predict_sample,
           mhat_marginal = weighted.mean(train_sample$treated, w = train_sample$weight),
           equalize0 = yhat0,
           equalize1 = yhat1,
           conditional = yhat1 * mhat_conditional + yhat0 * (1 - mhat_conditional),
           marginal = yhat1 * mhat_marginal + yhat0 * (1 - mhat_marginal),
           unadjusted = outcome) %>%
    select(X, equalize0, equalize1, marginal, conditional, unadjusted, weight) %>%
    melt(id = c("X","weight"), variable.name = "estimand") %>%
    group_by(X, estimand) %>%
    summarize(estimate = weighted.mean(value, w = weight)) %>%
    spread(key = X, value = estimate) %>%
    mutate(Imputation = `TRUE` - `FALSE`) %>%
    select(estimand, Imputation)
  
  aipw <- imputation %>%
    left_join(augmentation, by = "estimand") %>%
    mutate(AIPW = Imputation - Augmentation) %>%
    select(estimand, AIPW)
  
  return(ipw %>%
           left_join(imputation, by = "estimand") %>%
           left_join(aipw, by = "estimand") %>%
           melt(id = "estimand", variable.name = "strategy", value.name = "estimate"))
}

gap_estimator_allMethods <- function(data, treatment_formula, outcome_formula, treatment_formula_marginal, stratum_id, num_folds, learner) {
  # Apply the gap estimator on a single sample
  one_sample_estimate <- gap_estimator(train_sample = data, 
                                       predict_sample = data,
                                       treatment_formula = treatment_formula,
                                       outcome_formula = outcome_formula,
                                       treatment_formula_marginal = treatment_formula_marginal,
                                       learner = learner)
  
  # Assign folds within sampling strata for cross-fitting
  folded <- data %>%
    group_by_at(stratum_id) %>%
    mutate(fold = sample(rep(1:num_folds,
                             ceiling(n() / num_folds)),
                         n(),
                         replace = F))
  
  # Apply the cross-fitting estimator and average the estimate over folds
  cross_fit_estimate <- foreach(f = unique(folded$fold), .combine = "rbind") %do% {
    gap_estimator(train_sample = folded %>% filter(fold != f), 
                  predict_sample = folded %>% filter(fold == f),
                  treatment_formula = treatment_formula,
                  outcome_formula = outcome_formula,
                  treatment_formula_marginal = treatment_formula_marginal,
                  learner = learner)
  } %>%
    group_by(estimand, strategy) %>%
    summarize_all(.funs = mean) %>%
    group_by()
  
  # Return th one sample and cross-fit estimates
  one_sample_estimate %>%
    mutate(cross_fit = F) %>%
    bind_rows(cross_fit_estimate %>%
                mutate(cross_fit = T))
}
