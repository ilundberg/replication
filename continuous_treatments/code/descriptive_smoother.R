
descriptive_smoother <- function(outcome_name, delta, bs_reps = 10) {
  
  # Load helper functions
  source("code/load_data.R")
  
  # Load the data
  # Note that imputation will be inconsequential here
  # because the data are already restricted to valid treatments
  # and valid outcomes
  data <- load_data(outcome_name)
  
  # Fit smoothed estimate
  smoother <- function(data, delta, bs = F) {
    fit <- mgcv::gam(y ~ s(log(income), bs = "cr", k = 5),
                     data = data %>%
                       sample_frac(replace = bs),
                     family = binomial,
                     weights = w)
    yhat <- data %>%
      mutate(estimate = predict(fit, 
                                newdata = data, 
                                type = "response"),
             estimand = "yhat")
    difference <- foreach(delta_value = delta, .combine = "rbind") %do% {
      data %>% 
        mutate(estimate = predict(fit, 
                                  newdata = data %>% mutate(income = income + delta_value), 
                                  type = "response") -
                 predict(fit, 
                         newdata = data, 
                         type = "response")) %>%
        summarize(estimate = weighted.mean(estimate, w = w),
                  .groups = "drop") %>%
        mutate(estimand = paste0("delta_",delta_value))
    }
    return(yhat %>%
             bind_rows(difference))
  }
  
  # Calculate point estimate
  estimate <- smoother(data, delta = delta)
  
  # Calculate standard error
  se <- foreach(rep = 1:bs_reps, .combine = "rbind") %do% {
    smoother(data, bs = T, delta = delta)
  } %>%
    group_by(PUBID, estimand) %>%
    summarize(se = sd(estimate))
  
  # Return estimate combined with confidence interval
  estimate %>%
    left_join(se, by = c("PUBID","estimand")) %>%
    mutate(ci.min = estimate - qnorm(.975) * se,
           ci.max = estimate + qnorm(.975) * se,
           outcome = outcome_name)
}

