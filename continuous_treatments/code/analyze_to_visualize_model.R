analyze_to_visualize_model <- function(
    outcome = "enrolled_any", 
    bs_reps = 5
) {
  
  source("code/load_data.R")
  
  # Function to make estimate
  make_estimate <- function(learning, estimation) {
    fit <- mgcv::gam(y ~ s(log(income)) + 
                       log(income)*educJoint +
                       log(income)*race +
                       log(income)*log(wealth) + 
                       s(log(wealth)),
                     data = learning,
                     family = binomial,
                     weights = w)
    
    estimate <- foreach(wealth_val = c(25e3,100e3,200e3), .combine = "rbind") %do% {
      estimation %>%
        mutate(wealth = wealth_val) %>%
        mutate(estimate = predict(fit, type = "response",
                                  newdata = estimation %>% 
                                    mutate(wealth = wealth_val))) %>%
        select(PUBID, income, estimate, race, educJoint, wealth)
    }
    return(estimate)
  }
  
  # Point estimate
  t0_point <- Sys.time()
  cat(paste("\nBegin point estimate.",Sys.time()))
  d <- load_data(outcome, impute = T, bs = F)
  estimate <- make_estimate(learning = d,
                            estimation = d)
  
  # Bootstrap replicates
  bootstrap <- foreach(
    rep = 1:bs_reps,
    .packages = c("tidyverse","mgcv"),
    .export = c("load_data","make_estimate"),
    .combine = "rbind"
  ) %dorng% {
    cat(paste0(rep,"..."))
    d_star <- load_data(outcome, bs = T)
    estimate_star <- make_estimate(learning = d_star,
                                   estimation = d) %>%
      mutate(bs = rep)
    return(estimate_star)
  }
  
  # Estimate the standard error
  se <- bootstrap %>%
    group_by(PUBID) %>%
    summarize(se = sd(estimate))
  
  # Prepare result to return
  result <- estimate %>%
    left_join(se, by = "PUBID") %>%
    mutate(ci.min = estimate - qnorm(.975) * se,
           ci.max = estimate + qnorm(.975) * se,
           outcome = outcome)
  return(result)
}
