causal_estimator <- function(
    outcome = "enrolled_any", 
    bs_reps = 5,
    delta = 10e3,
    learning_pubids = NULL
) {
  
  source("code/load_data.R")
  
  # Function to make estimate
  make_estimate <- function(learning, estimation, delta) {
    fit <- mgcv::gam(
      y ~ s(log(income)) + 
        #log(income)*(
        educJoint + race + log(wealth) + 
        wealth_negative + wealth_low
      #),
      ,
      data = learning,
      family = binomial,
      weights = w
    )
    
    estimate <- foreach(delta_value = delta, .combine = "rbind") %do% {
      d %>%
        mutate(yhat1 = predict(fit, type = "response",
                               newdata = estimation %>% mutate(income = income + delta_value)),
               yhat0 = predict(fit, 
                               newdata = estimation,
                               type = "response"),
               effect = yhat1 - yhat0,
               delta = delta_value) %>%
        select(PUBID, w, effect, delta)
    }
    return(estimate)
  }
  
  # Point estimate
  t0_point <- Sys.time()
  cat(paste("\nBegin point estimate.",Sys.time()))
  d <- load_data(outcome, impute = T, bs = F, learning_pubids = learning_pubids)
  estimate <- make_estimate(learning = d, estimation = d, delta = delta)
  
  # Bootstrap replicates
  cat(paste("\nBegin",bs_reps,"bootstrap replicates.",Sys.time(),"\n"))
  cat(paste("\nEstimated bootstrap completion time:",
            Sys.time() + bs_reps * difftime(Sys.time(),t0_point) / num_cores,
            "\n"))
  bootstrap <- foreach(
    rep = 1:bs_reps,
    .packages = c("tidyverse","mgcv"),
    .export = c("load_data","make_estimate"),
    .combine = "rbind"
  ) %dorng% {
    cat(paste0(rep,"..."))
    d_star <- load_data(outcome, bs = T)
    estimate_star <- make_estimate(learning = d_star, estimation = d_star, delta = delta) %>%
      mutate(bs = rep)
    return(estimate_star)
  }
  
  return(list(
    arguments = list(
      outcome = outcome, 
      bs_reps = bs_reps,
      delta = delta,
      learning_pubids = learning_pubids
    ),
    estimate = estimate,
    bootstrap = bootstrap,
    data = d
  ))
}
