causal_estimator_forest <- function(
    outcome = "enrolled_any", 
    bs_reps = 5,
    delta = 10e3,
    learning_pubids = NULL
) {
  
  source("code/load_data.R")
  
  # Function to make estimate
  make_estimate <- function(learning, estimation, delta) {
    
    # Create predictor matrix of learning and estimation cases.
    # Pool into one matrix to ensure the columns are all defined the same.
    X <- model.matrix(
      ~ log(income)*(
        educJoint + race + log(wealth) + 
          wealth_negative + wealth_low
      ),
      data = rbind(learning, estimation, estimation |> mutate(income = income + delta))
    )
    # Create the learning part of X
    indices_learning <- 1:nrow(learning)
    X_learning <- X[indices_learning,]
    # Create the estimation part of X for predicting Y(A)
    indices_estimation0 <- (nrow(learning) + 1):(nrow(learning) + nrow(estimation))
    X0_estimation <- X[indices_estimation0,]
    # Create the estimation part of X for predicting Y(A + delta)
    indices_estimation1 <- (nrow(learning) + nrow(estimation) + 1):nrow(X)
    X1_estimation <- X[indices_estimation1,]
    
    # Learn forest on learning cases
    fit <- regression_forest(
      Y = learning$y,
      X = X_learning,
      sample.weights = learning$w,
      min.node.size = 15,
      num.trees = 10e3
    )
    
    # Produce a causal estimate
    estimate <- estimation |>
      mutate(
        yhat1 = predict(fit, newdata = X1_estimation)$predictions,
        yhat0 = predict(fit, newdata = X0_estimation)$predictions,
        effect = yhat1 - yhat0,
        delta = delta
      ) |>
      select(PUBID, w, yhat0, yhat1, effect, delta)
  
    return(estimate)
  }
  
  # Point estimate
  t0_point <- Sys.time()
  cat(paste("\nBegin point estimate.",Sys.time()))
  d <- load_data(outcome, impute = T, bs = F, learning_pubids = learning_pubids)
  estimate <- make_estimate(learning = d, estimation = d, delta = delta)
  
  # Bootstrap replicates
  cat(paste("\nBegin",bs_reps,"bootstrap replicates.",Sys.time(),"\n"))
  # Note that bootstrap will proceed in sequence
  # instead of in parallel because regression_forest
  # internally carries out parallel processing
  cat(paste("\nEstimated bootstrap completion time:",
            Sys.time() + bs_reps * difftime(Sys.time(),t0_point),
            "\n"))
  bootstrap <- foreach(
    rep = 1:bs_reps,
    .combine = "rbind"
  ) %do% {
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
