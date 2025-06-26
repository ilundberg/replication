
theme_set(theme_bw())
# Show poor performance of forest
# Plan: Wrap in function. Call function with two DGPs: one where logit wins and one where forest wins only in prediction

simulate_performance <- function(response_function, n = 1e3, delta = .2) {
  
  # Visualize the response function
  plot_of_response_function <- tibble(a = seq(-2,2,.01)) |>
    mutate(y = response_function(a)) |>
    ggplot(aes(x = a, y = y)) +
    geom_line() +
    xlab("Treatment Value A") +
    ylab("Conditional Outcome Mean E(Y | A)")
  
  # Simulate data
  sim_data <- tibble(a = rnorm(n)) |>
    mutate(y = rbinom(n(), 1, prob = response_function(a)))
  
  # Create counterfactual data for prediction
  sim_counterfactual <- sim_data |> 
    mutate(a = a + delta)
  
  # Define the truth for predicting Y and predicting causal effects
  truth <- sim_data |>
    mutate(
      prediction = response_function(a),
      causal = response_function(a + delta) - response_function(a)
    )
  
  # Define matrices for forest estimators
  X_factual <- as.matrix(sim_data$a)
  X_counterfactual <- as.matrix(sim_counterfactual$a)
  
  # Learn conditional mean of Y
  forest_fit <- regression_forest(Y = sim_data$y, X = X_factual, tune.parameters = "all")
  ll_forest_fit <- ll_regression_forest(Y = sim_data$y, X = X_factual, tune.parameters = "all")
  logit_fit <- glm(y ~ a, data = sim_data, family = binomial)
  
  # Create result
  result <- tibble(
    method = "forest",
    estimand = "prediction",
    estimate = predict(forest_fit, newdata = X_factual)$predictions,
    a = truth$a,
    truth = truth$prediction
  ) |>
    bind_rows(
      tibble(
        method = "forest",
        estimand = "causal",
        estimate = predict(forest_fit, newdata = X_counterfactual)$predictions - 
          predict(forest_fit, newdata = X_factual)$predictions,
        a = truth$a,
        truth = truth$causal
      )
    ) |>
    bind_rows(
      tibble(
        method = "local_linear_forest",
        estimand = "prediction",
        estimate = predict(ll_forest_fit, newdata = X_factual)$predictions,
        a = truth$a,
        truth = truth$prediction
      )
    ) |>
    bind_rows(
      tibble(
        method = "local_linear_forest",
        estimand = "causal",
        estimate = predict(ll_forest_fit, newdata = X_counterfactual)$predictions - 
          predict(ll_forest_fit, newdata = X_factual)$predictions,
        a = truth$a,
        truth = truth$causal
      )
    ) |>
    bind_rows(
      tibble(
        method = "logit",
        estimand = "prediction",
        estimate = predict(logit_fit, newdata = sim_data, type = "response"),
        a = truth$a,
        truth = truth$prediction
      )
    ) |>
    bind_rows(
      tibble(
        method = "logit",
        estimand = "causal",
        estimate = predict(logit_fit, newdata = sim_counterfactual, type = "response") - 
          predict(logit_fit, newdata = sim_data, type = "response"),
        a = truth$a,
        truth = truth$causal
      )
    ) 
  
  # Create labeller functions
  estimand_labeller <- function(x) {
    case_when(
      x == "causal" ~ "Predicting the Conditional\nAverage Causal Effect\nof Delta Increase in Treatment",
      x == "prediction" ~ "Predicting Conditional Mean\nof Factual Outcome Values"
    )
  }
  model_labeller <- function(x) {
    case_when(
      x == "logit" ~ "Logistic Regression",
      x == "forest" ~ "Random Forest",
      x == "local_linear_forest" ~ "Local Linear Forest"
    )
  }
  
  # MSE summary
  plot_of_mse <- result |>
    group_by(method, estimand) |>
    summarize(mse = mean((estimate - truth) ^ 2)) |>
    mutate(estimand = fct_rev(estimand)) |>
    ggplot(aes(y = method, x = mse)) +
    geom_point() +
    facet_wrap(
      ~estimand, scales = "free", labeller = as_labeller(estimand_labeller)
    ) +
    xlab("Mean Squared Error") +
    scale_y_discrete(
      name = "Prediction Method",
      labels = model_labeller
    )
  
  # Prediction vs treatment plots
  plot_of_yhat <- result |>
    filter(estimand == "prediction") |>
    ggplot(aes(x = a, y = estimate)) +
    geom_point(color = "gray", size = .2) +
    geom_line(aes(y = truth)) +
    facet_wrap(~method, labeller = as_labeller(model_labeller)) +
    ylab("Estimated P(Y = 1 | A)") +
    xlab("Treatment Value A") +
    ggtitle(label = NULL, subtitle = "Black line is the truth. Gray dots are model based estimates.")
  plot_of_tauhat <- result |>
    filter(estimand == "causal") |>
    ggplot(aes(x = a, y = estimate)) +
    geom_point(color = "gray", size = .2) +
    geom_line(aes(y = truth)) +
    facet_wrap(~method, labeller = as_labeller(model_labeller)) +
    ylab(paste0("Estimated Effect of\nIncreasing A by ",delta)) +
    xlab("Treatment Value A") +
    ggtitle(label = NULL, subtitle = "Black line is the truth. Gray dots are model based estimates.")
  return(list(
    plot_of_response_function = plot_of_response_function,
    plot_of_mse = plot_of_mse,
    plot_of_yhat = plot_of_yhat,
    plot_of_tauhat = plot_of_tauhat
  ))
}


set.seed(90095)
simple_dgp <- simulate_performance(
  response_function = function(a) plogis(a),
  delta = .01,
  n = 2000
)
complex_dgp <- simulate_performance(
  response_function = function(a) plogis(
    case_when(
      a < 1 ~ a - .5 * a ^ 2,
      T ~ .5
    )
  ),
  delta = .01,
  n = 2000
)

ggsave(
  plot = simple_dgp$plot_of_response_function,
  file = "figures/simulation_simple_dgp.pdf",
  height = 3, width = 5
)
ggsave(
  plot = complex_dgp$plot_of_response_function,
  file = "figures/simulation_complex_dgp.pdf",
  height = 3, width = 5
)
ggsave(
  plot = simple_dgp$plot_of_mse + scale_x_continuous(n.breaks = 3),
  file = "figures/simulation_simple_dgp_mse.pdf",
  height = 2, width = 7
)
ggsave(
  plot = complex_dgp$plot_of_mse + scale_x_continuous(n.breaks = 3),
  file = "figures/simulation_complex_dgp_mse.pdf",
  height = 2, width = 7
)

ggsave(
  plot = simple_dgp$plot_of_yhat + scale_x_continuous(n.breaks = 3),
  file = "figures/simulation_simple_dgp_yhat.pdf",
  height = 2.5, width = 7
)
ggsave(
  plot = complex_dgp$plot_of_yhat + scale_x_continuous(n.breaks = 3),
  file = "figures/simulation_complex_dgp_yhat.pdf",
  height = 2.5, width = 7
)

ggsave(
  plot = simple_dgp$plot_of_tauhat + scale_x_continuous(n.breaks = 3),
  file = "figures/simulation_simple_dgp_tauhat.pdf",
  height = 2.5, width = 7
)
ggsave(
  plot = complex_dgp$plot_of_tauhat + scale_x_continuous(n.breaks = 3),
  file = "figures/simulation_complex_dgp_tauhat.pdf",
  height = 2.5, width = 7
)

