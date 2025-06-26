
library(tidyverse)
library(mgcv)
library(foreach)
library(doParallel)
library(doRNG)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

simulate <- function(
    n = 100,         # sample size
    delta = .01,     # additive shift intervention size
    sigma = .5,      # conditional SD of Y given A, X
    interactive_truth = T, # whether dose response curve varies by X
    return_truth = F # whether to return true DGP data, rather than simulated MSE, for purpose of a graph
) {
  if (!interactive_truth) {
    truth <- tibble(
      x = rep(1:5, each = n)
    ) |>
      mutate(
        a = rep(seq(0,1,length.out = n), 5),
        mu = .1 * x + .5 * sqrt(a),
        mu1 = .1 * x + .5 * sqrt(a + delta),
        x = factor(x)
      )
  } else if (interactive_truth) {
    truth <- tibble(
      x = rep(1:5, each = n)
    ) |>
      mutate(
        a = rep(seq(0,1,length.out = n), 5),
        mu = case_when(
          x == 1 ~ a,
          x == 2 ~ log(10*a + 1) / log(21),
          x == 3 ~ a ^ 2,
          x == 4 ~ sqrt(a),
          x == 5 ~ sin(.5 * pi * a)
        ),
        mu1 = case_when(
          x == 1 ~ a + delta,
          x == 2 ~ log(10*(a + delta) + 1) / log(21),
          x == 3 ~ (a + delta) ^ 2,
          x == 4 ~ sqrt(a + delta),
          x == 5 ~ sin(.5 * pi * (a + delta))
        ),
        x = factor(x)
      )
  }
  if (return_truth) {
    return(truth)
  } else {
    sim <- truth |>
      mutate(y = rnorm(n(), mean = mu, sd = sigma))
    sim1 <- sim |> mutate(a = a + delta)
    interactive <- gam(y ~ x*a + s(a, by = x), data = sim)
    additive <- gam(y ~ x + s(a), data = sim)
    sim |>
      mutate(
        interactive = predict(interactive, newdata = sim1) - predict(interactive),
        additive = predict(additive, newdata = sim1) - predict(additive),
        truth = mu1 - mu
      ) |>
      summarize(
        additive = mean((additive - truth) ^ 2),
        interactive = mean((interactive - truth) ^ 2)
      ) |>
      mutate(n = n, interactive_truth = interactive_truth)
  }
} 

# Visualize the DGP
simulate(interactive_truth = F, return_truth = T) |>
  mutate(interactive_truth = F) |>
  bind_rows(
    simulate(interactive_truth = T, return_truth = T) |>
      mutate(interactive_truth = T)
  ) |>
  ggplot(aes(x = a, y = mu, color = factor(x))) +
  geom_line() +
  labs(
    x = "Treatment Value A",
    y = "True Mean Outcome Value\nE(Y | A, X)",
    color = "Population\nSubgroup X"
  ) +
  facet_wrap(
    ~interactive_truth,
    labeller = as_labeller(function(x) ifelse(x,"Interactive data\ngenerating process","Additive data\ngenerating process"))
  )
ggsave("figures/sim_joint_dgp.pdf", height = 2.5, width = 6.5)



simulate(interactive_truth = T, return_truth = T) |>
  ggplot(aes(x = a, y = mu, color = factor(x))) +
  geom_line() +
  labs(
    x = "Treatment Value A",
    y = "True Mean Outcome Value\nE(Y | A, X)",
    color = "Population\nSubgroup X"
  )

# Note true average effects
simulate(interactive_truth = F, return_truth = T) |>
  summarize(average_effect = mean(mu1 - mu))
simulate(interactive_truth = T, return_truth = T) |>
  summarize(average_effect = mean(mu1 - mu))

# Conduct simulations
simulations <- foreach(
  n_value = seq(10,200,10),
  .combine = "rbind"
) %do% {
  foreach(interactive_truth_value = c(F,T), .combine = "rbind") %do% {
    foreach(
      rep = 1:500, 
      .combine = "rbind",
      .packages = c("tidyverse","mgcv")
    ) %dorng% {
      simulate(n = n_value, sigma = .25, interactive_truth = interactive_truth_value)
    }
  }
}

simulations |>
  pivot_longer(cols = c("additive","interactive"), names_to = "model", values_to = "mse") |>
  group_by(interactive_truth, model, n) |>
  summarize(mse = mean(mse)) |>
  # Note full sample size is actually 5 times since there are p = 5 groups with each sample size n
  mutate(n = n*5) |>
  filter(n >= 100 & n <= 500) |>
  ggplot(aes(x = n, y = sqrt(mse), color = model)) +
  geom_point(size = 1) +
  geom_line() +
  facet_wrap(
    ~interactive_truth,
    labeller = as_labeller(function(x) ifelse(x,"Interactive data\ngenerating process","Additive data\ngenerating process"))
  ) +
  scale_color_discrete(
    name = "Model estimates",
    labels = function(x) case_when(
      x == "additive" ~ "Additive nonlinear model",
      x == "interactive" ~ "Interactive nonlinear model"
    )
  ) +
  xlab("Sample Size") +
  ylab("Root Mean Squared Error for\nCausal Additive Shift Estimates")
ggsave("figures/sim_joint_result.pdf", height = 2.5, width = 6.5)
