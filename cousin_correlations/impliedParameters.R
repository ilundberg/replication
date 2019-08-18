
## File to calculate implied parameters of multigenerational models
library(tidyverse)
library(reshape2)

setwd("C:/Users/iandl/Documents/Empirical")
sink("output/implied_parameters.txt")

# Function to extract parameters for a given set of sibling and cousin correlations

get_parameters <- function(sibling, cousin) {
  dynamic <- c(beta_g = sqrt(sibling),
               beta_gMinus1 = sqrt(cousin / sibling))
  latent <- c(eta = sqrt(sibling ^ 2 / cousin),
              beta = sqrt(cousin / sibling))
  beta_parent <- sqrt((sibling - cousin) / (1 - sibling))
  beta_grandparent <- polyroot(c(beta_parent^2 - sibling,
                                 beta_parent ^ 2 + sibling,
                                 1,
                                 -1))
  # Extract real roots
  beta_grandparent <- Re(beta_grandparent)[abs(Im(beta_grandparent)) < .001]
  return(list(
    dynamic = round(dynamic,2),
    latent = round(latent,2),
    order2 = list(beta_parent = round(beta_parent,2),
                  beta_grandparent = round(beta_grandparent,2))
  ))
}

estimates <- rbind(
  jaeger1 = c(.41, .14),
  jaeger2 = c(.56, .21),
  jaeger3 = c(0.38, 0.23),
  jaeger4 = c(0.37, 0.19),
  jaeger5 = c(0.37, 0.14),
  jaeger6 = c(0.37, 0.26),
  jaeger7 = c(0.45, 0.29),
  hallsten1 = c(0.39, 0.15),
  hallsten2 = c(0.29, 0.11),
  hallsten3 = c(0.51, 0.19),
  hallsten4 = c(0.47, 0.16),
  knigge1 = c(0.5, 0.32),
  pfeffer = c(0.34, 0.19)
)
for(estimate_name in rownames(estimates)) {
  print(estimate_name)
  estimate <- estimates[estimate_name,]
  print(get_parameters(sibling = estimate[1],
                       cousin = estimate[2]),2)
  print("-------------------")
}

# Get the implied parameters with my results
estimates_permanentLogIncome <- read_csv("intermediate_files/estimates_permanentLogIncome.csv")

parameter_samples <- data.frame(t(apply(estimates_permanentLogIncome,1,function(estimates) {
  parameters <- get_parameters(sibling = estimates[2], 
                               cousin = estimates[1])
  # Get the beta_grandparent coefficient closest to 0
  parameters$order2$beta_grandparent <- parameters$order2$beta_grandparent[order(abs(parameters$order2$beta_grandparent))][1]
  unlist(parameters)
})))

write_csv(
  parameter_samples %>%
    melt(id = NULL) %>%
    group_by(variable) %>%
    summarize(estimate = mean(value),
              ci.min = quantile(value, .025),
              ci.max = quantile(value, .975)),
  path = "output/consistent_parameters_empirical.csv"
)
sink()



