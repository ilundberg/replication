
# Replication code for:
# Does Opportunity Skip Generations?
# Reassessing Evidence from Sibling and Cousin Correlations
# Code by Ian Lundberg
# ilundberg@princeton.edu

# run_all.R sources all code files in order

## This file estimates the sibling correlation in the parent generation

library(tidyverse)
library(reshape2)
library(rstan)
library(doParallel)

setwd("C:/Users/iandl/Documents/Empirical")

## Define number of iterations
n.samp <- 10000
n.warmup <- 10000
n.chains <- 4

load("intermediate_files/d_parents.Rdata")

stan_code = "
data {
// Define variables in data

int<lower=0> N_siblingset; // Number of sibing sets
int<lower=0> N_person; // Number of persons
int<lower=0> N; // Number of observations
int<lower=0> num_ages; // Number of unique ages

// Cluster IDs for 
int<lower=1> siblingset[N];
int<lower=1> person[N];

// Outcomes
real Y[N];

// Predictors
int<lower=0> age[N];
}

parameters {
// Define parameters to estimate

// Grand coefficients
vector[num_ages] beta;
vector[N_siblingset] eta_siblingset;
vector[N_person] lambda_person;

// Variance parameters
real<lower=0> sigma_siblingset;
real<lower=0> sigma_person;
real<lower=0> sigma_observation;
}

model {
// Variance parameters
sigma_siblingset ~ cauchy(0,1);
sigma_person ~ cauchy(0,1);
sigma_observation ~ cauchy(0,1);

// Betas for age
beta ~ normal(0,1);

// Components of permanent income at each level
eta_siblingset ~ normal(0,sigma_siblingset);
lambda_person ~ normal(0,sigma_person);

// Outcome model
for (i in 1:N) {
Y[i] ~ normal(eta_siblingset[siblingset[i]] + lambda_person[person[i]] + beta[age[i]], sigma_observation);
}
}"

fit_parents <- stan(
  model_code = stan_code,
  data = list(N_siblingset = max(d_parents$siblingset),
              N_person = max(d_parents$person),
              N = nrow(d_parents),
              siblingset = d_parents$siblingset,
              person = d_parents$person,
              Y = d_parents$log_familyIncome,
              age = d_parents$age,
              num_ages = length(unique(d_parents$age))),
  chains = n.chains,
  iter = n.warmup + n.samp,
  seed = 08544,
  warmup = n.warmup,
  thin = 1,
  pars = c("beta","sigma_siblingset","sigma_person","sigma_observation"),
  include = T,
  save_warmup = F,
  init = "random",
  init_r = 1
)

estimates_parents <- data.frame(as.matrix(fit_parents)) %>%
  select(starts_with("sigma")) %>%
  select(-sigma_observation) %>%
  mutate(Sibling = (sigma_siblingset ^ 2) / (sigma_siblingset ^ 2 + sigma_person ^ 2)) %>%
  select(Sibling)

write_csv(estimates_parents, path = "intermediate_files/estimates_parents.csv")

sink("output/parent_sibling_correlation.txt")
print("The sibling correlation among parents:")
print(estimates_parents %>%
        group_by() %>%
        summarize(estimate = mean(Sibling),
                  ci.min = quantile(Sibling, .025),
                  ci.max = quantile(Sibling, .975))
)
sink()