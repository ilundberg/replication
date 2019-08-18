# Replication code for:
# Does Opportunity Skip Generations?
# Reassessing Evidence from Sibling and Cousin Correlations
# Code by Ian Lundberg
# ilundberg@princeton.edu

# run_all.R sources all code files in order

## This file fits models.

library(tidyverse)
library(reshape2)
library(rstan)
library(doParallel)

setwd("C:/Users/iandl/Documents/Empirical")

## Define number of iterations
n.samp <- 10000
n.warmup <- 10000
n.chains <- 4

## Load the data
load("intermediate_files/d.Rdata")

# Define the stan model
stan_code = "
data {
// Define variables in data

int<lower=0> N_cousinset; // Number of cousin sets
int<lower=0> N_siblingset; // Number of sibing sets
int<lower=0> N_person; // Number of persons
int<lower=0> N; // Number of observations
int<lower=0> num_ages; // Number of unique ages

// Cluster IDs for 
int<lower=1> cousinset[N];
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
vector[N_cousinset] delta_cousinset;
vector[N_siblingset] eta_siblingset;
vector[N_person] lambda_person;

// Variance parameters
real<lower=0> sigma_cousinset;
real<lower=0> sigma_siblingset;
real<lower=0> sigma_person;
real<lower=0> sigma_observation;
}

model {
// Variance parameters
sigma_cousinset ~ cauchy(0,1);
sigma_siblingset ~ cauchy(0,1);
sigma_person ~ cauchy(0,1);
sigma_observation ~ cauchy(0,1);

// Betas for age
beta ~ normal(0,1);

// Components of permanent income at each level
delta_cousinset ~ normal(0,sigma_cousinset);
eta_siblingset ~ normal(0,sigma_siblingset);
lambda_person ~ normal(0,sigma_person);

// Outcome model
for (i in 1:N) {
Y[i] ~ normal(delta_cousinset[cousinset[i]] + eta_siblingset[siblingset[i]] + lambda_person[person[i]] + beta[age[i]], sigma_observation);
}
}"

fit <- stan(
  model_code = stan_code,
  data = list(N_cousinset = max(d$cousinset),
              N_siblingset = max(d$siblingset),
              N_person = max(d$person),
              N = nrow(d),
              cousinset = d$cousinset,
              siblingset = d$siblingset,
              person = d$person,
              Y = d$log_familyIncome,
              age = d$age,
              num_ages = length(unique(d$age))),
  chains = n.chains,
  iter = n.warmup + n.samp,
  seed = 08544,
  warmup = n.warmup,
  thin = 1,
  pars = c("beta","sigma_cousinset","sigma_siblingset","sigma_person","sigma_observation"),
  include = T,
  save_warmup = F,
  init = "random",
  init_r = 1
)

write_csv(data.frame(as.matrix(fit)) %>% 
            select(starts_with("beta")) %>%
            mutate(draw = 1:n(),
                   chain = factor(1 + trunc((draw - 1) / n.samp))),
          path = "intermediate_files/beta_estimates.csv")

write_csv(data.frame(as.matrix(fit)) %>% 
            select(starts_with("sigma")) %>%
            mutate(draw = 1:n(),
                   chain = factor(1 + trunc((draw - 1) / n.samp))),
          path = "intermediate_files/sigma_estimates.csv")

estimates_permanentLogIncome <- data.frame(as.matrix(fit)) %>%
  select(starts_with("sigma")) %>%
  select(-sigma_observation) %>%
  mutate(Cousin = sigma_cousinset ^ 2 / (sigma_cousinset ^ 2 + sigma_siblingset ^ 2 + sigma_person ^ 2),
         Sibling = (sigma_cousinset ^ 2 + sigma_siblingset ^ 2) / (sigma_cousinset ^ 2 + sigma_siblingset ^ 2 + sigma_person ^ 2)) %>%
  select(Cousin, Sibling)

write_csv(estimates_permanentLogIncome, path = "intermediate_files/estimates_permanentLogIncome.csv")

##########################################
## Survey of Economic Opportunity (SEO) ##
##   subsample robustness check         ##
##########################################

load("intermediate_files/d_seo.Rdata")

fit_seo <- stan(
  model_code = stan_code,
  data = list(N_cousinset = max(d_seo$cousinset),
              N_siblingset = max(d_seo$siblingset),
              N_person = max(d_seo$person),
              N = nrow(d_seo),
              cousinset = d_seo$cousinset,
              siblingset = d_seo$siblingset,
              person = d_seo$person,
              Y = d_seo$log_familyIncome,
              age = d_seo$age,
              num_ages = length(unique(d_seo$age))),
  chains = n.chains,
  iter = n.warmup + n.samp,
  seed = 08544,
  warmup = n.warmup,
  thin = 1,
  pars = c("beta","sigma_cousinset","sigma_siblingset","sigma_person","sigma_observation"),
  include = T,
  save_warmup = F,
  init = "random",
  init_r = 1
)

estimates_seo <- data.frame(as.matrix(fit_seo)) %>%
  select(starts_with("sigma")) %>%
  select(-sigma_observation) %>%
  mutate(Cousin = sigma_cousinset ^ 2 / (sigma_cousinset ^ 2 + sigma_siblingset ^ 2 + sigma_person ^ 2),
         Sibling = (sigma_cousinset ^ 2 + sigma_siblingset ^ 2) / (sigma_cousinset ^ 2 + sigma_siblingset ^ 2 + sigma_person ^ 2)) %>%
  select(Cousin, Sibling)

write_csv(estimates_seo, path = "intermediate_files/estimates_seo.csv")

