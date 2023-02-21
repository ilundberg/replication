

sink("logs/2_analyze.txt")

cat("This file conducts the analysis")

print(Sys.time())

# Prepare environment
library(tidyverse)
library(mgcv)
library(foreach)
library(doParallel)
library(doRNG)
cl <- makeCluster(5)
registerDoParallel(cl)

# Set seed for replicability
set.seed(90095)

# Load prepared data
prepared <- readRDS(file = "intermediate/prepared.RDS")

# Set several parameters
## Number of times to replicate 5-fold cross-validation
n_replicates <- 20
## Number of draws when simulating uncertainty of coefficient etsimates
n_sims <- 100
## Increment to income for the first-difference estimator.
## This is the amount we counterfactually add to each person's income.
delta <- 25e3

####################
# HELPER FUNCTIONS #
####################

# Load a helper function to make weighted quantiles
source("code/weighted.quantile.R")

# Function to conduct cross-validation for candidate learners
fit_learner <- function(data, formula) {
  # Note here explains one non-default choice.
  # The gamma parameter controls the preference for smoothness.
  # mgcv package default is 1.
  # We see that 1 gives very wiggle curves when used with family = binomial.
  # We are increasing gamma to a higher value to prefer more smoothness.
  # When there is an interaction() in the model formula because income is
  # interacted with two or more variables, we strongly need smoothness.
  # In that case, set to 3.
  # Otherwise, we set to 1.5.
  gamma_value <- ifelse(any(grepl("interaction",formula)),
                        3, 1.5)
  gam(formula,
      data = data,
      family = binomial,
      weights = w,
      method = "REML",
      gamma = gamma_value)
}
mutate_cv_fit <- function(data, formula, gamma_value = 1) {
  foreach(i = 1:max(data$set), .combine = "rbind") %do% {
    train <- data %>%
      filter(set != i)
    test <- data %>%
      filter(set == i)
    fit <- fit_learner(data = train, 
                       formula = formula)
    test %>%
      mutate(yhat = predict(fit, type = "response",
                            newdata = test))
  }
}

# Function to combine standard errors of individual learners
# into a standard error for the ensemble.
# This is based on V(aA + bB) = a^2V(A) + b^2V(B) + 2abCov(A,B)
# under the conservative assumption that Cor(A,B) = 1.
combine_se <- function(se, w) {
  # First we have the sum of variance terms
  variance_terms <- w ^ 2 * se ^ 2
  # Then we have the sum of covariance terms
  index_pairs <- combn(1:length(se),2)
  covariance_terms <- apply(index_pairs,2,function(x) prod(w[x] * se[x]))
  total_variance <- sum(variance_terms) + sum(covariance_terms)
  total_se <- sqrt(total_variance)
  return(total_se)
  #sqrt(sum(foreach(i = 1:length(se), .combine = "c") %do% {
  #  foreach(j = 1:length(se), .combine = "c") %do% {
  #    se[i]*se[j]*w[i]*w[j]
  #  }
  #}))
}

##########################################
# DEFINE FORMULAS FOR CANDIDATE LEARNERS #
##########################################

# Candidate learners
learners <- list(formula(enrolled ~ log(income) + race + educJoint + log(wealth)*wealthTercile),
                   formula(enrolled ~ log(income)*educJoint + race + log(wealth)*wealthTercile),
                   formula(enrolled ~ log(income)*wealthTercile + educJoint + race + log(wealth)*wealthTercile),
                   formula(enrolled ~ log(income)*race + educJoint + race + log(wealth)*wealthTercile),
                   formula(enrolled ~ log(income)*wealthTercile*educJoint + race + log(wealth)*wealthTercile),
                   formula(enrolled ~ log(income)*race*educJoint + race + log(wealth)*wealthTercile),
                   formula(enrolled ~ s(log(income), bs = "cr", k = 5) + race + educJoint + s(log(wealth), bs = "cr", k = 5, by = wealthTercile) + wealthTercile),
                   formula(enrolled ~ s(log(income), bs = "cr", k = 5, by = educJoint) + educJoint + race + s(log(wealth), bs = "cr", k = 5, by = wealthTercile) + wealthTercile),
                   formula(enrolled ~ s(log(income), bs = "cr", k = 5, by = wealthTercile) + educJoint + race + s(log(wealth), bs = "cr", k = 5, by = wealthTercile) + wealthTercile),
                   formula(enrolled ~ s(log(income), bs = "cr", k = 5, by = race) + educJoint + race + s(log(wealth), bs = "cr", k = 5, by = wealthTercile) + wealthTercile),
                   formula(enrolled ~ s(log(income), bs = "cr", k = 5, by = interaction(educJoint, wealthTercile)) + race + s(log(wealth), bs = "cr", k = 5, by = wealthTercile) + wealthTercile),
                   formula(enrolled ~ s(log(income), bs = "cr", k = 5, by = interaction(educJoint, race)) + race + s(log(wealth), bs = "cr", k = 5, by = wealthTercile) + wealthTercile))
names(learners) <- c(paste("glm",c("additive","educ","wealth","race","educWealth","educRace"),sep = "_"),
                       paste("gam",c("additive","educ","wealth","race","educWealth","educRace"),sep = "_"))

####################
# CROSS VALIDATION #
####################

# Create a data frame of cross-validated out-of-fold predictions
# from each base learner

# Structure of CV loops:
# 2. Loop over replicates
# 1. Loop over learners within each replicate
# 3. Conduct 5-fold CV within each replicate

cv_yhat_out_of_fold <- foreach(replicate_index = 1:n_replicates, .combine = "rbind", .packages = c("tidyverse","mgcv","foreach")) %dorng% {
  # Define folded data for cross-validation
  folded <- prepared %>%
    # Create folds within strata of categorical covariates
    group_by(educJoint, wealthTercile, race) %>%
    # Randomly sample folds 1:5
    mutate(set = sample(rep(1:n_replicates, ceiling(n() / 5))[1:n()])) %>%
    ungroup()
  foreach(learner = names(learners), .combine = "rbind") %do% {
    # Within this learner, loop over replicates
    mutate_cv_fit(data = folded,
                  formula = learners[[learner]],
                  # If this fit interacts income with two other variables, increase the
                  # preference for smoothness with a higher gamma value
                  gamma_value = ifelse(grepl("educWealth|educRace",learner),3,1.5)) %>%
      # Record the learner and the index of this replicate
      mutate(learner = learner,
             replicate_index = replicate_index)
  }
}
saveRDS(cv_yhat_out_of_fold,
        file = "intermediate/cv_yhat_out_of_fold.RDS")

##################
# LEARN ENSEMBLE #
##################
# This code learns a weighted average of the base learners
# which optimally predicts out-of-fold cases from cross-validation

# Prepare a dataset for learning the ensemble
data_for_ensemble <- cv_yhat_out_of_fold %>%
  rename(y = enrolled) %>%
  select(PUBID, educJoint, wealthTercile, replicate_index, y, learner, yhat) %>%
  # Make the prediction from each base learner appear in its own column;
  # in this ensembling stage, these columns are now the predictors
  pivot_wider(names_from = "learner", values_from = "yhat") %>%
  group_by(educJoint, wealthTercile)

# Create inputs for quadprog::solve.QP()
# Define the number of learners
p <- length(learners)
# Create a matrix for constraints: all greater than 0 and sum to 1
Amat <- cbind(rep(1,p),
              diag(1,p))
# Create a model matrix for prediction
X <- as.matrix(
  data_for_ensemble %>% 
    ungroup() %>% 
    select(starts_with("gam"),starts_with("glm"))
)

# Learn the optimal weighting of the base learners
QP.out <- quadprog::solve.QP(Dmat = t(X) %*% X,
                             dvec = t(data_for_ensemble$y) %*% X,
                             Amat = Amat, 
                             bvec = c(1,rep(0,p)),
                             meq = 1)

# Create a data frame with the learned weight on each learner
ensemble_weights <- data.frame(learner = colnames(X),
                               weight = QP.out$solution)
saveRDS(ensemble_weights, file = "intermediate/ensemble_weights.RDS")

###########################################
# ESTIMATE EACH BASE LEARNER ON FULL DATA #
###########################################

base_learners_estimated <- foreach(learner_case = names(learners)) %do% {
  fit_learner(data = prepared,
              formula = learners[[learner_case]])
}
names(base_learners_estimated) <- names(learners)

###################################
# PREDICT THE DOSE RESPONSE CURVE #
###################################

# We will estimate dose response curve
# - x education x wealth
# - x education x race
# Define shorthand for each of those
grouping_names <- c("educWealth","educRace")
# For each grouping, estimate the dose-response curve with each base learner
dose_response_each_learner <- foreach(grouping_name = grouping_names, .combine = "rbind") %do% {
  if (grouping_name == "educWealth") {
    byVars <- c("educJoint","wealthTercile")
  } else if (grouping_name == "educRace") {
    byVars <- c("educJoint","race")
  }
  estimate_this_grouping <- foreach(learner = names(learners), .combine = "rbind") %do% {
    
    # Create a data frame to predict
    toPredict <- prepared %>%
      # Determine the 5th and 95th percentiles that will bound the counterfactual income values considered
      group_by(across(all_of(byVars))) %>%
      mutate(p05 = weighted.quantile(income, q = .05, w = w),
             p95 = weighted.quantile(income, q = .95, w = w)) %>%
      # Nest to have a dataset within each group
      nest() %>%
      # Within each dataset, expand so that each row appears 20 times
      # taking the sequence of counterfactual income values
      mutate(data = map(data, function(data_case) {
        a_seq <- seq(data_case$p05[1],data_case$p95[1],length.out = 20)
        foreach(a_val = a_seq, .combine = "rbind") %do% {
          data_case %>%
            mutate(income = a_val)
        }
      })) %>%
      # Unnest to create a more typical data frame
      unnest(cols = "data")
    
    # Extract model objects
    beta_hat <- coef(base_learners_estimated[[learner]])
    X <- predict(base_learners_estimated[[learner]], newdata = toPredict, type = "lpmatrix")
    
    # Predict for point estimate
    yhat <- plogis(X %*% beta_hat)
    point <- toPredict %>%
      ungroup() %>%
      mutate(yhat = yhat) %>%
      group_by(across(all_of(byVars)), income) %>%
      summarize(estimate = weighted.mean(yhat, w = w),
                .groups = "drop")
    
    # Predict over simulated coefficients for estimation uncertainty
    Sigma_hat <- vcov(base_learners_estimated[[learner]])
    beta_star <- mvtnorm::rmvnorm(n_sims, beta_hat, Sigma_hat)
    yhat_star <- plogis(X %*% t(beta_star))
    colnames(yhat_star) <- paste0("yhat_star",1:ncol(yhat_star))
    # Aggregate those to create a standard error
    se <- toPredict %>%
      ungroup() %>%
      bind_cols(yhat_star) %>%
      select(all_of(byVars), income, w, starts_with('yhat_star')) %>%
      pivot_longer(cols = starts_with("yhat_star")) %>%
      group_by(across(all_of(byVars)), income, name) %>%
      summarize(estimate_star = weighted.mean(value, w = w),
                .groups = "drop_last") %>%
      summarize(se = sd(estimate_star), .groups = "drop")
    
    # Return the point estimate and standard errors
    point %>%
      left_join(se, by = c(byVars,"income")) %>%
      mutate(learner = learner)
  }
  
  # Create a nested tibble to return for clean output
  clean_tibble <- estimate_this_grouping %>%
    mutate(grouping_name = grouping_name) %>%
    group_by(grouping_name, learner) %>%
    nest()
  return(clean_tibble)
}
saveRDS(dose_response_each_learner,
        file = "intermediate/dose_response_each_learner.RDS")

dose_response_ensemble <- foreach(grouping_name_case = unique(dose_response_each_learner$grouping_name), .combine = "rbind") %do% {
  # Restrict to the dose-response curves for this grouping
  dose_response_each_learner %>%
    filter(grouping_name == grouping_name_case) %>%
    # Unnest to work with a data frame
    unnest(cols = "data") %>%
    # Merge in the ensemble weights
    left_join(ensemble_weights, by = "learner") %>%
    # Take weighted average with ensemble weights
    group_by(grouping_name, across(any_of(c("educJoint","wealthTercile","race"))), income) %>%
    summarize(estimate = weighted.mean(estimate, w = weight),
              se = combine_se(se = se, w = weight),
              .groups = "drop") %>%
    # Prepare the tibble to return
    group_by(grouping_name) %>%
    nest()
}
saveRDS(dose_response_ensemble,
        file = "intermediate/dose_response_ensemble.RDS")

##############################
# FIRST DIFFERENCE ESTIMATES #
##############################

# Create datasets to predict for the first-difference estimator
d0 <- prepared
d1 <- prepared %>% mutate(income = income + delta)

# For each learner, calculate the first difference estimates
first_difference_each_learner <- foreach(learner = names(learners), .combine = "rbind") %do% {
  
  # Extract model objects
  beta_hat <- coef(base_learners_estimated[[learner]])
  X0 <- predict(base_learners_estimated[[learner]], newdata = d0, type = "lpmatrix")
  X1 <- predict(base_learners_estimated[[learner]], newdata = d1, type = "lpmatrix")
  
  # Predict the conditional effect estimate for each unit
  effect <- plogis(X1 %*% beta_hat) - plogis(X0 %*% beta_hat)
  
  # Predict over simulated estimation uncertainty
  Sigma_hat <- vcov(base_learners_estimated[[learner]])
  beta_star <- mvtnorm::rmvnorm(n_sims, beta_hat, Sigma_hat)
  effect_star <- plogis(X1 %*% t(beta_star)) - plogis(X0 %*% t(beta_star))
  colnames(effect_star) <- paste0("effect_star_",1:ncol(effect_star))
  
  # Function to make aggregate first-difference estimate and standard error
  # for a given vector of observation indices in the prepared data frame
  make_aggregate <- function(indices) {
    # Create the point estimate
    estimate <- weighted.mean(effect[indices], w = prepared$w[indices])
    # Create the simulated estimates
    point_star <- apply(effect_star, 2, function(x) {
      weighted.mean(x[indices], w = prepared$w[indices])
    })
    # Calculate the standard error over simulations
    se <- sd(point_star)
    return(data.frame(estimate = estimate,
                      se = se))
  }
  
  # Note the unique values of each predictor within which we will aggregate
  race_vals <- unique(prepared$race)
  educ_vals <- unique(prepared$educJoint)
  wealth_vals <- unique(prepared$wealthTercile)
  
  # Create aggregated results
  first_difference_estimates <- list(
    individual = prepared %>%
      select(PUBID) %>%
      mutate(estimate = effect[,1], 
             se = apply(effect_star,1,sd)),
    overall = make_aggregate(1:nrow(prepared)),
    byRace = foreach(x = race_vals, .combine = "rbind") %do% make_aggregate(which(prepared$race == x)) %>% mutate(race = race_vals),
    byEduc = foreach(x = educ_vals, .combine = "rbind") %do% make_aggregate(which(prepared$educJoint == x)) %>% mutate(educJoint = educ_vals),
    byWealth = foreach(x = wealth_vals, .combine = "rbind") %do% make_aggregate(which(prepared$wealthTercile == x)) %>% mutate(wealthTercile = wealth_vals),
    byEducRace = foreach(x = educ_vals, .combine = "rbind") %do% {
      foreach(y = race_vals, .combine = "rbind") %do% {
        make_aggregate(which(prepared$educJoint == x & prepared$race == y)) %>% mutate(educJoint = x, race = y)
      }
    },
    byEducWealth = foreach(x = educ_vals, .combine = "rbind") %do% {
      foreach(y = wealth_vals, .combine = "rbind") %do% {
        make_aggregate(which(prepared$educJoint == x & prepared$wealthTercile == y)) %>% mutate(educJoint = x, wealthTercile = y)
      }
    },
    byEducWealthRace = foreach(x = educ_vals, .combine = "rbind") %do% {
      foreach(y = wealth_vals, .combine = "rbind") %do% {
        foreach(z = race_vals, .combine = "rbind") %do% {
          make_aggregate(which(prepared$educJoint == x & prepared$wealthTercile == y & prepared$race == z)) %>% mutate(educJoint = x, wealthTercile = y, race = z)
        }
      }
    }
  )
  return(tibble(learner = learner,
                quantity = names(first_difference_estimates),
                estimate = first_difference_estimates))
}
saveRDS(first_difference_each_learner,
        file = "intermediate/first_difference_each_learner.RDS")

# Create ensemble first difference estimates
first_difference_ensemble <- first_difference_each_learner %>%
  left_join(ensemble_weights, by = "learner") %>%
  group_by(quantity) %>%
  nest() %>%
  # Work with data within each target quantity
  mutate(data = map(data, function(x) {
    x %>%
      # Un-nest learner-specific estimates
      unnest(cols = "estimate") %>%
      # Weighted average by ensemble weights
      group_by(across(-all_of(c("learner","estimate","se","weight")))) %>%
      summarize(estimate = weighted.mean(estimate, w = weight),
                se = combine_se(se = se, w = weight),
                .groups = "drop")
  }))
saveRDS(first_difference_ensemble,
        file = "intermediate/first_difference_ensemble.RDS")

print(Sys.time())
print(sessionInfo())

sink()

