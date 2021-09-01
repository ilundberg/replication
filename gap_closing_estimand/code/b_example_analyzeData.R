# Author: Ian Lundberg (ianlundberg@ucla.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Forthcoming in Sociological Methods and Research

# See README.md for guidance to the replication package.

# This file analyzes data for the empirical example.

# Initialize sink file to hold printed output
my_sink <- file("logs/b_example_analyzeData.txt", open = "wt")
sink(my_sink ,type = "output")
sink(my_sink, type = "message")

t0 <- Sys.time()
print("Time began:")
print(t0)

# Set seed
set.seed(08544)

# Load packages
source("code/prepare_environment.R")

# Print the computing environment information
print(sessionInfo())

# Load data
GSS_merged <- readRDS("intermediate/GSS_merged.Rds")
gss_brr <- readRDS("intermediate/gss_brr.Rds")

# This file proceeds in several sections.
# 1. Produce the analytic data and note sample restrictions
# 2. Descriptive statistics
# 3. Calculate point estimates
# 4. Calculate estimates on each set of replicate weights
# 5. Aggregate the main estimates over the replicate weights
#    to summarize with a point estimate and standard error
# 6. Aggregate coefficients (not of primary interest; reported
#     in appendix) to point estimates and standard errors
# 7. Print main results to log file

#############################################################
# 1. Produce the analytic data and note sample restrictions #
#############################################################

# A function to convert raw_data into imputed data.
# This will be called once for each point estimate and once within
# each balanced repeated replicate draw for uncertainty estimation
sample_restrictions <- function(raw_data, print_summaries = F) {
  if (print_summaries) {
    print("Number in age range:")
    print(sum(raw_data$AGE >= 30 & raw_data$AGE <= 45))
    print("Number not NA on parent occupation:")
    print(sum(raw_data$AGE >= 30 & raw_data$AGE <= 45 & raw_data$PAOCC10 > 0))
    print("Number not NA on own occupation:")
    print(sum(raw_data$AGE >= 30 & raw_data$AGE <= 45 & raw_data$PAOCC10 > 0 & raw_data$OCC10 > 0))
    print("Number reporting positive incomes:")
    print(sum((raw_data$AGE >= 30 & raw_data$AGE <= 45 & raw_data$PAOCC10 > 0 & raw_data$OCC10 > 0 & raw_data$REALRINC > 0)))
  }
  to_impute <- raw_data %>%
    filter(AGE >= 30 & AGE <= 45) %>%
    filter(PAOCC10 > 0) %>%
    filter(OCC10 > 0) %>%
    filter(REALRINC > 0) %>%
    mutate(outcome = log(REALRINC),
           DEGREE = case_when(!(DEGREE %in% 8:9) ~ DEGREE),
           RACE = factor(RACE),
           DEGREE = factor(DEGREE),
           id = 1:n()) %>%
    group_by(YEAR) %>%
    mutate(weight = WTSSALL / sum(WTSSALL)) %>%
    group_by() %>%
    select(id, VSTRAT, weight, outcome, treated, X, RACE, SEX, DEGREE, AGE)

  # Note prevalence of missingness
  if (print_summaries) {
    print("Weighted proportion missing")
    print(data.frame(
      to_impute %>%
        summarize(outcome = weighted.mean(is.na(outcome), w = weight),
                  treated = weighted.mean(is.na(treated), w = weight),
                  X = weighted.mean(is.na(X), w = weight),
                  RACE = weighted.mean(is.na(RACE), w = weight),
                  DEGREE = weighted.mean(is.na(DEGREE), w = weight),
                  SEX = weighted.mean(is.na(SEX), w = weight),
                  AGE = weighted.mean(is.na(AGE), w = weight),
                  .groups = "drop") %>%
        melt(id = NULL)
    ))
  }

  # Singly impute since uncertainty will come from BRR
  imputed <- amelia(data.frame(to_impute),
                    noms = c("RACE","SEX","DEGREE","treated","X"),
                    idvars = c("id","VSTRAT"),
                    boot.type = "none", m = 1)$imputations$imp1
  return(imputed)
}

# Produce the main data set for the analyses
d <- sample_restrictions(GSS_merged, print_summaries = T)
# Repeat that several times to average over randomness, and produce descriptive statistics
many_imputations <- foreach(i = 1:10, .combine = "rbind") %do% sample_restrictions(GSS_merged)

#############################
# 2. Descriptive statistics #
#############################

# Note prevalence of group and of treatment
print("Proportion upper class")
print(many_imputations %>%
        summarize(X = weighted.mean(X, w = weight),
                  treated = weighted.mean(treated, w = weight),
                  .groups = "drop"))

# Make table of descriptive statistics by X
matrix_of_variables <- model.matrix(~-1 + outcome + treated + RACE + DEGREE + SEX + AGE,
                                    data = many_imputations)
# Add omitted degree category
matrix_of_variables <- cbind(matrix_of_variables[,1:5],
                             apply(matrix_of_variables[,6:9],1,function(x) 1 - sum(x)),
                             matrix_of_variables[,6:11])
colnames(matrix_of_variables)[6] <- "DEGREE0"
# Make Sex coded as 0 = male, 1 = female instead of 1 = male, 2 = female
matrix_of_variables[,"SEX"] <- matrix_of_variables[,"SEX"] - 1
# Calculate the weighted mean in each category
descriptives_upperOrigin <- apply(matrix_of_variables[many_imputations$X,], 2, weighted.mean, w = many_imputations$weight[many_imputations$X])
descriptives_lowerOrigin <- apply(matrix_of_variables[!many_imputations$X,], 2, weighted.mean, w = many_imputations$weight[!many_imputations$X])
descriptives <- cbind(upperOrigin = descriptives_upperOrigin,
                      lowerOrigin = descriptives_lowerOrigin)
# Write a function to rename variables
rename_variables <- function(x) {
  case_when(x == "outcome" ~ "Log income (1986 dollars)",
            x == "XTRUE" ~ "Professional class origin",
            x == "treated:XTRUE" ~ "Professional origin x Professional destination",
            x == "treatedTRUE" | x == "treated" ~ "Professional class destination (own occupation)",
            x == "RACE1" ~ "Race White",
            x == "RACE2" ~ "Race Black",
            x == "RACE3" ~ "Race Other",
            x == "DEGREE0" ~ "Degree Less than high school",
            x == "DEGREE1" ~ "Degree High school",
            x == "DEGREE2" ~ "Degree Junior college",
            x == "DEGREE3" ~ "Degree Bachelor's",
            x == "DEGREE4" ~ "Degree Graduate",
            x == "SEX" ~ "Female",
            x == "AGE" ~ "Age",
            T ~ x)
}
rownames(descriptives) <- sapply(rownames(descriptives), rename_variables)
print(xtable::xtable(descriptives))

################################
# 3. Calculate point estimates #
################################

gss_point_estimator <- function(data) {
  # Create a list of counterfactual assignment rules
  list_counterfactual_assignments <- list(
    under_treatment = 1,
    under_control = 0,
    marginal = weighted.mean(data$treated, w = data$weight),
    conditional = (data %>%
                     group_by(RACE, DEGREE, SEX, AGE) %>%
                     mutate(prop_treated = weighted.mean(treated, w = weight)))$prop_treated
  )
  # Assign folds for cross-fitting
  num_folds <- 2
  folded <- data %>%
    group_by(VSTRAT) %>%
    mutate(fold = sample(rep(1:num_folds,
                             ceiling(n() / num_folds)),
                         n(),
                         replace = F)) %>%
    ungroup()
  # Calculate point estimates
  point_estimates <- lapply(list_counterfactual_assignments, function(counterfactual_assignments_case) {
    glm_estimates <- gapclosing(data = folded,
                                counterfactual_assignments = counterfactual_assignments_case,
                                treatment_formula = formula(treated ~ X + RACE + DEGREE + SEX + AGE),
                                outcome_formula = formula(outcome ~ treated*X + RACE + DEGREE + SEX + AGE),
                                category_name = "X",
                                treatment_algorithm = "glm",
                                outcome_algorithm = "lm",
                                weight_name = "weight")
    gam_estimates <- gapclosing(data = folded,
                                counterfactual_assignments = counterfactual_assignments_case,
                                treatment_formula = formula(treated ~ X + RACE + DEGREE + SEX + s(AGE)),
                                outcome_formula = formula(outcome ~ treated*X + RACE + DEGREE + SEX + s(AGE)),
                                category_name = "X",
                                treatment_algorithm = "gam",
                                outcome_algorithm = "gam",
                                sample_split = "cross_fit",
                                folds_name = "fold",
                                weight_name = "weight")
    ranger_estimates <- gapclosing(data = folded,
                                   counterfactual_assignments = counterfactual_assignments_case,
                                   treatment_formula = formula(treated ~ X + RACE + DEGREE + SEX + AGE),
                                   outcome_formula = formula(outcome ~ X + RACE + DEGREE + SEX + AGE),
                                   category_name = "X",
                                   treatment_algorithm = "ranger",
                                   outcome_algorithm = "ranger",
                                   weight_name = "weight",
                                   sample_split = "cross_fit",
                                   folds_name = "fold")
    return(list(glm_estimates = glm_estimates,
                gam_estimates = gam_estimates,
                ranger_estimates = ranger_estimates))
  })
  # For comparison, get coefficient in conditional model
  simple_fit <- lm(outcome ~ X + treated + RACE + DEGREE + SEX + AGE,
                   data = data,
                   weights = weight)
  simple_fit_noTreatment <- lm(outcome ~ X + RACE + DEGREE + SEX + AGE,
                               data = data,
                               weights = weight)
  return(c(point_estimates,comparison_coefs = list(coef(simple_fit)),
           comparison_coefs_notreatment = list(coef(simple_fit_noTreatment))))
}
# Calculate the point estimate
point <- gss_point_estimator(d)
saveRDS(point, file = "intermediate/point.Rds")

###########################################################
# 4. Calculate estimates on each set of replicate weights #
###########################################################

# Calculate estimates on balanced repeated replicates
cores_to_use <- round(detectCores() / 2)
print(paste("Running in parallel over",cores_to_use,"cores"))
cl <- makeCluster(cores_to_use)
registerDoParallel(cl)
t0 <- Sys.time()
brr_estimates <- foreach(
  i = 1:ncol(gss_brr$repweights),
  .packages = c("tidyverse","Amelia","gapclosing")
) %dorng% {
  if (i %% 100 == 0) {
    print(paste("Replicate",i,"of",ncol(gss_brr$repweights)))
  }
  # Restrict to the chosen PSUs
  GSS_replicate <- GSS_merged[gss_brr$repweights[,i] > 0,]
  d_replicate <- sample_restrictions(GSS_replicate)
  point_star <- gss_point_estimator(d_replicate)
  point_star.df <- foreach(counterfactual_case = names(point_star)[1:4]) %do% {
    counterfactual_case_df <- foreach(model_case = names(point_star[[1]])) %do% {
      as_tibble(
        as.data.frame(point_star[[counterfactual_case]][[model_case]]) %>%
          mutate(counterfactual = counterfactual_case,
                 model = model_case)
      )
    }
    names(counterfactual_case_df) <- names(point_star[[1]])
    return(counterfactual_case_df)
  }
  names(point_star.df) <- names(point_star)[1:4]
  coef_star.df <- data.frame(model = "treatment",
                             variable = names(coef(point_star$under_treatment$glm_estimates$treatment_model)),
                             coef = coef(point_star$under_treatment$glm_estimates$treatment_model)) %>%
    bind_rows(data.frame(model = "outcome",
                         variable = names(coef(point_star$under_treatment$glm_estimates$outcome_model)),
                         coef = coef(point_star$under_treatment$glm_estimates$outcome_model)))
  comparison_coef_star.df <- data.frame(comparison = "given_predictors",
                                        variable = names(point_star$comparison_coefs),
                                        coef = point_star$comparison_coefs) %>%
    bind_rows(data.frame(comparison = "given_predictors_except_treatment",
                         variable = names(point_star$comparison_coefs_notreatment),
                         coef = point_star$comparison_coefs_notreatment))
  rownames(coef_star.df) <- rownames(comparison_coef_star.df) <- NULL
  return(list(estimates = point_star.df,
              coefficients = coef_star.df,
              comparison_coefficients = comparison_coef_star.df))
}
spent <- difftime(Sys.time(),t0)
print("Time spent on BRR estimates")
print(spent)
stopCluster(cl)
saveRDS(brr_estimates, file = "intermediate/brr_estimates_glm.Rds")

##############################################################
# 5. Aggregate the main estimates over the replicate weights #
#    to summarize with a point estimate and standard error   #
##############################################################

# Aggregate to an overall estimate of the estimands of interest
aggregated_estimates <- foreach(counterfactual_case = names(point)[1:4]) %do% {
  counterfactual_case_result <- foreach(model_case = names(point[[1]])) %do% {
    result.df <- foreach(i = 1:length(brr_estimates), .combine = "rbind") %do% {
      brr_estimates[[i]]$estimates[[counterfactual_case]][[model_case]]
    } %>%
      group_by(X,estimand,estimator,primary,counterfactual,model) %>%
      summarize(se = sd(estimate),
                .groups = "drop") %>%
      # Merge in the point estimate
      left_join(as.data.frame(point[[counterfactual_case]][[model_case]]),
                by = c("X","estimand","estimator","primary")) %>%
      # Reorder columns
      select(estimator, primary, counterfactual, model, X, estimand, estimate, se)
    # Convert to a list (as in a gapclosing object)
    result.list <- df_to_gapclosing_list(result.df)
    # Append the point estimator elements that do not exist in this list
    original.list <- point[[counterfactual_case]][[model_case]]
    result.list <- c(result.list,
                     original.list[!names(original.list) %in% names(result.list)])
    class(result.list) <- "gapclosing"
    return(result.list)
  }
  names(counterfactual_case_result) <- names(point[[1]])
  return(counterfactual_case_result)
}
names(aggregated_estimates) <- names(point)[1:4]
saveRDS(aggregated_estimates, file = "intermediate/aggregated_estimates.Rds")

################################################################
# 6. Aggregate coefficients (not of primary interest; reported #
#     in appendix) to point estimates and standard errors.     #
################################################################

# This includes the nuisance functions for doubly robust GLM estimation
# and the comparison to outcome models with and without the treatment.

# Aggregate the GLM nuisance function (treatment and outcome model) coefficients for a table
coef_point <- data.frame(model = "treatment",
                         variable = names(coef(point$under_treatment$glm_estimates$treatment_model)),
                         coef = coef(point$under_treatment$glm_estimates$treatment_model)) %>%
  bind_rows(data.frame(model = "outcome",
                       variable = names(coef(point$under_treatment$glm_estimates$outcome_model)),
                       coef = coef(point$under_treatment$glm_estimates$outcome_model)))
coef_se <- glm_coefficients <- foreach(i = 1:length(brr_estimates), .combine = "rbind") %do% {
  brr_estimates[[i]]$coefficients
} %>%
  # Take the standard deviation over replicates
  group_by(model,variable) %>%
  summarize(se = sd(coef),
            .groups = "drop")
rownames(coef_point) <- rownames(coef_se) <- NULL
coef_aggregated <- coef_point %>%
  left_join(coef_se,
            by = c("model","variable"))
saveRDS(coef_aggregated, file = "intermediate/coef_aggregated.Rds")

# Aggregate the comparison coefficients for a table
comparison_coef_point <- data.frame(comparison = "given_predictors",
                                    variable = names(point$comparison_coefs),
                                    coef = point$comparison_coefs) %>%
  bind_rows(data.frame(comparison = "given_predictors_except_treatment",
                       variable = names(point$comparison_coefs_notreatment),
                       coef = point$comparison_coefs_notreatment))
comparison_coef_se <- foreach(i = 1:length(brr_estimates), .combine = "rbind") %do% {
  brr_estimates[[i]]$comparison_coefficients
} %>%
  group_by(comparison, variable) %>%
  summarize(se = sd(coef),
            .groups = "drop")
rownames(comparison_coef_point) <- rownames(comparison_coef_se) <- NULL
comparison_coef_aggregated <- comparison_coef_point %>%
  left_join(comparison_coef_se,
            by = c("comparison","variable"))
saveRDS(comparison_coef_aggregated, file = "intermediate/comparison_coef_aggregated.Rds")

#####################################
# 7. Print main results to log file #
#####################################

# Briefly override the print.tibble function
# to make these outputs work correctly in the text log
print.tibble <- function(x) print(data.frame(x))

print("DOUBLE-ROBUST GLM: UNDER TREATMENT")
summary(aggregated_estimates$under_treatment$glm_estimates)

print("DOUBLE-ROBUST GLM: UNDER CONTROL")
summary(aggregated_estimates$under_control$glm_estimates)

print("DOUBLE-ROBUST GLM: MARGINAL")
summary(aggregated_estimates$marginal$glm_estimates)

print("DOUBLE-ROBUST GLM: CONDITIONAL")
summary(aggregated_estimates$conditional$glm_estimates)

print("NUISANCE FUNCTIONS FOR PLACEMENT IN A TABLE")
print(xtable::xtable(data.frame(coef_aggregated %>%
                                  mutate(variable = fct_reorder(factor(variable),1:n())) %>%
                                  select(variable, coef, se, model) %>%
                                  melt(id = c("variable","model"), variable.name = "quantity") %>%
                                  spread(key = model, value = value) %>%
                                  mutate(variable = fct_relevel(variable,
                                                                "XTRUE","treated:XTRUE","treated","AGE",
                                                                "DEGREE1","DEGREE2","DEGREE3","DEGREE4",
                                                                "RACE2","RACE3","SEX")) %>%
                                  arrange(variable, quantity) %>%
                                  mutate(variable = rename_variables(as.character(variable))) %>%
                                  mutate(outcome = format(round(outcome,2),nsmall = 2),
                                         treatment = format(round(treatment,2),nsmall = 2),
                                         outcome = case_when(quantity == "coef" ~ outcome,
                                                             quantity == "se" ~ paste0("(",gsub(" ","",outcome),")")),
                                         treatment = case_when(quantity == "coef" ~ treatment,
                                                               quantity == "se" ~ paste0("(",gsub(" ","",treatment),")")),
                                         variable = case_when(quantity == "coef" ~ variable,
                                                              quantity == "se" ~ "")) %>%
                                  select(-quantity))),
      include.rownames = F)

print("COMPARISON COEFFICIENTS FOR CONDITIONAL ESTIMANDS")
print(xtable::xtable(data.frame(comparison_coef_aggregated %>%
                                  mutate(variable = fct_reorder(factor(variable),1:n())) %>%
                                  select(variable, coef, se, comparison) %>%
                                  melt(id = c("variable","comparison"), variable.name = "quantity") %>%
                                  spread(key = comparison, value = value) %>%
                                  mutate(variable = fct_relevel(variable,
                                                                "XTRUE","treated","AGE",
                                                                "DEGREE1","DEGREE2","DEGREE3","DEGREE4",
                                                                "RACE2","RACE3","SEX")) %>%
                                  arrange(variable, quantity) %>%
                                  mutate(variable = rename_variables(as.character(variable))) %>%
                                  mutate(given_predictors = format(round(given_predictors,2),nsmall = 2),
                                         given_predictors_except_treatment = format(round(given_predictors_except_treatment,2),nsmall = 2),
                                         given_predictors = case_when(quantity == "coef" ~ given_predictors,
                                                                      quantity == "se" ~ paste0("(",gsub(" ","",given_predictors),")")),
                                         given_predictors_except_treatment = case_when(quantity == "coef" ~ given_predictors_except_treatment,
                                                                                       quantity == "se" ~ paste0("(",gsub(" ","",given_predictors_except_treatment),")")),
                                         variable = case_when(quantity == "coef" ~ variable,
                                                              quantity == "se" ~ "")) %>%
                                  select(variable, given_predictors_except_treatment, given_predictors) %>%
                                  rename(not_conditional_on_treatment = given_predictors_except_treatment,
                                         conditional_on_treatment = given_predictors))),
      include.rownames = F)

print("COMPARISON MODELS COEFFICIENT ON CATEGORY, WITH P-VALUES")
print(data.frame(comparison_coef_aggregated %>%
                   filter(variable == "XTRUE") %>%
                   mutate(pval = 2*pnorm(-abs(coef / se)))))

# Close the sink
print(Sys.time())
print("Time spent:")
print(difftime(Sys.time(),t0))
sink()
