

# Author: Ian Lundberg (ilundberg@princeton.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Paper is a working draft. Code will be posted on the Harvard Dataverse when the paper is finalized for publication.

# See run_all.R to set up the working environment.
# This file analyzes the empirical example.

# Initialize sink file to hold printed output
sink("figures/class_gap_example_out.txt")

# Set seed
set.seed(08544)

# Load packages
source("code/prepare_environment.R")

# Load data
# The crosswalk comes from a GSS methodological memo.
# It is available at: https://osf.io/xb2yz/
crosswalk <- read_csv("data/occ10-to-egp-class-crosswalk.csv")
# The data come from the General Social Survey,
# available at: https://gss.norc.org/
source("data/GSS.r")

# Note that lone PSUs are very rare in the sample
GSS %>%
  group_by(VSTRAT) %>%
  mutate(lone_psu = n_distinct(VPSU) == 1) %>%
  group_by() %>%
  summarize(num_lone_psu = sum(lone_psu),
            mean_lone_psu = weighted.mean(lone_psu, w = WTSSALL),
            .groups = "drop")

# Remove the lone PSUs, merge in crosswalk, and define variables
GSS <- GSS  %>%
  group_by(VSTRAT) %>%
  mutate(lone_psu = n_distinct(VPSU) == 1) %>%
  group_by() %>%
  filter(!lone_psu) %>%
  select(-lone_psu) %>%
  left_join(crosswalk %>%
              select(occ10,egp10_10) %>%
              rename(OCC10 = occ10,
                     egp_r = egp10_10), by = "OCC10") %>%
  left_join(crosswalk %>%
              select(occ10,egp10_10) %>%
              rename(PAOCC10 = occ10,
                     egp_pa = egp10_10), by = "PAOCC10") %>%
  # Define treatment treated and group-defining category X
  mutate(treated = as.numeric(egp_r == 1),
         X = egp_pa == 1)
save(GSS, file = "intermediate/GSS_clean.Rdata")

# Define balanced repeated replicates that will be used for uncertainty.
set.seed(08544)
# Define the survey design in the survey package
gss_svydesign <- svydesign(ids = ~ cluster_id,
                           strata = ~ VSTRAT,
                           weights = ~ WTSSALL,
                           data = GSS %>%
                             mutate(cluster_id = paste(VSTRAT,VPSU, sep = "_")))
# Create balanced repeated replicate weights
# The advantage of a computational approach is that inference works for
# a wide variety of estimation algorithms, without any adaptation.
# This line takes a long time.
gss_brr <- as.svrepdesign(gss_svydesign, type = "BRR", compress = F)
save(gss_brr, file = "intermediate/gss_brr.Rdata")

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
d <- sample_restrictions(GSS, print_summaries = T)
# Repeat that several times to average over randomness, and produce descriptive statistics
set.seed(08544)
many_imputations <- foreach(i = 1:10, .combine = "rbind") %do% sample_restrictions(GSS)

# Note prevalence of group and of treatment
print("Proportion upper class")
print(many_imputations %>%
        summarize(X = weighted.mean(X, w = weight),
                  treated = weighted.mean(treated, w = weight),
                  .groups = "drop"))

# Make table of descriptive statistics by X
matrix_of_variables <- model.matrix(~-1 + outcome + treated + RACE + DEGREE + SEX + AGE,
                                    data = many_imputations)
descriptives_upperOrigin <- apply(matrix_of_variables[many_imputations$X,], 2, weighted.mean, w = many_imputations$weight[many_imputations$X])
descriptives_lowerOrigin <- apply(matrix_of_variables[!many_imputations$X,], 2, weighted.mean, w = many_imputations$weight[!many_imputations$X])
descriptives <- cbind(upperOrigin = descriptives_upperOrigin,
                      lowerOrigin = descriptives_lowerOrigin)
# Add the omitted categories
descriptives <- rbind(descriptives[1:3,], RACE1 = 1 - colSums(descriptives[4:5,]), descriptives[4:11,])
# Make sex 0 = male, 1 = female instead of 1 and 2
descriptives["SEX",] <- descriptives["SEX",] - 1
# Update names for the table
descriptives <- rbind(descriptives[1:6,], DEGREE0 = 1 - colSums(descriptives[7:10,]), descriptives[7:12,])
# Remove the redundant lower class occupation
descriptives <- descriptives[-2,]
rownames(descriptives) <- sapply(rownames(descriptives), function(x) {
  case_when(x == "outcome" ~ "Log income",
            x == "treatedTRUE" ~ "Upper class occupation",
            x == "RACE1" ~ "Race: White",
            x == "RACE2" ~ "Race: Black",
            x == "RACE3" ~ "Race: Other",
            x == "DEGREE0" ~ "Degree: Less than high school",
            x == "DEGREE1" ~ "Degree: High school",
            x == "DEGREE2" ~ "Degree: Junior college",
            x == "DEGREE3" ~ "Degree: Bachelor's",
            x == "DEGREE4" ~ "Degree: Graduate",
            x == "SEX" ~ "Female",
            x == "AGE" ~ "Age")
})
print(xtable::xtable(descriptives))

# Note some class 1 occupations
unique(crosswalk$title[crosswalk$egp10_10 == 1])

# Calculate point estimates
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
    group_by()
  # Calculate point estimates
  point_estimates <- lapply(list_counterfactual_assignments, function(counterfactual_assignments_case) {
    glm_estimates <- gapclosing(data = data,
                                counterfactual_assignments = counterfactual_assignments_case,
                                treatment_formula = formula(treated ~ X + RACE + DEGREE + SEX + AGE),
                                outcome_formula = formula(outcome ~ treated*X + RACE + DEGREE + SEX + AGE),
                                category_name = "X",
                                treatment_algorithm = "glm",
                                outcome_algorithm = "lm",
                                weight_name = "weight")
    gam_estimates <- gapclosing(data = data,
                                counterfactual_assignments = counterfactual_assignments_case,
                                treatment_formula = formula(treated ~ X + RACE + DEGREE + SEX + s(AGE)),
                                outcome_formula = formula(outcome ~ treated*X + RACE + DEGREE + SEX + s(AGE)),
                                category_name = "X",
                                treatment_algorithm = "gam",
                                outcome_algorithm = "gam",
                                sample_split = "cross_fit",
                                folds = folded$fold,
                                weight_name = "weight")
    ranger_estimates <- gapclosing(data = data,
                                   counterfactual_assignments = counterfactual_assignments_case,
                                   treatment_formula = formula(treated ~ X + RACE + DEGREE + SEX + AGE),
                                   outcome_formula = formula(outcome ~ X + RACE + DEGREE + SEX + AGE),
                                   category_name = "X",
                                   treatment_algorithm = "ranger",
                                   outcome_algorithm = "ranger",
                                   weight_name = "weight",
                                   sample_split = "cross_fit",
                                   folds = folded$fold)
    return(list(outcome_modeling = glm_estimates$all_estimates$outcome_modeling[,c("setting","category","estimate")],
                treatment_modeling = glm_estimates$all_estimates$treatment_modeling[,c("setting","category","estimate")],
                doubly_robust = glm_estimates$all_estimates$doubly_robust[,c("setting","category","estimate")],
                gam_estimates = gam_estimates$primary_estimate[,c("setting","category","estimate")],
                ranger_estimates = ranger_estimates$primary_estimate[,c("setting","category","estimate")],
                outcome_coefs = coef(glm_estimates$outcome_model),
                treatment_coefs = coef(glm_estimates$treatment_model)))
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

# Calculate the estimates on the balanced repeated replicates
cl <- makeCluster(4)
registerDoParallel(cl)
t0 <- Sys.time()
brr_estimates <- foreach(
  i = 1:ncol(gss_brr$repweights),
  .packages = c("tidyverse","Amelia","gapclosing")
) %dorng% {
  # Restrict to the chosen PSUs
  GSS_replicate <- GSS[gss_brr$repweights[,i] > 0,]
  d_replicate <- sample_restrictions(GSS_replicate)
  gss_point_estimator(d_replicate)
}
spent <- difftime(Sys.time(),t0)
print("Time spent on BRR estimates")
print(spent)
stopCluster(cl)
save(brr_estimates, file = "intermediate/brr_estimates_glm.Rdata")

# Aggregate to standard errors
se_estimate <- foreach(method_case = c("treatment_modeling","outcome_modeling","doubly_robust","gam_estimates","ranger_estimates"), .combine = "rbind") %do% {
  foreach(counterfactual_case = c("under_treatment","under_control","marginal","conditional"), .combine = "rbind") %do% {
    these_estimates <- foreach(r = 1:length(brr_estimates), .combine = "rbind") %do% {
      brr_estimates[[r]][[counterfactual_case]][[method_case]]
    }
    se <- these_estimates %>%
      group_by(setting, category) %>%
      summarize(se = sd(estimate), .groups = "drop") %>%
      mutate(method = method_case,
             counterfactual = counterfactual_case)
  }
}

# Combine the point and SE estimates
counterfactual_estimates <- foreach(method_case = c("treatment_modeling","outcome_modeling","doubly_robust","gam_estimates","ranger_estimates"), .combine = "rbind") %do% {
  foreach(counterfactual_case = c("under_treatment","under_control","marginal","conditional"), .combine = "rbind") %do% {
    point[[counterfactual_case]][[method_case]] %>%
      mutate(method = method_case,
             counterfactual = counterfactual_case)
  }
} %>%
  left_join(se_estimate, by = c("method","counterfactual","setting","category"))
save(counterfactual_estimates, file = "intermediate/counterfactual_estimates.Rdata")

# Extract coefficients of the treatment model, outcome model, and comparison model
# model_name will take values comparison_coefs, treatment_coefs, outcome_coefs
get_coefficient_estimates <- function(model_name) {
  if (model_name == "comparison_coefs") {
    coef_point <- point$comparison_coefs
  } else if (model_name == "comparison_coefs_notreatment") {
    coef_point <- point$comparison_coefs_notreatment
  } else {
    # Note that the treatment and outcome coefficients are the same under any counterfactual rule,
    # so extract them from only one rule (under_treatment)
    coef_point <- point$under_treatment[[model_name]]
  }
  coef_reps <- foreach(r = 1:length(brr_estimates), .combine = "rbind") %do% {
    if (model_name == "comparison_coefs") {
      coef_star <- brr_estimates[[r]]$comparison_coefs
    } else if (model_name == "comparison_coefs_notreatment") {
      coef_star <- brr_estimates[[r]]$comparison_coefs_notreatment
    } else {
      coef_star <- brr_estimates[[r]]$under_treatment[[model_name]]
    }
    if (!all(names(coef_star) == names(coef_point))) {
      stop("ERROR: Replicate and point estimate names do not match")
    }
    coef_star
  }
  to_return <- data.frame(covariate = names(coef_point),
                          coefficient = coef_point,
                          se = apply(coef_reps,2,sd)) %>%
    mutate(ci.min = coefficient - qnorm(.975) * se,
           ci.max = coefficient + qnorm(.975) * se)
  rownames(to_return) <- NULL
  return(to_return)
}
# Use that function to extract the coefficients and standard errors
comparison_coefs <- get_coefficient_estimates("comparison_coefs")
comparison_coefs_notreatment <- get_coefficient_estimates("comparison_coefs_notreatment")
treatment_coefs <- get_coefficient_estimates("treatment_coefs")
outcome_coefs <- get_coefficient_estimates("outcome_coefs")
save(comparison_coefs, file = "intermediate/comparison_coefs.Rdata")
save(comparison_coefs_notreatment, file = "intermediate/comparison_coefs_notreatment.Rdata")
save(treatment_coefs, file = "intermediate/treatment_coefs.Rdata")
save(outcome_coefs, file = "intermediate/outcome_coefs.Rdata")

######################
# Print main results #
######################

print("Doubly-robust estimates")
print(counterfactual_estimates %>%
        filter(method == "doubly_robust") %>%
        select(counterfactual, setting, category, estimate, se),
      digits = 2)

print("Fitted treatment model")
print(treatment_coefs, digits = 2)

print("Fitted outcome model")
print(outcome_coefs, digits = 2)

print("Fitted comparison model")
print(comparison_coefs, digits = 2)

print("Fitted comparison model, without treatment")
print(comparison_coefs_notreatment, digits = 2)

print("Significance of comparison coefficient on X in comparison model")
print(comparison_coefs %>%
        filter(covariate == "XTRUE") %>%
        mutate(pval = 2*pnorm(-abs(coefficient / se))))

print("Significance of comparison coefficient on X in comparison model with no treatment")
print(comparison_coefs_notreatment %>%
        filter(covariate == "XTRUE") %>%
        mutate(pval = 2*pnorm(-abs(coefficient / se))))

print("Outcome and treatment coefficients arranged for placement in a table")
print(data.frame(outcome_coefs %>%
                   mutate(model = "outcome") %>%
                   bind_rows(treatment_coefs %>%
                               mutate(model = "treatment")) %>%
                   mutate(covariate = fct_reorder(factor(covariate),1:n())) %>%
                   select(covariate, coefficient, se, model) %>%
                   melt(id = c("covariate","model"), variable.name = "quantity") %>%
                   spread(key = model, value = value) %>%
                   mutate(covariate = fct_relevel(covariate,
                                                  "XTRUE","treated:XTRUE","treated","AGE",
                                                  "DEGREE1","DEGREE2","DEGREE3","DEGREE4",
                                                  "RACE2","RACE3","SEX")) %>%
                   arrange(covariate, quantity) %>%
                   mutate(outcome = format(round(outcome,2),nsmall = 2),
                          treatment = format(round(treatment,2),nsmall = 2))))

print("Comparison coefficients arranged for placement in a table")
print(data.frame(comparison_coefs_notreatment %>%
                   mutate(model = "no_treatment") %>%
                   bind_rows(comparison_coefs %>%
                               mutate(model = "with_treatment")) %>%
                   mutate(covariate = fct_reorder(factor(covariate),1:n())) %>%
                   select(covariate, coefficient, se, model) %>%
                   melt(id = c("covariate","model"), variable.name = "quantity") %>%
                   spread(key = model, value = value) %>%
                   mutate(covariate = fct_relevel(covariate,
                                                  "XTRUE","treated","AGE",
                                                  "DEGREE1","DEGREE2","DEGREE3","DEGREE4",
                                                  "RACE2","RACE3","SEX")) %>%
                   arrange(covariate, quantity) %>%
                   mutate(no_treatment = format(round(no_treatment,2),nsmall = 2),
                          with_treatment = format(round(with_treatment,2),nsmall = 2))))

# Close the sink
sink()
