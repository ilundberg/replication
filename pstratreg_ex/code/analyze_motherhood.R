
sink("../logs/analyze_motherhood.txt")

t0 <- Sys.time()
print(t0)

library(tidyverse)
library(Amelia)
library(foreach)
library(doParallel)
library(doRNG)
cl <- makeCluster(6)
registerDoParallel(cl)
library(pstratreg)

bs_reps <- 100

set.seed(14850)

# Load the prepared data
d_prepared <- readRDS("../intermediate/motherhood.RDS")

estimator <- function(data, bs = F, homoskedastic = F, monotonicity_positive, monotonicity_negative, diagnostic_file_prefix = NULL) {
  
  # Bootstrap if requested
  if (bs) {
    # choose people, not rows, for a cluster bootstrap at the person level
    # to mimic the sampling process where people are sampled
    original_people <- unique(data$PUBID)
    chosen <- sample(original_people, replace = T)
    data_star <- foreach(case = chosen, .combine = "rbind") %do% {
      data %>%
        filter(PUBID == case)
    }
    data_star <- sample_frac(data, replace = T)
  } else {
    data_star <- data
  }
  
  # Impute missing values
  amelia.out <- amelia(
    x = data_star %>% 
      select(-year_1,-year_2,
             -age_at_birth,-age_2,
             -sex, # since will be separate by sex anyhow
             -birth_year),
    m = 1,
    idvars = c("PUBID","w"),
    noms = c("race","marital","fulltime"),
    ords = "educ",
    boot.type = "none"
  )
  
  # Keep original unimputed outcome
  data_imp <- amelia.out$imputations$imp1 %>%
    mutate(wage = data_star$wage)

  # and enforce original bounds on numeric variables
  numeric_vars <- c("age_1","wage_baseline","tenure","experience")
  for (varname in numeric_vars) {
    original_range <- range(data[[varname]], na.rm = T)
    x <- data_imp[[varname]]
    if (any(x < original_range[1])) {
      x[x < original_range[1]] <- original_range[1]
    }
    if (any(x > original_range[2])) {
      x[x > original_range[2]] <- original_range[2]
    }
    data_imp[[varname]] <- x
  }

  result <- pstratreg(
    formula_y = wage ~ treated*(race + poly(age_1,2) + educ + marital + fulltime + log(tenure + 1) + 
                                  log(experience + 1) + wage_baseline + employed_baseline),
    formula_m = employed ~ treated*(race + poly(age_1,2) + educ + marital + fulltime + log(tenure + 1) + 
                                      log(experience + 1) + wage_baseline + employed_baseline),
    formula_sq_resid = ~ (treated + race + poly(age_1,2) + educ + marital + fulltime + log(tenure + 1) + 
                            log(experience + 1) + wage_baseline + employed_baseline),
    data = data_imp,
    weights = data_imp$w,
    treatment_name = "treated",
    homoskedastic = homoskedastic,
    monotonicity_positive = monotonicity_positive,
    monotonicity_negative = monotonicity_negative,
    aggregate = T,
    group_vars = "treated"
  )
  
  if (!is.null(diagnostic_file_prefix)) {
    
    # Histogram of residuals
    data.frame(x = result$fit_y$resid) %>%
      ggplot(aes(x = x)) +
      geom_histogram(bins = 30) +
      theme_bw() +
      xlab("Residuals in Log Wage Model") +
      ylab("Count")
    ggsave(paste0(diagnostic_file_prefix,"resid_hist.pdf"),
           height = 3, width = 3.5)
    
    # QQ plot of residuals
    sample_quantiles <- sapply(seq(.01,.99,.01), function(x) quantile(result$fit_y$resid / sd(result$fit_y$resid), prob = x))
    theory_quantiles <- qnorm(seq(.01,.99,.01))
    data.frame(sample = sample_quantiles,
               theory = theory_quantiles) %>%
      ggplot(aes(x = theory, y = sample)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1) +
      ylab("Quantiles of\nStandardized Residuals") +
      xlab("Normal Quantiles") +
      theme_bw()
    ggsave(paste0(diagnostic_file_prefix,"qq.pdf"),
           height = 3, width = 3.5)
    
    # QQ plot adjusted for heteroskedasticity
    if (!homoskedastic) {
      sd_hat <- sqrt(predict(result$fit_sq_resid, type = "response"))
      scaled_resid <- result$fit_y$residuals / sd_hat
      sample_quantiles <- sapply(seq(.01,.99,.01), function(x) quantile(scaled_resid, prob = x))
      theory_quantiles <- qnorm(seq(.01,.99,.01))
      data.frame(sample = sample_quantiles,
                 theory = theory_quantiles) %>%
        ggplot(aes(x = theory, y = sample)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1) +
        ylab("Quantiles of\nStandardized Residuals") +
        xlab("Normal Quantiles") +
        theme_bw()
      ggsave(paste0(diagnostic_file_prefix,"qq_heteroskedasticity.pdf"),
             height = 3, width = 3.5)
    }
    
  }
  
  # Create a data frame to return, with the ATT estimates
  to_return <- result$estimates_m %>% filter(treated) %>% select(-treated) %>%
    bind_cols(result$estimates_y %>% filter(treated) %>% select(-treated))
  
  return(to_return)
}

# MOTHERHOOD, HETEROSKEDASTIC
# Point estimate
point <- estimator(
  data = d_prepared %>% filter(sex == "Women"), 
  bs = F,
  monotonicity_positive = F,
  monotonicity_negative = T,
  diagnostic_file_prefix = "../figures/motherhood_"
)
# Bootstrap estimates
point_star <- foreach(
  rep = 1:bs_reps, 
  .combine = "rbind", 
  .packages = c("tidyverse","Amelia","pstratreg")
) %dorng% {
  estimator(data = d_prepared %>% filter(sex == "Women"), 
            bs = T,
            monotonicity_positive = F,
            monotonicity_negative = T)
}
# Standard error over bootstrap estimates
se <- apply(point_star,2,sd)
# Visualize result
motherhood_result <- data.frame(estimand = names(point),
           estimate = t(point),
           se = se) %>%
  mutate(ci.min = estimate - qnorm(.975) * se,
         ci.max = estimate + qnorm(.975) * se,
         label = format(round(estimate,2),nsmall=2)) %>%
  print()
saveRDS(motherhood_result,
        file = "../intermediate/motherhood_result.RDS")

# MOTHERHOOD, ASSUMED HOMOSKEDASTIC
# Point estimate
point <- estimator(
  data = d_prepared %>% filter(sex == "Women"), 
  bs = F,
  homoskedastic = T,
  monotonicity_positive = F,
  monotonicity_negative = T,
  diagnostic_file_prefix = "../figures/motherhood_homoskedastic_"
)
# Bootstrap estimates
point_star <- foreach(
  rep = 1:bs_reps, 
  .combine = "rbind", 
  .packages = c("tidyverse","Amelia","pstratreg")
) %dorng% {
  estimator(data = d_prepared %>% filter(sex == "Women"), 
            bs = T,
            homoskedastic = T,
            monotonicity_positive = F,
            monotonicity_negative = T)
}
# Standard error over bootstrap estimates
se <- apply(point_star,2,sd)
# Visualize result
motherhood_result <- data.frame(estimand = names(point),
                                estimate = t(point),
                                se = se) %>%
  mutate(ci.min = estimate - qnorm(.975) * se,
         ci.max = estimate + qnorm(.975) * se,
         label = format(round(estimate,2),nsmall=2)) %>%
  print()
saveRDS(motherhood_result,
        file = "../intermediate/motherhood_result_homoskedastic.RDS")

# FATHERHOOD, HETEROSKEDASTIC
# Point estimate
point <- estimator(
  data = d_prepared %>% filter(sex == "Men"), 
  bs = F,
  monotonicity_positive = F,
  monotonicity_negative = F,
  diagnostic_file_prefix = "../figures/fatherhood_"
)
# Bootstrap estimates
point_star <- foreach(
  rep = 1:bs_reps, 
  .combine = "rbind", 
  .packages = c("tidyverse","Amelia","pstratreg")
) %dorng% {
  estimator(data = d_prepared %>% filter(sex == "Men"), 
            bs = T,
            monotonicity_positive = F,
            monotonicity_negative = F)
}
# Standard error over bootstrap estimates
se <- apply(point_star,2,sd)
# Visualize result
fatherhood_result <- data.frame(estimand = names(point),
           estimate = t(point),
           se = se) %>%
  mutate(ci.min = estimate - qnorm(.975) * se,
         ci.max = estimate + qnorm(.975) * se,
         label = format(round(estimate,2),nsmall=2)) %>%
  print()
saveRDS(fatherhood_result,
        file = "../intermediate/fatherhood_result.RDS")

# FATHERHOOD, HOMOSKEDASTIC
# Point estimate
point <- estimator(
  data = d_prepared %>% filter(sex == "Men"), 
  bs = F,
  homoskedastic = T,
  monotonicity_positive = F,
  monotonicity_negative = F,
  diagnostic_file_prefix = "../figures/motherhood_homoskedastic_"
)
# Bootstrap estimates
point_star <- foreach(
  rep = 1:bs_reps, 
  .combine = "rbind", 
  .packages = c("tidyverse","Amelia","pstratreg")
) %dorng% {
  estimator(data = d_prepared %>% filter(sex == "Men"), 
            bs = T,
            homoskedastic = T,
            monotonicity_positive = F,
            monotonicity_negative = F)
}
# Standard error over bootstrap estimates
se <- apply(point_star,2,sd)
# Visualize result
fatherhood_result <- data.frame(estimand = names(point),
                                estimate = t(point),
                                se = se) %>%
  mutate(ci.min = estimate - qnorm(.975) * se,
         ci.max = estimate + qnorm(.975) * se,
         label = format(round(estimate,2),nsmall=2)) %>%
  print()
saveRDS(fatherhood_result,
        file = "../intermediate/fatherhood_result_homoskedastic.RDS")

sessionInfo()

print("Finish time")
print(Sys.time())
print("Time spent")
difftime(Sys.time(),t0)

sink()