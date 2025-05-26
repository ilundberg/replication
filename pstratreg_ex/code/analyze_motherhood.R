
sink("../logs/analyze_motherhood.txt")

t0 <- Sys.time()
print(t0)

library(tidyverse)
library(Amelia)
library(foreach)
library(doParallel)
library(doRNG)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
library(pstratreg)

bs_reps <- 500
print(paste("Bootstrap reps:",bs_reps))
print(paste("Cores:",detectCores()))

set.seed(14850)

# Load the prepared data
d_prepared <- readRDS("../intermediate/motherhood.RDS")

estimator <- function(
    data, 
    bs = F, 
    homoskedastic = F, 
    monotonicity_positive = F, 
    monotonicity_negative = F, 
    mean_dominance_y1_positive = F,
    mean_dominance_y1_negative = F,
    mean_dominance_y0_positive = F,
    mean_dominance_y0_negative = F,
    diagnostic_file_prefix = NULL,
    group_by_baseline_wage = F
) {
  
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
  
  # Create categories of baseline wages
  if (group_by_baseline_wage) {
    data_imp <- data_imp |>
      mutate(
        wage_baseline_categories = factor(
          case_when(
            !employed ~ 1,
            wage_baseline < log(15) ~ 2,
            wage_baseline < log(20) ~ 3,
            T ~ 4
          ),
          labels = c("Not employed","Below $15","$15 to $20","Over $20")
        )
      )
    group_vars <- c("treated","wage_baseline_categories")
  } else {
    group_vars <- "treated"
  }

  result <- pstratreg(
    formula_y = wage ~ treated*(race + poly(age_1,2) + educ + marital + fulltime + log(tenure + 1) + 
                                  log(experience + 1) + wage_baseline + employed_baseline),
    formula_s = employed ~ treated*(race + poly(age_1,2) + educ + marital + fulltime + log(tenure + 1) + 
                                      log(experience + 1) + wage_baseline + employed_baseline),
    formula_sq_resid = ~ (treated + race + poly(age_1,2) + educ + marital + fulltime + log(tenure + 1) + 
                            log(experience + 1) + wage_baseline + employed_baseline),
    data = data_imp,
    weights = data_imp$w,
    treatment_name = "treated",
    homoskedastic = homoskedastic,
    monotonicity_positive = monotonicity_positive,
    monotonicity_negative = monotonicity_negative,
    mean_dominance_y1_positive = mean_dominance_y1_positive,
    mean_dominance_y1_negative = mean_dominance_y1_negative,
    mean_dominance_y0_positive = mean_dominance_y0_positive,
    mean_dominance_y0_negative = mean_dominance_y0_negative,
    aggregate = T,
    group_vars = group_vars
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
  to_return <- result$estimates_s |> filter(treated) |>
    left_join(
      result$estimates_y |> filter(treated),
      by = group_vars
    ) |>
    select(-treated)
  
  return(to_return)
}

# ESTIMATE EMPLOYMENT WITHIN SUBGROUPS, AMONG WOMEN
point <- estimator(
  data = d_prepared |>  filter(sex == "Women"),
  bs = F, 
  group_by_baseline_wage = TRUE
)
point_star <- foreach(
  rep = 1:bs_reps, 
  .combine = "rbind", 
  .packages = c("tidyverse","pstratreg","Amelia")
) %dorng% {
  estimator(
    data = d_prepared |>  filter(sex == "Women"),
    bs = T, 
    group_by_baseline_wage = TRUE
  )
}
point |>
  select(wage_baseline_categories, effect_s) |>
  left_join(
    point_star |>
      group_by(wage_baseline_categories) |>
      summarize(se = sd(effect_s), .groups = "drop"),
    by = join_by(wage_baseline_categories)
  ) |>
  mutate(
    ci.min = effect_s - qnorm(.975) * se, 
    ci.max = effect_s + qnorm(.975) * se
  ) |>
  saveRDS("../intermediate/motherhood_within_subgroups.RDS")

# ESTIMATE EMPLOYMENT WITHIN SUBGROUPS, AMONG MEN
point <- estimator(
  data = d_prepared |>  filter(sex == "Men"),
  bs = F, 
  group_by_baseline_wage = TRUE
)
point_star <- foreach(
  rep = 1:bs_reps, 
  .combine = "rbind", 
  .packages = c("tidyverse","pstratreg","Amelia")
) %dorng% {
  estimator(
    data = d_prepared |>  filter(sex == "Men"),
    bs = T, 
    group_by_baseline_wage = TRUE
  )
}
point |>
  select(wage_baseline_categories, effect_s) |>
  left_join(
    point_star |>
      group_by(wage_baseline_categories) |>
      summarize(se = sd(effect_s), .groups = "drop"),
    by = join_by(wage_baseline_categories)
  ) |>
  mutate(
    ci.min = effect_s - qnorm(.975) * se, 
    ci.max = effect_s + qnorm(.975) * se
  ) |>
  saveRDS("../intermediate/fatherhood_within_subgroups.RDS")

# Estimates for main quantities of interest
# Create a set of arguments.
# For women, consider both mean dominance and monotonicity.
# For men, consider only mean dominance since monotonicity is unlikely to hold.
args <- tibble(
  sex = c("Men","Men","Women","Women","Women","Women"),
  monotonicity = c(F,F,F,F,T,T),
  mean_dominance = c(F,T,F,T,F,T)
)

estimates <- foreach(i = 1:nrow(args), .combine = "rbind") %do% {
  
  # Extract arguments for this setting
  args_case <- args[i,]
  
  # Construct point estimate
  point <- estimator(
    data = d_prepared |> filter(sex == args_case$sex),
    bs = F, 
    monotonicity_negative = args_case$monotonicity,
    mean_dominance_y0_positive = args_case$mean_dominance,
    mean_dominance_y1_positive = args_case$mean_dominance
  )
  
  # Construct bootstrap replicates
  point_star <- foreach(
    rep = 1:bs_reps, 
    .combine = "rbind", 
    .packages = c("tidyverse","pstratreg","Amelia")
  ) %dorng% {
    estimator(
      data = d_prepared |> filter(sex == args_case$sex),
      bs = T, 
      monotonicity_negative = args_case$monotonicity,
      mean_dominance_y0_positive = args_case$mean_dominance,
      mean_dominance_y1_positive = args_case$mean_dominance
    )
  }
  
  # Construct standard errors and confidence intervals
  se <- point_star |>
    summarize_all(.funs = sd)
  ci.min <- point_star |>
    summarize_all(.funs = function(x) quantile(x, .025))
  ci.max <- point_star |>
    summarize_all(.funs = function(x) quantile(x, .975))
  
  estimate <- point |>
    pivot_longer(
      cols = everything(), 
      names_to = "estimand", 
      values_to = "estimate"
    ) |>
    left_join(
      se |>
        pivot_longer(
          cols = everything(), 
          names_to = "estimand", 
          values_to = "se"
        ),
      by = "estimand"
    ) |>
    left_join(
      ci.min |>
        pivot_longer(
          cols = everything(), 
          names_to = "estimand", 
          values_to = "ci.min"
        ),
      by = "estimand"
    ) |>
    left_join(
      ci.max |>
        pivot_longer(
          cols = everything(), 
          names_to = "estimand", 
          values_to = "ci.max"
        ),
      by = "estimand"
    ) |>
    mutate(
      ci.min.normal = estimate - qnorm(.975) * se,
      ci.max.normal = estimate + qnorm(.975) * se
    ) |>
    bind_cols(args_case)
  
  return(estimate)
}

saveRDS(estimates, file = "../intermediate/estimates.RDS")

sessionInfo()

print("Finish time")
print(Sys.time())
print("Time spent")
difftime(Sys.time(),t0)

sink()