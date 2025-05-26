

sink("../logs/standard_practice_comparison.txt")

t0 <- Sys.time()
print(t0)

library(tidyverse)
library(Amelia)
library(foreach)
library(doParallel)
library(doRNG)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

bs_reps <- 100
print(paste("Bootstrap reps:",bs_reps))
print(paste("Cores:",detectCores()))

set.seed(14850)

# Load the prepared data
d_prepared <- readRDS("../intermediate/motherhood.RDS")

standard_practice_estimator <- function(
    data, bs = F
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
  fit <- lm(
    wage ~ treated + race + poly(age_1,2) + educ + marital + fulltime + log(tenure + 1) + 
      log(experience + 1) + wage_baseline + employed_baseline,
    data = data_imp,
    weights = w
  )
  return(coef(fit)["treatedTRUE"])
}
  

point <- standard_practice_estimator(
  d_prepared |>  filter(sex == "Women"),
  bs = F
)
point_star <- foreach(
  rep = 1:bs_reps, 
  .combine = "c", 
  .packages = c("Amelia","tidyverse")
) %dorng% {
  standard_practice_estimator(d_prepared |>  filter(sex == "Women"), bs = T)
}

se <- sd(point_star)

print(
  tibble(
    estimate = point,
    se = se,
    ci.min = point - qnorm(.975) * se,
    ci.max = point + qnorm(.975) * se
  )
)

sink()
