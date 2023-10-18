# Create descriptive decile estimates for comparison
descriptive_binner <- function(outcome_name) {
  
  # Load helper functions
  source("code/load_data.R")
  source("code/weighted.quantile.R")
  
  # Load data
  data <- load_data(outcome_name)
  
  # Create and return decile estimates
  deciles <- data %>%
    arrange(income) %>%
    mutate(cdf = (cumsum(w) - w) / sum(w),
           decile = trunc(10*cdf) + 1) %>%
    group_by(decile) %>%
    summarize(income = weighted.quantile(income, q = .5, w = w),#exp(weighted.mean(log(income), w = w)),
              estimate = weighted.mean(y, w = w)) %>%
    mutate(outcome = outcome_name)
  
  return(deciles)
}