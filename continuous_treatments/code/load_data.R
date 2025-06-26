
# This function loads data and optionally imputes missing values and/or carries out a bootstrap sample

load_data <- function(outcome, impute = T, bs = F, learning_pubids = NULL) {
  
  source("code/label_terciles.R")
  
  # Read data file and merge weights
  data <- readRDS(paste0("intermediate/data_",outcome,".RDS")) %>%
    # merge weights into data
    left_join(read_delim(paste0("intermediate/weights_",outcome,".dat"), col_names = F) %>%
                rename(PUBID = X1, w = X2),
              by = "PUBID") %>%
    # normalize weights
    mutate(w = w / mean(w)) %>%
    mutate(learning = PUBID %in% learning_pubids)
  
  # If bootstrap, take bootstrap sample
  if (bs) {
    data <- data %>%
      # Label income and wealth terciles
      label_terciles() %>%
      # Bootstrap within strata
      group_by(race, educJoint, label_wealth, learning) %>%
      sample_frac(replace = T) %>%
      ungroup() %>%
      select(-starts_with("label"))
  }
  
  # If not imputing, just return data with labeled terciles
  if (!impute) {
    return(data %>%
             label_terciles())
  } else {
    # Otherwise, impute missing values first
    imputed <- Amelia::amelia(data,
                              noms = c("female","race","educJoint","wealth_negative","wealth_low"),
                              logs = c("income","wealth"),
                              boot.type = "none",
                              idvars = "learning",
                              m = 1)$imputations$imp1 %>%
      # enforce original data range and coding on wealth
      mutate(
        # Correct inconsistent imputations of low wealth
        wealth_low = ifelse(wealth_negative, 0, wealth_low),
        # Correct inconsistent imputations of numeric wealth
        wealth = case_when(
          wealth_low | wealth_negative ~ min(data$wealth, na.rm = T),
          wealth < min(data$wealth, na.rm = T) ~ min(data$wealth, na.rm = T),
          wealth > max(data$wealth, na.rm = T) ~ max(data$wealth, na.rm = T),
          T ~ wealth
        )
      ) %>%
      # label income and wealth terciles in imputed data
      label_terciles()
    return(imputed)
  }
}
