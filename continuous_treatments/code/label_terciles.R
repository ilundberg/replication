label_terciles <- function(data) {
  cutoffs <- readRDS("intermediate/cutoffs.RDS") %>%
    pivot_longer(cols = c("low","high")) %>%
    pivot_wider(names_from = c("variable","name"),
                values_from = value)
  data %>%
    mutate(label_income = case_when(income < cutoffs$income_low ~ 1,
                                    income < cutoffs$income_high ~ 2,
                                    !is.na(income) ~ 3),
           label_wealth = case_when(wealth < cutoffs$wealth_low ~ 1,
                                    wealth < cutoffs$wealth_high ~ 2,
                                    !is.na(wealth) ~ 3),
           label_income = factor(
             label_income, 
             labels = c(
               paste0("Low\n< $",prettyNum(round(cutoffs$income_low), big.mark = ",")),
               paste0("Middle\n$",prettyNum(round(cutoffs$income_low), big.mark = ","),
                      " to\n$",prettyNum(round(cutoffs$income_high), big.mark = ",")),
               paste0("High\n$",prettyNum(round(cutoffs$income_high), big.mark = ",")," +")
             )
            ),
           label_wealth = factor(
             label_wealth, 
             labels = c(
               paste0("Low\n< $",prettyNum(round(cutoffs$wealth_low), big.mark = ",")),
               paste0("Middle\n$",prettyNum(round(cutoffs$wealth_low), big.mark = ","),
                      " to\n$",prettyNum(round(cutoffs$wealth_high), big.mark = ",")),
               paste0("High\n$",prettyNum(round(cutoffs$wealth_high), big.mark = ",")," +")
             )
           ))
}
