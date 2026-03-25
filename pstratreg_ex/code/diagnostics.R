

sink("../logs/diagnostics.txt")

t0 <- Sys.time()
print(t0)

library(tidyverse)
library(Amelia)
library(pstratreg)

set.seed(90095)

# Load the prepared data
d_prepared <- readRDS("../intermediate/motherhood.RDS")

data <- d_prepared |>  filter(sex == "Women")

# Impute missing values
amelia.out <- amelia(
  x = data %>% 
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
  mutate(wage = data$wage)

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
  
# Model for outcome existence
s_fit <- glm(
  employed ~ treated*(race + poly(age_1,2) + educ + marital + fulltime + log(tenure + 1) + 
                        log(experience + 1) + wage_baseline + employed_baseline),
  data = data_imp,
  family = binomial
)

# Model for outcome values
y_fit <- lm(
  wage ~ treated*(race + poly(age_1,2) + educ + marital + fulltime + log(tenure + 1) + 
                    log(experience + 1) + wage_baseline + employed_baseline),
  data = data_imp
)

# Augment data with fitted values and residuals
data_with_resid <- data_imp |>
  mutate(
    s_hat = predict(s_fit, newdata = data_imp, type = "response"),
    y_hat = predict(y_fit, newdata = data_imp),
    y_resid = wage - predict(y_fit, newdata = data_imp)
  )

# Model for squared residuals
resid_sq_fit <- glm(
  I(y_resid ^ 2) ~ (treated + race + poly(age_1,2) + educ + marital + fulltime + log(tenure + 1) + 
                      log(experience + 1) + wage_baseline + employed_baseline),
  data = data_with_resid, 
  family = Gamma(link = "log")
)

# Augment data with fitted values for squared residuals
data_with_resid_sq <- data_with_resid |>
  mutate(
    y_resid_sq_hat = predict(resid_sq_fit, newdata = data_with_resid, type = "response"),
    y_resid_sq = y_resid ^ 2
  )

# DIAGNOSTIC: MODEL FOR S
deciles <- data_with_resid |>
  mutate(decile = ntile(s_hat, 10)) |>
  group_by(decile) |>
  summarize(s_hat = mean(s_hat), employed = mean(employed))
s_plot <- data_with_resid |>
  slice_sample(prop = .05) |>
  mutate(employed = employed |> as.numeric()) |>
  ggplot(
    aes(x = s_hat, y = employed)
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_jitter(width = 0, height = .05, size = .4, alpha = .6, color = "gray") +
  geom_smooth(
    data = data_with_resid |>
      mutate(employed = employed |> as.numeric()),
    method = "loess",
    se = F, linewidth = .5
  ) +
  geom_point(data = deciles, size = 3) +
  theme_bw() +
  xlim(c(-.05,1.05)) +
  ylim(c(-.05,1.05)) +
  annotate(
    geom = "text", x = .1, y = .1, 
    vjust = -.5, size = 2, angle = 37, 
    label = "Line of Perfect Calibration"
  ) +
  labs(
    x = "Predicted Probability\nof Employment",
    y = "Employment\n(Outcome Existence)",
    caption = "Gray dots are a 5% sample of the data, visualized with vertical jitter.\nDashed line is the 45 degree line of perfect calibration.\nBlack dots are means within 10 deciles of predicted values.\nBlue curve is a LOESS smoother."
  )

# DIAGNOSTIC: MODEL FOR MEAN OF Y
deciles <- data_with_resid |>
  filter(!is.na(wage)) |>
  mutate(decile = ntile(y_hat, 10)) |>
  group_by(decile) |>
  summarize(y_hat = mean(y_hat), y_resid = mean(y_resid))
y_mean_plot <- data_with_resid |>
  filter(!is.na(wage)) |>
  slice_sample(prop = .05) |>
  ggplot(
    aes(x = y_hat, y = y_resid)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = .4, alpha = .6, color = "gray") +
  geom_smooth(
    data = data_with_resid,
    method = "loess",
    se = F, linewidth = .5
  ) +
  geom_point(data = deciles, size = 2) +
  theme_bw() +
  labs(
    x = "Predicted\nLog Wage",
    y = "Log Wage Residual\n(Observed Value - Predicted Value)",
    caption = "Gray dots are a 5% sample of the data.\nDashed line is the line of 0 residual.\nBlack dots are means within 10 deciles of predicted values.\nBlue curve is a LOESS smoother."
  )

# DIAGNOSTIC: MODEL FOR SQUARED RESIDUALS
deciles <- data_with_resid_sq |>
  filter(!is.na(wage)) |>
  mutate(
    decile = ntile(y_resid_sq_hat, 10)
  ) |>
  group_by(decile) |>
  summarize(
    y_resid_sq_hat = mean(y_resid_sq_hat),
    y_resid_sq = mean(y_resid_sq)
  )

y_resid_sq_plot <- data_with_resid_sq |>
  filter(!is.na(wage)) |>
  slice_sample(prop = .05) |>
  filter(y_resid_sq <= 1) |>
  ggplot(aes(x = y_resid_sq_hat, y = y_resid_sq)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(size = .4, alpha = .6, color = "gray") +
  geom_smooth(
    data = data_with_resid_sq,
    method = "loess",
    se = F, linewidth = .5
  ) +
  geom_point(data = deciles, size = 2) +
  theme_bw() +
  labs(
    x = "Predicted Residual Variance\n",
    y = "Squared Log Wage Residuals",
    caption = "Gray dots are a 5% sample of the data.\nDashed line is the line of equality. Average of dots should equal the line.\nBlack dots are means within 10 deciles of predicted values.\nBlue curve is a LOESS smoother."
  ) +
  coord_cartesian(ylim = c(0,1)) +
  annotate(
    geom = "text",
    x = .1, y = 1,
    label = paste(
      "Top",
      data_with_resid_sq |> 
        filter(!is.na(wage)) |>
        summarize(prop = mean(y_resid_sq > 1)) |> 
        pull(prop) |> 
        scales::label_percent()(),
      "of squared\nlog wage residuals\nare not visualized."
    ),
    size = 2,
    hjust = 0, vjust = 1
  ) + 
  annotate(
    geom = "text", x = .35, y = .35, 
    vjust = 1.5, size = 2, angle = 12, 
    label = "Line of Equality"
  )


# DIAGNOSTIC: CONDITIONAL NORMALITY
data_standardized <- data_with_resid_sq |>
  filter(!is.na(wage)) |>
  mutate(resid_standardized = y_resid / sqrt(y_resid_sq_hat))
normal_plot <- data_standardized |>
  ggplot(aes(sample = resid_standardized)) +
  annotate(
    geom = "rect",
    xmin = qnorm(.005),
    xmax = qnorm(.995),
    ymin = -Inf, ymax = Inf,
    color = "gray", alpha = .2
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_qq(size = .5) +
  annotate(
    geom = "text", x = -3.5, y = -3.5, 
    vjust = -.5, size = 2, angle = 24, 
    label = "Line of Equality"
  ) +
  annotate(
    geom = "text", x = qnorm(.99), y = -5, 
    hjust = 1, vjust = 0, size = 2,
    label = "Gray region is\nthe middle 99% of\ntheoretical quantiles"
  ) +
  theme_bw() +
  labs(
    x = "Theoretical Quantiles of\nStandardized Residuals\n(Standard Normal)",
    y = "Empirical Quantiles of\nStandardized Residuals",
    caption = "Points are quantile estimates.\nDashed line is the 45 degree line of equality.\nGray region is the middle 99%\nof theoretical quantiles."
  )

print("Wage distribution of cases in the bottom 0.5% of standardized residuals.")
data_standardized |> 
  filter(!is.na(wage)) |>
  arrange(resid_standardized) |>
  # Keep the bottom 0.5%
  slice_head(n = floor(.005 * nrow(data_standardized |> filter(!is.na(wage))))) |>
  # Summarize range of observed wages
  summarize(
    wage_min = min(exp(wage)),
    wage_max = max(exp(wage)),
    prop_wage_5 = mean(wage == log(5))
  ) |>
  print()

print("Wage distribution of cases in the top 0.5% of standardized residuals.")
data_standardized |> 
  filter(!is.na(wage)) |>
  arrange(-resid_standardized) |>
  # Keep the bottom 0.5%
  slice_head(n = floor(.005 * nrow(data_standardized |> filter(!is.na(wage))))) |>
  # Summarize range of observed wages
  summarize(
    wage_min = min(exp(wage)),
    wage_max = max(exp(wage)),
    prop_wage_100 = mean(wage == log(100))
  ) |>
  print()

pdf("../figures/illustration_diagnostics.pdf", height = 9, width = 9)
gridExtra::grid.arrange(
  s_plot + 
    ggtitle("A) Calibration plot assessing the model\n     for outcome existence.") +
    theme(plot.title = element_text(hjust = 0)), 
  y_mean_plot + 
    ggtitle("B) Residual plot assessing the model\n     for the conditional outcome mean.") +
    theme(plot.title = element_text(hjust = 0)), 
  y_resid_sq_plot + 
    ggtitle("C) Residual plot assessing the model\n     for conditional outcome variance.") +
    theme(plot.title = element_text(hjust = 0)), 
  normal_plot + 
    ggtitle("D) Quantile-quantile plot assessing\n     conditional normality of outcome residuals.") +
    theme(plot.title = element_text(hjust = 0)),
  widths = c(1,1)
)
dev.off()

sink()