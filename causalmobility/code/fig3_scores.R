# Dependencies
library(future.apply)
library(progressr)
library(here)
library(tidyverse)
theme_set(theme_bw())
library(mgcv)

sink(here("logs","fig3_scores.txt"))
print(Sys.time())

# Import analysis panel
nlsy_analysis <- tibble(readRDS(here("data/nlsy_panel.RDS")))

# Data frame with one row per confounder stratum
confounder_strata <- nlsy_analysis |>
  group_by(resp_race, parental_educ) |>
  summarize(stratum_weight = sum(weight),
            .groups = "drop")

# ANALYSIS WITH OCCUPATIONAL SCORE

# Unadjusted parent occupation -> child occupation

fit_marginal_score <- gam(
  hwsei ~ parental_hwsei + s(parental_occ_title, bs = "re"),
  data = nlsy_analysis,
  weights = weight
)

## Fit a bootstrapped model for slope CI
n_replicates <- 1000
plan(multicore, workers = parallel::detectCores())
with_progress({
  p <- progressor(along = 1:n_replicates)
  fit_marginal_score_bootstrapped <- future_vapply(
    1:n_replicates,
    function(iter) {
      # Generate bootstrap indices
      boot_idx <- sample(1:nrow(nlsy_analysis), nrow(nlsy_analysis), replace = TRUE)
      fit_marginal_score_iter <- gam(
        hwsei ~ parental_hwsei + s(parental_occ_title, bs = "re"),
        data = nlsy_analysis[boot_idx, ],
        weights = weight
      )
      p()
      return(coef(fit_marginal_score_iter)["parental_hwsei"])
    },
    numeric(1),
    future.seed = TRUE
  )
})
plan(sequential)

marginal_coef_summary <- tibble(
    coef = coef(fit_marginal_score)["parental_hwsei"]
  ) |>
  mutate(
    ci.min = quantile(fit_marginal_score_bootstrapped, 0.025),
    ci.max = quantile(fit_marginal_score_bootstrapped, 0.975)
  )

to_predict_marginal <- nlsy_analysis |>
  group_by(parental_hwsei, parental_occ_title) |>
  summarize(occ_weight = sum(weight),
            .groups = "drop")
fitted_marginal_all <- predict(
  fit_marginal_score,
  newdata = to_predict_marginal,
  type = "terms"
)
fitted_marginal_points <- (
  rowSums(fitted_marginal_all) + attr(fitted_marginal_all, "constant")
)
fitted_marginal_line <- (
  fitted_marginal_all[,1] + attr(fitted_marginal_all,"constant")
)

to_predict_marginal |>
  mutate(points = fitted_marginal_points,
         line = fitted_marginal_line) |>
  ggplot(aes(x = parental_hwsei)) +
  geom_point(aes(y = points, size = occ_weight),
             color = "steelblue",
             show.legend = F, alpha = .6) +
  geom_line(aes(y = line)) +
  scale_size_continuous(range = c(.2,4)) +
  xlab("Parent Occupational Score (HW-SEI)") +
  ylab("Average Child Occupational Score (HW-SEI)") +
  ylim(c(23,55)) +
  annotate(
    geom = "text", x = 60, y = 28,
    hjust = 0, vjust = 1, size = 3,
    label = paste0(
      "Slope: ",
      format(round(marginal_coef_summary$coef,2), nsmall = 2),
      "\nCI: (",
      format(round(marginal_coef_summary$ci.min,2), nsmall = 2),
      ", ",
      format(round(marginal_coef_summary$ci.max,2), nsmall = 2),
      ")"
    )
  )

ggsave(here::here("figures/score_marginal.pdf"), height = 3.5, width = 5)

# Conditional model for parent occupation -> child occupation
fit <- gam(
  hwsei ~ parental_hwsei +
    s(
      parental_hwsei,
      bs = "re",
      by = interaction(resp_race, parental_educ)
    ) +
    resp_race + parental_educ +
    s(parental_occ_title, bs = "re"),
  data = nlsy_analysis,
  weights = weight
)

to_predict <- nlsy_analysis |>
  group_by(resp_race, parental_educ, parental_occ_title, parental_hwsei) |>
  summarize(occ_weight = sum(weight),
            .groups = "drop")

fitted <- predict(fit, type = "terms", newdata = to_predict)
random_intercept_columns <- which(grepl("parental_occ",colnames(fitted)))
line <- rowSums(fitted[,-random_intercept_columns]) + attr(fitted,"constant")
points <- rowSums(fitted) + attr(fitted,"constant")

# Extract the slopes from that fit
estimate <- coef(fit)
slope_terms <- which(grepl("hwsei",names(estimate)))
estimate <- estimate[slope_terms]
Sigma <- vcov(fit)[slope_terms,slope_terms]

# Examine slopes in each subgroup
A <- cbind(1,diag(rep(1,12)))
slope_estimates <- A %*% as.matrix(estimate)
slope_vcov <- A %*% Sigma %*% t(A)
subgroup_slopes <- data.frame(term = gsub(".*)","",names(estimate)[-1]),
                              slope = slope_estimates,
                              se = sqrt(diag(slope_vcov))) |>
  mutate(ci.min = slope - qnorm(.975) * se,
         ci.max = slope + qnorm(.975) * se) |>
  separate(term, into = c("resp_race","parental_educ","unused"),
           sep = "[.]") |>
  select(-unused) |>
  mutate(resp_race = factor(resp_race,
                            levels = levels(nlsy_analysis$resp_race)),
         parental_educ = factor(parental_educ,
                                levels = levels(nlsy_analysis$parental_educ)))
print(subgroup_slopes)

# Examine population-average slope
with_weights <- subgroup_slopes |>
  select(resp_race, parental_educ) |>
  left_join(confounder_strata, by = c("resp_race","parental_educ")) |>
  mutate(stratum_weight = stratum_weight / sum(stratum_weight))
A <- c(1,with_weights$stratum_weight)
pop_slope_estimate <- A %*% as.matrix(estimate)
pop_slope_se <- sqrt(t(as.matrix(A)) %*% Sigma %*% as.matrix(A))
data.frame(population_slope = pop_slope_estimate,
           se = pop_slope_se) |>
  mutate(ci.min = population_slope - qnorm(.975) * se,
         ci.max = population_slope + qnorm(.975) * se)

# Visualize the subgroup slopes
to_predict |>
  mutate(points = points,
         line = line) |>
  ggplot(aes(x = parental_hwsei)) +
  geom_point(aes(y = points, size = occ_weight), color = "steelblue",
             alpha = 0.6,
             show.legend = F) +
  geom_line(aes(y = line)) +
  facet_grid(resp_race~parental_educ) +
  scale_size_continuous(range = c(.2,4)) +
  xlab("Parent Occupational Score (HW-SEI)") +
  ylab("Average Child Occupational Score (HW-SEI)") +
  ylim(c(23,55)) +
  geom_text(
    data = subgroup_slopes |>
      mutate(
        label = paste0("Slope: ", format(round(slope, 2), nsmall = 2)),
        parental_hwsei = 80
      ),
    aes(label = label),
    y = 28,
    hjust = 1,
    vjust = 1,
    size = 3
  )
ggsave(here::here("figures/score_conditional.pdf"),
       height = 5, width = 6.5)

sessionInfo()
sink()