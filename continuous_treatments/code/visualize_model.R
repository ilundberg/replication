
source("code/load_data.R")

data <- load_data(outcome = "enrolled_any")

logit <- glm(
  y ~ log(income)*(
    educJoint + race + log(wealth) + 
      wealth_negative + wealth_low
  ),
  data = data,
  family = binomial,
  weights = w
)

gam <- mgcv::gam(
  y ~ s(log(income)) + 
    educJoint + race + log(wealth) + 
    wealth_negative + wealth_low,
  data = data,
  family = binomial,
  weights = w
)

to_predict <- data |>
  filter(!wealth_negative & !wealth_low) |>
  group_by(educJoint, race, label_wealth) |>
  mutate(income_max = quantile(income, .95), income_min = quantile(income, .05)) |>
  ungroup() |>
  mutate(wealth = case_when(
    grepl("Low", label_wealth) ~ 25e3,
    grepl("Middle", label_wealth) ~ 100e3,
    grepl("High", label_wealth) ~ 200e3
  )) |>
  select(educJoint, race, wealth, wealth_negative, wealth_low, income_min, income_max) |>
  distinct() |>
  mutate(data = map2(.x = income_min, .y = income_max, .f = \(x,y) tibble(income = seq(x, y, length.out = 100)))) |>
  unnest(data)

to_predict |>
  mutate(
    estimate = predict(logit, type = "response", newdata = to_predict)
  ) |>
  mutate(educJoint = fct_rev(educJoint), race = fct_relevel(race,"Hispanic","Non-Hispanic Black","White or Other")) |>
  ggplot(aes(x = income, y = estimate, color = factor(wealth))) +
  geom_line() +
  facet_grid(
    race ~ educJoint,
    labeller = as_labeller(
      \(x) case_when(
        x == "Non-Hispanic Black" ~ "Non-Hispanic\nBlack",
        x == "White or Other" ~ "White or\nOther",
        T ~ x
      )
    )
  ) +
  scale_color_discrete(
    name = "Wealth",
    labels = function(x) scales::label_currency(scale = 1e-3, suffix = "k")(as.numeric(x))
  ) +
  scale_x_continuous(
    name = "Family Income at Age 17", 
    labels = scales::label_currency(),
    breaks = seq(0,300e3,100e3),
    limits = c(0,300e3)
  ) +
  scale_y_continuous(
    name = "Probability of College Enrollment", 
    labels = scales::label_number(accuracy = .01),
    limits = c(0,1)
  ) +
  geom_hline(
    yintercept = c(0,1),
    color = "gray",
    linetype = "dashed"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(hjust = 1, angle = 45),
    strip.text.y = element_text(angle = 0)
  )
ggsave("figures/figure7_logit.pdf", height = 4, width = 6)

to_predict |>
  mutate(
    estimate = predict(gam, type = "response", newdata = to_predict)
  ) |>
  mutate(educJoint = fct_rev(educJoint), race = fct_relevel(race,"Hispanic","Non-Hispanic Black","White or Other")) |>
  ggplot(aes(x = income, y = estimate, color = factor(wealth))) +
  geom_line() +
  facet_grid(
    race ~ educJoint,
    labeller = as_labeller(
      \(x) case_when(
        x == "Non-Hispanic Black" ~ "Non-Hispanic\nBlack",
        x == "White or Other" ~ "White or\nOther",
        T ~ x
      )
    )
  ) +
  scale_color_discrete(
    name = "Wealth",
    labels = function(x) scales::label_currency(scale = 1e-3, suffix = "k")(as.numeric(x))
  ) +
  scale_x_continuous(
    name = "Family Income at Age 17", 
    labels = scales::label_currency(),
    breaks = seq(0,300e3,100e3),
    limits = c(0,300e3)
  ) +
  scale_y_continuous(
    name = "Probability of College Enrollment", 
    labels = scales::label_number(accuracy = .01),
    limits = c(0,1)
  ) +
  geom_hline(
    yintercept = c(0,1),
    color = "gray",
    linetype = "dashed"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(hjust = 1, angle = 45),
    strip.text.y = element_text(angle = 0)
  )
ggsave("figures/figure7_gam.pdf", height = 4, width = 6)



to_predict |>
  mutate(
    GAM = predict(gam, type = "response", newdata = to_predict),
    Logit = predict(logit, type = "response", newdata = to_predict)
  ) |>
  pivot_longer(cols = c("GAM","Logit"), names_to = "Model", values_to = "estimate") |>
  mutate(educJoint = fct_rev(educJoint), race = fct_relevel(race,"Hispanic","Non-Hispanic Black","White or Other")) |>
  ggplot(aes(x = income, y = estimate, linetype = Model, color = factor(wealth))) +
  geom_line() +
  facet_grid(race ~ educJoint) +
  scale_color_discrete(
    name = "Wealth",
    labels = function(x) scales::label_currency(scale = 1e-3, suffix = "k")(as.numeric(x))
  ) +
  scale_x_continuous(name = "Parent Income", labels = scales::label_currency(scale = 1e-3, suffix = "k")) +
  scale_y_continuous(name = "Any College Enrollment by Age 21", labels = scales::label_percent())
ggsave("figures/model_visualization.pdf", height = 5, width = 6.5)


  