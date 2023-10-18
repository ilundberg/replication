
source("code/weighted.quantile.R")
source("code/load_data.R")

# Load data
data <- load_data("enrolled_any") %>%
  mutate(label_wealth = fct_rev(label_wealth))

dots <- data %>%
  arrange(income) %>%
  group_by(educJoint, label_wealth) %>%
  mutate(cdf = cumsum(w) / sum(w)) %>%
  mutate(income_ntile = trunc(5*cdf),
         income_ntile = ifelse(income_ntile == 5, 4, income_ntile),
         income_ntile = income_ntile + 1) %>%
  group_by(educJoint, label_wealth, income_ntile) %>%
  summarize(income = weighted.mean(income, w = w),
            y = weighted.mean(y, w = w),
            .groups = "drop") %>%
  mutate(educJoint = fct_rev(educJoint))
fit <- glm(y ~ log(income) + educJoint + label_wealth, 
           data = data,
           family = binomial,
           weights = w)
to_predict <- foreach(a_val = seq(10e3,300e3,2e3), .combine = "rbind") %do% {
  data %>%
    group_by(educJoint, label_wealth) %>%
    slice_head(n = 1) %>%
    mutate(income = a_val) %>%
    ungroup()
}
fitted <- to_predict %>%
  mutate(y = predict(fit, newdata = to_predict, type = "response"))

sample_size <- data %>%
  group_by(educJoint, label_wealth) %>%
  summarize(sample_size = paste0("n = ",prettyNum(n(), big.mark = ",")),
            .groups = "drop")

dots %>%
  ggplot(aes(x = income, y = y)) +
  geom_point(size = .8, color = "blue") +
  geom_line(data = fitted, color = "darkgray") +
  geom_text(data = sample_size,
            aes(label = sample_size),
            color = "gray",
            x = 290e3, y = .0625,
            hjust = 1,
            size = 3) +
  facet_grid(label_wealth ~ educJoint,
             labeller = as_labeller(function(x) {
               x %>%
                 str_replace("Low","Low wealth") %>%
                 str_replace("Middle","Middle wealth") %>%
                 str_replace("High","High wealth") %>%
                 str_replace("to\n","to ")
             })) +
  scale_x_continuous(name = "Family Income at Age 17",
                     labels = scales::label_dollar()) +
  scale_y_continuous(name = "Probability of College Enrollment by Age 21",
                     labels = scales::label_percent(),
                     limits = c(0,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(angle = 0))
ggsave("figures/extrapolation_danger.pdf",
       height = 5, width = 6.5)

