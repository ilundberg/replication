
sink("logs/5_pedagogical.txt")

print("This file creates a pedagogical supporting results.")

library(tidyverse)
theme_set(theme_bw())
library(foreach)
library(mgcv)
source("code/weighted.quantile.R")

# Load data
prepared <- readRDS(file = "intermediate/prepared.RDS")


#######################################
# DENSITIES FOR EXTRAPOLATION PROBLEM #
#######################################
prepared %>% 
  ggplot(aes(x = income,  weights = w, fill = educJoint)) + 
  geom_density(alpha = .2) + 
  facet_wrap(~educJoint, ncol = 1) + 
  theme(legend.position = "none") + 
  scale_x_continuous(labels = scales::label_dollar(scale = .001, suffix = "k")) +
  scale_y_continuous(name = "Density", breaks = NULL) +
  xlab("Family Income in 1997\n(2022 Dollars)")
ggsave("figures/densities.pdf",
       height = 5, width = 2.5)

###########################
# CURSE OF DIMENSIONALITY #
###########################

fully_stratified <- prepared %>%
  group_by(race, educJoint, wealthTercile) %>%
  #filter(n() >= 10) %>%
  nest() %>%
  mutate(data = map(data, function(this_data) {
    fit <- mgcv::gam(enrolled ~ log(income),#s(log(income), bs = "cr", k = 5),
                     data = this_data,
                     family = binomial,
                     weights = w)
    this_data %>%
      mutate(fitted = predict(fit, type = "response"))
  })) %>%
  unnest(cols = "data")

fit_pooled <- mgcv::gam(enrolled ~ log(income) + educJoint + race + wealthTercile,
                        data = prepared,
                        family = binomial,
                        weights = w)
fitted_pooled <- foreach(a = seq(10e3,280e3,10e3), .combine = "rbind") %do% {
  prepared %>%
    group_by(educJoint, race, wealthTercile) %>%
    summarize(income = a, .groups = "drop")
}
fitted_pooled$fitted <- predict(fit_pooled, newdata = fitted_pooled, type = "response")

forplot <- fully_stratified %>%
  ungroup() %>%
  mutate(pooled = predict(fit_pooled, type = "response")) %>%
  filter(race == "Black") %>%
  mutate(wealthTercile = fct_relabel(wealthTercile, function(x) gsub(":"," Wealth\n",x))) %>%
  mutate(educJoint = fct_rev(educJoint))

for_pooled_plot <- fitted_pooled %>%
  filter(race == "Black") %>%
  mutate(wealthTercile = fct_relabel(wealthTercile, function(x) gsub(":"," Wealth\n",x))) %>%
  mutate(educJoint = fct_rev(educJoint)) %>%
  left_join(forplot %>%
              group_by(educJoint, wealthTercile) %>%
              summarize(p05 = weighted.quantile(income, .05, w = w),
                        p95 = weighted.quantile(income, .95, w = w),
                        num = n(),
                        .groups = "drop"), 
            by = c("educJoint","wealthTercile"))


p0 <- forplot %>%
  ggplot(aes(x = income, y = as.numeric(enrolled))) +
  facet_grid(wealthTercile ~ educJoint) +
  scale_x_continuous(labels = scales::label_dollar(scale = .001, suffix = "k"),
                     name = "Family Income in 1997\n(2022 Dollars)") +
  ylab("College Enrollment") +
  ggtitle("Among respondents who identify as Black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/stratified_0.pdf",
       plot = p0,
       height = 6, width = 6)
p1 <- p0 +
  geom_point(aes(y = as.numeric(enrolled)))
ggsave("figures/stratified_1.pdf",
       plot = p1,
       height = 6, width = 6)
p <- p1 +
  geom_line(aes(y = fitted))
ggsave("figures/stratified.pdf",
       plot = p,
       height = 6, width = 6)
p_gray <- p0 +
  geom_point(aes(y = as.numeric(enrolled)), color = "gray") +
  geom_line(aes(y = fitted), color = "gray")
ggsave("figures/stratified_gray.pdf",
       plot = p_gray,
       height = 6, width = 6)
p_gray +
  geom_line(data = for_pooled_plot,
            aes(y = fitted), color = "blue", linewidth = 1.2)
ggsave("figures/stratified_additive.pdf",
       height = 6, width = 6)

forplot_restricted <- forplot %>%
  group_by(educJoint, wealthTercile) %>%
  mutate(num = n(),
         p05 = weighted.quantile(income, .05, w),
         p95 = weighted.quantile(income, .95, w))
  
forplot_restricted %>%
  ggplot(aes(x = income, y = as.numeric(enrolled))) +
  facet_grid(wealthTercile ~ educJoint) +
  scale_x_continuous(labels = scales::label_dollar(),
                     name = "Family Income in 1997\n(2022 Dollars)") +
  ylab("College Enrollment") +
  ggtitle("Among respondents who identify as Black") +
  geom_point(color = "gray") +
  geom_line(aes(y = case_when(num >= 25 ~ fitted)))

forplot_restricted %>%
  ggplot(aes(x = income, y = as.numeric(enrolled))) +
  facet_grid(wealthTercile ~ educJoint) +
  scale_x_continuous(labels = scales::label_dollar(),
                     name = "Family Income in 1997\n(2022 Dollars)") +
  ylab("College Enrollment") +
  ggtitle("Among respondents who identify as Black") +
  geom_point(color = "gray") +
  geom_line(aes(y = case_when(num < 25 | income > p95 ~ fitted)),
            linetype = "dotted") +
  geom_line(aes(y = case_when(num >= 25 & income >= p05 & income <= p95 ~ fitted))) +
  geom_line(data = for_pooled_plot,
            aes(y = case_when(num < 25 | income > p95 ~ fitted)),
            linetype = "dotted", color = "blue") +
  geom_line(data = for_pooled_plot,
            aes(y = case_when(num >= 25 & income >= p05 & income <= p95 ~ fitted)),
            color = "blue")

#################################
# Introduction to cubic splines #
#################################

fit_logit <- gam(enrolled ~ log(income),
                 data = prepared,
                 family = binomial,
                 gamma = 1.5)
fit <- gam(enrolled ~ s(log(income), bs = "cr", k = 5),
           data = prepared,
           family = binomial,
           gamma = 1.5)

knots <- fit$smooth[[1]]$xp[2:4]

plot_log_odds <- function(fit, points = T) {
  p <- prepared %>%
    mutate(yhat = predict(fit)) %>%
    ggplot(aes(x = income, y = yhat)) +
    geom_line() +
    scale_x_continuous(trans = "log",
                       breaks = scales::breaks_log(),
                       labels = scales::label_dollar(scale = .001, suffix = "k",
                                                     largest_with_cents = 1e2)) +
    scale_y_continuous() +
    xlab("Income on Log Scale") +
    ylab("Log Odds of College Enrollment")
  if (points) {
    p <- p +
      geom_point(data = data.frame(income = exp(knots),
                                   yhat = predict(fit, 
                                                  newdata = data.frame(income = exp(knots)))))
  }
  return(p)
}
plot_log_odds(fit_logit, points = F)
ggsave("figures/gam_linear.pdf",
       height = 3, width = 3)
plot_log_odds(fit_logit, points = T)
ggsave("figures/gam_linear_points.pdf",
       height = 3, width = 3)
p <- plot_log_odds(fit)
ggsave("figures/gam_smooth_odds_logincome.pdf",
       plot = p,
       height = 3, width = 3)
p <- prepared %>%
  mutate(yhat = predict(fit, type = "response")) %>%
  ggplot(aes(x = income, y = yhat)) +
  geom_line() +
  scale_x_continuous(trans = "log",
                     breaks = scales::breaks_log(),
                     labels = scales::label_dollar(scale = .001, suffix = "k",
                                                   largest_with_cents = 1e2)) +
  scale_y_continuous() +
  xlab("Income on Log Scale") +
  ylab("Probability College Enrollment") +
  geom_point(data = data.frame(income = exp(knots),
                               yhat = predict(fit, type = "response",
                                              newdata = data.frame(income = exp(knots)))))
ggsave("figures/gam_smooth_prob_logincome.pdf",
       plot = p,
       height = 3, width = 3)
p <- p +
  scale_x_continuous(name = "Income",
                     breaks = seq(0,250e3,50e3),
                     labels = scales::label_dollar(scale = .001, suffix = "k"))
ggsave("figures/gam_smooth_prob_income.pdf",
       plot = p,
       height = 3, width = 3)


sessionInfo()

sink()