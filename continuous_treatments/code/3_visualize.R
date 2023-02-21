
sink("logs/3_visualize.txt")

cat("This file visualizes results")

t0 <- Sys.time()
print(t0)
set.seed(90095)

# Prepare environment
library(tidyverse)
theme_set(theme_bw())

prepared <- readRDS("intermediate/prepared.RDS")

# Helper function to drop subgroups from a plot if they correspond to
# fewer than 25 people
filter_num <- function(data, byVars, cutoff = 25) {
  counts <- prepared %>%
    group_by(across(all_of(byVars))) %>%
    summarize(num = n(),
              .groups = "drop")
  data %>%
    left_join(counts, by = byVars) %>%
    filter(num >= cutoff)
}

# Helper function to convert short learner names for estimators into long labels
label_learner <- function(data) {
  data %>%
    mutate(learner = factor(case_when(learner == "glm_additive" ~ 1,
                                      learner == "glm_educ" ~ 2,
                                      learner == "glm_wealth" ~ 3,
                                      learner == "glm_race" ~ 4,
                                      learner == "glm_educWealth" ~ 5,
                                      learner == "glm_educRace" ~ 6,
                                      learner == "gam_additive" ~ 7,
                                      learner == "gam_educ" ~ 8,
                                      learner == "gam_wealth" ~ 9,
                                      learner == "gam_race" ~ 10,
                                      learner == "gam_educWealth"~ 11,
                                      learner == "gam_educRace"~ 12),
                            labels = c("Linear\nAdditive Income Effect","Linear\nIncome x Education","Linear\nIncome x Wealth","Linear\nIncome x Race","Linear\nIncome x Education x Wealth","Linear\nIncome x Education x Race",
                                       "Smooth\nAdditive Income Effect","Smooth\nIncome x Education","Smooth\nIncome x Wealth","Smooth\nIncome x Race","Smooth\nIncome x Education x Wealth","Smooth\nIncome x Education x Race")),
           learner = fct_rev(learner))
}


##########################
# PREDICTIVE PERFORMANCE #
##########################

# Load cross-validation results
cv_yhat_out_of_fold <- readRDS("intermediate/cv_yhat_out_of_fold.RDS")

# Visualize overall MSE from cv
p <- cv_yhat_out_of_fold %>%
  group_by(learner) %>%
  summarize(mse = weighted.mean((enrolled - yhat) ^ 2,
                                w = w),
            .groups = "drop") %>%
  arrange(mse) %>%
  mutate(best = 1:n() == 1) %>%
  label_learner() %>%
  ggplot(aes(y = learner, x = mse, color = best)) +
  scale_color_manual(values = c("gray","blue")) +
  theme(legend.position = "none") +
  ylab("learner") +
  scale_x_continuous(name = "Mean Squared Error\n(Cross-Validated)",
                     breaks = scales::breaks_pretty(n = 3))
p
ggsave("figures/cv_0.pdf",
       height = 5, width = 4)
p +
  geom_point()
ggsave("figures/cv.pdf",
       height = 5, width = 4)

####################
# ENSEMBLE WEIGHTS #
####################

ensemble_weights <- readRDS("intermediate/ensemble_weights.RDS")

ensemble_weights %>%  
  label_learner() %>%
  ggplot(aes(x = weight, y = learner)) +
  geom_point() +
  ylab("learner") +
  scale_x_continuous(labels = scales::label_percent(),
                     name = "Contribution to Ensemble Learner")
ggsave("figures/ensemble_weights.pdf",
       height = 4, width = 5)

ensemble_weights %>%  
  label_learner() %>%
  filter(weight > .01) %>%
  mutate(learner = fct_reorder(learner, weight)) %>%
  ggplot(aes(x = weight, y = learner)) +
  geom_point() +
  ylab("learner") +
  scale_x_continuous(labels = scales::label_percent(),
                     name = "Contribution to Ensemble Learner") +
  geom_vline(xintercept = 0, alpha = 0)
ggsave("figures/ensemble_weights_positive.pdf",
       height = 4, width = 5)

#################
# DOSE RESPONSE #
#################

dose_response_each_learner <- readRDS("intermediate/dose_response_each_learner.RDS")
dose_response_ensemble <- readRDS("intermediate/dose_response_ensemble.RDS")

for (learner_case in c("glm_additive","gam_additive","gam_educWealth")) {
  dose_response_each_learner %>%
    filter(grouping_name == "educWealth") %>%
    filter(learner == learner_case) %>%
    unnest(cols = "data") %>%
    filter_num(byVars = c("educJoint","wealthTercile")) %>%
    mutate(educJoint = fct_rev(educJoint),
           wealthTercile = fct_relabel(wealthTercile, function(x) gsub(":.*","",x))) %>%
    ggplot(aes(x = income, y = estimate, color = wealthTercile,
               fill = wealthTercile,
               ymin = estimate - qnorm(.975) * se,
               ymax = estimate + qnorm(.975) * se)) +
    facet_grid(~ educJoint) +
    geom_line() +
    geom_ribbon(alpha = .2, color = NA) +
    scale_x_continuous(name = "Family Income in 1997\n(2022 Dollars)",
                       labels = scales::label_dollar(scale = .001, suffix = "k"),
                       breaks = seq(0,250e3,50e3)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Probability of College Enrollment") +
    scale_color_discrete(name = "Wealth") +
    scale_fill_discrete(name = "Wealth")
  ggsave(paste0("figures/dose_response_",learner_case,".pdf"), 
         height = 4, width = 6)
}

# Visualize ensemble dose-response curve
dose_response_ensemble %>%
  filter(grouping_name == "educWealth") %>%
  unnest(cols = "data") %>%
  filter_num(byVars = c("educJoint","wealthTercile")) %>%
  mutate(educJoint = fct_rev(educJoint),
         wealthTercile = fct_relabel(wealthTercile, function(x) gsub(":.*","",x))) %>%
  mutate(ci.min = estimate - qnorm(.975) * se,
         ci.max = estimate + qnorm(.975) * se) %>%
  ggplot(aes(x = income, y = estimate, color = wealthTercile,
             fill = wealthTercile,
             ymin = ci.min,
             ymax = ci.max)) +
  facet_grid(~ educJoint) +
  geom_line() +
  geom_ribbon(alpha = .2, color = NA) +
  scale_x_continuous(name = "Family Income in 1997\n(2022 Dollars)",
                     labels = scales::label_dollar(scale = .001, suffix = "k"),
                     breaks = seq(0,250e3,50e3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Probability of College Enrollment") +
  scale_color_discrete(name = "Wealth") +
  scale_fill_discrete(name = "Wealth")
ggsave("figures/dose_response_ensemble_educWealth.pdf", height = 4, width = 6)

dose_response_ensemble %>%
  filter(grouping_name == "educRace") %>%
  unnest(cols = "data") %>%
  mutate(ci.min = estimate - qnorm(.975) * se,
         ci.max = estimate + qnorm(.975) * se) %>%
  filter_num(byVars = c("educJoint","race")) %>%
  mutate(educJoint = fct_rev(educJoint)) %>%
  ggplot(aes(x = income, y = estimate, color = race,
             fill = race,
             ymin = ci.min,
             ymax = ci.max)) +
  facet_grid(~ educJoint) +
  geom_line() +
  geom_ribbon(alpha = .2, color = NA) +
  scale_x_continuous(name = "Family Income in 1997\n(2022 Dollars)",
                     labels = scales::label_dollar(scale = .001, suffix = "k"),
                     breaks = seq(0,250e3,50e3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Probability of College Enrollment") +
  scale_color_discrete(name = "Race") +
  scale_fill_discrete(name = "Race")
ggsave("figures/dose_response_ensemble_educRace.pdf", height = 4, width = 6)


dose_response_each_learner %>%
  filter(grouping_name == "educWealth") %>%
  unnest(cols = "data") %>%
  ungroup() %>%
  filter_num(byVars = c("educJoint","wealthTercile")) %>%
  label_learner() %>%
  mutate(learner = fct_rev(learner)) %>%
  mutate(educJoint = fct_rev(educJoint)) %>%
  ggplot(aes(x = income, y = estimate, color = wealthTercile,
             fill = wealthTercile,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se)) +
  facet_grid(learner ~ educJoint) +
  geom_line() +
  geom_ribbon(alpha = .2, color = NA) +
  scale_x_continuous(name = "Family Income in 1997\n(2022 Dollars)",
                     labels = scales::label_dollar(scale = .001, suffix = "k")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Probability of College Enrollment") +
  scale_color_discrete(name = "Wealth") +
  scale_fill_discrete(name = "Wealth") +
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle = 0))
ggsave("figures/dose_response_each_learner_educWealth.pdf", height = 16, width = 8)

dose_response_each_learner %>%
  filter(grouping_name == "educRace") %>%
  unnest(cols = "data") %>%
  ungroup() %>%
  filter_num(byVars = c("educJoint","race")) %>%
  label_learner() %>%
  mutate(learner = fct_rev(learner)) %>%
  mutate(educJoint = fct_rev(educJoint)) %>%
  ggplot(aes(x = income, y = estimate, color = race,
             fill = race,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se)) +
  facet_grid(learner ~ educJoint) +
  geom_line() +
  geom_ribbon(alpha = .2, color = NA) +
  scale_x_continuous(name = "Family Income in 1997\n(2022 Dollars)",
                     labels = scales::label_dollar(scale = .001, suffix = "k")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Probability of College Enrollment") +
  scale_color_discrete(name = "Race") +
  scale_fill_discrete(name = "Race") +
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle = 0))
ggsave("figures/dose_response_each_learner_educRace.pdf", height = 16, width = 8)

####################
# FIRST DIFFERENCE #
####################

first_difference_each_learner <- readRDS("intermediate/first_difference_each_learner.RDS")
first_difference_ensemble <- readRDS("intermediate/first_difference_ensemble.RDS")

first_difference_ensemble %>%
  filter(quantity == "byEduc") %>%
  unnest(cols = "data") %>%
  ungroup() %>%
  mutate(educJoint = fct_rev(educJoint)) %>%
  ggplot(aes(x = educJoint, y = estimate,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se)) +
  geom_errorbar(width = .2) +
  geom_label(aes(label = format(round(estimate,2), nsmall = 2))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Parents' Education") +
  ylab("Effect of $25,000 on\nProbability of College Enrollment")
ggsave("figures/firstdifference_ensemble_byEduc.pdf", height = 3, width = 5)

first_difference_ensemble %>%
  filter(quantity == "byRace") %>%
  unnest(cols = "data") %>%
  ungroup() %>%
  mutate(race = fct_rev(race)) %>%
  ggplot(aes(x = race, y = estimate,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se)) +
  geom_errorbar(width = .2) +
  geom_label(aes(label = format(round(estimate,2), nsmall = 2))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Race") +
  ylab("Effect of $25,000 on\nProbability of College Enrollment")
ggsave("figures/firstdifference_ensemble_byRace.pdf", height = 3, width = 5)

first_difference_ensemble %>%
  filter(quantity == "byWealth") %>%
  unnest(cols = "data") %>%
  ungroup() %>%
  mutate(wealthTercile = fct_rev(wealthTercile)) %>%
  ggplot(aes(x = wealthTercile, y = estimate,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se)) +
  geom_errorbar(width = .2) +
  geom_label(aes(label = format(round(estimate,2), nsmall = 2))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Family Net Worth") +
  ylab("Effect of $25,000 on\nProbability of College Enrollment") +
  scale_x_discrete(labels = function(x) gsub(": ","\n",x))
ggsave("figures/firstdifference_ensemble_byWealth.pdf", height = 3, width = 5)

first_difference_ensemble %>%
  filter(quantity == "byEducWealth") %>%
  unnest(cols = "data") %>%
  ungroup() %>%
  filter_num(byVars = c("educJoint","wealthTercile")) %>%
  mutate(educJoint = fct_rev(educJoint)) %>%
  mutate(wealthTercile = fct_relabel(wealthTercile, function(x) gsub(":.*","",x)),
         wealthTercile = fct_rev(wealthTercile)) %>%
  ggplot(aes(x = wealthTercile, y = estimate,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .2) +
  geom_label(aes(label = format(round(estimate,2), nsmall = 2))) +
  facet_wrap(~educJoint) +
  xlab("Race") +
  ylab("Effect of $25,000 on\nProbability of College Enrollment")

first_difference_ensemble %>%
  filter(quantity == "byEducRace") %>%
  unnest(cols = "data") %>%
  ungroup() %>%
  filter_num(byVars = c("educJoint","race")) %>%
  mutate(educJoint = fct_rev(educJoint)) %>%
  mutate(race = fct_rev(race)) %>%
  ggplot(aes(x = race, y = estimate,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .2) +
  geom_label(aes(label = format(round(estimate,2), nsmall = 2))) +
  facet_wrap(~educJoint) +
  xlab("Race") +
  ylab("Effect of $25,000 on\nProbability of College Enrollment")
ggsave("figures/firstdifference_ensemble_byEducRace.pdf", height = 3, width = 10)

first_difference_ensemble %>%
  filter(quantity == "byEducWealthRace") %>%
  unnest(cols = "data") %>%
  ungroup() %>%
  filter_num(byVars = c("educJoint","wealthTercile","race")) %>%
  mutate(educJoint = fct_rev(educJoint)) %>%
  mutate(wealthTercile = fct_relabel(wealthTercile, function(x) gsub(":.*","",x)),
         wealthTercile = fct_rev(wealthTercile)) %>%
  mutate(race = fct_rev(race)) %>%
  ggplot(aes(x = wealthTercile, y = estimate,
             color = race,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(position = position_dodge(width = .2), width = .2) +
  geom_point(position = position_dodge(width = .2)) +
  facet_wrap(~educJoint) +
  xlab("Family Net Worth") +
  ylab("Effect of $25,000 on\nProbability of College Enrollment") +
  scale_color_discrete(name = "Race")
ggsave("figures/firstdifference_ensemble_byEducWealthRace.pdf", height = 3, width = 10)

######################################
# FIRST DIFFERENCE MODEL COMPARISONS #
######################################

plot_model_comparison <- function(first_difference_1, first_difference_2, name_1, name_2, filename) {
  combined <- first_difference_1 %>%
    mutate(name = 1) %>%
    bind_rows(first_difference_2 %>%
                mutate(name = 2)) %>%
    select(-se)
  # Note prop cut off if limit graph to [0, .1]
  cutoffs <- combined %>%
    group_by(name) %>%
    summarize(cut_low = mean(estimate < 0),
              cut_high = mean(estimate > .1)) %>%
    pivot_longer(cols = starts_with("cut"), 
                 names_to = "position",
                 values_to = "label") %>%
    mutate(label = paste0(format(round(100*label,1),nsmall = 1),"% not shown"),
           x = case_when(name == 1 & position == "cut_low" ~ -.001,
                         name == 1 & position == "cut_high" ~ .1001,
                         name == 2 ~ .05),
           y = case_when(name == 1 ~ .05,
                         position == "cut_low" ~ -.001,
                         position == "cut_high" ~ .1001),
           angle = case_when(name == 2 ~ 0,
                             T ~ 270),
           vjust = case_when(position == "cut_low" ~ 1,
                             position == "cut_high" ~ -.5))
  
  # Prepare data for the plot
  forplot <- combined %>%
    mutate(name = case_when(name == 1 ~ "x", 
                            name == 2 ~ "y")) %>%
    pivot_wider(names_from = "name", values_from = "estimate") %>%
    filter(x > 0 & x < .1 & y > 0 & y < .1)
  
  # Prepare correlation label for the plot
  cor_estimate <- (
    forplot %>%
      summarize(cor = cor(x,y))
  )
  cor_label <- paste0("Correlation = ",format(round(cor_estimate$cor, 2), nsmall = 2))
  
  # Make the plot
  p0 <- forplot %>%
    ggplot(aes(x = x, y = y)) +
    coord_fixed(xlim = c(0,.1), ylim = c(0,.1)) +
    xlab(name_1) +
    ylab(name_2)
  ggsave(filename = paste0("figures/",filename,"_0.pdf"),
         plot = p0  +
           geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
           annotate(geom = "text", x = .085, y = .085, angle = 45, vjust = -.5,
                    label = "Line of Equal Estimates",
                    size = 3),
         height = 6, width = 6)
  p <- p0 +
    geom_point(color = "gray", alpha = .5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    annotate(geom = "rect", xmin = 0, xmax = .1, ymin = 0, ymax = .1,
             fill = NA, color = "gray") +
    geom_text(data = cutoffs,
              aes(label = label, angle = angle, vjust = vjust),
              size = 3) +
    annotate(geom = "text", x = .085, y = .085, angle = 45, vjust = -.5,
             label = "Line of Equal Estimates",
             size = 3) +
    annotate(geom = "text", size = 3, x = .1, y = 0, hjust = 1.1, vjust = -.5,
             label = cor_label)
  ggsave(filename = paste0("figures/",filename,".pdf"),
         plot = p,
         height = 6, width = 6)
  return(p)
}

plot_model_comparison(first_difference_ensemble %>%
                        filter(quantity == "individual") %>%
                        unnest(cols = "data"),
                      first_difference_each_learner %>%
                        filter(learner == "glm_additive") %>%
                        filter(quantity == "individual") %>%
                        unnest(cols = "estimate") %>%
                        select(-learner),
                      name_1 = "Flexible Machine Learning",
                      name_2 = "Standard Logistic Regression",
                      filename = "firstdifference_comparison_ensemble")


plot_model_comparison(first_difference_each_learner %>%
                        filter(quantity == "individual") %>%
                        filter(learner == "gam_educWealth") %>%
                        unnest(cols = "estimate") %>%
                        select(-learner),
                      first_difference_each_learner %>%
                        filter(learner == "glm_additive") %>%
                        filter(quantity == "individual") %>%
                        unnest(cols = "estimate") %>%
                        select(-learner),
                      name_1 = "Interactive GAM",
                      name_2 = "Additive GLM",
                      filename = "firstdifference_comparison_extremes")

