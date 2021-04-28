# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This code produces several descriptive plots
# 1. Prevalence of disability and its onset
# 2. Transitions in disability status
# 3. Disparities in employment by work disability
# 4. Age trend in disability
# 5. Weight grows over time
# 6. Change in onset over time
# 7. Change in onset disparity over time
# 8. Subgroup-specific trends over time

source("code/prepare_data.R")

# Load data
all_data <- prepare_data(target_years = 2005:2020)


# 1. Prevalence of work-limiting disability and its onset
make_plot <- function(data_source, plot_name, lower_limit = 0, upper_limit = .145) {
  replicates <- foreach(
    i = 1:160, 
    .combine = "rbind", 
    .packages = c("tidyverse","mgcv","foreach")
  ) %do% {
    this_case <- data_source
    this_case$weight <- data_source[[paste0("REPWTP",i)]]
    this_case %>%
      group_by(RACE) %>%
      summarize(y = weighted.mean(y, w = weight)) %>%
      mutate(replicate = i)
  }
  
  forplot <- data_source %>%
    group_by(RACE) %>%
    summarize(point = weighted.mean(y, w = ASECWT)) %>%
    right_join(replicates, by = "RACE") %>%
    group_by(RACE) %>%
    summarize(point = mean(point),
              se = sqrt(4 / 160 * sum((y - point) ^ 2)))
  forplot %>%
    mutate(RACE = fct_relevel(RACE,"Non-Hispanic Black","Non-Hispanic White","Hispanic","Other")) %>%
    ggplot(aes(x = RACE, y = point,
               ymin = point - qnorm(.975) * se,
               ymax = point + qnorm(.975) * se,
               label = paste0(format(round(100 * point, 1), nsmall = 1),"%"))) +
    geom_hline(yintercept = 0) +
    geom_point() +
    geom_errorbar(width = .2, size = .2) +
    geom_text(size = 3, hjust = -.4) +
    theme_bw() +
    xlab("Race / Ethnicity") +
    scale_y_continuous(name = "Work-Limiting Disability",
                       limits = c(lower_limit,upper_limit),
                       labels = function(x) paste0(format(round(100*x, ifelse(lower_limit == 0, 0, 1)),nsmall = ifelse(lower_limit == 0, 0, 1)),"%")) +
    theme(axis.title = element_text(face = "bold")) +
    ggsave(paste0("figures/",plot_name),
           height = 3, width = 6.5)
}

make_plot(all_data$full_population,"factual_full_population.pdf")
make_plot(all_data$linked,"factual_linked.pdf")
make_plot(all_data$d_onset,"factual_d_onset.pdf")
make_plot(all_data$d_onset,"factual_d_onset_bigScale.pdf", lower_limit = 0.015, upper_limit = .032)

# Bar plot of disparities in work-limiting disability
prepare_disparity_plot <- function(weight_name) {
  all_data$full_population %>%
    rename_at(.vars = all_of(weight_name), .fun = function(x) "weight") %>%
    group_by(RACE) %>%
    summarize(estimate = weighted.mean(y, w = weight),
              .groups = "drop_last") %>%
    group_by() %>%
    mutate(RACE = gsub(" ","\n",RACE),
           RACE = fct_relevel(RACE,"Non-Hispanic\nBlack","Non-Hispanic\nWhite","Hispanic","Other"))
}
forplot <- foreach(i = 1:160, .combine = "rbind") %do% {
  prepare_disparity_plot(paste0("REPWTP",i))
} %>%
  rename(estimate_star = estimate) %>%
  left_join(prepare_disparity_plot("ASECWT"),
            by = c("RACE")) %>%
  group_by(RACE) %>%
  summarize(estimate = mean(estimate),
            estimate_se = sqrt(4 / 160 * sum((estimate_star - estimate) ^ 2)),
            .groups = "drop_last")
forplot %>%
  ggplot(aes(x = RACE, y = estimate, ymin = estimate - qnorm(.975) * estimate_se, ymax = estimate + qnorm(.975) * estimate_se)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  geom_text(aes(y = 0, vjust = -.5,
                label = paste0(format(round(100 * estimate,1), nsmall = 1), "%")),
            color = "white", fontface = "bold", size = 6) +
  ylab("Work-Limiting Disability") +
  xlab("Race / Ethnicity") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ggsave("figures/disability_by_race.pdf",
         height = 3.5, width = 5)

# 2. Transitions in disability status
all_data$linked %>%
  #filter(YEAR <= 2012) %>%
  group_by(lag,y) %>%
  summarize(weight = sum(ASECWT)) %>%
  group_by() %>%
  mutate(weight = weight / sum(weight),
         group = factor(case_when(!lag & !y ~ 1,
                                  !lag & y ~ 2,
                                  lag & !y ~ 3,
                                  lag & y ~ 4),
                        labels = c("No disability","Onset of disability",
                                   "Recovery from disability","Persistent disability"))) %>%
  filter(group != "No disability") %>%
  ggplot(aes(x = group, y = weight)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = 0, vjust = -.5,
                label = paste0(format(round(100 * weight,1), nsmall = 1), "%")),
            color = "white", fontface = "bold", size = 6) +
  xlab("Work-Limiting Disability Transitions") +
  ylab("Prevalence") +
  theme_bw() +
  ggsave("figures/transitions.pdf",
         height = 3, width = 5)
prepare_transition_plot <- function(weight_name) {
  all_data$linked %>%
    rename_at(.vars = all_of(weight_name), .fun = function(x) "weight") %>%
    #filter(YEAR <= 2012) %>%
    group_by(RACE) %>%
    summarize(onset = sum(as.numeric(y & !lag & employed) * weight) / sum(as.numeric(!lag & employed) * weight),
              recovery = sum(as.numeric(lag & !y) * weight) / sum(as.numeric(lag) * weight),
              .groups = "drop_last") %>%
    mutate(RACE = gsub(" ","\n",RACE),
           RACE = fct_relevel(RACE,"Non-Hispanic\nBlack","Non-Hispanic\nWhite","Hispanic","Other"))
}
forplot <- foreach(i = 1:160, .combine = "rbind") %do% {
  prepare_transition_plot(paste0("REPWTP",i))
} %>%
  rename(onset_star = onset, recovery_star = recovery) %>%
  left_join(prepare_transition_plot("ASECWT"),
            by = "RACE") %>%
  group_by(RACE) %>%
  summarize(onset = mean(onset),
            onset_se = sqrt(4 / 160 * sum((onset_star - onset) ^ 2)),
            recovery = mean(recovery),
            recovery_se = sqrt(4 / 160 * sum((recovery_star - recovery) ^ 2)),
            .groups = "drop_last")
forplot %>%
  ggplot(aes(x = RACE, y = onset, ymin = onset - qnorm(.975) * onset_se, ymax = onset + qnorm(.975) * onset_se)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  geom_text(aes(y = 0, vjust = -.5,
                label = paste0(format(round(100 * onset,1), nsmall = 1), "%")),
            color = "white", fontface = "bold", size = 6) +
  scale_y_continuous(name = 'Onset of Work-Limiting Disability') +
  xlab("Race / Ethnicity") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ggsave("figures/onset_by_race.pdf",
         height = 3.5, width = 5)
forplot %>%
  ggplot(aes(x = RACE, y = recovery, ymin = recovery - qnorm(.975) * recovery_se, ymax = recovery + qnorm(.975) * recovery_se)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = .2) +
  geom_text(aes(y = 0, vjust = -.5,
                label = paste0(format(round(100 * recovery,1), nsmall = 1), "%")),
            color = "white", fontface = "bold", size = 6) +
  scale_y_continuous(name = expression(bold(Recovery)~'from Work-Limiting Disability')) +
  xlab("Race / Ethnicity") +
  theme_bw() +
  theme(axis.title.x = element_text(face = "bold")) +
  ggsave("figures/recovery_by_race.pdf",
         height = 3.5, width = 5)

# 3. Disparities in employment by work disability
prepare_employment_plot <- function(weight_name) {
  all_data$full_population %>%
    rename_at(.vars = all_of(weight_name), .fun = function(x) "weight") %>%
    group_by(RACE, y) %>%
    summarize(employed = weighted.mean(employed, w = weight),
              .groups = "drop_last") %>%
    group_by() %>%
    mutate(RACE = gsub(" ","\n",RACE),
           RACE = fct_relevel(RACE,"Non-Hispanic\nBlack","Non-Hispanic\nWhite","Hispanic","Other"),
           y = ifelse(y,"Yes","No"))
}
forplot <- foreach(i = 1:160, .combine = "rbind") %do% {
  prepare_employment_plot(paste0("REPWTP",i))
} %>%
  rename(employed_star = employed) %>%
  left_join(prepare_employment_plot("ASECWT"),
            by = c("RACE","y")) %>%
  group_by(RACE,y) %>%
  summarize(employed = mean(employed),
            employed_se = sqrt(4 / 160 * sum((employed_star - employed) ^ 2)),
            .groups = "drop_last")
forplot %>%
  ggplot(aes(x = RACE, fill = y, y = employed, ymin = employed - qnorm(.975) * employed_se, ymax = employed + qnorm(.975) * employed_se)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9)) +
  geom_errorbar(width = .2, position = position_dodge(width = .9), size = .1) +
  geom_text(aes(y = 0, vjust = -.5,
                label = paste0(format(round(100 * employed,1), nsmall = 1), "%")),
            color = "white", fontface = "bold", size = 6, position = position_dodge(width = .9)) +
  ylab("Proportion Employed") +
  xlab("Race / Ethnicity") +
  scale_fill_manual(values = c("gray60","gray30"),
                    name = "Reports a\nWork-Limiting\nDisability") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ggsave("figures/employment_disparities.pdf",
         height = 3.5, width = 12)

# 4. Examine the age trend in disability
all_data$full_population %>%
  group_by(AGE) %>%
  summarize(y = weighted.mean(y, w = ASECWT)) %>%
  ggplot(aes(x = AGE, y = y))  +
  geom_point() +
  geom_smooth(se = F, size = .2, color = "black") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  ylab("Work-Limiting Disability") +
  scale_x_continuous(name = "Age") +
  theme(axis.title = element_text(face = "bold")) +
  ggsave("figures/age_trend.pdf",
         height = 3, width = 5)

# 5. Show how the weight grows over time
all_data$full_population %>%
  group_by(YEAR) %>%
  summarize(weight = sum(ASECWT)) %>%
  group_by() %>%
  mutate(weight = weight / sum(weight)) %>%
  ggplot(aes(x = YEAR, y = weight))  +
  geom_point() +
  theme_bw() +
  xlab("Year") +
  ylab("Proportion of total weight") +
  ggsave("figures/weight_over_time.pdf",
         height = 3, width = 5)

# 6. Change in onset over time
all_data_all_years <- prepare_data(1988:2020)
all_data_all_years$d_onset %>%
  group_by(YEAR, questionnaire_redesign) %>%
  summarize(onset = weighted.mean(y, w = ASECWT)) %>%
  ggplot(aes(x = YEAR, y = onset)) +
  geom_vline(xintercept = 2013, color = "gray") +
  geom_point(aes(alpha = factor(questionnaire_redesign))) +
  #geom_smooth(method = "lm", se = F, aes(y = ifelse(!questionnaire_redesign, onset, NA))) +
  theme_bw() +
  ylab("Onset of Work-Limiting Disability\nAmong the Employed") +
  ylim(c(0,.03)) +
  xlab("Year") +
  scale_alpha_manual(values = c(1,0)) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, color = "gray") +
  ggsave("figures/hazard_trend_earlyOnly.pdf",
         height = 3, width = 6.5)
all_data_all_years$d_onset %>%
  group_by(YEAR, questionnaire_redesign) %>%
  summarize(onset = weighted.mean(y, w = ASECWT)) %>%
  ggplot(aes(x = YEAR, y = onset, color = factor(questionnaire_redesign))) +
  geom_vline(xintercept = 2013, color = "gray") +
  geom_vline(xintercept = 2014.5, color = "gray") +
  geom_point() +
  #geom_smooth(method = "lm", se = F) +
  theme_bw() +
  ylab("\nOnset of Work-Limiting Disability\nAmong the Employed") +
  ylim(c(0,.03)) +
  xlab("Year") +
  scale_color_manual(values = c("black","gray")) +
  geom_hline(yintercept = 0, color = "gray") +
  theme(legend.position = "none") +
  ggsave("figures/hazard_trend.pdf",
         height = 3, width = 6.5)

# 7. Change in onset disparity over time
all_data_all_years$d_onset %>%
  filter(grepl("Non-Hispanic",RACE)) %>%
  mutate(RACE = gsub("-| ","",RACE)) %>%
  group_by(YEAR, RACE, questionnaire_redesign) %>%
  summarize(onset = weighted.mean(y, w = ASECWT)) %>%
  spread(key = RACE, value = onset) %>%
  mutate(disparity = NonHispanicBlack - NonHispanicWhite) %>%
  ggplot(aes(x = YEAR, y = disparity, color = factor(questionnaire_redesign))) +
  geom_vline(xintercept = 2013, color = "gray") +
  geom_vline(xintercept = 2014.5, color = "gray") +
  geom_point() +
  theme_bw() +
  ylab("Black-White Disparity in\nOnset of Work-Limiting Disability\nAmong the Employed") +
  xlab("Year") +
  scale_color_manual(values = c("black","gray")) +
  theme(legend.position = "none") +
  ggsave("figures/hazard_disparity_trend.pdf",
         height = 3, width = 6.5)

# 8. Subgroup-specific trends over time
all_data$full_population %>%
  group_by(RACE, YEAR, questionnaire_redesign) %>%
  summarize(y = weighted.mean(y, w = ASECWT)) %>%
  group_by() %>%
  mutate(RACE = fct_relevel(RACE,"Non-Hispanic Black","Non-Hispanic White","Hispanic","Other")) %>%
  ggplot(aes(x = YEAR, y = y, color = RACE, shape = RACE, group = interaction(RACE,questionnaire_redesign))) +
  geom_vline(xintercept = 2014, color = "gray") +
  geom_point() +
  geom_smooth(se = F) +
  theme_bw() +
  scale_color_discrete(name = "Race / Ethnicity") +
  scale_shape_discrete(name = "Race / Ethnicity") +
  xlab("Year") +
  ylab("Work-Limiting Disability") +
  geom_hline(yintercept = 0, color = "gray") +
  ggsave("figures/proportion_by_race_time_trend.pdf",
         height = 4, width = 6.5)
all_data$d_onset %>%
  group_by(RACE, YEAR, questionnaire_redesign) %>%
  summarize(y = weighted.mean(y, w = ASECWT)) %>%
  group_by() %>%
  mutate(RACE = fct_relevel(RACE,"Non-Hispanic Black","Non-Hispanic White","Hispanic","Other")) %>%
  ggplot(aes(x = YEAR, y = y, color = RACE, shape = RACE, group = interaction(RACE,questionnaire_redesign))) +
  geom_vline(xintercept = 2013, color = "gray") +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  scale_color_discrete(name = "Race / Ethnicity") +
  scale_shape_discrete(name = "Race / Ethnicity") +
  xlab("Year") +
  ylab("Work-Limiting Disability") +
  geom_hline(yintercept = 0, color = "gray") +
  ggsave("figures/proportion_onset_by_race_time_trend.pdf",
         height = 4, width = 6.5)

