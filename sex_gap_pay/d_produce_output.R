
setwd("/Users/iandl/Dropbox/Dissertation/gender_gap_occupations")
source("code/a_prepare_environment.R")

# Load the estimates
load("intermediate/estimates.Rdata")
load("intermediate/within_each_occupation.Rdata")
load("intermediate/between_each_occupation.Rdata")

###########
# FIGURES #
###########

fig_within_occupation <- estimates %>%
  filter(estimand == "median" & quantity %in% c("total","within_occupation")) %>%
  ggplot(aes(x = year, y = ratio, color = quantity_label, alpha = quantity_label)) +
  geom_point(size = .5) +
  stat_smooth(geom='line', size = .7, method = "loess", formula = y ~ x) +
  theme_bw() +
  scale_y_continuous(name = "Ratio: Women / Men",
                     labels = function(x) paste0("$",format(x, nsmall = 2))) +
  xlab("Year") +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 1, color = "gray", linetype = "dashed")

fig_within_occupationHours <- estimates %>%
  filter(estimand == "median" & quantity %in% c("total","within_occupation","within_occupationHours")) %>%
  ggplot(aes(x = year, y = ratio, color = quantity_label, size = quantity_label, shape = quantity_label, alpha = quantity_label)) +
  geom_point() +
  stat_smooth(geom='line', size = .7, method = "loess", formula = y ~ x,
              aes(y = ifelse(quantity != "within_occupationHours", ratio, NA)),
              show.legend = F) +
  theme_bw() +
  scale_y_continuous(name = "Ratio: Women / Men",
                     labels = function(x) paste0("$",format(x, nsmall = 2))) +
  xlab("Year") +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 1, color = "gray", linetype = "dashed")

fig_between_occupation <- estimates %>%
  filter(estimand == "median" & quantity %in% c("total","between_occupation")) %>%
  ggplot(aes(x = year, y = ratio, color = quantity_label, alpha = quantity_label)) +
  geom_point(size = .5) +
  stat_smooth(geom='line', size = .7, method = "loess", formula = y ~ x) +
  theme_bw() +
  scale_y_continuous(name = "Ratio: Women / Men",
                     labels = function(x) paste0("$",format(x, nsmall = 2))) +
  xlab("Year") +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 1, color = "gray", linetype = "dashed")

fig_between_occupationHours <- estimates %>%
  filter(estimand == "median" & quantity %in% c("total","between_occupation","between_occupationHours")) %>%
  ggplot(aes(x = year, y = ratio, color = quantity_label, size = quantity_label, shape = quantity_label, alpha = quantity_label)) +
  geom_point() +
  stat_smooth(geom='line', size = .7, method = "loess", formula = y ~ x,
              aes(y = ifelse(quantity != "between_occupationHours", ratio, NA))) +
  theme_bw() +
  scale_y_continuous(name = "Ratio: Women / Men",
                     labels = function(x) paste0("$",format(x, nsmall = 2))) +
  xlab("Year") +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 1, color = "gray", linetype = "dashed")

fig_ols_comparison <- estimates %>%
  filter((estimand == "median" & quantity %in% c("total","within_occupation")) |
           (estimand == "gmean" & quantity == "within_occupation_OLS")) %>%
  ggplot(aes(x = year, y = ratio, color = quantity_label, alpha = quantity_label)) +
  geom_point(size = .5) +
  stat_smooth(geom='line', size = .7, method = "loess", formula = y ~ x) +
  theme_bw() +
  scale_y_continuous(name = "Ratio: Women / Men",
                     labels = function(x) paste0("$",format(x, nsmall = 2))) +
  xlab("Year") +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 1, color = "gray", linetype = "dashed")

# Function to make a density plot of the within-occupation median pay ratios in a given year
make_density_plot <- function(y) {
  print(paste("Making denstiy plot for year",y))
  vertical_lines_data <- estimates %>%
    filter(quantity %in% c("total","within_occupation") &
             year == y &
             estimand == "median") %>%
    mutate(group = case_when(quantity == "total" ~ "a_total",
                             quantity == "within_occupation" ~ "c_within"),
           color = case_when(quantity == "total" ~ "a",
                             quantity == "within_occupation" ~ "b"),
           label_title = case_when(quantity == "total" ~ "Marginal Ratio",
                                   quantity == "within_occupation" ~ "Conditional Ratio"),
           label_sub = case_when(quantity == "total" ~ paste0("Women were paid\n$",
                                                              format(ratio, nsmall = 2, digits = 2),
                                                              " per hour\nfor each dollar\npaid to men"),
                                 quantity == "within_occupation" ~ paste0("Within the median\noccupation, the\nratio was $",
                                                                          format(ratio, nsmall = 2, digits = 2),"\n")),
           hjust = case_when(quantity == "total" ~ 1,
                             quantity == "within_occupation" ~ 0),
           nudge_x = case_when(quantity == "total" ~ -.01,
                               quantity == "within_occupation" ~ .01))
  for_density <- within_each_occupation %>%
    filter(year == y & estimand == "median" & !is.na(gap)) %>%
    mutate(weight = weight / sum(weight))
  density_estimate <- density(x = for_density$gap, 
                              weights = for_density$weight,
                              from = .5, to = 1.25)
  density_df <- data.frame(x = density_estimate$x,
                           y = density_estimate$y,
                           group = "b_density",
                           color = "b")
  truncated <- data.frame(left = weighted.mean(for_density$gap < .5, w = for_density$weight),
                          right = weighted.mean(for_density$gap > 1.25, w = for_density$weight))
  print("Truncated space:")
  print(truncated)
  
  print("Ratios in sample occupations:")
  
  print(for_density %>%
          mutate(tercile = ntile(gap, 4)) %>%
          group_by(tercile) %>%
          arrange(tercile, -weight) %>%
          filter(1:n() <= 3) %>%
          select(tercile, gap, weight, occ90ly))
  
  plot <- density_df %>%
    ggplot() +
    geom_line(aes(x = x, y = y, alpha = group, color = color)) +
    # Equality vertical reference line
    geom_vline(xintercept = 1) +
    annotate(geom = "text",
             x = 1 + .01,
             y = .1,
             vjust = 0,
             hjust = 0,
             label = "Equality",
             size = 2.5,
             color = "black") +
    # Marginal and conditional vertical lines
    geom_vline(data = vertical_lines_data,
               aes(xintercept = ratio, color = color, alpha = group, linetype = color)) +
    scale_color_manual(values = c("blue","seagreen4"), guide = F) +
    scale_linetype_manual(values = c("solid","dashed"), guide = F) +
    theme_bw() +
    scale_x_continuous(breaks  = seq(.5,1.25,.25),
                       labels = function(x) paste0("$",format(x, nsmall = 2, digits = 2)),
                       name = "Sex Ratio in Hourly Pay: Women / Men") +
    ylab("Density")
  
  # If 2019, then place customly-located text annotations
  if (y == 2019) {
    plot <- plot + 
      geom_text(data = vertical_lines_data,
                aes(x = ratio + nudge_x, color = color, alpha = group,
                    label = label_title, hjust = hjust),
                size = 2.5, y = 1.05, vjust = 0, fontface = "bold") +
      geom_text(data = vertical_lines_data,
                aes(x = ratio + nudge_x, color = color, alpha = group,
                    label = label_sub, hjust = hjust),
                size = 2.5, y = .95, vjust = 1) +
      geom_text(data = data.frame(x = .6, y = 2.5, color = "b", group = "b_density"),
                aes(x = x, y = y, color = color, alpha = group),
                hjust = 0, size = 2.5,
                label = "Distribution over\noccupations of the\nwithin-occupation\nsex ratios in pay")
  }
  return(plot)
}

# Plot the between-occupation density over time
fig_between <- estimates %>%
  filter(quantity == "between_occupation" & estimand == "median") %>%
  ggplot(aes(x = year, y = ratio, alpha = year == 1976)) +
  geom_point(color = "gray", size = 1) +
  stat_smooth(geom='line', se=FALSE, method = "loess", formula = y ~ x) +
  theme_bw() +
  scale_y_continuous(name = "Ratio: Women / Men",
                     labels = function(x) paste0("$",format(x, nsmall = 2))) +
  xlab("Year") +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 1, color = "gray", linetype = "dashed")

##############
# Save plots #
##############

# Within occupation
fig_within_occupation +
  scale_alpha_manual(values = c(0,0), guide = F) +
  scale_color_manual(values = c("seagreen4","blue")) +
  ggsave("figures/fig_within_occupation_1.pdf",
         height = 3, width = 4.5)
fig_within_occupation +
  scale_alpha_manual(values = c(0,1), guide = F) +
  scale_color_manual(values = c("seagreen4","blue")) +
  ggsave("figures/fig_within_occupation_2.pdf",
         height = 3, width = 4.5)
fig_within_occupation +
  scale_alpha_manual(values = c(1,1), guide = F) +
  scale_color_manual(values = c("seagreen4","blue")) +
  ggsave("figures/fig_within_occupation.pdf",
         height = 3, width = 4.5)

# Between occupation
fig_between_occupation +
  scale_alpha_manual(values = c(0,0), guide = F) +
  ggsave("figures/fig_between_occupation_1.pdf",
         height = 3, width = 4.5)
fig_between_occupation +
  scale_alpha_manual(values = c(0,1), guide = F) +
  ggsave("figures/fig_between_occupation_2.pdf",
         height = 3, width = 4.5)
fig_between_occupation +
  scale_alpha_manual(values = c(1,1), guide = F) +
  ggsave("figures/fig_between_occupation.pdf",
         height = 3, width = 4.5)
# Simple gray version of between occupation for slides
fig_between_occupation +
  scale_alpha_manual(values = c(1,0), guide = F) +
  scale_color_manual(values = c("gray","gray")) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks)
  ylim(c(.65,1)) +
  ggsave("figures/fig_between_occupation_gray.pdf",
         height = 3, width = 4.5)

# OLS comparison
fig_ols_comparison +
  scale_alpha_manual(values = c(1,0,1), guide = F) +
  ggsave("figures/fig_ols_comparison_1.pdf",
         height = 3, width = 4.5)
fig_ols_comparison +
  scale_alpha_manual(values = c(1,1,1), guide = F) +
  ggsave("figures/fig_ols_comparison.pdf",
         height = 3, width = 4.5)


# Within occupation x hours
fig_within_occupationHours +
  scale_alpha_manual(values = c(0,0,0), guide = F) +
  scale_shape_manual(values = c(4,20,20)) +
  scale_size_manual(values = c(4,.5,.5)) +
  scale_color_manual(values = c("black","seagreen4","blue")) +
  ggsave("figures/fig_within_occupationHours_1.pdf",
         height = 3, width = 4.5)
fig_within_occupationHours +
  scale_alpha_manual(values = c(0,0,1), guide = F) +
  scale_shape_manual(values = c(4,20,20)) +
  scale_size_manual(values = c(4,.5,.5)) +
  scale_color_manual(values = c("black","seagreen4","blue")) +
  ggsave("figures/fig_within_occupationHours_2.pdf",
         height = 3, width = 4.5)
fig_within_occupationHours +
  scale_alpha_manual(values = c(0,1,1), guide = F) +
  scale_shape_manual(values = c(4,20,20)) +
  scale_size_manual(values = c(4,.5,.5)) +
  scale_color_manual(values = c("black","seagreen4","blue")) +
  ggsave("figures/fig_within_occupationHours_3.pdf",
         height = 3, width = 4.5)
fig_within_occupationHours +
  scale_alpha_manual(values = c(1,1,1), guide = F) +
  scale_shape_manual(values = c(4,20,20)) +
  scale_size_manual(values = c(4,.5,.5)) +
  scale_color_manual(values = c("black","seagreen4","blue")) +
  ggsave("figures/fig_within_occupationHours.pdf",
         height = 3, width = 4.5)

# Between occupation x hours
fig_between_occupationHours +
  scale_alpha_manual(values = c(0,0,0), guide = F) +
  scale_shape_manual(values = c(20,4,20)) +
  scale_size_manual(values = c(.5,4,.5)) +
  ggsave("figures/fig_between_occupationHours_1.pdf",
         height = 3, width = 4.5)
fig_between_occupationHours +
  scale_alpha_manual(values = c(0,0,1), guide = F) +
  scale_shape_manual(values = c(20,4,20)) +
  scale_size_manual(values = c(.5,4,.5)) +
  ggsave("figures/fig_between_occupationHours_2.pdf",
         height = 3, width = 4.5)
fig_between_occupationHours +
  scale_alpha_manual(values = c(1,0,1), guide = F) +
  scale_shape_manual(values = c(20,4,20)) +
  scale_size_manual(values = c(.5,4,.5)) +
  ggsave("figures/fig_between_occupationHours_3.pdf",
         height = 3, width = 4.5)
fig_between_occupationHours +
  scale_alpha_manual(values = c(1,1,1), guide = F) +
  scale_shape_manual(values = c(20,4,20)) +
  scale_size_manual(values = c(.5,4,.5)) +
  ggsave("figures/fig_between_occupationHours.pdf",
         height = 3, width = 4.5)

# Density of within-occupation gaps in 2019
fig_density_2019 <- make_density_plot(2019)
fig_density_2019 +
  scale_alpha_manual(values = c(0,0,0), guide = F) +
  ggsave("figures/fig_density_2019_1.pdf",
         height = 3, width = 6.5)
fig_density_2019 +
  scale_alpha_manual(values = c(1,0,0), guide = F) +
  ggsave("figures/fig_density_2019_2.pdf",
         height = 3, width = 6.5)
fig_density_2019 +
  scale_alpha_manual(values = c(1,1,0), guide = F) +
  ggsave("figures/fig_density_2019_3.pdf",
         height = 3, width = 6.5)
fig_density_2019 +
  scale_alpha_manual(values = c(1,1,1), guide = F) +
  ggsave("figures/fig_density_2019.pdf",
         height = 3, width = 6.5)

# Between-occupation ratio over time
fig_between +
  scale_alpha_manual(values = c(0,1),
                     guide = F) +
  ggsave("figures/fig_between_1.pdf",
         height = 2.5, width = 4)
fig_between +
  scale_alpha_manual(values = c(1,1),
                     guide = F) +
  ggsave("figures/fig_between.pdf",
         height = 2.5, width = 4)

# Example occupation: Physicians
between_each_occupation %>%
  filter(occ90ly == 84) %>%
  select(year, sex, weight) %>%
  spread(key = sex, value = weight) %>%
  mutate(prop_women = women / (women + men)) %>%
  ggplot(aes(x = year, y = prop_women)) +
  geom_point(size = .5, color = "black") +
  geom_smooth(size = .3, color = "black", se = F, method = "loess", formula = y ~ x) +
  geom_hline(yintercept = c(0,1), color = "gray") +
  geom_hline(yintercept = .5, linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_y_continuous(name = "Percent of Physicians\nWho Are Women",
                     labels = function(x) paste0(round(100*x),"%")) +
  xlab("Year") +
  ggsave("figures/fig_ex_physicians_prop_women.pdf",
         height = 2.5, width = 4)

# Example occupation: Nurses
between_each_occupation %>%
  filter(occ90ly == 95) %>%
  select(year, sex, weight) %>%
  spread(key = sex, value = weight) %>%
  mutate(prop_women = women / (women + men)) %>%
  ggplot(aes(x = year, y = prop_women)) +
  geom_point(size = .5, color = "black") +
  geom_smooth(size = .3, color = "black", se = F, method = "loess", formula = y ~ x) +
  geom_hline(yintercept = c(0,1), color = "gray") +
  geom_hline(yintercept = .5, linetype = "dashed", color = "gray") +
  theme_bw() +
  scale_y_continuous(name = "Percent of Nurses\nWho Are Women",
                     labels = function(x) paste0(round(100*x),"%")) +
  xlab("Year") +
  ggsave("figures/fig_ex_nurses_prop_women.pdf",
         height = 2.5, width = 4)
between_each_occupation %>%
  filter(occ90ly == 95 & sex == "women") %>%
  ggplot(aes(x = year, y = median)) +
  geom_point(size = .5, color = "black") +
  geom_smooth(size = .3, color = "black", se = F, method = "loess", formula = y ~ x) +
  theme_bw() +
  scale_y_continuous(name = "Median Hourly Pay\nAmong Nurses",
                     labels = function(x) paste0("$",format(x, nsmall = 2, digits = 2))) +
  xlab("Year") +
  ggsave("figures/fig_ex_nurses_pay.pdf",
         height = 2.5, width = 4)


