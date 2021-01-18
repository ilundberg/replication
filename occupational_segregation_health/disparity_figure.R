
# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This code produces the disparity figure

load("intermediate/counterfactual_results.Rdata")
counterfactual_estimate <- counterfactual_results$counterfactual_estimate

# Disparity figure
for_disparity_figure <- counterfactual_estimate %>%
  filter(target == "disparity_vs_white" & estimand %in% c("factual","counterfactual_within_educ")) %>%
  group_by() %>%
  rename(Estimand = estimand) %>%
  mutate(Estimand = case_when(Estimand == "factual" ~ "Factual Disparity",
                              Estimand == "counterfactual_within_educ" ~ "Counterfactual Disparity"),
         Estimand = fct_rev(Estimand),
         RACE = paste0(RACE,"\n- Non-Hispanic White"),
         RACE = fct_relevel(RACE,"Non-Hispanic Black\n- Non-Hispanic White","Hispanic\n- Non-Hispanic White","Other\n- Non-Hispanic White"))
for_disparity_figure %>%
  ggplot(aes(x = RACE, y = point, color = Estimand, shape = Estimand,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .5),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .5)) +
  geom_text(position = position_dodge(width = .5),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.15, -.15)),
            size = 4,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity Contrast") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold")) +
  ggsave("figures/disparity.pdf",
         height = 3.75, width  = 6.5)

# Note the percent reductions
sink("figures/counterfactual_factor_change.txt")
print(
  counterfactual_estimate %>% 
    filter(estimand %in% c("counterfactual_within_educ","factual")) %>%
    filter(target == "disparity_vs_white") %>%
    select(RACE, estimand, point) %>%
    spread(key = estimand, value = point) %>%
    mutate(factor_change = counterfactual_within_educ / factual)
)
sink()

# Slide version appears at the bottom of this code

print("figures/disparity_change.txt")
print(counterfactual_estimate %>%
        filter(target == "disparity_vs_white") %>%
        filter(estimand %in% c("factual","counterfactual_within_educ")) %>%
        select(-se) %>%
        spread(key = estimand, value = point) %>%
        mutate(ratio_change = counterfactual_within_educ / factual))

sink("figures/disparity_change_significance.txt")
significance <- counterfactual_estimate %>%
  filter(target == "disparity_vs_white" & estimand == "difference_within_vs_factual") %>%
  group_by() %>%
  mutate(RACE = fct_relevel(RACE,"Non-Hispanic Black","Hispanic","Other"),
         pval = 2 * pnorm(abs(point / se), lower.tail = F),
         significance = case_when(pval < .001 ~ "***",
                                  pval < .01 ~ "**",
                                  pval < .05 ~ "*",
                                  T ~ "ns")) %>%
  arrange(RACE)
print(significance)
sink()

###############################
# Compare to conditional gaps #
###############################

for_comparison_figure <- counterfactual_estimate %>%
  filter(target == "disparity_vs_white" & estimand %in% c("factual","counterfactual_within_educ","conditional_vs_white")) %>%
  filter(RACE != "Non-Hispanic White") %>%
  group_by() %>%
  rename(Estimand = estimand) %>%
  mutate(Estimand = case_when(Estimand == "factual" ~ "Factual Disparity",
                              Estimand == "counterfactual_within_educ" ~ "Counterfactual Disparity",
                              Estimand == "conditional_vs_white" ~ "Conditional Disparity"),
         Estimand = fct_rev(Estimand),
         RACE = paste0(RACE,"\n- Non-Hispanic White"),
         RACE = fct_relevel(RACE,"Non-Hispanic Black\n- Non-Hispanic White","Hispanic\n- Non-Hispanic White","Other\n- Non-Hispanic White"))
for_comparison_figure %>%
  ggplot(aes(x = RACE, y = point, color = Estimand, shape = Estimand,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .7),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .7)) +
  geom_text(position = position_dodge(width = .7),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.1, -.1)),
            size = 3,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity Contrast") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold")) +
  ggsave("figures/conditional_comparison.pdf",
         height = 3.75, width  = 6.5)

##############
# FOR SLIDES #
##############

# All three gaps for black-white only
for_comparison_figure %>%
  filter(RACE == "Non-Hispanic Black\n- Non-Hispanic White") %>%
  ggplot(aes(x = Estimand, y = point,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .7),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .7)) +
  geom_text(position = position_dodge(width = .7),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.2, -.2)),
            size = 3,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Black-White Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Estimand") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold")) +
  ggsave("figures/conditional_gap_comparison_blackwhite.pdf",
         height = 3.75, width  = 6.5)

# For first slide, make the conditional disparity invisible
for_comparison_figure %>%
  filter(RACE == "Non-Hispanic Black\n- Non-Hispanic White") %>%
  ggplot(aes(x = Estimand, y = point,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"),
             alpha = Estimand != "Conditional Disparity")) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .7),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .7)) +
  geom_text(position = position_dodge(width = .7),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.1, -.1)),
            size = 3,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Black-White Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Estimand") +
  scale_alpha_manual(values = c(0,1)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold")) +
  ggsave("figures/conditional_gap_comparison_blackwhite_0.pdf",
         height = 3.75, width  = 6.5)

#####################################
# SLIDE VERSION OF DISPARITY FIGURE #
#####################################

for_disparity_figure %>%
  ggplot(aes(x = RACE, y = point, color = Estimand, shape = Estimand,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"),
             alpha = RACE == "Non-Hispanic Black\n- Non-Hispanic White" & Estimand == "Factual Disparity")) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .5),
                width = .2, size = .2,
                alpha = 0) +
  geom_point(position = position_dodge(width = .5),
             alpha = 0) +
  geom_text(position = position_dodge(width = .5),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.15, -.15)),
            size = 4,
            show.legend = F, fontface = "bold",
            alpha = 0) +
  theme_bw() +
  scale_y_continuous(name = "Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity Contrast") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold")) +
  scale_alpha_manual(values = c(0,1),
                     guide = "none") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ggsave("figures/disparity_1.pdf",
         height = 3.75, width  = 6.5)

for_disparity_figure %>%
  ggplot(aes(x = RACE, y = point, color = Estimand, shape = Estimand,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"),
             alpha = RACE == "Non-Hispanic Black\n- Non-Hispanic White" & Estimand == "Factual Disparity")) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .5),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .5)) +
  geom_text(position = position_dodge(width = .5),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.15, -.15)),
            size = 4,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity Contrast") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold")) +
  scale_alpha_manual(values = c(0,1),
                     guide = "none") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ggsave("figures/disparity_2.pdf",
         height = 3.75, width  = 6.5)

for_disparity_figure %>%
  ggplot(aes(x = RACE, y = point, color = Estimand, shape = Estimand,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"),
             alpha = RACE == "Non-Hispanic Black\n- Non-Hispanic White")) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .5),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .5)) +
  geom_text(position = position_dodge(width = .5),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.15, -.15)),
            size = 4,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity Contrast") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold")) +
  scale_alpha_manual(values = c(0,1),
                     guide = "none") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ggsave("figures/disparity_3.pdf",
         height = 3.75, width  = 6.5)

for_disparity_figure %>%
  ggplot(aes(x = RACE, y = point, color = Estimand, shape = Estimand,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"),
             alpha = RACE == "Non-Hispanic Black\n- Non-Hispanic White" | 
               (RACE == "Hispanic\n- Non-Hispanic White" & Estimand == "Factual Disparity"))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .5),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .5)) +
  geom_text(position = position_dodge(width = .5),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.15, -.15)),
            size = 4,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity Contrast") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold")) +
  scale_alpha_manual(values = c(0,1),
                     guide = "none") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ggsave("figures/disparity_4.pdf",
         height = 3.75, width  = 6.5)

for_disparity_figure %>%
  ggplot(aes(x = RACE, y = point, color = Estimand, shape = Estimand,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"),
             alpha = RACE %in% c("Non-Hispanic Black\n- Non-Hispanic White","Hispanic\n- Non-Hispanic White"))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .5),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .5)) +
  geom_text(position = position_dodge(width = .5),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.15, -.15)),
            size = 4,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity Contrast") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold")) +
  scale_alpha_manual(values = c(0,1),
                     guide = "none") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ggsave("figures/disparity_5.pdf",
         height = 3.75, width  = 6.5)

for_disparity_figure %>%
  ggplot(aes(x = RACE, y = point, color = Estimand, shape = Estimand,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"),
             alpha = RACE %in% c("Non-Hispanic Black\n- Non-Hispanic White","Hispanic\n- Non-Hispanic White") | 
               (RACE == "Other\n- Non-Hispanic White" & Estimand == "Factual Disparity"))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .5),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .5)) +
  geom_text(position = position_dodge(width = .5),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.15, -.15)),
            size = 4,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity Contrast") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold")) +
  scale_alpha_manual(values = c(0,1),
                     guide = "none") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ggsave("figures/disparity_6.pdf",
         height = 3.75, width  = 6.5)

# Plot 7 is the original un-numbered plot from the paper
