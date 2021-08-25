
# Author: Ian Lundberg (ilundberg@princeton.edu)
# Title: A causal approach to study interventions that close gaps across social categories
# Paper is a working draft. Code will be posted on the Harvard Dataverse when the paper is finalized for publication.

# To run this code, you will need the private files in a data/ directory

# This file produces plots for the class gap in pay example

# Set working directory
setwd("/Users/iandl/Documents/gap_closing_estimand")

# Set seed
set.seed(08544)

# Load packages
library(tidyverse)

# Load the estimates
load("intermediate/counterfactual_estimates.Rdata")

########################
# Plot under treatment #
########################

forplot <- counterfactual_estimates %>%
  filter(method == "doubly_robust" & 
           counterfactual == "under_treatment") %>%
  mutate(description = case_when(setting == "factual" ~ "As observed",
                                 setting == "counterfactual" ~ "Assigned to\nprofessional class"),
         description = fct_rev(description)) %>%
  mutate(category = case_when(category == "TRUE" ~ "Professional",
                              category == "FALSE" ~ "Working Class",
                              T ~ "difference")) %>%
  mutate(estimate_with_ci = paste0(format(round(estimate,2), nsmall = 2),
                                   " (",
                                   format(round(estimate - qnorm(.975) * se, 2), nsmall = 2),
                                   ", ",
                                   format(round(estimate + qnorm(.975) * se, 2), nsmall = 2),
                                   ")"))

plot_under_treatment <- forplot %>%
  filter(setting %in% c("factual","counterfactual") & category != "difference") %>%
  ggplot(aes(x = category, y = estimate,
             ymin = estimate - qnorm(.975) * se, 
             ymax = estimate + qnorm(.975) * se,
             color = description, shape = description)) +
  geom_point() +
  geom_errorbar(width = .1, size = .2) +
  geom_line(aes(x = 1.5)) +
  geom_segment(aes(xend = 1.5, yend = estimate),
               linetype = "dashed") +
  # Label the descriptive disparity and gap-closing estimand
  annotate(geom = "text", x = 1.33, y = mean(forplot$estimate[forplot$setting == "counterfactual" & forplot$category != "difference"]),
           label = "Gap-Closing\nEstimand",
           size = 2) +
  annotate(geom = "text", x = 1.35, y = mean(forplot$estimate[forplot$setting == "factual" & forplot$category != "difference"]),
           label = "Descriptive\nDisparity",
           size = 2) +
  # Note the estimates for those disparities on the plot
  annotate(geom = "text", x = 1.55, y = mean(forplot$estimate[forplot$setting == "counterfactual" & forplot$category != "difference"]),
           label = forplot$estimate_with_ci[forplot$setting == "counterfactual" & forplot$category == "difference"],
           size = 2, hjust = 0) +
  annotate(geom = "text", x = 1.55, y = mean(forplot$estimate[forplot$setting == "factual" & forplot$category != "difference"]),
           label = forplot$estimate_with_ci[forplot$setting == "factual" & forplot$category == "difference"],
           size = 2, hjust = 0) +
  # Note the treatment effects
  annotate(geom = "segment",
           x = .8, xend = .8, 
           y = forplot$estimate[forplot$setting == "factual" &  forplot$category == "Professional"],
           yend = forplot$estimate[forplot$setting == "counterfactual" & forplot$category == "Professional"],
           arrow = arrow(length = unit(.1,"in")),
           color = "gray") +
  annotate(geom = "text",
           x = .8, y = mean(forplot$estimate[forplot$setting %in% c("factual","counterfactual") & 
                                               forplot$category == "Professional"]),
           label = "Causal Effect", vjust = -.5, angle = 90,
           color = "gray", size = 2) +
  annotate(geom = "segment",
           x = 2.2, xend = 2.2, 
           y = forplot$estimate[forplot$setting == "factual" & forplot$category == "Working Class"],
           yend = forplot$estimate[forplot$setting == "counterfactual" & forplot$category == "Working Class"],
           arrow = arrow(length = unit(.1,"in")),
           color = "gray") +
  annotate(geom = "text",
           x = 2.2, y = mean(forplot$estimate[forplot$setting %in% c("factual","counterfactual") & 
                                                forplot$category == "Working Class"]),
           label = "Causal Effect", vjust = -.5, angle = 90,
           color = "gray", size = 2) +
  xlab("Father's Social Class") +
  ylab("Mean Log Income") +
  scale_color_discrete(name = "Respondent's\nSocial Class") +
  scale_shape_discrete(name = "Respondent's\nSocial Class") +
  theme_bw() +
  theme(legend.key.height = grid::unit(24,"pt"))
plot_under_treatment +
  ggsave("figures/empirical_example.pdf",
         height = 3, width = 6.5)

###########################
# ANIMATED SLIDE VERSION  #
# OF PLOT UNDER TREATMENT #
###########################

forplot <- counterfactual_estimates %>%
  filter(method == "doubly_robust" & 
           counterfactual == "under_treatment") %>%
  mutate(description = case_when(setting == "factual" ~ "Outcomes as\nobserved",
                                 setting == "counterfactual" ~ "Potential outcomes\nin a professional\noccupation"),
         description = fct_rev(description)) %>%
  mutate(category = case_when(category == "TRUE" ~ "Professional",
                              category == "FALSE" ~ "Working Class",
                              T ~ "difference")) %>%
  mutate(estimate_with_ci = paste0(format(round(estimate,2), nsmall = 2),
                                   " (",
                                   format(round(estimate - qnorm(.975) * se, 2), nsmall = 2),
                                   ", ",
                                   format(round(estimate + qnorm(.975) * se, 2), nsmall = 2),
                                   ")"))

# Animated version of the plot under treatment
for (slide_number in 0:3) {
  forplot %>%
    filter(setting %in% c("factual","counterfactual") & category != "difference") %>%
    ggplot() +
    geom_point(aes(x = category, y = estimate,
                   color = description, shape = description, alpha = description),
               position = position_dodge(width = -.1)) +
    geom_errorbar(aes(x = category,
                      ymin = estimate - qnorm(.975) * se, 
                      ymax = estimate + qnorm(.975) * se,
                      color = description, alpha = description),
                  position = position_dodge(width = -.1),
                  width = .1, size = .3) +
    geom_line(aes(x = ifelse(setting == "factual", 1.4, 1.6),
                  y = estimate,
                  color = description, alpha = description),
              position = position_dodge(width = -.1)) +
    geom_segment(aes(x = category,
                     xend = ifelse(setting == "factual", 1.4, 1.6),
                     y = estimate,
                     yend = estimate,
                     color = description, alpha = description),
                 linetype = "dashed",
                 position = position_dodge(width = -.1)) +
    # Label the descriptive disparity and gap-closing estimand
    geom_text(data = forplot %>%
                filter(setting %in% c("factual","counterfactual") & category != "difference") %>%
                group_by(setting, description) %>%
                summarize(y = mean(estimate)),
              aes(x = ifelse(setting == "counterfactual", 1.65, 1.35),
                  y = y,
                  label = ifelse(setting == "counterfactual", "Gap-Closing\nEstimand","Descriptive\nDisparity"),
                  hjust = ifelse(setting == "counterfactual", 0, 1),
                  color = description, alpha = description),
              size = 3, fontface = "bold", vjust = -.25,
              show.legend = F) +
    # Note the estimates for those disparities on the plot
    geom_text(data = forplot %>%
                filter(setting %in% c("factual","counterfactual") & category != "difference") %>%
                group_by(setting, description) %>%
                summarize(y = mean(estimate)) %>%
                left_join(forplot %>%
                            filter(setting %in% c("factual","counterfactual") & category == "difference") %>%
                            select(setting, estimate_with_ci) %>%
                            mutate(estimate_with_ci = gsub(" [(]","\n(",estimate_with_ci)),
                          by = "setting"),
              aes(x = ifelse(setting == "counterfactual", 1.65, 1.35),
                  y = y,
                  label = estimate_with_ci,
                  hjust = ifelse(setting == "counterfactual", 0, 1),
                  color = description, alpha = description),
              size = 3, fontface = "bold", vjust = 1.25,
              show.legend = F) +
    # Note the treatment effects
    geom_segment(data = forplot %>%
                   filter(category != "difference" & setting %in% c("factual","counterfactual")) %>%
                   select(setting, category, estimate) %>%
                   spread(key = setting, value = estimate) %>%
                   mutate(x = ifelse(category == "Professional", .92, 2.08)),
                 aes(x = x, xend = x,
                     y = factual, yend = counterfactual),
                 arrow = arrow(length = unit(.1,"in")),
                 color = "gray", alpha = ifelse(slide_number > 2, 1, 0)) +
    geom_text(data = forplot %>%
                filter(category != "difference" & setting %in% c("factual","counterfactual")) %>%
                group_by(category) %>%
                summarize(y = mean(estimate)) %>%
                mutate(x = ifelse(category == "Professional", .92, 2.08),
                       vjust = ifelse(category == "Professional", -1.5, 2.5)),
              aes(x = x, y = y, vjust = vjust),
              label = "Causal Effect", angle = 90,
              color = "gray", size = 3, alpha = ifelse(slide_number > 2, 1, 0)) +
    scale_x_discrete(name = "Your Father's Social Class",
                     expand = expansion(add = .3)) +
    scale_y_continuous(name = "Your Log Income") +
    scale_color_discrete(name = "Unit-Specific Quantity") +
    scale_shape_discrete(name = "Unit-Specific Quantity") +
    scale_alpha_manual(values = c(rep(0,max(c(0,2 - slide_number))),rep(1,max(c(2,slide_number))))) +
    guides(alpha = F) +
    theme_bw() +
    theme(legend.key.height = grid::unit(36,"pt")) +
    ggtitle("Intervention: Lift you out of the working class") +
    ggsave(paste0("figures/empirical_example_slide_",slide_number,".pdf"),
           height = 4, width = 6.5)
}


########################
# ADD FACETS FOR OTHER #
# INTERVENTIONS        #
########################

for (counterfactual_case in unique(counterfactual_estimates$counterfactual[counterfactual_estimates$counterfactual != "under_treatment"])) {
  forplot <- counterfactual_estimates %>%
    filter(method == "doubly_robust" & 
             setting %in% c("factual","counterfactual") & 
             category %in% c(FALSE,TRUE) &
             counterfactual == counterfactual_case) %>%
    mutate(category = ifelse(category, "Professional", "Working Class"))
  
  plot <- forplot %>%
    ggplot(aes(color = setting, shape = setting)) +
    geom_point(aes(x = category, y = estimate),
               position = position_dodge(width = -.1)) +
    geom_errorbar(aes(x = category,
                      ymin = estimate - qnorm(.975) * se, 
                      ymax = estimate + qnorm(.975) * se),
                  width = .1, size = .3,
                  position = position_dodge(width = -.1)) +
    geom_line(data = forplot,
              aes(x = ifelse(setting == "factual", 1.25, 1.75),
                  y = estimate),
              position = position_dodge(width = -.1),
              size = .3) +
    geom_segment(data = forplot,
                 aes(x = category, xend = ifelse(setting == "factual", 1.25, 1.75), 
                     y = estimate, yend = estimate),
                 linetype = "dashed",
                 position = position_dodge(width = -.1),
                 size = .3) +
    geom_text(data = counterfactual_estimates %>%
                filter(counterfactual == counterfactual_case & 
                         method == "doubly_robust" &
                         setting %in% c("factual","counterfactual") &
                         category == "TRUE - FALSE") %>%
                mutate(ci.min = estimate - qnorm(.975) * se,
                       ci.max = estimate + qnorm(.975) * se,
                       estimate_label = paste0(format(round(estimate,2),nsmall = 2),
                                               "\n(",format(round(ci.min,2),nsmall = 2),
                                               ", ",
                                               format(round(ci.max, 2), nsmall = 2),
                                               ")")),
              aes(x = ifelse(setting == "factual", 1.25, 1.75),
                  label = paste0(ifelse(setting == "factual","Descriptive\nDisparity","Gap Closing\nEstimand"),
                                 "\n",estimate_label),
                  y = 10.2),
              size = 1.7) +
    scale_x_discrete(name = "Father's Social Class",
                     expand = expansion(add = .35)) +
    ylab("Mean Log Income") + 
    ylim(c(9.55,10.35)) +
    ggtitle(case_when(counterfactual_case == "under_treatment" ~ "Intervention: Assign a\nprofessional occupation",
                      counterfactual_case == "under_control" ~ "B) Intervention: Assign a\nworking-class occupation",
                      counterfactual_case == "marginal" ~ "C) Intervention: Assign a\nrandom occupational class",
                      counterfactual_case == "conditional" ~ "D) Intervention: Assign randomly\nwithin covariates")) +
    theme_bw() +
    theme(legend.key.height = grid::unit(48,"pt"),
          legend.position = "none",
          #plot.title = element_text(size = 4),
          text = element_text(size = 7))
  assign(paste0("plot_",counterfactual_case),plot)
}
# Change the legend labels to work for all plot facets rather than just Panel A
change_labels <- c(
  "Assigned to\nprofessional class" = "Assigned by\nthe intervention",
  "As observed" = "As observed"
)
# Save the PDF
pdf("figures/main_with_alternates.pdf",
    height = 5, width = 6.5)
gridExtra::grid.arrange(plot_under_treatment +
                          scale_color_discrete(labels = as_labeller(change_labels),
                                               name = "Respondent's\nSocial Class") +
                          scale_shape_discrete(labels = as_labeller(change_labels),
                                               name = "Respondent's\nSocial Class") +
                          ggtitle("A) Intervention: Assign the respondent to a professional occupation"),
                        plot_under_control,
                        plot_marginal,
                        plot_conditional,
                        layout_matrix = rbind(c(1,1,1),c(2,3,4)),
                        heights = c(1.3,1))
dev.off()



####################################################
# Supplemental plot: Various estimation strategies #
####################################################

counterfactual_estimates %>%
  filter(setting %in% c("factual","counterfactual")) %>%
  filter(category == "TRUE - FALSE") %>%
  mutate(estimand = factor(case_when(setting == "factual" ~ 1,
                                     setting == "counterfactual" & counterfactual == "under_treatment" ~ 2,
                                     setting == "counterfactual" & counterfactual == "under_control" ~ 3,
                                     setting == "counterfactual" & counterfactual == "marginal" ~ 4,
                                     setting == "counterfactual" & counterfactual == "conditional" ~ 5),
                           labels = c("Factual\ngap",
                                      "Counterfactual\ngap if assigned\nto a professional\noccupation",
                                      "Counterfactual\ngap if assigned\nto a working class\noccupation",
                                      "Counterfactual\ngap under\nmarginal\nequalization",
                                      "Counterfactual\ngap under\nconditional\nequalization")),
         method = factor(case_when(setting == "factual" ~ 1,
                                   method == "doubly_robust" ~ 2,
                                   method == "treatment_modeling" ~ 3,
                                   method == "outcome_modeling" ~ 4,
                                   method == "gam_estimates" ~ 5,
                                   method == "ranger_estimates" ~ 6),
                         labels = c("Unadjusted\nmean difference",
                                    "Doubly-robust with\ngeneralized\nlinear models",
                                    "Estimation by\npredicted treatment\nprobabilities\nfrom generalized\nlinear models",
                                    "Estimation by\npredicted outcomes\nfrom generalized\nlinear models",
                                    "Cross-fitting\nwith generalized\nadditive models",
                                    "Cross-fitting\nwith random\nforests"))) %>%
  # Remove repeated factual rows that are identical
  group_by(estimand, method) %>%
  filter(1:n() == 1) %>%
  # Make the plot
  ggplot(aes(x = estimand, y = estimate, 
             color = method, shape = method,
             ymin = estimate - qnorm(.975) * se,
             ymax = estimate + qnorm(.975) * se,
             label = format(round(estimate, 2), nsmall = 2))) +
  geom_errorbar(position = position_dodge(width = .7),
                width = .2) +
  geom_point(position = position_dodge(width = .7)) +
  theme_bw() +
  xlab("Estimand") +
  ylab("Gap in log annual income by class origin\n(professional origin - working class origin)") +
  scale_color_discrete(name = "Estimation\nStrategy") +
  scale_shape_discrete(name = "Estimation\nStrategy") +
  theme(legend.key.height = unit(1.5,"cm")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggsave("figures/class_gap_all_estimators.pdf",
         height = 5, width = 8)

