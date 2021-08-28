# Author: Ian Lundberg (ianlundberg@ucla.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Forthcoming in Sociological Methods and Research

# See README.md for guidance to the replication package.

# This file converts the results of the empirical example into figures.

# Initialize sink file to hold printed output
my_sink <- file("logs/c_example_produceFigures.txt", open = "wt")
sink(my_sink ,type = "output")
sink(my_sink, type = "message")

t0 <- Sys.time()
print("Time began:")
print(t0)

# Set seed
set.seed(08544)

# Load packages
source("code/prepare_environment.R")

# Print the computing environment information
print(sessionInfo())

# Load the estimates
aggregated_estimates <- readRDS("intermediate/aggregated_estimates.Rds")

########################
# Plot under treatment #
########################

# This begins with the basic disparityplot() result.
# Then, it uses some relatively complex code to improve that for this
# particular case, since this is a main plot for the empirical example.

p_under_treatment <- disparityplot(aggregated_estimates$under_treatment$glm_estimates,
              category_A = "TRUE", category_B = "FALSE") +
  scale_color_discrete(labels = function(x) case_when(x == "Factual" ~ "As observed",
                                                      x == "Counterfactual" ~ "Assigned by the intervention"),
                       guide = guide_legend(reverse = T),
                       name = "Respondent's\nSocial Class") +
  scale_shape_discrete(labels = function(x) case_when(x == "Factual" ~ "As observed",
                                                      x == "Counterfactual" ~ "Assigned by the intervention"),
                       guide = guide_legend(reverse = T),
                       name = "Respondent's\nSocial Class") +
  theme(legend.title = element_text(hjust = 0)) +
  ylab("Mean Log Income") +
  scale_x_discrete(name = "Father's Social Class",
                   labels = function(x) ifelse(x == "TRUE", "Professional", "Working Class")) +
  ggtitle("A) Intervention: Assign the respondent to a professional occupation")

for (i in 1:7) {
  p_under_treatment$layers[[i]]$position <- position_identity()
}
for (i in c(2,5)) {
  p_under_treatment$layers[[i]]$mapping$x <- 1.5
}
p_under_treatment$layers[[3]]$mapping$xend <- 1.5
p_under_treatment$layers[[4]]$mapping$x <- 1.25
p_under_treatment$layers[[4]]$mapping$y <- c(mean(aggregated_estimates$under_treatment$glm_estimates$factual_means$estimate),
                                             mean(aggregated_estimates$under_treatment$glm_estimates$counterfactual_means$estimate))
p_under_treatment$layers[[4]]$aes_params$vjust <- .5
p_under_treatment$layers[[5]]$aes_params$hjust <- 0
p_under_treatment$layers[[5]]$mapping$x <- 1.55
p_under_treatment$layers[[5]]$data <- p_under_treatment$layers[[5]]$data %>%
  # Remove the label
  select(-label) %>%
  # Replace with a label containing the CI
  left_join(aggregated_estimates$under_treatment$glm_estimates$factual_disparities %>%
              filter(X == "TRUE - FALSE") %>%
              mutate(ci.min = estimate - qnorm(.975) * se,
                     ci.max = estimate + qnorm(.975) * se) %>%
              mutate(across(all_of(c("estimate","ci.min","ci.max")),
                            function(value) format(round(value,2),nsmall = 2))) %>%
              transmute(setting = "Factual",
                        label = paste0(estimate," (",ci.min,", ",ci.max,")")) %>%
              bind_rows(aggregated_estimates$under_treatment$glm_estimates$counterfactual_disparities %>%
                          filter(X == "TRUE - FALSE") %>%
                          mutate(ci.min = estimate - qnorm(.975) * se,
                                 ci.max = estimate + qnorm(.975) * se) %>%
                          mutate(across(all_of(c("estimate","ci.min","ci.max")),
                                        function(value) format(round(value,2),nsmall = 2))) %>%
                          transmute(setting = "Counterfactual",
                                    label = paste0(estimate," (",ci.min,", ",ci.max,")"))),
            by = "setting")
saveRDS(p_under_treatment, file = "intermediate/p_under_treatment.Rds")
ggsave(p_under_treatment,
       file = "figures/empirical_example.pdf",
       height = 3, width = 6.5)

#######################################
# Plots under each other intervention #
#######################################

# This begins with the basic disparityplot() result.
# Then, it uses some relatively complex code to improve that for this
# particular case, since this is a main plot for the empirical example.

for (counterfactual_case in c("under_control","marginal","conditional")) {
  p <- disparityplot(aggregated_estimates[[counterfactual_case]]$glm_estimates,
                     category_A = "TRUE",
                     category_B = "FALSE") +
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
  # Remove the causal effect arrows and note
  # Remove the text annotations
  p$layers <- p$layers[c(1:3)]
  # Add text annotations for these plots
  p <- p +
    geom_text(data = aggregated_estimates[[counterfactual_case]]$glm_estimates$counterfactual_disparities %>%
                mutate(setting = "Counterfactual") %>%
                bind_rows(aggregated_estimates[[counterfactual_case]]$glm_estimates$factual_disparities %>%
                            mutate(setting = "Factual")) %>%
                mutate(setting = factor(setting)) %>%
                filter(X == "TRUE - FALSE"),
      aes(x = ifelse(setting == "Factual", 1.25, 1.75),
                  y = 10.1,
                  label = paste0(ifelse(setting == "Factual",
                                        "Descriptive\nDisparity",
                                        "Gap Closing\nEstimand"),
                                 "\n",format(round(estimate,2),nsmall = 2),
                                 "\n(",format(round(estimate - qnorm(.975) * se,2),nsmall = 2),
                                 ", ",format(round(estimate + qnorm(.975) * se,2),nsmall = 2),
                                 ")"),
                  color = setting),
              vjust = 0, size = 2)
  assign(paste0("p_",counterfactual_case),p)
}

# Save the figure containing the four interventions
pdf("figures/main_with_alternates.pdf",
    height = 5, width = 6.5)
gridExtra::grid.arrange(p_under_treatment,
                        p_under_control,
                        p_marginal,
                        p_conditional,
                        layout_matrix = rbind(c(1,1,1),c(2,3,4)),
                        heights = c(1.3,1))
dev.off()

####################################################
# Supplemental plot: Various estimation strategies #
####################################################

results <- foreach(counterfactual_case = names(aggregated_estimates), .combine = "rbind") %do% {
  foreach(estimator = names(aggregated_estimates[[counterfactual_case]]), .combine = "rbind") %do% {
    as.data.frame(aggregated_estimates[[counterfactual_case]][[estimator]]) %>%
      filter(estimand == "counterfactual_disparities" & X == "TRUE - FALSE")
  }
} %>%
  filter(model == "glm_estimates" | primary) %>%
  # Add in one copy of the factual (same in all estimators)
  bind_rows(as.data.frame(aggregated_estimates$under_treatment$glm_estimates) %>%
              filter(estimand == "factual_disparities" & X == "TRUE - FALSE") %>%
              mutate(counterfactual = NA, model = NA))

class_gap_all_estimators <- results %>%
  mutate(estimand = factor(case_when(estimand == "factual_disparities" ~ 1,
                                     estimand == "counterfactual_disparities" & counterfactual == "under_treatment" ~ 2,
                                     estimand == "counterfactual_disparities" & counterfactual == "under_control" ~ 3,
                                     estimand == "counterfactual_disparities" & counterfactual == "marginal" ~ 4,
                                     estimand == "counterfactual_disparities" & counterfactual == "conditional" ~ 5),
                           labels = c("Factual\ngap",
                                      "Counterfactual\ngap if assigned\nto a professional\noccupation",
                                      "Counterfactual\ngap if assigned\nto a working class\noccupation",
                                      "Counterfactual\ngap under\nmarginal\nequalization",
                                      "Counterfactual\ngap under\nconditional\nequalization")),
         method = factor(case_when(estimator == "mean" ~ 1,
                                   model == "glm_estimates" & estimator == "doubly_robust" ~ 2,
                                   model == "glm_estimates" & estimator == "treatment_modeling" ~ 3,
                                   model == "glm_estimates" & estimator == "outcome_modeling" ~ 4,
                                   model == "gam_estimates" & estimator == "doubly_robust" ~ 5,
                                   model == "ranger_estimates" & estimator == "doubly_robust" ~ 6),
                         labels = c("Unadjusted\nmean difference",
                                    "Doubly-robust with\ngeneralized\nlinear models",
                                    "Estimation by\npredicted treatment\nprobabilities\nfrom generalized\nlinear models",
                                    "Estimation by\npredicted outcomes\nfrom generalized\nlinear models",
                                    "Cross-fitting\nwith generalized\nadditive models",
                                    "Cross-fitting\nwith random\nforests"))) %>%
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
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave(plot = class_gap_all_estimators,
       file = "figures/class_gap_all_estimators.pdf",
       height = 5, width = 8)


# Close the sink
print(Sys.time())
print("Time spent:")
print(difftime(Sys.time(),t0))
sink()
