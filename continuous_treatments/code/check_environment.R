
code_files <- list.files("code")

needed_files <- c(
  # main file
  "main.R",
  # files called from main file
  "check_environment.R",
  "nonlinear_heterogeneous_conceptual.R",
  "strong_confounding.R",
  "prepare_data.R",
  "descriptive_smoother.R",
  "descriptive_binner.R",
  "strong_confounding.R",
  "causal_estimator.R",
  "causal_estimator_gam.R",
  "visualize_model.R",
  "visualize_descriptive.R",
  "visualize_causal.R",
  "visualize_logit_gam_scatter.R",
  "summarize_x_given_effect.R",
  "visualize_model.R",
  "simulation_forest_can_underperform.R",
  "simulation_interactive_nonlinear_mse.R",
  # supporting files called within scripts above
  "load_data.R",
  "label_outcomes_treatments.R",
  "label_terciles.R",
  "weighted.quantile.R"
)

if (!all(needed_files %in% code_files)) {
  stop(paste("code directory needs the following files:",
             paste(needed_files[!(needed_files %in% code_files)],
                   collapse = " ")))
}