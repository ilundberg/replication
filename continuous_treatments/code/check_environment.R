
code_files <- list.files("code")

needed_files <- c(
  # main file
  "main.R",
  # files called from main file
  "check_environment.R",
  "prepare_data.R",
  "descriptive_binner.R",
  "descriptive_smoother.R",
  "extrapolation_danger.R",
  "strong_confounding.R",
  "causal_estimator.R",
  "analyze_to_visualize_model.R",
  "visualize_model.R",
  "visualize_descriptive.R",
  "visualize_causal.R",
  "visualize_causal_subgroup_estimates.R",
  "summarize_x_given_effect.R",
  "visualize_inductive.R",
  # supporting files called within scripts above
  "label_outcomes_treatments.R",
  "label_terciles.R",
  "load_data.R",
  "weighted.quantile.R"
)

if (!all(needed_files %in% code_files)) {
  stop(paste("code directory needs the following files:",
             paste(needed_files[!(needed_files %in% code_files)],
                   collapse = " ")))
}