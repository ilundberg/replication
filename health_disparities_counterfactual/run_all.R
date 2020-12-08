
# Main code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

setwd("C:/Users/iandl/Documents/health_disparities")

# This code calls the following packages
library(tidyverse)
library(labelled)
library(reshape2)
library(ipumsr)
library(mgcv)
library(foreach)
library(doParallel)
library(ggrepel)

sink("figures/session_info.txt")
print("Date and time of code run:")
print(Sys.time())
print("R VERSION:")
print(data.frame(R.Version()) %>% melt(id = NULL))
print("PACKAGE VERSIONS:")
for (package_name in c("tidyverse","labelled","reshape2","ipumsr","mgcv","foreach","doParallel","ggrepel")) {
  print(paste(package_name,packageVersion(package_name)))
}
sink()

# ESTIMATE THE MAIN RESULTS
# This requires the data:
# - data/cps_00050.xml
# - data/cps_00050.dat
# This code internally calls:
#- prepare_data.R
#- estimator_functions.R
source("code/cps_disability_estimation.R")

# PRODUCE FIGURES
# These files require:
# - The raw data exists in the data folder
# - The code (e.g. prepare_data.R) exists in the code folder
# - The intermediate results (output from estimation) exist in the intermediate folder
source("code/proportion_figure.R")
source("code/disparity_figure.R")
source("code/descriptive_figures.R")
source("code/occupation_scatters.R")
source("code/model_summaries.R")
source("code/intervention_density_shift.R")
source("code/alternative_specifications_figure.R")
source("code/year_specific_estimates.R")
source("code/occupational_hazards_within_between.R")

print("Finished at")
print(Sys.time())
