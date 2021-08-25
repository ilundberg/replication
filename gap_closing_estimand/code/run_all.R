
# Author: Ian Lundberg (ilundberg@princeton.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Paper is a working draft. Code will be posted on the Harvard Dataverse when the paper is finalized for publication.
# See run_all.R to set up the working environment.

# Set working directory
setwd("/Users/iandl/Documents/gap_closing_estimand")

# Prepare the working environment.
# This loads packages and checks the directory structure.
source("code/prepare_environment.R")
# Save the environment information
sink("figures/session_info.txt")
sessionInfo()
sink()

# Code for the empirical example
source("code/class_gap_example.R")
rm(list = ls(all = T))
source("code/class_gap_plots.R")
rm(list = ls(all = T))

# Code for the simulations
source("code/sim_parametric_double_robust.R")
rm(list = ls(all = T))
source("code/sim_parametric_double_robust_complex_sample.R")
rm(list = ls(all = T))
source("code/sim_cross_fitting_helps.R")
rm(list = ls(all = T))
