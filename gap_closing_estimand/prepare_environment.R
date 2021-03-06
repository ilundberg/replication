
# Author: Ian Lundberg (ilundberg@princeton.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Paper is a working draft. Code will be posted on the Harvard Dataverse when the paper is finalized for publication.
# See run_all.R to set up the working environment.

# This file checks the working environment and loads packages.

# Load packages
library(tidyverse)
library(reshape2)
library(ggridges)
library(haven)
library(survey)
library(foreach)
library(doParallel)
library(doRNG)
library(Amelia)
library(xtable)
library(gapclosing)

# 1. Check that the environment is correctly organized to run the code
# You will need subdirectories called code, data, intermediate, figures
files_in_directory <- list.files()
has_all_subdirectories <- all(c("code","data","intermediate","figures") %in% files_in_directory)
if (!has_all_subdirectories) {
  stop("You need to create the necessary subdirectories.")
}
# You will need all the code files (including this one) in the code subdirectory
files_in_code <- list.files("code/")
has_all_code <- all(c("run_all.R",
                      "class_gap_example.R","class_gap_plots.R",
                      "sim_parametric_double_robust.R","sim_parametric_double_robust_complex_sample.R",
                      "sim_cross_fitting_helps.R") %in% files_in_code)
if (!has_all_code) {
  stop("Your code subdirectory does not have the required code files.")
}
# You will need the private files in a data/ directory
files_in_data <- list.files("data/")
has_all_data_files <- all(c("occ10-to-egp-class-crosswalk.csv",
                            "GSS.dat",
                            "GSS.dct") %in% files_in_data)
if (!has_all_data_files) {
  stop("Your data subdirectory does not have the required files.")
}