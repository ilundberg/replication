
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
has_all_code <- all(c("a_example_prepareData.R",
                      "b_example_analyzeData.R",
                      "c_example_produceFigures.R",
                      "d_sim_double_robust.R",
                      "e_sim_complex_sample.R",
                      "f_sim_cross_fitting.R",
                      "g_slide_animated_figures.R",
                      "prepare_environment.R") %in% files_in_code)
if (!has_all_code) {
  stop("Your code subdirectory does not have the required code files.")
}
# You will need the private files in a data/ directory
files_in_data <- list.files("data/")
has_all_data_files <- all(c("GSS.dat","GSS.dct","GSS.r") %in% files_in_data)
if (!has_all_data_files) {
  stop("Your data subdirectory does not have the required files.")
}
