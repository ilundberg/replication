
# Replication code for:
# What is Your Estimand? Defining the Target Quantity Connects Statistical Evidence to Theory
# Ian Lundberg, Rebecca Johnson, and Brandon Stewart
# Email: ilundberg@princeton.edu

# This file calls the specific R scripts for each empirical analysis.

# This line should be set to your own working directory
setwd("/Users/iandl/downloads/estimands")

# This code assumes that your working directory contains three sub-directories:
# code, data, and output
# The code from the replication package goes in the code subdirectory.
# The data (access described in the readme) goes in the data subdirectory.
# All results will be placed in the output subdirectory.

set.seed(08544)

# The following code files can be run independently and in any order.

source("code/a_estimation_example.R")
rm(list = ls())

source("code/b_specific_example_1.R")
rm(list = ls())

source("code/c_specific_example_2.R")
rm(list = ls())

source("code/d_risk_ratio_difference.R")
rm(list = ls())
