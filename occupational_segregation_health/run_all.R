
# This code calls the following packages
library(tidyverse)
library(ipumsr)
library(mgcv)
library(foreach)
library(doParallel)
library(doRNG)
library(ggrepel)

sink("figures/log_session_info.txt")
print("Date and time of code run:")
print(Sys.time())
print("R VERSION:")
print(data.frame(R.Version()) %>% pivot_longer(cols = everything()))
print("PACKAGE VERSIONS:")
for (package_name in c("tidyverse","ipumsr","mgcv","foreach","doParallel","ggrepel")) {
  print(paste(package_name,packageVersion(package_name)))
}
sink()

# PREPARE DATA
# This requires the data:
# - data/cps_00050.xml
# - data/cps_00050.dat
source("code/prepare_data.R")

# ESTIMATE THE MAIN RESULTS
# This requires the file code/estimator_functions.R
source("code/estimation.R")

# PRODUCE FIGURES

# Notes:

# Figure 1 takes minutes to run because it estimates
#   the x- and y-coordinates of dots in the scatters by multilevel models.
# Figure 6 takes minutes because it constructs
#  confidence intervals calculated with replicate weights.
# Figures 15 and 16 take about a minute because they each create
#  new point estimates under alternative sample restrictions.

source("code/figure_1.R")
source("code/figure_4.R")
source("code/figure_6.R")
source("code/figure_7a.R")
source("code/figure_7b.R")
source("code/figure_8.R")
source("code/figure_14.R")
source("code/figure_15.R")
source("code/figure_16.R")

sink("figures/log_finish_time.txt")
print("Finished at")
print(Sys.time())
sink()
