
# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# This file prepares the R environment and checks the directory structure.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
sink("logs/prepare_environment.txt")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Set the seed
set.seed(08544)

# Packages to be installed
package_names <- c("tidyverse","reshape2","labelled","ipumsr",
                   "mgcv","foreach","doParallel","ggrepel","nnet")

# Install packages
for (package_name in package_names) {
  if (!(package_name %in% rownames(installed.packages()))) {
    install.packages(package_name,
                     repos = "https://cloud.r-project.org")
  }
}

# Load packages
for (package_name in package_names) {
  library(package_name, character.only = T)
}

# Record session info, which contains version numbers
print(sessionInfo())

# Check directory structure
if (!(all(c("data","code","intermediate","logs","figures") %in% list.files()))) {
  stop("ERROR: Missing one of these folders: data, code, intermediate, logs, figures")
}
if (!(all(c("cps_00050.xml","cps_00050.dat") %in% list.files("data")))) {
  stop("ERROR: Missing one of data/cps_00050.xml or data/cps_00050.dat")
}

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
