
# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# Main code file. Calls all individual code files.

setwd("/home/ubuntu/aws_output")

print("Prepare the R environment")
source("code/prepare_environment.R")

print("Prepare data")
source("code/prepare_data.R")

print("Note sample restrictions in a text log")
source("code/sample_restrictions.R")

print("Estimate. Each file internally calls estimator_functions.R.")

print("Descriptive estimates")
source("code/estimate_descriptive.R")

print("Scatters")
source("code/estimate_scatters.R")

print("Counterfactual estimates")
source("code/estimate_counterfactual.R")

print("Alternative specifications")
source("code/estimate_additionalControls.R")
source("code/estimate_withoutImmigrants.R")
source("code/estimate_forwardLinkingWeight.R")
source("code/estimate_from1988.R")
source("code/estimate_beforeRedesign.R")
source("code/estimate_afterRedesign.R")
source("code/estimate_doublyRobust.R")

print("Figures")
# Note: Some figures are entirely LaTeX and thus have no R code.
source("code/fig01.R")
source("code/fig03.R")
source("code/fig06.R")
source("code/fig07.R")
source("code/fig10.R")
source("code/fig14.R")
source("code/fig15.R")
source("code/fig16.R")

print("Finished at")
print(Sys.time())
