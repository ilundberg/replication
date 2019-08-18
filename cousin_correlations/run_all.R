

# Replication code for:
# Does Opportunity Skip Generations?
# Reassessing Evidence from Sibling and Cousin Correlations
# Code by Ian Lundberg
# ilundberg@princeton.edu

# This file calls all code files in order.

setwd("C:/Users/iandl/Documents/Empirical")
set.seed(08544)

source("code/prepData.R")
source("code/fitModels.R")
source("code/produceGraphs.R")
source("code/impliedParameters.R")
source("code/frequentistComparison.R")
source("code/fitParents.R")
