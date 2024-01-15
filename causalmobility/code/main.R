library(here)

# This file must be run before the others
source("code/prepare_nlsy.R")

# These files can be run in any order
source("code/fig2_class_positivity.R")
source("code/fig3_scores.R")
source("code/fig4_classes.R")
source("code/text_notes.R")
source("code/appendix_figures.R")
