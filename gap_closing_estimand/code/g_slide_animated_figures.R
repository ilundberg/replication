# Author: Ian Lundberg (ianlundberg@ucla.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Forthcoming in Sociological Methods and Research

# See README.md for guidance to the replication package.

# This file produces animated versions of figures used in slides.

# Initialize sink file to hold printed output
my_sink <- file("logs/g_slide_animated_figures.txt", open = "wt")
sink(my_sink ,type = "output")
sink(my_sink, type = "message")

t0 <- Sys.time()
print("Time began:")
print(t0)

# Set seed
set.seed(08544)

# Load packages
source("code/prepare_environment.R")

###############################################
# Main empirical example plot: Slide versions #
###############################################

p <- readRDS("intermediate/p_under_treatment.Rds")
# Note that the layers are as follows:
# Layer 1 is the points
# Layer 2 is the vertical lines
# Layer 3 is the horizontal lines
# Layer 4 are the word annotations
# Layer 5 is the estimate and CI annotations
# Layer 6 is the causal effect arrows
# Layer 7 is the causal effect words

# Slide 1: Blank plot
for (i in 1:7) {
  if (is.null(p$layers[[i]]$aes_params)) {
    p$layers[[i]]$aes_params <- list(alpha = 0)
  } else {
    p$layers[[i]]$aes_params <- c(p$layers[[i]]$aes_params,
                                  list(alpha = 0))
  }
}
ggsave(plot = p,
       filename = paste0("figures/empirical_example_slide_1.pdf"),
       height = 4, width = 6.5)

# Slide 2: Descriptive disparity
for (i in 1:3) {
  p$layers[[i]]$aes_params$alpha <- c(1,1,0,0)
}
for (i in 4:5) {
  p$layers[[i]]$aes_params$alpha <- c(1,0)
}
ggsave(plot = p,
       filename = paste0("figures/empirical_example_slide_2.pdf"),
       height = 4, width = 6.5)

# Slide 3: Causal effect among working class
p$layers[[1]]$aes_params$alpha <- c(1,1,1,0)
p$layers[[6]]$aes_params$alpha <- c(0,1)
p$layers[[7]]$aes_params$alpha <- c(0,0,1,1)
ggsave(plot = p,
       filename = paste0("figures/empirical_example_slide_3.pdf"),
       height = 4, width = 6.5)

# Slide 4: Causal effect among professional class
p$layers[[1]]$aes_params$alpha <- 1
p$layers[[6]]$aes_params$alpha <- 1
p$layers[[7]]$aes_params$alpha <- 1
ggsave(plot = p,
       filename = paste0("figures/empirical_example_slide_4.pdf"),
       height = 4, width = 6.5)

# Slide 5: Gap-closing estimand
for (i in 2:5) {
  p$layers[[i]]$aes_params$alpha <- 1
}
ggsave(plot = p,
       filename = paste0("figures/empirical_example_slide_5.pdf"),
       height = 4, width = 6.5)

#######################################################
# Parametric double robust simulation: Slide versions #
#######################################################

sim_densities <- readRDS("intermediate/sims_densities.Rds")
for (slide_number in 0:3) {
  p <- sim_densities  +
    scale_alpha_manual(values = c(rep(0,3 - slide_number),rep(1,slide_number))) +
    theme(legend.position = "none")
  ggsave(plot = p,
         file = paste0("figures/sim_x1mx0_",slide_number,".pdf"),
         height = 2, width = 8)
}

########################################################
# Cross-fitting convergence simulation: Slide versions #
########################################################

sim_cross_fitting_convergence <- readRDS("intermediate/sim_cross_fitting_convergence.Rds")
for (slide_number in 0:3) {
  p <- sim_cross_fitting_convergence  +
    scale_alpha_manual(values = c(rep(1,slide_number),rep(0,3 - slide_number)))
  ggsave(plot = p,
         file = paste0("figures/sim_cross_fitting_convergence_",slide_number,".pdf"),
         height = 2, width = 8)
}

# Close the sink
print(Sys.time())
print("Time spent:")
print(difftime(Sys.time(),t0))
sink()
