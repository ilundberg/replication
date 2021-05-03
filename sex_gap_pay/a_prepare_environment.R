
library(tidyverse)
library(haven)
library(reshape2)
library(doParallel)
library(foreach)
library(ggrepel)
library(gridExtra)

setwd("/Users/iandl/Dropbox/Dissertation/gender_gap_occupations")

#########################
# Convenience functions #
#########################

# Functions to produce weighted quantiles
weighted.quantile <- function(x, q, w) {
  (data.frame(x = x, w = w) %>%
     arrange(x) %>%
     mutate(cum_prop = cumsum(w) / sum(w)) %>%
     filter(cum_prop > q) %>%
     filter(1:n() == 1))$x
}
weighted.median <- function(x,w) {
  weighted.quantile(x, q = .5, w = w)
}
