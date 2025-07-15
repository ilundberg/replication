

sink("figures/log_estimation.txt")
print(Sys.time())

source("code/estimator_functions.R")

set.seed(90095)

cl <- makeCluster(round(detectCores() / 2))
registerDoParallel(cl)

#################
# Load the data #
#################

# The intermediate data files from prepare_data() contain all years.
# When loading each file, restrict to a subset for the main analyses.
full_population <- readRDS("intermediate/full_population.Rds") %>%
  filter(YEAR >= 2005)
linked <- readRDS("intermediate/linked.Rds") %>%
  filter(YEAR >= 2005)
d_onset <- readRDS("intermediate/d_onset.Rds") %>%
  filter(YEAR >= 2005)
d <- readRDS("intermediate/d.Rds") %>%
  filter(YEAR >= 2005)

# Print sample restrictions for main analyses
print("Full population size")
print(nrow(full_population))
print("Oversamples")
print(sum(full_population$CPSIDP == 0))
print("From March basic CPS")
print(sum(full_population$CPSIDP != 0))
print("Not linked (person-years)")
print(sum(full_population$CPSIDP != 0) - 2 * nrow(linked))
print("Linked (persons)")
print(nrow(linked))
print("Risk of recovery")
print(sum(linked$lag))
print("History of health limitations")
print(sum(linked$QUITSICK & !linked$lag))
print("Not employed")
print(sum(!linked$employed & !linked$lag & !linked$QUITSICK))
print("Risk of onset")
print(nrow(d_onset))
print("Lack of common support")
print(nrow(d_onset) - nrow(d))
print("Occupation analyses")
print(nrow(d))

#######################
# CALCULATE ESTIMATES #
#######################

print("Beginning counterfactual disparity estimation")
t0 <- Sys.time()
print(t0)
counterfactual_point <- counterfactual_estimator(weight_name = "ASECWT",
                                                 save_intermediate = T)
spent <- difftime(Sys.time(),t0)

t0 <- Sys.time()
print(t0)
counterfactual_reps <- foreach(
  i = 1:160, 
  .combine = "rbind", 
  .packages = c("tidyverse","foreach","mgcv")
) %dorng% {
  # Load this replicate weight
  this_repwt <- readRDS(paste0("intermediate/REPWTP",i,".rds"))
  # Apply the counterfactual estimator with that replicate weight
  counterfactual_estimator(weight_name = paste0("REPWTP",i),
                           data = d %>%
                             left_join(this_repwt,
                                       by = c("CPSIDP","YEAR"))) %>%
    mutate(replicate = i)
}
spent <- difftime(Sys.time(),t0)
print("Time spent on replicates")
print(spent)

counterfactual_estimate <- estimate_from_point_reps(counterfactual_point, counterfactual_reps) 

counterfactual_results <- list(counterfactual_estimate = counterfactual_estimate,
                               counterfactual_point = counterfactual_point,
                               counterfactual_reps = counterfactual_reps)
save(counterfactual_results, file = "intermediate/counterfactual_results.Rdata")
print("Finished counterfactual disparity estimation")


stopCluster(cl)


sink()
