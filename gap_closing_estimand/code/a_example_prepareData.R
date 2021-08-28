# Author: Ian Lundberg (ianlundberg@ucla.edu)
# Title: The gap-closing estimand: A causal approach to study interventions that close gaps across social categories
# Available at https://doi.org/10.31235/osf.io/gx4y3
# Forthcoming in Sociological Methods and Research

# See README.md for guidance to the replication package.

# This file prepares data for the empirical example.

# Initialize sink file to hold printed output
my_sink <- file("logs/a_example_prepareData.txt", open = "wt")
sink(my_sink ,type = "output")
sink(my_sink, type = "message")

t0 <- Sys.time()
print("Time began:")
print(t0)

# Set seed
set.seed(08544)

# Load packages
source("code/prepare_environment.R")

# Print the computing environment information
print(sessionInfo())

# Load data
# The crosswalk comes from a GSS methodological memo: https://osf.io/xb2yz/
crosswalk <- read_csv(url("https://osf.io/xb2yz/download"))

print("Note some Class I occupations")
unique(crosswalk$title[crosswalk$egp10_10 == 1])

# The data come from the General Social Survey,
# available at: https://gss.norc.org/
# Temporarily change to the working directory of the data
# so that calls within the GSS.r file (provided by GSS) work correctly.
setwd(paste0(getwd(),"/data"))
source("GSS.r")
setwd(gsub("/data$","",getwd()))

# Restrict to the years 1975-2018.
# This restriction is because the complex survey sample variables are not defined before 1975.
GSS <- GSS %>% filter(YEAR >= 1975)

# Note that lone PSUs are very rare in the sample
GSS %>%
  group_by(VSTRAT) %>%
  mutate(lone_psu = n_distinct(VPSU) == 1) %>%
  group_by() %>%
  summarize(num_lone_psu = sum(lone_psu),
            mean_lone_psu = weighted.mean(lone_psu, w = WTSSALL),
            .groups = "drop")

# Remove the lone PSUs, merge in crosswalk, and define variables
GSS <- GSS  %>%
  group_by(VSTRAT) %>%
  mutate(lone_psu = n_distinct(VPSU) == 1) %>%
  group_by() %>%
  filter(!lone_psu) %>%
  select(-lone_psu) %>%
  left_join(crosswalk %>%
              select(occ10,egp10_10) %>%
              rename(OCC10 = occ10,
                     egp_r = egp10_10), by = "OCC10") %>%
  left_join(crosswalk %>%
              select(occ10,egp10_10) %>%
              rename(PAOCC10 = occ10,
                     egp_pa = egp10_10), by = "PAOCC10") %>%
  # Define treatment treated and group-defining category X
  mutate(treated = as.numeric(egp_r == 1),
         X = egp_pa == 1)
saveRDS(GSS, file = "intermediate/GSS_merged.Rds")

# Define balanced repeated replicates that will be used for uncertainty.
set.seed(08544)
# Define the survey design in the survey package
gss_svydesign <- svydesign(ids = ~ cluster_id,
                           strata = ~ VSTRAT,
                           weights = ~ WTSSALL,
                           data = GSS %>%
                             mutate(cluster_id = paste(VSTRAT,VPSU, sep = "_")))
# Create balanced repeated replicate weights
# The advantage of a computational approach is that inference works for
# a wide variety of estimation algorithms, without any adaptation.
# This line takes a long time.
gss_brr <- as.svrepdesign(gss_svydesign, type = "BRR", compress = F)
saveRDS(gss_brr, file = "intermediate/gss_brr.Rds")

# Close the sink
print(Sys.time())
print("Time spent:")
print(difftime(Sys.time(),t0))
sink()
