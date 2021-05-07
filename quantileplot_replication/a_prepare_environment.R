
# Replication code for
# "Smooth quantile visualizations enhance understanding of bivariate population distributions"
# Robin C. Lee, Ian Lundberg, and Brandon M. Stewart

# This file: Prepares the environment

library(tidyverse)
library(reshape2)
library(lubridate)
library(haven)
library(readxl)
library(tidycensus)
library(foreach)
library(quantileplot)

# Check that data files exist
data_files <- list.files("data")

has_files <- c(
  mortality = all(c("API_SP.DYN.IMRT.IN_DS2_en_csv_v2_2166906.csv",
                    "API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2163510.csv") %in% data_files),
  citations = all(paste0("demography_",c("a","b","c"),".xls") %in% data_files),
  poverty = "R12822111_SL140.csv" %in% data_files,
  covid = "census_apikey.txt" %in% data_files,
  mobility = all(c("J263447.dta","cpi.xlsx") %in% data_files)
)

print("Are the needed data files present for each example?")
print(has_files)
