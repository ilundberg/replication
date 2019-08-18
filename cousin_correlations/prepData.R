
# Replication code for:
# Does Opportunity Skip Generations?
# Reassessing Evidence from Sibling and Cousin Correlations
# Code by Ian Lundberg
# ilundberg@princeton.edu

# run_all.R sources all code files in order

# This file prepares data for analysis.

setwd("C:/Users/iandl/Documents/Empirical")

# This call stores all printouts in a text file
# of notes from data prep
sink("output/data_prep_notes.txt")

####################
## 1. PREPARATION ##
####################

## Load packages
library(tidyverse)
library(stringr)
library(reshape2)
library(haven)
library(foreach)
library(doParallel)

setwd("C:/Users/iandl/Documents/Empirical")

## Load the main PSID data
main.psid <- read_dta("Data/J241239.dta") %>%
  ## Create a person and a familyid
  mutate(familyid = ER30001,
         person_number = ER30002,
         person = paste0(familyid,"_",person_number),
         src_sample = (familyid <= 2930),
         sample_member_type = factor(ER32006, labels = c(
           "Nonsample","Original","Born in","Moved in",
           "Followable nonsample parent","Nonsample elderly"
         )),
         female = ifelse(ER32000 == 9, NA,
                         ER32000 == 2))
rownames(main.psid) <- NULL

#######################
## 2. READ VARIABLES ##
#######################

## Make a vector of all PSID years
years <- c(1968:1997, seq(1999,2015,2))

## Code to turn PSID documentation into usable output
## This takes a string like "[68]ER30003 [69]ER30022"
## and returns those variables from the data in a clean form
clean <- function(string, data = main.psid, years.data = years, varName = varName) {
  ## Separate the many variable names in the character string
  vector_of_variables <- strsplit(string, split = " ")[[1]]
  
  ## Split each variable into its year and variable name
  df_years_varNames <- (foreach(x = vector_of_variables, .combine = "rbind") %do% {
    separated <- data.frame(t(strsplit(x, split = "]")[[1]]))
    ## Convert the year to 
    return(separated)
  }) %>%
    transmute(year = as.character(X1),
              variable = as.character(X2)) %>%
    mutate(year = str_replace(year,"\\[0","200"),
           year = str_replace(year,"\\[1","201"),
           year = str_replace(year,"\\[","19"),
           year = as.numeric(year))
  
  ## Produce a tidy data frame of this variable,
  ## with rows identified by person and year
  df_this_variable <- main.psid %>%
    select(person,
           matches(paste(df_years_varNames$var,collapse="|"))) %>%
    melt(id = "person", value.name = varName,
         warn = F) %>%
    mutate(variable = as.character(variable)) %>%
    left_join(df_years_varNames, by = "variable") %>%
    select(-variable)
  return(df_this_variable)
}

## Clean many variables
t0 <- Sys.time()
d_beforeRestrict <- clean("[68]ER30003 [69]ER30022 [70]ER30045 [71]ER30069 [72]ER30093 [73]ER30119 [74]ER30140 [75]ER30162 [76]ER30190 [77]ER30219 [78]ER30248 [79]ER30285 [80]ER30315 [81]ER30345 [82]ER30375 [83]ER30401 [84]ER30431 [85]ER30465 [86]ER30500 [87]ER30537 [88]ER30572 [89]ER30608 [90]ER30644 [91]ER30691 [92]ER30735 [93]ER30808 [94]ER33103 [95]ER33203 [96]ER33303 [97]ER33403 [99]ER33503 [01]ER33603 [03]ER33703 [05]ER33803 [07]ER33903 [09]ER34003 [11]ER34103 [13]ER34203 [15]ER34303",
                          varName = "reltohead") %>%
  filter(reltohead %in% c(1,2,10,20,22)) %>%
  left_join(
    clean("[68]ER30004 [69]ER30023 [70]ER30046 [71]ER30070 [72]ER30094 [73]ER30120 [74]ER30141 [75]ER30163 [76]ER30191 [77]ER30220 [78]ER30249 [79]ER30286 [80]ER30316 [81]ER30346 [82]ER30376 [83]ER30402 [84]ER30432 [85]ER30466 [86]ER30501 [87]ER30538 [88]ER30573 [89]ER30609 [90]ER30645 [91]ER30692 [92]ER30736 [93]ER30809 [94]ER33104 [95]ER33204 [96]ER33304 [97]ER33404 [99]ER33504 [01]ER33604 [03]ER33704 [05]ER33804 [07]ER33904 [09]ER34004 [11]ER34104 [13]ER34204 [15]ER34305",
          varName = "age"),
    by = c("person","year")
  ) %>%
  filter(age >= 25 & age <= 45) %>%
  left_join(
    clean("[68]V81 [69]V529 [70]V1514 [71]V2226 [72]V2852 [73]V3256 [74]V3676 [75]V4154 [76]V5029 [77]V5626 [78]V6173 [79]V6766 [80]V7412 [81]V8065 [82]V8689 [83]V9375 [84]V11022 [85]V12371 [86]V13623 [87]V14670 [88]V16144 [89]V17533 [90]V18875 [91]V20175 [92]V21481 [93]V23322 [94]ER4153 [95]ER6993 [96]ER9244 [97]ER12079 [99]ER16462 [01]ER20456 [03]ER24099 [05]ER28037 [07]ER41027 [09]ER46935 [11]ER52343 [13]ER58152 [15]ER65349",
          varName = "head.familyIncome"),
    by = c("person", "year")
  ) %>%
  mutate(wife.familyIncome = head.familyIncome) %>%
  melt(id = c("person","reltohead","year","age")) %>%
  ## This separate takes several minutes
  separate(variable, into = c("respondent","variable"),
           sep = "\\.") %>%
  spread(key = "respondent", value = "value") %>%
  mutate(value = ifelse(reltohead %in% c(1,10), head,
                        ifelse(reltohead %in% c(2,20,22), wife, NA))) %>%
  select(person, reltohead, year, age, variable, value) %>%
  spread(key = "variable", value = "value") %>%
  ## Adjust incomes by cpi
  ## CPI values for 1967 - 1996, 1998, 2000, ..., 2014
  ## (for the year before each survey, since income reported for last year)
  ## adjusting to 2015 dollars
  ## Calculated for July (middle of year)
  ## https://www.bls.gov/data/inflation_calculator.htm
  left_join(
    data.frame(year = c(1968:1997,seq(1999,2015,2)),
               cpi = c(
                 7.15,6.84,6.49,6.12,5.86,5.70,5.39,4.83,4.40,4.18,
                 3.91,3.63,3.26,2.89,2.61,2.45,2.39,2.29,2.21,2.18,
                 2.10,2.05,1.92,1.83,1.75,1.70,1.65,1.61,1.56,1.52,
                 1.46,1.38,1.33,1.26,1.17,1.08,1.09,1.04,1.00
               )),
    by = "year"
  ) %>%
  mutate(familyIncome = familyIncome * cpi)

##########################
## 4. IDENTIFY LINEAGES ##
##########################

# Load the Family Identification Mapping System file
# for all (biological only) grandparents mapped prospectively (grandparent to child)
# in a (balanced map)
fims_grandparents_prospective <- readxl::read_xlsx("data/fim9490_gidpro_BO_3_BAL.xlsx")

# Make one row per grandchild, 
# with all 4 grandparents indicated (or NA if not in sample)
# and with both parents indicated
lineage_identifiers <- fims_grandparents_prospective %>%
  # Remove rows where the grandchild is unknown
  filter(!is.na(G3PN)) %>%
  # Restrict to those where the grandparent-parent-offspring are generations 1, 2, and 3
  filter(G1POS == 1 & G2POS == 2 & G3POS == 3) %>%
  mutate(g1_identifier = paste0(G1ID68,"_",G1PN),
         g2_identifier = paste0(G2ID68,"_",G2PN),
         g3_identifier = paste0(G3ID68,"_",G3PN)) %>%
  select(g3_identifier, g1_identifier, g2_identifier) %>%
  group_by(g3_identifier) %>%
  arrange(g1_identifier) %>%
  mutate(gp_index = paste0("grandparent_",1:n())) %>%
  spread(key = gp_index, value = g1_identifier) %>%
  arrange(g2_identifier) %>%
  mutate(parent_index = paste0("parent_",1:n())) %>%
  spread(key = parent_index, value = g2_identifier)

# Confirm some expected facts of the PSID following a lineage.
print("Parent 2 is almost always missing")
table(is.na(lineage_identifiers$parent_2))
print("Grandparents 3 and 4 are almost always missing")
table(is.na(lineage_identifiers$grandparent_3))
table(is.na(lineage_identifiers$grandparent_4))
# Almost everyone appears only once
print(data.frame(lineage_identifiers %>%
                   summarize(num_appearances = n()) %>%
                   group_by(num_appearances) %>%
                   summarize(num = n())))
# Those 4 that appear twice are all ones who have a second parent,
# i.e. in these cases I think both parents were already sample
# members before they partnered
print(data.frame(lineage_identifiers %>%
                   group_by(g3_identifier) %>%
                   mutate(ever_has_second_parent = any(!is.na(parent_2))) %>%
                   filter(ever_has_second_parent)))
# That is also where grandparent_3 and grandparent_4 come from

# Restrict to those with parent_1 and grandparent_1.
# This allows parent_1 to be the siblingset identifier and grandparent_1
# to be the cousinset identifier.
lineage_identifiers_restricted <- lineage_identifiers %>%
  filter(!is.na(parent_1) & !is.na(grandparent_1)) %>%
  select(g3_identifier, grandparent_1, parent_1) %>%
  group_by() %>%
  transmute(person = g3_identifier,
            siblingset = parent_1,
            cousinset = grandparent_1)

# Also note who has children in the PSID,
# because we want to note this in the sample restrictions
# Load the Family Identification Mapping System file
# for all (biological only) parents mapped prospectively (parent to child)
# in a (balanced map)
fims_parents_prospective <- readxl::read_xlsx("data/fim9491_gidpro_BO_2_BAL.xlsx")
children_in_psid <- fims_parents_prospective %>%
  filter(!is.na(G2ID68) & G2POS == 2) %>%
  transmute(household = G1ID68) %>%
  group_by(household) %>%
  filter((1:n()) == 1) %>%
  mutate(children_in_psid = T)

grandchildren_in_psid <- fims_grandparents_prospective %>%
  filter(!is.na(G3ID68) & G3POS == 3) %>%
  transmute(household = G1ID68) %>%
  group_by(household) %>%
  filter((1:n()) == 1) %>%
  mutate(grandchildren_in_psid = T)

#############################
## 5. MAKE ONE BIG DATASET ##
#############################

data_merged <- main.psid %>%
  mutate(female = ifelse(ER32000 == 9, NA,
                         ER32000 == 2)) %>%
  select(person, src_sample, female) %>%
  filter(src_sample) %>%
  mutate(household = as.numeric(gsub("_.*","",person))) %>%
  mutate(num_sampledHouseholds = length(unique(household))) %>%
  left_join(children_in_psid, by = "household") %>%
  filter(children_in_psid) %>%
  mutate(num_withChildren = length(unique(household))) %>%
  left_join(grandchildren_in_psid, by = "household") %>%
  filter(grandchildren_in_psid) %>%
  mutate(num_withGrandchildren = length(unique(household))) %>%
  left_join(lineage_identifiers_restricted, by = "person") %>%
  filter(!is.na(cousinset) & !is.na(siblingset)) %>%
  mutate(num_withLineage = length(unique(household))) %>%
  left_join(d_beforeRestrict, by = "person") %>%
  filter(!is.na(age)) %>%
  mutate(num_observed25Plus = length(unique(household)))

# Note sample restrictions
print(data.frame(data_merged %>%
                   mutate(num_siblingset = length(unique(siblingset)),
                          num_person = length(unique(person)),
                          num_obs = n()) %>%
                   select(starts_with("num")) %>%
                   filter((1:n()) == 1)))

print("What percent of SRC observations are in 1997 or later?")
print("This is noted in PSID sample selection.")
print(round(mean(data_merged$year >= 1997),2))

d <- data_merged %>%
  select(person, siblingset, cousinset,
         year, age, female, familyIncome) %>%
  group_by() %>%
  mutate(bottomcode = 5000,
         log_familyIncome = log(ifelse(familyIncome < bottomcode, bottomcode, familyIncome))) %>%
  select(cousinset, siblingset, person,
         age,log_familyIncome) %>%
  ## Scale variables
  group_by() %>%
  mutate(age = age - min(age) + 1,
         log_familyIncome = 0.5 * (log_familyIncome - mean(log_familyIncome)) / sd(log_familyIncome)) %>%
  ## Create sequential identifiers for level-constructs
  arrange(cousinset) %>%
  group_by(cousinset) %>%
  mutate(first = 1:n() == 1) %>%
  group_by() %>%
  mutate(cousinset = cumsum(first)) %>%
  arrange(siblingset) %>%
  group_by(siblingset) %>%
  mutate(first = 1:n() == 1) %>%
  group_by() %>%
  mutate(siblingset = cumsum(first)) %>%
  group_by(person) %>%
  mutate(first = 1:n() == 1) %>%
  group_by() %>%
  mutate(person = cumsum(first)) %>%
  group_by() %>%
  select(cousinset, siblingset, person, age, log_familyIncome)

## Number with siblings
print(d %>%
        group_by(person) %>%
        filter(1:n() == 1) %>%
        group_by(siblingset) %>%
        summarize(num_people = n()) %>%
        group_by() %>%
        summarize(num_with_siblings = sum(num_people*(num_people > 1)),
                  num_without_siblings = sum(num_people*(num_people == 1))))

## Number with cousins
print(d %>% 
        group_by(person) %>%
        filter(1:n() == 1) %>%
        group_by(cousinset, siblingset) %>%
        summarize(num_in_siblingset = n()) %>%
        group_by(cousinset) %>%
        summarize(num_of_siblingset = n(),
                  num_people = sum(num_in_siblingset)) %>%
        group_by() %>%
        summarize(num_with_cousins = sum(num_people*(num_of_siblingset > 1)),
                  num_without_cousins = sum(num_people*(num_of_siblingset == 1))))

# Note observations per person
print("Number of observations per person:")
print(d %>% 
        group_by(person) %>% 
        summarize(num = n()) %>% 
        group_by() %>% 
        summarize(q1 = quantile(num, .25), 
                  mean = mean(num), 
                  q3 = quantile(num, .75)))

save(d, file = "intermediate_files/d.Rdata")


# Prepare SEO sample for robustness check
for_seo <- main.psid %>%
  mutate(female = ifelse(ER32000 == 9, NA,
                         ER32000 == 2)) %>%
  filter(familyid >= 5000 & familyid <= 7000) %>%
  select(person, src_sample, female) %>%
  mutate(household = as.numeric(gsub("_.*","",person))) %>%
  mutate(num_sampledHouseholds = length(unique(household))) %>%
  left_join(children_in_psid, by = "household") %>%
  filter(children_in_psid) %>%
  mutate(num_withChildren = length(unique(household))) %>%
  left_join(grandchildren_in_psid, by = "household") %>%
  filter(grandchildren_in_psid) %>%
  mutate(num_withGrandchildren = length(unique(household))) %>%
  left_join(lineage_identifiers_restricted, by = "person") %>%
  filter(!is.na(cousinset) & !is.na(siblingset)) %>%
  mutate(num_withLineage = length(unique(household))) %>%
  left_join(d_beforeRestrict, by = "person") %>%
  filter(!is.na(age)) %>%
  mutate(num_observed25Plus = length(unique(household)))
print("SEO sample:")
print(for_seo %>%
        filter((1:n()) == 1) %>%
        select(starts_with("num")))
d_seo <- for_seo %>%
  select(person, siblingset, cousinset,
         year, age, female, familyIncome) %>%
  group_by() %>%
  mutate(bottomcode = 5000,
         log_familyIncome = log(ifelse(familyIncome < bottomcode, bottomcode, familyIncome))) %>%
  select(cousinset, siblingset, person,
         age,log_familyIncome) %>%
  ## Scale variables
  group_by() %>%
  mutate(age = age - min(age) + 1,
         log_familyIncome = 0.5 * (log_familyIncome - mean(log_familyIncome)) / sd(log_familyIncome)) %>%
  ## Create sequential identifiers for level-constructs
  arrange(cousinset) %>%
  group_by(cousinset) %>%
  mutate(first = 1:n() == 1) %>%
  group_by() %>%
  mutate(cousinset = cumsum(first)) %>%
  arrange(siblingset) %>%
  group_by(siblingset) %>%
  mutate(first = 1:n() == 1) %>%
  group_by() %>%
  mutate(siblingset = cumsum(first)) %>%
  group_by(person) %>%
  mutate(first = 1:n() == 1) %>%
  group_by() %>%
  mutate(person = cumsum(first)) %>%
  group_by() %>%
  select(cousinset, siblingset, person, age, log_familyIncome)
save(d_seo, file = "intermediate_files/d_seo.Rdata")

print("Cousin sets in SEO:")
length(unique(d_seo$cousinset))
print("Sibling sets in SEO:")
length(unique(d_seo$siblingset))
print("Persons in SEO:")
length(unique(d_seo$person))
print("Observations in SEO:")
nrow(d_seo)

## Number with siblings
print(d_seo %>%
        group_by(person) %>%
        filter(1:n() == 1) %>%
        group_by(siblingset) %>%
        summarize(num_people = n()) %>%
        group_by() %>%
        summarize(num_with_siblings = sum(num_people*(num_people > 1)),
                  num_without_siblings = sum(num_people*(num_people == 1))))

## Number with cousins
print(d_seo %>% 
        group_by(person) %>%
        filter(1:n() == 1) %>%
        group_by(cousinset, siblingset) %>%
        summarize(num_in_siblingset = n()) %>%
        group_by(cousinset) %>%
        summarize(num_of_siblingset = n(),
                  num_people = sum(num_in_siblingset)) %>%
        group_by() %>%
        summarize(num_with_cousins = sum(num_people*(num_of_siblingset > 1)),
                  num_without_cousins = sum(num_people*(num_of_siblingset == 1))))

# Prepare sample of SRC parents to estimate sibling correlation in that generation

# For an alternate result that looks at sibling correlations in g2
lineage_identifiers_parents_restricted <- fims_grandparents_prospective %>%
  # Remove rows where the grandchild is unknown
  filter(!is.na(G3PN)) %>%
  # Restrict to those where the grandparent-parent-offspring are generations 1, 2, and 3
  filter(G1POS == 1 & G2POS == 2 & G3POS == 3) %>%
  mutate(g1_identifier = paste0(G1ID68,"_",G1PN),
         g2_identifier = paste0(G2ID68,"_",G2PN),
         g3_identifier = paste0(G3ID68,"_",G3PN)) %>%
  select(g1_identifier, g2_identifier) %>%
  group_by(g2_identifier) %>%
  arrange(g1_identifier) %>%
  filter((1:n()) == 1) %>%
  transmute(person = g2_identifier,
            siblingset = g1_identifier)

d_parents <- main.psid %>%
  mutate(female = ifelse(ER32000 == 9, NA,
                         ER32000 == 2)) %>%
  select(person, src_sample, female) %>%
  filter(src_sample) %>%
  mutate(household = as.numeric(gsub("_.*","",person))) %>%
  mutate(num_sampledHouseholds = length(unique(household))) %>%
  left_join(children_in_psid, by = "household") %>%
  filter(children_in_psid) %>%
  mutate(num_withChildren = length(unique(household))) %>%
  left_join(grandchildren_in_psid, by = "household") %>%
  filter(grandchildren_in_psid) %>%
  mutate(num_withGrandchildren = length(unique(household))) %>%
  left_join(lineage_identifiers_parents_restricted, by = "person") %>%
  filter(!is.na(siblingset)) %>%
  mutate(num_withLineage = length(unique(household))) %>%
  left_join(d_beforeRestrict, by = "person") %>%
  filter(!is.na(age)) %>%
  mutate(num_observed25Plus = length(unique(household))) %>%
  select(person, siblingset,
         year, age, female, familyIncome) %>%
  group_by() %>%
  mutate(bottomcode = 5000,
         log_familyIncome = log(ifelse(familyIncome < bottomcode, bottomcode, familyIncome))) %>%
  select(siblingset, person,
         age,log_familyIncome) %>%
  ## Scale variables
  group_by() %>%
  mutate(age = age - min(age) + 1,
         log_familyIncome = 0.5 * (log_familyIncome - mean(log_familyIncome)) / sd(log_familyIncome)) %>%
  ## Create sequential identifiers for level-constructs
  arrange(siblingset) %>%
  group_by(siblingset) %>%
  mutate(first = 1:n() == 1) %>%
  group_by() %>%
  mutate(siblingset = cumsum(first)) %>%
  group_by(person) %>%
  mutate(first = 1:n() == 1) %>%
  group_by() %>%
  mutate(person = cumsum(first)) %>%
  group_by() %>%
  select(siblingset, person, age, log_familyIncome)
save(d_parents, file = "intermediate_files/d_parents.Rdata")

# Close the sink
sink()
