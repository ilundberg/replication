
# Replication code for:
# What is Your Estimand? Defining the Target Quantity Connects Statistical Evidence to Theory
# Ian Lundberg, Rebecca Johnson, and Brandon Stewart
# Email: ilundberg@princeton.edu

# This file conducts the analysis for Specific Example 1
# Descriptive Estimands Can Be More Compelling Without Multiple Regression

# The data for this example are available from the GSS at
# https://gssdataexplorer.norc.org/
# 1. Register for an account
# 2. Put the following variables in your cart
# YEAR, WTSSNR, WTSS, BALLOT, COHORT, FAMILY16, RACE, SEX, MAEDUC, PAEDUC,
# EDUC, AGE, ID_, WTSSALL
# 3. Download the data and place it in the data subdirectory.
# You will need a .dat and a .dct file. Name them GSS.dat and GSS.dct.


library(tidyverse)
library(reshape2)
library(foreach)
library(mgcv)

sink("output/BuchmannDiPrete_output.txt")
print("Output from replication of Buchmann and DiPrete")

#################
# Load the data #
#################

library(foreign)
read.dct <- function(dct, labels.included = "yes") {
  temp <- readLines(dct)
  temp <- temp[grepl("_column", temp)]
  switch(labels.included,
         yes = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
           classes <- c("numeric", "character", "character", "numeric", "character")
           N <- 5
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
         },
         no = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
           classes <- c("numeric", "character", "character", "numeric")
           N <- 4
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
         })
  temp_metadata <- setNames(lapply(1:N, function(x) {
    out <- gsub(pattern, paste("\\", x, sep = ""), temp)
    out <- gsub("^\\s+|\\s+$", "", out)
    out <- gsub('\"', "", out, fixed = TRUE)
    class(out) <- classes[x] ; out }), NAMES)
  temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
  temp_metadata
}

read.dat <- function(dat, metadata_var, labels.included = "yes") {
  read.fwf(dat, widths = metadata_var[["ColWidth"]], col.names = metadata_var[["ColName"]])
}

GSS_metadata <- read.dct("data/GSS.dct")
GSS_ascii <- read.dat("data/GSS.dat", GSS_metadata)


attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii

####################
# Analyze the data #
####################

# Restrict data and prepare variables
d_init <- GSS %>%
  # Sample restrictions:
  
  ## Restrict to white (race == 1)
  ## 25-34 inclusive
  filter(RACE == 1 & AGE >= 25 & AGE <= 34) %>% # & YEAR >= 1972 & YEAR <= 2002) %>%
  
  # Prepare variables
  ## 97-99 tend to represent missing values so we code them to NA
  mutate(cohort = COHORT,
         college = case_when(EDUC < 98 ~ EDUC >= 16),
         age = case_when(AGE < 98 ~ AGE),
         father_educ = case_when(PAEDUC < 97 ~ PAEDUC),
         mother_educ = case_when(MAEDUC < 97 ~ MAEDUC),
         father_some_college = father_educ >= 13,
         mother_some_college = mother_educ >= 13,
         
         ## code whether respondent living w/ a father at time they 
         ## were 16
         no_father = case_when(FAMILY16 != 9 ~ !(FAMILY16 %in% c(1,2,4))),
         weight = WTSSALL,
         father_educ = ifelse(!no_father, father_educ, 0), 
         father_some_college = ifelse(!no_father, father_some_college, F),
         father_category = factor(case_when(no_father  | !father_some_college ~ 1,
                                            father_some_college ~ 2),
                                  labels = c("Father did not attend\ncollege or no father",
                                             "Father attended\ncollege")),
         mother_category = ifelse(mother_some_college, "Mother\nattended\ncollege",
                                  "Mother did\nnot attend\ncollege"),
         Gender = case_when(SEX == 1 ~ "Men",
                            SEX == 2 ~ "Women")) %>%
  select(college, cohort, father_category, mother_category, Gender, weight) %>%
  
  # Create flags for filtering
  ## Restrict so that all cohorts can be observed in all ages
  mutate(is_focal_cohort = cohort >= (1972 - 25) & cohort <= (2018 - 34))

## code any NA in rel cols
rel_cols = c("college", "cohort", "father_category",
             "mother_category", "Gender",
             "weight")

d_init["is_observed_all"] = ifelse(rowSums(is.na(d_init[, rel_cols])) == 0,
                              TRUE, FALSE)

# Note sample size changes when successively apply filters
cat(sprintf("Sample size from focusing on focal cohorts is %s, and then from
              removing those with any missingness (listwise del) is %s",
              nrow(d_init %>% filter(is_focal_cohort)),
              nrow(d_init %>% filter(is_focal_cohort & is_observed_all))))

# Apply those filters and update weight
d <- d_init %>% filter(is_focal_cohort & is_observed_all) %>%
  mutate(weight = weight/mean(weight))

# OUR MODEL
fit_gam <- gam(college ~ Gender*mother_category*father_category + s(cohort, by = interaction(Gender,mother_category,father_category)),
               family = binomial(link = "logit"),
               data = d,
               weights = weight)
fitted <- predict(fit_gam, se.fit = T)

annotations <- d %>%
  group_by(mother_category, father_category) %>%
  filter(1:n() == 1) %>%
  mutate(label = case_when(father_category == "Father attended\ncollege" &
                             mother_category == "Mother\nattended\ncollege" ~ 
                             "High rates of completion across all cohorts.\nAlways nearly equal with no reversal.",
                           father_category == "Father did not attend\ncollege or no father" &
                             mother_category == "Mother did\nnot attend\ncollege" ~
                             "In these groups, men's\ncompletion was unusually\nhigh in early cohorts."),
         cohort = 1967,
         estimate = case_when(father_category == "Father attended\ncollege" &
                                mother_category == "Mother\nattended\ncollege" ~ 
                                .5,
                              father_category == "Father did not attend\ncollege or no father" &
                                mother_category == "Mother did\nnot attend\ncollege" ~
                                .7))

d %>%
  mutate(estimate = plogis(fitted$fit),
         ci.min = plogis(fitted$fit - qnorm(.975) * fitted$se.fit),
         ci.max = plogis(fitted$fit + qnorm(.975) * fitted$se.fit)) %>%
  group_by(Gender, mother_category, father_category, cohort) %>%
  filter(1:n() == 1) %>%
  ggplot(aes(x = cohort, y = estimate)) +
  geom_ribbon(aes(color = Gender, fill = Gender,
                  ymin = ci.min, ymax = ci.max),
              color = NA,
              alpha = .4) +
  geom_line(aes(color = Gender, linetype = Gender)) +
  geom_text(data = annotations,
            aes(label = label),
            hjust = .5, size = 2, vjust = 1) +
  facet_grid(mother_category ~ father_category) +
  scale_y_continuous(name = "Probability of\nCollege Completion") +
  xlab("Cohort") +
  scale_color_manual(values = c("seagreen3","dodgerblue")) +
  scale_fill_manual(values = c("seagreen3","dodgerblue")) +
  theme_bw() +
  theme(plot.title = element_text(size = 7),
        strip.background = element_blank()) +
  ggsave("output/gss_fourPanel.pdf",
         height = 2.5, width = 6.5)


# REPLICATE THEIR RESULT
fit <- glm(college ~ binaryCohort*Gender*mother_category*father_category,
           family = binomial(link = "logit"),
           weights = weight,
           data = d %>%
             mutate(binaryCohort = cohort >= 1966 & cohort <= 1977,
                    Gender = fct_rev(Gender),
                    mother_category = fct_rev(mother_category),
                    father_category = fct_rev(father_category)))
print("Summary of entire model")
print(summary(fit))
# Focus on just that interaction term
print("Coefficient on born 1966-1977 x Men x Father < HS or Father no college")
print(summary(fit)$coefficients["binaryCohortTRUE:GenderMen:father_categoryFather did not attend\ncollege or no father",])

sink()

