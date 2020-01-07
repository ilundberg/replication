
# Code for "Comment: Summarizing income mobility with multiple smooth quantiles instead of parameterized means"

# Authors: Ian Lundberg and Brandon Stewart
# Email: ilundberg@princeton.edu

# Code will be posted on the Harvard Dataverse if the comment is accepted for publication.

# Original paper:
# Mitnik, Pablo, and David Grusky.
# "The Intergenerational Elasticity of What?
# The Case for Redefining the Workhorse Measure of Economic Mobility."
# Forthcoming in Sociological Methodology.

setwd("/users/iandl/Dropbox/mitnik_grusky_comment")

library(tidyverse)
library(scales)
library(reshape2)
library(haven)
library(foreach)
library(mgcv)
library(qgam)

sink("figures/text_output.txt")

#####################
# Analyze PSID data #
#####################

# To download these data, visit https://psidonline.isr.umich.edu/ to register for an account.
# Choose Data -> Data Center -> Previous Carts and enter the email address ilundberg@princeton.edu
# to view Ian's publicly-available carts.
# Select cart 263447. Download the file.

main.psid <- read_dta("data/J263447.dta") %>%
  mutate(familyid = ER30001, # 1968 interview number
         person_number = ER30002, # 1968 person number
         person = paste0(familyid,"_",person_number), # unique individual identifier
         sex = ER32000) %>%
  # Restrict to SRC sample
  filter(familyid <= 3000) %>%
  # Restrict to Sample Members (original sample individuals and their descendants)
  filter(person_number < 170)

# Adjust for inflation using the
# Consumer Price Index for All Urban Consumers
# https://data.bls.gov/timeseries/CUUR0000SA0
# from 1967 to 2019
cpi <- readxl::read_xlsx("data/cpi.xlsx",
                         skip = 11) %>%
  select(-HALF1,-HALF2) %>%
  melt(id = "Year") %>%
  group_by(Year) %>%
  summarize(cpi = mean(value)) %>%
  group_by() %>%
  rename(year = Year) %>%
  # Adjust to 2016 US dollars
  mutate(cpi_2016 = mean(ifelse(year == 2016, cpi, NA), na.rm = T),
         to_multiply = cpi_2016 / cpi) %>%
  select(year, to_multiply) %>%
  # Add 1 to year since the PSID data will be reported in year k for income earned in year k - 1
  mutate(year = year + 1)

# Code to turn PSID documentation into usable output
# This takes a string like "[68]ER30003 [69]ER30022"
# and returns those variables from the data in a clean form
clean <- function(string, data = main.psid, years.data = years, varName = varName) {
  # Separate the many variable names in the character string
  vector_of_variables <- strsplit(string, split = " ")[[1]]
  
  # Split each variable into its year and variable name
  df_years_varNames <- (foreach(x = vector_of_variables, .combine = "rbind") %do% {
    separated <- data.frame(t(strsplit(x, split = "]")[[1]]))
    # Convert the year to 
    return(separated)
  }) %>%
    transmute(year = as.character(X1),
              variable = as.character(X2)) %>%
    mutate(year = str_replace(year,"\\[0","200"),
           year = str_replace(year,"\\[1","201"),
           year = str_replace(year,"\\[","19"),
           year = as.numeric(year))
  
  # Produce a tidy data frame of this variable,
  # with rows identified by person and year
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

unrestricted <- clean("[68]ER30004 [69]ER30023 [70]ER30046 [71]ER30070 [72]ER30094 [73]ER30120 [74]ER30141 [75]ER30163 [76]ER30191 [77]ER30220 [78]ER30249 [79]ER30286 [80]ER30316 [81]ER30346 [82]ER30376 [83]ER30402 [84]ER30432 [85]ER30466 [86]ER30501 [87]ER30538 [88]ER30573 [89]ER30609 [90]ER30645 [91]ER30692 [92]ER30736 [93]ER30809 [94]ER33104 [95]ER33204 [96]ER33304 [97]ER33404 [99]ER33504 [01]ER33604 [03]ER33704 [05]ER33804 [07]ER33904 [09]ER34004 [11]ER34104 [13]ER34204 [15]ER34305 [17]ER34504",
                      varName = "age",
                      years = c(1968:1997,seq(1999,2017,2))) %>% 
  left_join(
    clean("[68]V81 [69]V529 [70]V1514 [71]V2226 [72]V2852 [73]V3256 [74]V3676 [75]V4154 [76]V5029 [77]V5626 [78]V6173 [79]V6766 [80]V7412 [81]V8065 [82]V8689 [83]V9375 [84]V11022 [85]V12371 [86]V13623 [87]V14670 [88]V16144 [89]V17533 [90]V18875 [91]V20175 [92]V21481 [93]V23322 [94]ER4153 [95]ER6993 [96]ER9244 [97]ER12079 [99]ER16462 [01]ER20456 [03]ER24099 [05]ER28037 [07]ER41027 [09]ER46935 [11]ER52343 [13]ER58152 [15]ER65349 [17]ER71426",
          varName = "familyIncome"),
    by = c("person", "year")
  ) %>%
  left_join(
    clean("[83]ER30404 [84]ER30434 [85]ER30468 [86]ER30503 [87]ER30540 [88]ER30575 [89]ER30611 [90]ER30647 [91]ER30694 [92]ER30738 [93]ER30811 [94]ER33106 [95]ER33206 [96]ER33306 [97]ER33406 [99]ER33506 [01]ER33606 [03]ER33706 [05]ER33806 [07]ER33906 [09]ER34006 [11]ER34106 [13]ER34206 [15]ER34307 [17]ER34506",
          varName = "birth_year",
          years = c(1983:1997,seq(1999,2017,2))),
    by = c("person","year")
  ) %>% 
  # The original authors examined birth years 1954 -- 1966.
  # We can go up to birth in 1972 because then you would be 45 in 2017.
  filter(birth_year >= 1954 & birth_year <= 1972) %>% 
  mutate(period = case_when(age >= 13 & age <= 17 ~ "parent",
                            age >= 35 & age <= 45 ~ "offspring")) %>%
  filter(!is.na(period)) %>%
  group_by(person) %>%
  filter(any(period == "parent")) %>%
  group_by() %>%
  mutate(num_observedInChildhood = n_distinct(person)) %>%
  # Adjust incomes to 2016 dollars by the CPI
  left_join(cpi,
            by = "year") %>%
  mutate(familyIncome = familyIncome * to_multiply) %>%
  # Convert to permanent income
  group_by(person, period) %>%
  summarize(familyIncome = mean(familyIncome),
            age = mean(age),
            observations = n(),
            # This mean simply aggregates since the num is constant
            num_observedInChildhood = mean(num_observedInChildhood)) %>%
  # Restrict to those observed in childhood and adulthood
  group_by(person) %>%
  filter(n() == 2) %>%
  group_by() %>%
  mutate(num_observedInAdulthood = n_distinct(person)) %>%
  melt(id = c("person","period","num_observedInChildhood","num_observedInAdulthood")) %>%
  mutate(variable = paste0(period,"_",variable)) %>%
  select(-period) %>%
  spread(key = variable, value = value) %>%
  mutate(log_parent_familyIncome = log(parent_familyIncome))

d <- unrestricted %>%
  # Remove upper tail of predictor (parent family income) %>%
  filter(parent_familyIncome <= 200000) %>%
  mutate(num_withoutUpperTailPredictor = n()) %>%
  # Remove lower tail of predictor (parent family income) %>%
  filter(parent_familyIncome >= 100) %>%
  mutate(num_withoutLowerTailPredictor = n())

print("Mean years over which parent income is averaged")
print(mean(d$parent_observations))
print("Mean years over which offspring income is averaged")
print(mean(d$offspring_observations))

print(d %>%
        filter(1:n() == 1) %>%
        select(starts_with("num")))
print("Proportion of sample in analytic range:")
print(d %>%
        filter(1:n() == 1) %>%
        transmute(prop = num_withoutLowerTailPredictor / num_observedInAdulthood))

print("Summary of parent family income (X, 2016$)")
print(summary(d$parent_familyIncome))
print("Summary of offspring family income (Y, 2016$)")
print(summary(d$offspring_familyIncome))

###########################
# Fit candidate summaries #
###########################

ige_parametric <- lm(log(offspring_familyIncome) ~ log_parent_familyIncome,
                     data = d)
ige_smooth <- gam(log(offspring_familyIncome) ~ s(parent_familyIncome),
                  data = d)
mg_parametric <- glm(offspring_familyIncome ~ log_parent_familyIncome,
                     data = d,
                     family = quasipoisson(link = "log"))
mg_smooth <- gam(offspring_familyIncome ~ s(parent_familyIncome),
                 data = d,
                 family = quasipoisson(link = "log"))
my_quantiles <- c(.1,.25,.5,.75,.9)
quantiles_smooth <- mqgam(list(offspring_familyIncome ~ s(parent_familyIncome), 
                               ~ s(parent_familyIncome)), # second term allows heteroskedasticity, see package vignette: https://cran.r-project.org/web/packages/qgam/vignettes/qgam.html#dealing-with-heteroscedasticity
                          data = d,
                          qu = my_quantiles)

convergence <- sapply(quantiles_smooth$fit, function(x) x$converged)
if (!all(convergence)) {
  stop("ERROR: Did not achieve convergence")
}

##############################
# Note predictions at median #
# parent income              #
##############################

# Note the values of the fitted quantiles at the median of the predictor
fitted <- qdo(quantiles_smooth, qu = my_quantiles, predict, newdata = data.frame(parent_familyIncome = median(d$parent_familyIncome)))
print("Median parent income (at which predictions are made):")
print(median(d$parent_familyIncome))
print("Predicted quantiles of offspring income:")
print(data.frame(quantile = my_quantiles,
                 yhat = unlist(fitted)))

############################
# Plot candidate summaries #
############################

# Get all predictions
to_predict <- data.frame(parent_familyIncome = seq(1000,200000,1000)) %>%
  mutate(log_parent_familyIncome = log(parent_familyIncome))
ige_parametric_yhat <- data.frame(predict(ige_parametric, newdata = to_predict, se = T)) %>%
  transmute(parent_familyIncome = to_predict$parent_familyIncome,
            estimate = exp(fit),
            ci.min = exp(fit - qnorm(.975) * se.fit),
            ci.max = exp(fit + qnorm(.975) * se.fit),
            Form = "Log-linear",
            Summary = "ige",
            quantile = "mean")

mg_parametric_yhat <- data.frame(predict(mg_parametric, newdata = to_predict, se = T)) %>%
  transmute(parent_familyIncome = to_predict$parent_familyIncome,
            estimate = exp(fit),
            ci.min = exp(fit - qnorm(.975) * se.fit),
            ci.max = exp(fit + qnorm(.975) * se.fit),
            Form = "Log-linear",
            Summary = "mg",
            quantile = "mean")

predicted_list <- qdo(quantiles_smooth, qu = my_quantiles, predict, newdata = to_predict, se = T)
quantiles_smooth_yhat <- foreach(i = 1:length(predicted_list), .combine = "rbind") %do% {
  data.frame(parent_familyIncome = to_predict$parent_familyIncome,
             estimate = predicted_list[[i]]$fit,
             ci.min = predicted_list[[i]]$fit - qnorm(.975) * predicted_list[[i]]$se.fit,
             ci.max = predicted_list[[i]]$fit + qnorm(.975) * predicted_list[[i]]$se.fit,
             Form = "Smooth",
             Summary = "quantiles",
             quantile = paste0(100*my_quantiles[i],"th percentile"))
}

# Fit a kernel density to add to the plot 

density_fit <- MASS::kde2d(d$parent_familyIncome,d$offspring_familyIncome,
                           lims = c(0,200000,0,350000),
                           n = 40)
colnames(density_fit$z) <- paste0("offspring_",density_fit$y)
normalized <- data.frame(density_fit$z) %>%
  mutate(parent = density_fit$x) %>%
  melt(id = "parent", variable.name = "offspring", value.name = "density") %>%
  mutate(offspring = as.numeric(gsub("offspring_","",offspring))) %>%
  mutate(density = density / max(density)) %>%
  # Normalize within parent incomes
  group_by(parent) %>%
  mutate(density = density / sum(density) * 270000) %>%
  group_by(offspring) %>%
  # Keep only 7 densities
  # so the figure is reasonably simple
  mutate(parent_number = order(parent)) %>%
  filter(parent_number %in% seq(5,35,5)) %>%
  filter(!is.na(offspring))

density_with_ends <- normalized %>%
  mutate(end = 0) %>%
  bind_rows(normalized %>%
              group_by(parent) %>%
              filter(offspring %in% c(min(offspring),max(offspring))) %>%
              mutate(density = 0) %>%
              group_by() %>%
              arrange(parent,offspring) %>%
              mutate(end = rep(c(-1,1), length(unique(parent))))) %>%
  arrange(parent,offspring,end)

parent_density <- density(d$parent_familyIncome,
                          from = 100, to = 200000)
parent_density_data <- data.frame(parent_familyIncome = parent_density$x,
                                  density = parent_density$y)

points_toPredict <- density_with_ends %>%
  filter(end == -1) %>%
  group_by() %>%
  transmute(parent_familyIncome = parent,
            log_parent_familyIncome = log(parent_familyIncome))
quantile_points <- qdo(quantiles_smooth, qu = my_quantiles, predict, newdata = points_toPredict)
names(quantile_points) <- paste0("p",c(10,25,50,75,90))
points <- points_toPredict %>%
  mutate(mg = exp(predict(mg_smooth, newdata = points_toPredict)),
         ige = exp(predict(ige_smooth, newdata = points_toPredict))) %>%
  bind_cols(data.frame(quantile_points)) %>%
  select(-log_parent_familyIncome) %>%
  melt(id = "parent_familyIncome", variable.name = "Summary",
       value.name = "offspring")

#################################
# Comparison plot of IGE vs. MG #
#################################

comparison_parametric_curves <- ige_parametric_yhat %>%
  bind_rows(mg_parametric_yhat)

comparison_parametric_points <- points %>%
  filter(Summary %in% c("ige","mg"))

ggplot() +
  geom_polygon(data = density_with_ends,
               aes(x = parent + density, y = offspring, group = parent),
               fill = "gray", alpha = .8) +
  geom_line(data = comparison_parametric_curves,
            aes(x = parent_familyIncome, y = estimate, linetype = Summary)) +
  geom_point(data = comparison_parametric_points,
             aes(x = parent_familyIncome, y = offspring, shape = Summary),
             size = 1.2) +
  geom_text(data = comparison_parametric_curves %>%
              group_by(Summary) %>%
              filter(parent_familyIncome == max(parent_familyIncome)) %>%
              mutate(parent_familyIncome = parent_familyIncome + 5000,
                     label = case_when(Summary == "ige" ~ "Geometric\nMean",
                                       Summary == "mg" ~ "Arithmetic\nMean")),
            aes(x = parent_familyIncome, y = estimate,
                label = label, vjust = case_when(label == "Arithmetic\nMean" ~ .25,
                                                 label == "Geometric\nMean" ~ .75,
                                                 T ~ .5)),
            hjust = 0, size = 2) +
  geom_point(data = comparison_parametric_curves %>%
               group_by(Summary) %>%
               filter(parent_familyIncome == max(parent_familyIncome)) %>%
               mutate(parent_familyIncome = parent_familyIncome + 28000),
             aes(x = parent_familyIncome, y = estimate - case_when(Summary == "ige" ~ 19000,
                                                                   Summary == "mg" ~ 4000),
                 shape = Summary)) +
  scale_linetype_manual(values = c("solid","dashed")) +
  scale_shape_manual(values = c(3,16)) +
  scale_x_continuous(labels = function(x) paste0("$",x / 1000,"k"),
                     name = "Predictor: Parent Income",
                     breaks = seq(0,200000,100000),
                     limits = c(0,250000)) +
  scale_y_continuous(labels = function(x) paste0("$",x / 1000,"k"),
                     name = "Outcome: Offspring Income",
                     breaks = seq(0,300000,100000)) +
  geom_text(data = data.frame(x = 205000, y = c(250000,40000),
                              label = c("Mitnik and\nGrusky\nProposal",
                                        "Classic\nIntergenerational\nElasticity")),
            aes(x = x, y = y, label = label),
            size = 1.7, hjust = 0) +
  geom_segment(data = data.frame(x = 215000, y = c(220000,70000),
                                 yend = c(200000, 90000),
                                 Summary_collapsed = "A) Previous Proposals"),
               aes(x = x, xend = x, y = y, yend = yend),
               arrow = arrow(length = unit(.1,"cm"))) +
  # Add density at the bottom
  geom_hline(yintercept = 0) +
  geom_ribbon(data = parent_density_data,
              aes(x = parent_familyIncome,
                  ymin = -100000,
                  ymax = -100000 + 80000 * (density / max(density))),
              fill = "gray", alpha = .8) +
  annotate(geom = "text", x = 90000, y = -94000,
           label = "Parent\nincome density",
           hjust = 0, vjust = 0, size = 2) + #, color = "white", fontface = "bold") +
  annotate(geom = "text", x = 75000, y = -60000,
           label = "Median",
           hjust = 0.5, vjust = 0, angle = 90, size = 2) +
  geom_vline(xintercept = median(d$parent_familyIncome), linetype = "dotted") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(hjust = .8),
        strip.text = element_text(face = "bold", hjust = 0),
        strip.background = element_blank()) +
  ggsave("figures/comparing_previous_proposals.pdf",
         height = 3, width = 4)

########################
# Plot of our proposal #
########################

our_proposal_points <- points %>%
  filter(!(Summary %in% c("ige","mg")))
ggplot() +
  geom_polygon(data = density_with_ends,
               aes(x = parent + density, y = offspring, group = parent),
               fill = "gray", alpha = .8) +
  geom_line(data = quantiles_smooth_yhat,
            aes(x = parent_familyIncome, y = estimate, group = quantile)) +
  geom_point(data = our_proposal_data,
             aes(x = parent_familyIncome, y = offspring),
             size = 1.2) +
  geom_text(data = quantiles_smooth_yhat %>%
              group_by(Summary) %>%
              filter(parent_familyIncome == max(parent_familyIncome)) %>%
              mutate(parent_familyIncome = parent_familyIncome + 5000),
            aes(x = parent_familyIncome, y = estimate,
                label = quantile),
            hjust = 0, size = 2) +
  scale_x_continuous(labels = function(x) paste0("$",x / 1000,"k"),
                     name = "Predictor: Parent Income",
                     breaks = seq(0,200000,100000),
                     limits = c(0,250000)) +
  scale_y_continuous(labels = function(x) paste0("$",x / 1000,"k"),
                     name = "Outcome: Offspring Income",
                     breaks = seq(0,300000,100000)) +
  # Add density at the bottom
  geom_hline(yintercept = 0) +
  geom_ribbon(data = parent_density_data,
              aes(x = parent_familyIncome,
                  ymin = -100000,
                  ymax = -100000 + 80000 * (density / max(density))),
              fill = "gray", alpha = .8) +
  annotate(geom = "text", x = 90000, y = -94000,
           label = "Parent\nincome density",
           hjust = 0, vjust = 0, size = 2) +
  annotate(geom = "text", x = 75000, y = -60000,
           label = "Median",
           hjust = 0.5, vjust = 0, angle = 90, size = 2) +
  geom_vline(xintercept = median(d$parent_familyIncome), linetype = "dotted") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(hjust = .8),
        strip.text = element_text(face = "bold", hjust = 0),
        strip.background = element_blank()) +
  ggsave("figures/our_proposal.pdf",
         height = 3, width = 4)

# Note that first differences can provide one-number summaries of smooth curves
print("Marginal increase in offspring income associated with a $1,000 increase in parent income")
fitted_as_observed <- qdo(quantiles_smooth, qu = my_quantiles, predict, 
                          newdata = d)
fitted_10k_higher <- qdo(quantiles_smooth, qu = my_quantiles, predict, 
                         newdata = d %>% mutate(parent_familyIncome = parent_familyIncome + 10000))
print("Change in median")
print(mean(fitted_10k_higher[[which(my_quantiles == .5)]] -
             fitted_as_observed[[which(my_quantiles == .5)]]))
print("Change in 90th percentile")
print(mean(fitted_10k_higher[[which(my_quantiles == .9)]] -
             fitted_as_observed[[which(my_quantiles == .9)]]))

##############################################################
# Appendix: Comparison plot with unrestricted parent incomes #
##############################################################

ige_parametric_unrestricted <- lm(log(offspring_familyIncome) ~ log_parent_familyIncome,
                                  data = unrestricted)
ige_smooth_unrestricted <- gam(log(offspring_familyIncome) ~ s(parent_familyIncome),
                               data = unrestricted)
mg_parametric_unrestricted <- glm(offspring_familyIncome ~ log_parent_familyIncome,
                                  data = unrestricted,
                                  family = quasipoisson(link = "log"))
mg_smooth_unrestricted <- gam(offspring_familyIncome ~ s(parent_familyIncome),
                              data = unrestricted,
                              family = quasipoisson(link = "log"))
ige_parametric_yhat_unrestricted <- data.frame(predict(ige_parametric_unrestricted, newdata = to_predict, se = T)) %>%
  transmute(parent_familyIncome = to_predict$parent_familyIncome,
            estimate = exp(fit),
            ci.min = exp(fit - qnorm(.975) * se.fit),
            ci.max = exp(fit + qnorm(.975) * se.fit),
            Form = "Log-linear",
            Summary = "ige",
            quantile = "mean")
mg_parametric_yhat_unrestricted <- data.frame(predict(mg_parametric_unrestricted, newdata = to_predict, se = T)) %>%
  transmute(parent_familyIncome = to_predict$parent_familyIncome,
            estimate = exp(fit),
            ci.min = exp(fit - qnorm(.975) * se.fit),
            ci.max = exp(fit + qnorm(.975) * se.fit),
            Form = "Log-linear",
            Summary = "mg",
            quantile = "mean")
comparison_parametric_points_unrestricted <- data.frame(as.matrix(
  points_toPredict %>%
    mutate(mg = exp(predict(mg_smooth_unrestricted, newdata = points_toPredict)),
           ige = exp(predict(ige_smooth_unrestricted, newdata = points_toPredict))) %>%
    select(-log_parent_familyIncome)
)) %>%
  melt(id = "parent_familyIncome", variable.name = "Summary",
       value.name = "offspring") %>%
  mutate(parent_familyIncome = as.numeric(parent_familyIncome))

comparison_parametric_curves_unrestricted <- ige_parametric_yhat_unrestricted %>%
  bind_rows(mg_parametric_yhat_unrestricted)

ggplot() +
  geom_polygon(data = density_with_ends,
               aes(x = parent + density, y = offspring, group = parent),
               fill = "gray", alpha = .8) +
  geom_line(data = comparison_parametric_curves_unrestricted,
            aes(x = parent_familyIncome, y = estimate, linetype = Summary)) +
  geom_point(data = comparison_parametric_points_unrestricted,
             aes(x = parent_familyIncome, y = offspring, shape = Summary),
             size = 1.2) +
  geom_text(data = comparison_parametric_curves_unrestricted %>%
              group_by(Summary) %>%
              filter(parent_familyIncome == max(parent_familyIncome)) %>%
              mutate(parent_familyIncome = parent_familyIncome + 5000,
                     label = case_when(Summary == "ige" ~ "Geometric\nMean",
                                       Summary == "mg" ~ "Arithmetic\nMean")),
            aes(x = parent_familyIncome, y = estimate,
                label = label, vjust = case_when(label == "Arithmetic\nMean" ~ .25,
                                                 label == "Geometric\nMean" ~ .75,
                                                 T ~ .5)),
            hjust = 0, size = 2) +
  geom_point(data = comparison_parametric_curves_unrestricted %>%
               group_by(Summary) %>%
               filter(parent_familyIncome == max(parent_familyIncome)) %>%
               mutate(parent_familyIncome = parent_familyIncome + 28000),
             aes(x = parent_familyIncome, y = estimate - case_when(Summary == "ige" ~ 19000,
                                                                   Summary == "mg" ~ 4000),
                 shape = Summary)) +
  scale_linetype_manual(values = c("solid","dashed")) +
  scale_shape_manual(values = c(3,16)) +
  scale_x_continuous(labels = function(x) paste0("$",x / 1000,"k"),
                     name = "Predictor: Parent Income",
                     breaks = seq(0,200000,100000),
                     limits = c(0,250000)) +
  scale_y_continuous(labels = function(x) paste0("$",x / 1000,"k"),
                     name = "Outcome: Offspring Income",
                     breaks = seq(0,300000,100000)) +
  geom_text(data = data.frame(x = 205000, y = c(250000,40000),
                              label = c("Mitnik and\nGrusky\nProposal",
                                        "Classic\nIntergenerational\nElasticity")),
            aes(x = x, y = y, label = label),
            size = 1.7, hjust = 0) +
  geom_segment(data = data.frame(x = 215000, y = c(220000,70000),
                                 yend = c(200000, 90000),
                                 Summary_collapsed = "A) Previous Proposals"),
               aes(x = x, xend = x, y = y, yend = yend),
               arrow = arrow(length = unit(.1,"cm"))) +
  # Add density at the bottom
  geom_hline(yintercept = 0) +
  geom_ribbon(data = parent_density_data,
              aes(x = parent_familyIncome,
                  ymin = -100000,
                  ymax = -100000 + 80000 * (density / max(density))),
              fill = "gray", alpha = .8) +
  annotate(geom = "text", x = 90000, y = -94000,
           label = "Parent\nincome density",
           hjust = 0, vjust = 0, size = 2) + #, color = "white", fontface = "bold") +
  annotate(geom = "text", x = 75000, y = -60000,
           label = "Median",
           hjust = 0.5, vjust = 0, angle = 90, size = 2) +
  geom_vline(xintercept = median(d$parent_familyIncome), linetype = "dotted") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(hjust = .8),
        strip.text = element_text(face = "bold", hjust = 0),
        strip.background = element_blank()) +
  ggsave("figures/comparing_previous_proposals_unrestricted.pdf",
         height = 3, width = 4)
 
####################################
# Appendix: Plot utility functions #
####################################

data.frame(y = c(0,.1,.5,.75,1,500,seq(1000,100000,1000))) %>%
  mutate(Linear = y,
         log = log(y),
         ihs = log(y + sqrt(y ^ 2 + 1)),
         log_plus_5000 = log(y + 5000)) %>%
  melt(id = "y") %>%
  mutate(variable = case_when(variable == "log" ~ "A. Log",
                              variable == "Linear" ~ "B. Linear",
                              variable == "ihs" ~ "C. Inverse hyperbolic sine",
                              variable == "log_plus_5000" ~ "D. Log(Income + $5,000)")) %>%
  ggplot(aes(x = y, y = value, group = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free", nrow = 1) +
  scale_y_continuous(name = "Utility") +
  scale_x_continuous(name = "Income (thousands of dollars)",
                     labels = function(x) x / 1000) +
  geom_segment(data = data.frame(variable = rep("A. Log",3)),
               aes(x = c(30000,30000,20000),
                   xend = c(3000,3000,3000),
                   y = c(6,6,-.5),
                   yend = c(5,2,-2.5)),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_label(data = data.frame(variable = "A. Log"),
             aes(x = 60000, y = 6, label = "Very sensitive to\nlow incomes"),
             size = 2.5) +
  geom_label(data = data.frame(variable = "A. Log"),
             aes(x = 60000, y = 0, label = "Goes to negative\ninfinity when income = 0"),
             size = 2.5) +
  geom_segment(data = data.frame(variable = rep("B. Linear",2)),
               aes(x = c(50000,23000),
                   xend = c(75000,23000),
                   y = c(80000,80000),
                   yend = c(80000,30000)),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_label(data = data.frame(variable = "B. Linear"),
             aes(x = 30000, y = 80000, label = "More income\nequally valuable\nat top and bottom"),
             size = 2.5) +
  geom_segment(data = data.frame(variable = "C. Inverse hyperbolic sine"),
               aes(x = 35000,
                   xend = 5000,
                   y = 3,
                   yend = .5),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_label(data = data.frame(variable = "C. Inverse hyperbolic sine"),
             aes(x = 60000, y = 3, label = "Like log, but\nwell-behaved\nwhen income = 0"),
             size = 2.5) +
  geom_label(data = data.frame(variable = "D. Log(Income + $5,000)"),
             aes(x = 60000, y = 9.5, label = "Concavity between\nlinear and log"),
             size = 2.5) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 8)) +
  ggsave("figures/utility_functions.pdf",
         height = 2.5, width = 6.5)

sink()
