
# Code for a comment prepared for Sociological Methodology

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

sink("figures/text_output.txt")

# Plot utility functions
data.frame(y = c(0,.1,.5,.75,1,500,seq(1000,100000,1000))) %>%
  mutate(Linear = y,
         log = log(y),
         mine = ifelse(y < 10000, log(10000) - 1 + y / 10000, log(y)),
         sqrt = sqrt(y),
         log_plus_.01 = log(y + .01),
         log_plus_1 = log(y + 1),
         log_plus_100 = log(y + 100),
         log_plus_10000 = log(y + 10000)) %>%
  melt(id = "y") %>%
  mutate(variable = case_when(variable == "log" ~ "A. Log",
                              variable == "Linear" ~ "B. Linear",
                              variable == "sqrt" ~ "C. Square root",
                              variable == "mine" ~ "D. Linear + log",
                              variable == "log_plus_.01" ~ "E. Log(Income + $0.01)",
                              variable == "log_plus_1" ~ "F. Log(Income + $1)",
                              variable == "log_plus_100" ~ "G. Log(Income + $100)",
                              variable == "log_plus_10000" ~ "H. Log(Income + $10k)")) %>%
  ggplot(aes(x = y, y = value, group = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free", nrow = 2) +
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
  geom_point(data = data.frame(variable = "D. Linear + log"),
             aes(x = 10000, y = log(10000))) +
  geom_segment(data = data.frame(variable = rep("D. Linear + log",2)),
               aes(x = c(50000,61000),
                   xend = c(12000,45000),
                   y = c(8.5,10.1),
                   yend = c(8.5,10.5)),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_label(data = data.frame(variable = rep("D. Linear + log",2)),
             aes(x = c(50000,75000), y = c(8.5,10), 
                 label = c("Linear up to\na point",
                           "Log after\nthat point")),
             size = 2.5) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  ggsave("figures/utility_functions_withEpsilon.pdf",
         height = 4, width = 6.5)

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

# Define a function that we will use to transform income in some analyses. Also define its inverse.
my_transformation <- function(x) log(x + 5000)
my_inverse <- function(x) exp(x) - 5000

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

d <- clean("[68]ER30004 [69]ER30023 [70]ER30046 [71]ER30070 [72]ER30094 [73]ER30120 [74]ER30141 [75]ER30163 [76]ER30191 [77]ER30220 [78]ER30249 [79]ER30286 [80]ER30316 [81]ER30346 [82]ER30376 [83]ER30402 [84]ER30432 [85]ER30466 [86]ER30501 [87]ER30538 [88]ER30573 [89]ER30609 [90]ER30645 [91]ER30692 [92]ER30736 [93]ER30809 [94]ER33104 [95]ER33204 [96]ER33304 [97]ER33404 [99]ER33504 [01]ER33604 [03]ER33704 [05]ER33804 [07]ER33904 [09]ER34004 [11]ER34104 [13]ER34204 [15]ER34305 [17]ER34504",
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
  # Adjust incomes to 2016 dollars by the CPI
  left_join(cpi,
            by = "year") %>%
  mutate(familyIncome = familyIncome * to_multiply) %>%
  # Convert to permanent income
  group_by(person, period) %>%
  summarize(familyIncome = mean(familyIncome),
            age = mean(age),
            observations = n()) %>%
  # Restrict to those observed in childhood and adulthood
  group_by(person) %>%
  filter(n() == 2) %>%
  group_by() %>%
  melt(id = c("person","period")) %>%
  mutate(variable = paste0(period,"_",variable)) %>%
  select(-period) %>%
  spread(key = variable, value = value) %>%
  mutate(parent_familyIncome_group = ntile(parent_familyIncome,4),
         parent_familyIncome_group = factor(parent_familyIncome_group,
                                               labels = c("1st quartile\nof parent income (X)",
                                                          "2nd quartile\nof parent income (X)",
                                                          "3rd quartile\nof parent income (X)",
                                                          "4th quartile\nof parent income (X)")),
         log_parent_familyIncome = log(parent_familyIncome),
         transformed_childhood = my_transformation(parent_familyIncome),
         transformed_adulthood = my_transformation(offspring_familyIncome))

print("Sample size:")
print(nrow(d))
print("Summary of parent family income (X, 2016$)")
print(summary(d$parent_familyIncome))
print("Summary of offspring family income (Y, 2016$)")
print(summary(d$offspring_familyIncome))

summary_measures <- d %>%
  group_by(parent_familyIncome_group) %>%
  summarize(mean = mean(offspring_familyIncome),
            exp_mean_log = exp(mean(log(offspring_familyIncome))),
            median = median(offspring_familyIncome)) %>%
  melt(id = "parent_familyIncome_group",
       variable.name = "measure",
       value.name = "estimate") %>%
  mutate(measure = case_when(measure == "mean" ~ "Mean",
                             measure == "median" ~ "Median",
                             measure == "exp_mean_log" ~ "Exponentiated mean of log"),
         measure = fct_relevel(measure,"Exponentiated mean of log","Median","Mean"))

d %>%
  ggplot(aes(x = offspring_familyIncome)) +
  geom_density() +
  geom_vline(data = summary_measures,
             aes(xintercept = estimate, color = measure),
             size = .5) +
  # Labels on vertical lines
  geom_text(data = summary_measures %>%
              group_by(parent_familyIncome_group) %>%
              arrange(estimate) %>%
              mutate(index = 1:n()) %>%
              mutate(x_position = estimate + case_when(
                1:n() == 1 ~ -1000,
                1:n() == 2 ~ 1000,
                1:n() == 3 ~ 1000
              )) %>%
              mutate(measure_label = ifelse(measure == "Exponentiated mean of log",
                                            "Exponentiated\nmean of log", as.character(measure))),
            aes(x = x_position, y = 5e-7, label = measure_label, color = measure,
                vjust = ifelse(index == 1, 0, 1)),
            angle = 90, hjust = 0, size = 2, lineheight = .7,
            show.legend = F) +
  # Labels on facets
  geom_text(data = d %>%
              group_by(parent_familyIncome_group) %>%
              filter(1:n() == 1) %>%
              mutate(offspring_familyIncome = 250000),
            aes(label = paste0("Subgroup: ",parent_familyIncome_group)),
            y = .85e-5,
            size = 3,
            hjust = 1) +
  # Note about high-income folks
  geom_text(data = d %>%
              group_by(parent_familyIncome_group) %>%
              summarize(num_omitted = sum(offspring_familyIncome > 250000)) %>%
              mutate(label = paste0("+ ",num_omitted,ifelse(num_omitted == 1," family"," families"),
                                   "\nbeyond axis")) %>%
              mutate(offspring_familyIncome = 250000),
            aes(label = label),
            y = 4e-6,
            hjust = 1, size = 2) +
  annotate(geom = "segment", x = 230000, xend = 250000,
           y = 2e-6, yend = 2e-6,
           arrow = arrow(length = unit(.1,"cm"))) +
  facet_wrap(~parent_familyIncome_group, ncol = 1) +
  scale_color_manual(name = element_blank(),
                     values = c("blue","black","seagreen4")) +
  scale_linetype_manual(name = element_blank(),
                        values = c("solid","dashed","dotdash")) +
  scale_y_continuous(name = "Density") +
  scale_x_continuous(name = "Offspring family income (Y)\n(thousands of 2016 dollars)",
                     labels = function(x) x / 1000,
                     limits = c(0,250000)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank()) +
  ggsave("figures/densities_dollar_scale.pdf",
         height = 5.5, width = 6.5)

print("Table of those excluded from the figure for high incomes:")
print(table(d$parent_familyIncome_group,d$offspring_familyIncome > 250000))


# Quantile plot
my_quantiles <- c(.1,.25,.5,.75,.9)
to_predict <- data.frame(parent_familyIncome = matrix(seq(0,250000,1000))) %>%
  mutate(transformed_childhood = my_transformation(parent_familyIncome))

library(qgam)

fit_transformed <- mqgam(list(transformed_adulthood ~ s(transformed_childhood), 
                              ~ s(transformed_childhood)), # second term allows heteroskedasticity, see package vignette: https://cran.r-project.org/web/packages/qgam/vignettes/qgam.html#dealing-with-heteroscedasticity
                         data = d,
                         qu = my_quantiles)
predicted_list <- qdo(fit_transformed, qu = my_quantiles, predict, newdata = to_predict, se = T)
predicted_df <- foreach(i = 1:length(predicted_list), .combine = "rbind") %do% {
  data.frame(quantile = my_quantiles[i],
             x = to_predict$parent_familyIncome,
             y = my_inverse(predicted_list[[i]]$fit),
             ci.min = my_inverse(predicted_list[[i]]$fit - qnorm(.975) * predicted_list[[i]]$se.fit),
             ci.max = my_inverse(predicted_list[[i]]$fit + qnorm(.975) * predicted_list[[i]]$se.fit))
} %>%
  filter(x <= quantile(d$parent_familyIncome, .95))

dollars_density <- density(d$parent_familyIncome, from = 0)
dollars_density <- data.frame(x = dollars_density$x,
                               y = dollars_density$y) %>%
  mutate(y = - y / max(y) * 80000) %>%
  filter(x <= quantile(d$parent_familyIncome, .95))

predicted_df %>%
  ggplot() +
  geom_ribbon(aes(x = x, ymin = ci.min, ymax = ci.max,
                  group = quantile),
              alpha = .3, color = NA) +
  geom_line(aes(x = x, y = y, group = quantile)) +
  geom_ribbon(data = dollars_density, 
              aes(x = x, ymax = 0, ymin = y),
              fill = "darkgray") +
  theme_bw() +
  geom_vline(xintercept = median(d$parent_familyIncome),
             linetype = "dashed") +
  annotate(geom = "text",
           x = 70000,
           y = -25000,
           label = "Parent income\ndensity",
           color = "white",
           fontface = "bold",
           hjust = 1,
           size = 2.5) +
  annotate(geom = "text",
           x = median(d$parent_familyIncome),
           y = -35000,
           label = "Median",
           angle = 270, vjust = -.5, color = "white",
           fontface = "bold",
           size = 2.4) +
  annotate(geom = "text",
           x = quantile(d$parent_familyIncome, .95),
           y = -25000,
           label = "Upper 5%\nomitted in plot",
           hjust = 1, vjust = 1.3,
           size = 2.5) +
  annotate(geom = "segment",
           x = 155000, xend = quantile(d$parent_familyIncome, .95),
           y = -25000, yend = -25000,
           arrow = arrow(length = unit(.1,"cm"))) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(labels = function(x) paste0("$",x / 1000,"k"),
                     name = "Parent income",
                     limits = c(0,230000)) +
  scale_y_continuous(labels = function(x) paste0("$",x / 1000,"k"),
                     name = "Offspring income",
                     breaks = seq(0,300000,100000)) +
  geom_text(data = predicted_df %>%
              group_by(quantile) %>%
              arrange(-x) %>%
              filter(1:n() == 1),
            aes(x = quantile(d$parent_familyIncome,.95), 
                y = y,
                label = paste0(paste0(100*quantile),"th percentile")),
            hjust = 0, size = 2.5) +
  theme(panel.grid.minor.y = element_blank()) +
  ggsave("figures/quantile_plot.pdf",
         height = 3.2, width = 4)

# Note the values of the fitted quantiles at the median of the predictor
fitted <- qdo(fit_transformed, qu = my_quantiles, predict,
              newdata = data.frame(transformed_childhood = my_transformation(median(d$parent_familyIncome))))
print("Median parent income (at which predictions are made):")
print(median(d$parent_familyIncome))
print("Predicted quantiles of offspring income:")
print(data.frame(quantile = my_quantiles,
                 yhat = sapply(fitted, my_inverse)))


print("Marginal increase in offspring income associated with a $1,000 increase in parent income")
fitted_as_observed <- qdo(fit_transformed, qu = my_quantiles, predict, 
                          newdata = d)
fitted_1k_higher <- qdo(fit_transformed, qu = my_quantiles, predict, 
                        newdata = d %>% mutate(transformed_childhood = my_transformation(parent_familyIncome + 1000)))
print("Change in median")
print(mean(my_inverse(fitted_1k_higher[[which(my_quantiles == .5)]]) -
             my_inverse(fitted_as_observed[[which(my_quantiles == .5)]])))
print("Change in 90th percentile")
print(mean(my_inverse(fitted_1k_higher[[which(my_quantiles == .9)]]) -
             my_inverse(fitted_as_observed[[which(my_quantiles == .9)]])))

# Show the density of parent incomes
d %>%
  ggplot(aes(x = parent_familyIncome)) +
  theme_bw() +
  annotate(geom = "rect",
           ymin = -Inf, ymax = Inf,
           xmin = quantile(d$parent_familyIncome,.95),
           xmax = Inf,
           color = NA, fill = "darkgray") +
  geom_density() +
  annotate(geom = "text",
           label = "Plot at left\nomits upper\n5% of parent\nincome due to\nsparse data",
           x = quantile(d$parent_familyIncome, .98),
           hjust = 0,
           y = 5e-6, size = 2, color = "white", fontface = "bold") +
  scale_y_continuous(name = "Density",
                     breaks = NULL) +
  scale_x_continuous(labels = function(x) paste0("$",x / 1000,"k"),
                     name = "Parent income",
                     breaks = c(0,200000,400000,800000)) +
  theme(plot.margin = unit(c(5.5, 8, 5.5, 5.5), "points"),
        panel.grid.minor.x = element_blank()) +
  ggsave("figures/parent_income_density.pdf",
         height = 1.6, width = 2.6)

# Show the density of transformed parent incomes
d %>%
  ggplot(aes(x = parent_familyIncome)) +
  theme_bw() +
  annotate(geom = "rect",
           ymin = -Inf, ymax = Inf,
           xmin = quantile(d$parent_familyIncome,.95),
           xmax = Inf,
           color = NA, fill = "darkgray") +
  geom_density() +
  annotate(geom = "text",
           label = "Plot at left\nomits upper\n5% of parent\nincome due to\nsparse data",
           x = quantile(d$parent_familyIncome, .96),
           hjust = 0,
           y = .4, size = 2, color = "white", fontface = "bold") +
  scale_y_continuous(name = "Density",
                     breaks = NULL) +
  scale_x_continuous(labels = function(x) paste0("$",x / 1000,"k"),
                     name = "Transformed parent income",
                     breaks = c(2000,10000,50000,250000),
                     trans = trans_new(name = "my_transformation",
                                       transform = my_transformation,
                                       inverse = my_inverse)) +
  theme(plot.margin = unit(c(5.5, 8, 5.5, 5.5), "points"),
        panel.grid.minor.x = element_blank()) +
  ggsave("figures/parent_income_transformed_density.pdf",
         height = 1.6, width = 2.5)

# Calculate the IGE and IGEE for comparison
ige <- coef(lm(log(offspring_familyIncome) ~ I(log(parent_familyIncome)),
               data = d))[2]
igee <- coef(glm(offspring_familyIncome ~ I(log(parent_familyIncome)),
                 data = d,
                 family = quasipoisson(link = "log")))[2]
print(c(IGE = ige, IGEE = igee))

sink()
