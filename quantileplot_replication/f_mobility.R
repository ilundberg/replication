
# Replication code for
# "Smooth quantile visualizations enhance understanding of bivariate population distributions"
# Robin C. Lee, Ian Lundberg, and Brandon M. Stewart

# This file: Conducts the mortality example
# Prerequisite: Run a_prepare_environment.R

# Note: The first part taken of this code is taken from
# comment_summarizing_income_mobility.R
# which was written for Lundberg and Stewart (2021).
# Paper: https://doi.org/10.1177%2F0081175020931126
# Replication package: https://doi.org/10.7910/DVN/JERJ0C

#####################
# Analyze PSID data #
#####################

# To download these data, visit https://psidonline.isr.umich.edu/ to register for an account.
# Choose Data -> Data Center -> Previous Carts and enter the email address ilundberg@princeton.edu
# to view Ian's publicly-available carts.
# Select cart 263447. Download the file.

# The PSID does not support R directly. You can download the data file and
# use the code provided to load it in Stata, then save the .dta file.
# This code begins assuming you have already converted to a .dta file.

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

# Prepare an unrestricted data frame
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
            num_observedInChildhood = mean(num_observedInChildhood),
            .groups = "drop") %>%
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

# Print sample size notes
print(d %>% select(starts_with("num")) %>% filter(1:n() == 1))

# Produce the mobility point estimate plot
mobility <- quantileplot(offspring_familyIncome ~ s(log(parent_familyIncome)),
                         data = d,
                         x_range = c(500,200000),
                         y_range = c(0,350000),
                         xlab = "Family Income Experienced in Childhood",
                         ylab = "Family Income Attained as an Adult",
                         x_break_labeller = function(x) paste0("$",x / 1000,"k"),
                         y_break_labeller = function(x) paste0("$",x / 1000,"k"),
                         quantile_notation = "legend")

pdf("figures/mobility.pdf",
    height = 4.5, width = 6.5)
plot(mobility)
dev.off()

# Compare to the log-log plot
log_log_plot <- quantileplot(log_offspring_familyIncome ~ s(log_parent_familyIncome),
                             data = d %>% mutate(log_offspring_familyIncome = log(offspring_familyIncome)),
                             x_range = log(c(10000,200000)),
                             y_range = log(c(10000,350000)),
                             xlab = "Family Income Experienced in Childhood\n(Log Scale)",
                             ylab = "Family Income Attained as an Adult\n(Log Scale)",
                             quantile_notation = "legend",
                             y_bw = .25)

print("When customizing the plot, we are changing layers that are already present. Warnings about this are to be expected.")
log_log_plot$plot +
  scale_x_continuous(breaks = log(c(5000,20000,40000,80000,160000)),
                     labels = function(x) paste0("$",round(exp(x) / 1000),"k")) +
  scale_y_continuous(breaks = log(c(20000,80000,320000)),
                     labels = function(x) paste0("$",round(exp(x) / 1000),"k")) +
  ggsave("figures/mobility_log_log.pdf",
         height = 4.5, width = 6.5)

# Produce both plots with uncertainty.
# Call with the same arguments as before, but add new uncertainty arguments
log_log_uncertainty <- do.call(
  quantileplot,
  c(list(show_ci = T, uncertainty_draws = 10),
    log_log_plot$arguments[!(names(log_log_plot$arguments) %in% c("show_ci","uncertainty_draws"))])
)
mobility_uncertainty <- do.call(
  quantileplot,
  c(list(show_ci = T, uncertainty_draws = 10),
    mobility$arguments[!(names(mobility$arguments) %in% c("show_ci","uncertainty_draws"))])
)

# Customize axis labels in the log-log plot
print("When customizing the plot, we are changing layers that are already present. Warnings about this are to be expected.")
log_log_plot_uncertainty$plot <- log_log_plot_uncertainty$plot +
  scale_x_continuous(breaks = log(c(12500,50000,200000)),
                     labels = function(x) paste0("$",exp(x) / 1000,"k")) +
  scale_y_continuous(breaks = log(c(12500,50000,200000)),
                     labels = function(x) paste0("$",exp(x) / 1000,"k"))
log_log_plot_uncertainty$sim_curve_plots <- lapply(log_log_plot_uncertainty$sim_curve_plots, function(p) {
  p +
    scale_x_continuous(breaks = log(c(12500,50000,200000)),
                       labels = function(x) paste0("$",exp(x) / 1000,"k")) +
    scale_y_continuous(breaks = log(c(12500,50000,200000)),
                       labels = function(x) paste0("$",exp(x) / 1000,"k"))
})

# Save the uncertainty figures
pdf("figures/mobility_uncertainty.pdf",
    height = 6.5, width = 6.5)
plot(mobility_uncertainty, bottom_xlab_angle = 45)
dev.off()
pdf("figures/log_log_plot_uncertainty.pdf",
    height = 6.5, width = 6.5)
plot(log_log_plot_uncertainty, bottom_xlab_angle = 45)
dev.off()

# Make a basic scatter version of each
d %>%
  ggplot(aes(x = parent_familyIncome, y = offspring_familyIncome)) +
  geom_point(size = .5) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0,200000,50000),
                     labels = function(x) paste0("$",x / 1000,"k"),
                     limits = c(0,200000)) +
  scale_y_continuous(breaks = seq(0,300000,100000),
                     labels = function(x) paste0("$",x / 1000,"k"),
                     limits = c(0,350000)) +
  geom_smooth(method = "lm", se = F) +
  xlab(mobility$call$xlab) +
  ylab(mobility$call$ylab) +
  ggsave("figures/mobility_scatter.pdf",
         height = 4.55, width = 5.5)
d %>%
  ggplot(aes(x = parent_familyIncome, y = offspring_familyIncome)) +
  geom_point(size = .5) +
  theme_bw() +
  scale_x_continuous(breaks = 20000 * 2 ^ (0:3),
                     labels = function(x) paste0("$",round(x / 1000),"k"),
                     limits = exp(log_log_plot$x_range),
                     trans = "log") +
  scale_y_continuous(breaks = 20000 * 4 ^ (0:2),
                     labels = function(x) paste0("$",round(x / 1000),"k"),
                     limits = exp(log_log_plot$y_range),
                     trans = "log") +
  geom_smooth(method = "lm", se = F) +
  xlab(log_log_plot$call$xlab) +
  ylab(log_log_plot$call$ylab) +
  ggsave("figures/log_log_scatter.pdf",
         height = 4.55, width = 5.5)
