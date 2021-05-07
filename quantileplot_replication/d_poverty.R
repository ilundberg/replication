
# Replication code for
# "Smooth quantile visualizations enhance understanding of bivariate population distributions"
# Robin C. Lee, Ian Lundberg, and Brandon M. Stewart

# This file: Conducts the spatial inequality example with poverty over Census tracts
# Prerequisite: Run a_prepare_environment.R

# These data come from Social Explorer for the 2015--2019 5-year ACS.

# To access the data, visit https://www.socialexplorer.com/
# You will need credentials to access, likely through your university.
# Select "Tables"
# Select "American Community Surveys (5-Year Estimates)
# Click "Begin Report" next to "American Community Survey (ACS) 2015"
# The top of the screen will say "ACS 2019 (5-Year Estimates)" because this is the pooled 2015-2019 ACS.
# For "Select a geographic type" choose "140 Census Tract"
# Add "All census tracts" to your selections and click "Proceed to Tables"
# Select variables
# A00002. Population Density (Per Sq. Mile)
# A13002. Poverty Status in of Families by Family Type by Presence of Children Under 18 Years
# Click "Show Results"
# Click "Data Download"
# Check the box for "Output percents"
# Cick "Census Tract data (CSV)
# Your file will have a different name from the file below.

# Incomes are already adjusted to 2019 dollars in the data

d <- read_csv("data/R12822111_SL140.csv") %>%
  transmute(population_density = SE_A00002_002,
            log_population_density = log(population_density),
            total_population = SE_A00002_001,
            percent_poverty = PCT_SE_A13002_002) %>%
  filter(!is.na(percent_poverty) & !is.na(log_population_density) & log_population_density > -Inf)

# Produce the plot
poverty <- quantileplot(percent_poverty ~ s(log_population_density),
                        data = d %>% filter(!is.na(log_population_density)),
                        x_range = quantile(d$log_population_density, c(.01,.99), na.rm = T),
                        y_range = c(0,50),
                        quantile_notation = "legend")

print("When customizing the plot, we are changing layers that are already present. Warnings about this are to be expected.")
plot <- poverty$plot +
  scale_x_continuous(breaks = log(c(10,100,1000,10000)),
                     labels = exp,
                     name = "Population Density: People Per Square Mile\n(Log Scale)") +
  ylab("Percent of Families Below Poverty Line") +
  coord_cartesian(xlim = c(1.1,11),
                  ylim = c(-10,51)) +
  ggsave("figures/tract_poverty_density.pdf",
         height = 3.5, width = 6.5)


print("Share of tracts between 1k and 10k")
print(mean(d$population_density >= 1000 & d$population_density <= 10000, na.rm = T))
print("Share of population in tracts between 1k and 10k")
print(weighted.mean(d$population_density >= 1000 & d$population_density <= 10000,
                    w = d$total_population,
                    na.rm = T))
print("Share of population in tracts less than 1k")
print(weighted.mean(d$population_density < 1000,
                    w = d$total_population,
                    na.rm = T))
print("Share of population in tracts greater than 10k")
print(weighted.mean(d$population_density > 10000,
                    w = d$total_population,
                    na.rm = T))

# Make a version with uncertainty
poverty_uncertainty <- do.call(
  quantileplot,
  c(list(show_ci = T, uncertainty_draws = 10, lsig = poverty$mqgam.out$calibr$lsig),
    poverty$arguments[!(names(poverty$arguments) %in% c("show_ci","uncertainty_draws"))])
)
poverty_uncertainty$plot <- poverty_uncertainty$plot +
  scale_x_continuous(breaks = log(c(10,100,1000,10000)),
                     labels = exp,
                     name = "Population Density: People Per Square Mile\n(Log Scale)") +
  ylab("Percent of Families Below Poverty Line")
poverty_uncertainty$sim_curve_plots <- lapply(
  poverty_uncertainty$sim_curve_plots,
  function(p) {
    p +
      scale_x_continuous(breaks = log(c(10,10000)),
                         labels = exp)
  }
)
pdf("figures/poverty_uncertainty.pdf",
    height = 6.5, width = 6.5)
plot(poverty_uncertainty)
dev.off()

# Scatter plot
d %>%
  sample_frac(.1) %>%
  ggplot(aes(x = log_population_density,
             y = percent_poverty)) +
  geom_point(size = .1) +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  scale_x_continuous(breaks = log(c(10,100,1000,10000)),
                     labels = exp,
                     limits = poverty$x_range,
                     name = "Population Density: People Per Square Mile\n(Log Scale)") +
  ylab("Percent of Families Below Poverty Line") +
  ggsave("figures/poverty_scatter.pdf",
         height = 4.55, width = 5.5)


