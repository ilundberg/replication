

library(tidyverse)
library(reshape2)
library(haven)
library(quantileplot)

setwd("/users/iandl/Dropbox/quantileplot_me/")

# Infant mortality data from https://data.worldbank.org/indicator/SP.DYN.IMRT.IN
mortality <- read_csv("data/API_SP.DYN.IMRT.IN_DS2_en_csv_v2_2166906.csv",
                      skip = 4)

# GDP data from https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
gdp <- read_csv("data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2163510.csv",
                 skip = 4)

d_mortality <- mortality %>%
  transmute(code = `Country Code`,
            mortality = `2018`) %>%
  left_join(gdp %>%
              transmute(code = `Country Code`,
                        gdp_pc = `2018`),
            by = "code") %>%
  mutate(log_gdp_pc = log(gdp_pc))

# Plot for infant mortality rate
mortality <- quantileplot(mortality ~ s(log_gdp_pc),
                          data = d_mortality,
                          quantile_notation = "legend")

mortality$plot +
  scale_x_continuous(breaks = log(c(1000,10000,100000)),
                     labels = function(x) paste0(exp(x) / 1000,"k")) +
  ylab("Infant Mortality Rate\n(Per Thousand Live Births)") +
  xlab("GDP Per Capita\n(Log Scale)") +
  ggsave("figures/mortality_gdp.pdf",
         height = 3.5, width = 6.5)

# Plot with uncertainty
mortality_uncertainty <- do.call(
  quantileplot,
  c(list(show_ci = T, uncertainty_draws = 10),
    mortality$arguments[!(names(mortality$arguments) %in% c("show_ci","uncertainty_draws"))])
)

# Customize some aspects of the plot
mortality_uncertainty$plot <- mortality_uncertainty$plot +
  scale_x_continuous(breaks = log(c(1000,10000,100000)),
                     labels = function(x) paste0(exp(x) / 1000,"k")) +
  ylab("Infant Mortality Rate\n(Per Thousand Live Births)") +
  xlab("GDP Per Capita\n(Log Scale)")
mortality_uncertainty$sim_curve_plots <- lapply(
  mortality_uncertainty$sim_curve_plots, function(p) {
    p +
      scale_x_continuous(breaks = log(c(1000,10000,100000)),
                         labels = function(x) paste0(exp(x) / 1000,"k"))
  }
)
pdf("figures/mortality_uncertainty.pdf",
    height = 6.5, width = 6.5)
plot(mortality_uncertainty, bottom_xlab_angle = 45)
dev.off()

# Scatter plot
scatter_no_line <- d_mortality %>%
  ggplot(aes(x = log_gdp_pc,
             y = mortality)) +
  geom_point(size = .5) +
  theme_bw() +
  scale_x_continuous(name = mortality_uncertainty$plot$labels$x,
                     breaks = log(c(1000,10000,100000)),
                     labels = function(x) paste0("$", exp(x) / 1000, "k")) +
  ylab(mortality_uncertainty$plot$labels$y) +
  ylim(c(-15,84)) +
  ggsave("figures/mortality_scatter_no_line.pdf",
         height = 4.55, width = 5.5)
scatter_with_line <- scatter_no_line +
  geom_smooth(method = "lm", se = F) +
  ylim(c(-15,84)) +
  ggsave("figures/mortality_scatter.pdf",
         height = 4.55, width = 5.5)

# SLIDE VERSIONS

mortality$plot +
  scale_x_continuous(breaks = log(c(1000,10000,100000)),
                     labels = function(x) paste0(exp(x) / 1000,"k")) +
  ylab("Infant Mortality Rate\n(Per Thousand Live Births)") +
  xlab("GDP Per Capita\n(Log Scale)") +
  ggsave("figures/mortality_gdp_tall.pdf",
         height = 4.55, width = 5.5)


