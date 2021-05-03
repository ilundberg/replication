
library(tidyverse)
library(tidycensus)

# Prepare for API access to Census data for county populations
api_key <- read_table("data/census_apikey.txt", col_names = "key")[[1]]
census_api_key(api_key, install = TRUE)

# Load county population counts in 2019.
# These become the denominator of the death rates
county_population <- get_estimates("county", product = "population", year = 2019) %>%
  pivot_wider(id_cols = c(NAME, GEOID), values_from = value, names_from = variable) %>%
  select(-DENSITY) %>%
  rename(county_name = NAME, fips = GEOID, pop = POP)

# Load cumulative case and death counts from NY Times
covid_county <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# Prepare a data frame with every date and every county
d <- data.frame(date = rep(unique(covid_county$date), length(unique(covid_county$fips))),
                fips = rep(unique(covid_county$fips), each = length(unique(covid_county$date)))) %>%
  # Merge in the reported data
  left_join(covid_county, by = c("date","fips")) %>%
  # If NA in January, then there have been 0 cases in this county
  mutate(cases = ifelse(date == min(date) & is.na(cases), 0, cases),
         deaths = ifelse(date == min(date) & is.na(deaths), 0, deaths)) %>%
  # If no report, the cumulative count is from the previous report
  group_by(fips) %>%
  arrange(fips, date) %>%
  fill(cases, .direction = "down") %>%
  fill(county, .direction = "updown") %>%
  fill(state, .direction = "updown") %>%
  group_by() %>%
  left_join(county_population %>% select(fips,pop), by = "fips") %>%
  mutate(cases_rate = cases / pop * 1000,
         death_rate = deaths / pop * 1000,
         numeric_date = as.numeric(difftime(date, "2020-01-01", units = "days"))) %>%
  # Get rid of the early period where many counties are 0
  filter(date >= date("2020-07-01")) %>%
  # Thin the date frequency a bit to reduce file size. Keep every 10th date.
  group_by(fips) %>%
  arrange(fips, date) %>%
  filter(1:n() %% 25 == 1)

# Produce the plot
covid <- quantileplot(cases_rate ~ s(numeric_date),
                      data = d,
                      quantiles = c(.1,.25,.5,.75,.9),
                      y_range = c(0,150),
                      x_bw = 25)
covid$plot +
  scale_x_continuous(breaks = sapply(c("2020-07-01","2020-10-01","2021-01-01","2021-04-01"),
                                     function(x) difftime(x,"2020-01-01",units = "days")),
                     labels = c("1 Jul\n2020","1 Oct\n2020","1 Jan\n2021","1 Apr\n2021")) +
  xlab("Date") +
  ylab("Cumulative Covid Cases Per Thousand") +
  coord_cartesian(ylim = c(-25,153),
                  xlim = c(181,510)) +
  ggsave("figures/covid.pdf",
         height = 3.5, width = 6.5)

# Make a version with uncertainty
covid_uncertainty <- do.call(
  quantileplot,
  c(list(show_ci = T, uncertainty_draws = 10),
    covid$arguments[!(names(covid$arguments) %in% c("show_ci","uncertainty_draws"))])
)
covid_uncertainty$plot <- covid_uncertainty$plot +
  scale_x_continuous(breaks = sapply(c("2020-07-01","2020-10-01","2021-01-01","2021-04-01"),
                                     function(x) difftime(x,"2020-01-01",units = "days")),
                     labels = c("1 Jul\n2020","1 Oct\n2020","1 Jan\n2021","1 Apr\n2021")) +
  xlab("Date") +
  ylab("Cumulative Covid Cases Per Thousand") +
  coord_cartesian(ylim = c(-25,153),
                  xlim = c(181,510))
covid_uncertainty$sim_curve_plots <- lapply(
  covid_uncertainty$sim_curve_plots,
  function(p) {
    p +
      scale_x_continuous(breaks = sapply(c("2020-07-01","2021-04-01"),
                                         function(x) difftime(x,"2020-01-01",units = "days")),
                         labels = c("1 Jul 2020","1 Apr 2021")) +
      xlab("Date") +
      ylab("Cumulative Covid Cases Per Thousand") +
      coord_cartesian(ylim = c(0,153),
                      xlim = c(181,456)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
)
pdf("figures/covid_uncertainty.pdf",
    height = 6.5, width = 6.5)
plot(covid_uncertainty, angle = 45, hjust = 1)
dev.off()

# Scatter plot
d %>%
  sample_frac(.1) %>%
  ggplot(aes(x = jitter(numeric_date,30),
             y = cases_rate)) +
  geom_point(size = .1) +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  ylim(c(0,150)) +
  scale_x_continuous(breaks = sapply(c("2020-07-01","2020-10-01","2021-01-01","2021-04-01"),
                                     function(x) difftime(x,"2020-01-01",units = "days")),
                     labels = c("1 Jul\n2020","1 Oct\n2020","1 Jan\n2021","1 Apr\n2021")) +
  xlab("Date") +
  ylab("Cumulative Covid Cases Per Thousand") +
  ggsave("figures/covid_scatter.pdf",
         height = 4.55, width = 5.5)

