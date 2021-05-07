
# Replication code for
# "Smooth quantile visualizations enhance understanding of bivariate population distributions"
# Robin C. Lee, Ian Lundberg, and Brandon M. Stewart

# This file: Conducts the cumulative citations example
# Prerequisite: Run a_prepare_environment.R

# How to create data:
# 1) Do a search in Web of Science
# 2) Click "Create Citation Report"
# 3) Under "Export Data" choose "Save to Excel File"
# Do (3) in batches of 500 articles.

d <- read_xls("data/demography_a.xls",
              skip = 27) %>%
  select_at(c("Authors","Title","Source Title","Publication Year",2000:2020)) %>%
  bind_rows(read_xls("data/demography_b.xls",
                     skip = 27) %>%
              select_at(c("Authors","Title","Source Title","Publication Year",2000:2020))) %>%
  bind_rows(read_xls("data/demography_c.xls",
                     skip = 27) %>%
              select_at(c("Authors","Title","Source Title","Publication Year",2000:2020))) %>%
  # Make sure we only have Demography
  filter(`Source Title` == "DEMOGRAPHY") %>%
  select(-`Source Title`) %>%
  melt(id = c("Authors","Title","Publication Year"), variable.name = "Year", value.name = "Citations") %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  mutate(Years_Since_Publication = Year - `Publication Year`) %>%
  filter(Years_Since_Publication >= 0) %>%
  group_by(Authors,Title) %>%
  arrange(Authors,Title,Years_Since_Publication) %>%
  mutate(Citations = cumsum(Citations)) %>%
  group_by()

# Produce the quantileplot
citations <- quantileplot(Citations ~ s(Years_Since_Publication),
                          data = d,
                          y_range = c(0,150))

print("The warning about missing values of text is only because the 90th percentile label would be off the plot. It is manually placed instead.")
citations$plot +
  ylab("Cumulative Citations\nin Web of Science") +
  xlab("Years Since Publication") +
  annotate(geom = "text", x = 13, y = 145, vjust = 1, hjust = 0,
           size = 3,
           label = "90th percentile") +
  coord_cartesian(ylim = c(-25,155),
                  xlim = c(0,24)) +
  ggsave("figures/demography_cum_cites.pdf",
         height = 3.5, width = 6.5)

# Make a version with uncertainty
citations_uncertainty <- do.call(
  quantileplot,
  c(list(show_ci = T, uncertainty_draws = 10),
    citations$arguments[!(names(citations$arguments) %in% c("show_ci","uncertainty_draws"))])
)
citations_uncertainty$plot <- citations_uncertainty$plot +
  xlab("Years Since Publication")

pdf("figures/citations_uncertainty.pdf",
    height = 6.5, width = 6.5)
plot(citations_uncertainty)
dev.off()

# Scatter plot
d %>%
  ggplot(aes(x = jitter(Years_Since_Publication,.9),
             y = Citations)) +
  geom_point(size = .5) +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  coord_cartesian(ylim = citations$y_range) +
  xlab("Years Since Publication") +
  ggsave("figures/citations_scatter.pdf",
         height = 4.55, width = 5.5)

