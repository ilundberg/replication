# Dependencies
library(here)
library(tidyverse)
theme_set(theme_bw())

sink(here("logs","fig4_classes.txt"))
print(Sys.time())

# Import analysis panel
nlsy_analysis <- tibble(readRDS(here("data/nlsy_panel.RDS")))

# Descriptive mobility table
nlsy_analysis |>
  group_by(parental_egp, egp) |>
  summarize(weight = sum(weight),
            .groups = "drop") |>
  group_by(parental_egp) |>
  mutate(prop = weight / sum(weight)) |>
  ggplot(aes(x = parental_egp, y = egp, fill = prop,
             label = format(round(prop,2),nsmall=2))) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5), color = "gray") +
  geom_hline(yintercept = c(1.5,2.5,3.5,4.5), color = "gray") +
  geom_tile(stat = "identity", width = 1) +
  geom_text(color = "white", fontface = "bold") +
  scale_fill_continuous(low = "steelblue1", high = "steelblue4") +
  scale_x_discrete(name = "Parent Occupational Class",
                   labels = function(x) gsub(" ","\n",x),
                   expand = c(0,0)) +
  scale_y_discrete(name = "Child Occupational Class",
                   labels = function(x) gsub(" ","\n",x),
                   expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank())
ggsave("figures/classes_unconditional.pdf",
       height = 2.5, width = 4)

# Causal mobility table
# First create a mobility table within confounders
classes_conditional <- nlsy_analysis |>
  mutate(egp = factor(egp)) |>
  group_by(parental_educ, resp_race, parental_egp) |>
  # Ensure that every child EGP appears, even if it has 0 people
  complete(egp) |>
  # Recode as 0 those that had not previously appeared
  mutate(weight = ifelse(is.na(weight),0,weight)) |>
  # Get total weight in the confounder-treatment-outcome stratum
  group_by(parental_educ, resp_race, parental_egp, egp) |>
  summarize(weight_confounder_treatment_outcome = sum(weight),
            .groups = "drop") |>
  # Get total weight in this confounder-treatment stratum
  group_by(parental_educ, resp_race, parental_egp) |>
  mutate(
    weight_confounder_treatment = sum(weight_confounder_treatment_outcome)
  ) |>
  # Get total weight in this confounder stratum
  group_by(parental_educ, resp_race) |>
  mutate(weight_confounder = sum(weight_confounder_treatment_outcome)) |>
  ungroup() |>
  # Get conditional outcome probability
  mutate(
    p_Y = weight_confounder_treatment_outcome / weight_confounder_treatment
  ) |>
  # Create propensity score
  mutate(p_A = weight_confounder_treatment / weight_confounder)

# Visualize the conditional mobility tables
classes_conditional |>
  filter(p_A >= .1) |>
  ggplot(aes(x = parental_egp, y = egp, fill = p_Y,
             label = format(round(p_Y,2),nsmall=2))) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5), color = "gray") +
  geom_hline(yintercept = c(1.5,2.5,3.5,4.5), color = "gray") +
  geom_tile(stat = "identity", width = 1) +
  geom_text(color = "white", fontface = "bold") + #, size = 1.5) +
  scale_fill_continuous(low = "steelblue1", high = "steelblue4") +
  scale_x_discrete(name = "Parent Occupational Class",
                   labels = function(x) gsub(" ","\n",x),
                   expand = c(0,0)) +
  scale_y_discrete(name = "Child Occupational Class",
                   labels = function(x) gsub(" ","\n",x),
                   expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()) +
  facet_grid(resp_race ~ parental_educ)
ggsave("figures/classes_conditional_disaggregated.pdf",
       height = 6.5, width = 13)

# Causal table: Within education
classes_conditional |>
  filter(p_A >= .1) |>
  # Keep only the parental EGP values that are observed in all strata
  group_by(parental_educ, parental_egp) |>
  filter(n_distinct(resp_race) == 3) |>
  # Estimate the population average mobility table
  group_by(egp, .add = T) |>
  summarize(p_Ya = weighted.mean(p_Y, w = weight_confounder),
            .groups = "drop") |>
  mutate(race = "Aggregated over race") |>
  ggplot(aes(x = parental_egp, y = egp, fill = p_Ya,
             label = format(round(p_Ya,2),nsmall=2))) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5), color = "gray") +
  geom_hline(yintercept = c(1.5,2.5,3.5,4.5), color = "gray") +
  geom_tile(stat = "identity", width = 1) +
  geom_text(color = "white", fontface = "bold") +
  scale_fill_continuous(low = "steelblue1", high = "steelblue4") +
  scale_x_discrete(name = "Parent Occupational Class",
                   labels = function(x) gsub(" ","\n",x),
                   #expand = c(0,0),
                   drop = F) +
  scale_y_discrete(name = "Child Occupational Class",
                   labels = function(x) gsub(" ","\n",x),
                   expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()) +
  facet_grid(race~parental_educ)
ggsave(here("figures/classes_conditional_aggregated_inEduc.pdf"),
       height = 3, width = 13)

sessionInfo()
sink()
