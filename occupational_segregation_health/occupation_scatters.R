# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This code produces scatter plots of occupation-specific onset
# as functions of racial/ethnic composition

# Slide versions are at the end

source("code/prepare_data.R")
all_data <- prepare_data(2005:2020)

# Extract the occupational titles to facilitate labeling points
occupation_titles <- all_data$full_population %>%
  select(OCC2010) %>%
  group_by(OCC2010) %>%
  filter(1:n() == 1) %>%
  group_by() %>%
  mutate(occ_title = as.character(to_factor(OCC2010)),
         OCC2010 = factor(OCC2010))

# I will fit models on all_data$d_onset
# I will predict for to_predict, which has one row per occupation
to_predict <- all_data$d_onset %>%
  group_by(OCC2010) %>%
  summarize(size = sum(ASECWT)) %>%
  group_by() %>%
  left_join(occupation_titles, by = "OCC2010")

# Fit the proportion in each category of race
# This model does partial pooling to improve precision of estimates
categories <- unique(all_data$d_onset$RACE)
category_proportions <- foreach(r = categories, .combine = "rbind") %do% {
  fit <- bam(as.numeric(RACE == r) ~ s(OCC2010, bs = "re"),
             data = all_data$d_onset,
             weights = ASECWT)
  return(to_predict %>%
           mutate(category = r,
                  proportion = predict(fit, newdata = to_predict)))
}
# Fit the outcome model
# This model does partial pooling to improve precision of estimates
fit_outcome <- bam(y ~ s(OCC2010, bs = "re"),
                   data = all_data$d_onset,
                   weights = ASECWT)
# Combine those in a dataset for the scatter
for_scatter <- category_proportions %>%
  left_join(to_predict %>%
              select(OCC2010) %>%
              mutate(y = predict(fit_outcome, newdata = to_predict)),
            by = "OCC2010")

# Get the slope of the best-fit line as a function of each category proportion
slopes <- foreach(r = categories, .combine = "c") %do% {
  fit <- lm(y ~ proportion,
            weights = size,
            data = for_scatter %>%
              filter(category == r))
  return(coef(fit)[2])
}
slopes <- format(round(slopes,2),nsmall = 2)
names(slopes) <- categories

####################
# PRODUCE SCATTERS #
####################

# Non-Hispanic black
plot <- for_scatter %>%
  filter(category == "Non-Hispanic Black") %>%
  ggplot(aes(x = proportion, y = y, size = size)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, aes(weight = size)) +
  xlab("Proportion Non-Hispanic Black") +
  ylab("Onset of Work-Limiting Disability") +
  xlim(c(0,.65)) +
  ylim(c(0,.056)) +
  geom_hline(yintercept = 0, color = "gray") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(face = "bold")) +
  scale_size_continuous(range = c(.2,3)) +
  annotate(geom = "text", x = .65, y = .002, hjust = 1,
           label = paste0("Slope of Best Fit Line: ",slopes["Non-Hispanic Black"]),
           size = 3)
plot +
  ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack.pdf",
         height = 3, width = 3.5)

plot +
  geom_text_repel(aes(label = case_when(occ_title == "Personal Care Aides" ~ "Personal\nCare Aides",
                                        occ_title == "Nursing, Psychiatric, and Home Health Aides" ~ "Home Health\nAides",
                                        occ_title == "Taxi Drivers and Chauffeurs" ~ "Taxi Drivers",
                                        occ_title == "Combined Food Preparation and Serving Workers, Including Fast Food" ~ "Fast Food",
                                        occ_title == "Social Workers" ~ "Social\nWorkers",
                                        occ_title == "Dishwashers" ~ "Dishwashers",
                                        occ_title == "Physicians and Surgeons" ~ "Physicians",
                                        occ_title == "Carpet, Floor, and Tile Installers and Finishers" ~ "Carpet\nInstallers",
                                        occ_title == "Postal Service Mail Sorters, Processors, and Processing Machine Operators" ~ "Mail\nSorters",
                                        T ~ "")),
                  size = 2) +
  ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_annotated.pdf",
         height = 3, width = 3.5)

# Plot as a function of Hispanic
plot <- for_scatter %>%
  filter(category == "Hispanic") %>%
  ggplot(aes(x = proportion, y = y, size = size)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, aes(weight = size)) +
  xlab("Proportion Hispanic") +
  ylab("Onset of Work-Limiting Disability") +
  xlim(c(0,.65)) +
  ylim(c(0,.056)) +
  scale_size_continuous(range = c(.2,3)) +
  geom_hline(yintercept = 0, color = "gray") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(face = "bold")) +
  annotate(geom = "text", x = .65, y = .002, hjust = 1,
           label = paste0("Slope of Best Fit Line: ",slopes["Hispanic"]),
           size = 3)
plot + 
  ggsave("figures/scatter_factual_outcome_re_Hispanic.pdf",
         height = 3, width = 3.5)
plot +
  geom_text_repel(aes(label = case_when(occ_title == "Combined Food Preparation and Serving Workers, Including Fast Food" ~ "Fast Food",
                                        occ_title == "Taxi Drivers and Chauffeurs" ~ "Taxi Drivers",
                                        occ_title == "Agricultural workers, nec" ~ "Agricultural\nworkers",
                                        occ_title == "Physicians and Surgeons" ~ "Physicians",
                                        occ_title == "Dishwashers" ~ "Dishwashers",
                                        occ_title == "Carpet, Floor, and Tile Installers and Finishers" ~ "Carpet\nInstallers",
                                        T ~ "")),
                  size = 2) +
  # Add the titles that need to be manually nudged
  geom_text_repel(aes(label = case_when(occ_title == "Roofers" ~ "Roofers")),
                  nudge_x = .007,
                  nudge_y = .00002,
                  size = 2) +
  geom_text_repel(aes(label = case_when(occ_title == "Plasterers and Stucco Masons" ~ "Plasterers")),
                  nudge_y = -.001,
                  size = 2) +
  geom_text_repel(aes(label = case_when(occ_title == "Drywall Installers, Ceiling Tile Installers, and Tapers" ~ "Drywall\nInstallers")),
                  nudge_y = -.001,
                  size = 2) +
  geom_text_repel(aes(label = case_when(occ_title == "Farmers, Ranchers, and Other Agricultural Managers" ~ "Farmers")),
                  nudge_y = .009,
                  nudge_x = -.005,
                  size = 2) +
  ggsave("figures/scatter_factual_outcome_re_Hispanic_annotated.pdf",
         height = 3, width = 3.5)

# Non-Hispanic White
plot <-  for_scatter %>%
  filter(category == "Non-Hispanic White") %>%
  ggplot(aes(x = proportion, y = y, size = size)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, aes(weight = size)) +
  xlab("Proportion Non-Hispanic White") +
  ylab("Onset of Work-Limiting Disability") +
  geom_hline(yintercept = 0, color = "gray") +
  theme_bw() +
  xlim(c(.3,.95)) +
  ylim(c(0,.056)) +
  theme(legend.position = "none", axis.title = element_text(face = "bold")) +
  scale_size_continuous(range = c(.2,3)) +
  annotate(geom = "text", x = .3, y = 0.002, hjust = 0,
           label = paste0("Slope of Best Fit Line: ",slopes["Non-Hispanic White"]),
           size = 3)
plot + 
  ggsave("figures/scatter_factual_outcome_re_NonHispanicWhite.pdf",
         height = 3, width = 3.5)
plot +
  geom_text_repel(aes(label = case_when(occ_title == "Chief executives and legislators/public administration" ~ "CEOs",
                                        occ_title == "Physicians and Surgeons" ~ "Physicians",
                                        occ_title == "Combined Food Preparation and Serving Workers, Including Fast Food" ~ "Fast Food",
                                        occ_title == "Nonfarm Animal Caretakers" ~ "Nonfarm\nAnimal\nCaretakers",
                                        occ_title == "Sheet Metal Workers, metal-working" ~ "Sheet\nMetal\nWorkers",
                                        occ_title == "Farmers, Ranchers, and Other Agricultural Managers" ~ "Farmers",
                                        occ_title == "Taxi Drivers and Chauffeurs" ~ "Taxi Drivers",
                                        occ_title == "Software Developers, Applications and Systems Software" ~ "Software\nDevelopers",
                                        y == max(y) ~ occ_title,
                                        T ~ "")),
                  size = 2) +
  geom_text_repel(aes(label = case_when(occ_title == "Personal Appearance Workers, nec" ~ "Personal\nAppearance\nWorkers",
                                        T ~ "")),
                  size = 2, nudge_y = -.001, nudge_x = -.02) +
  ggsave("figures/scatter_factual_outcome_re_NonHispanicWhite_annotated.pdf",
         height = 3, width = 3.5)

# Other
plot <- for_scatter %>%
  filter(category == "Other") %>%
  ggplot(aes(x = proportion, y = y, size = size)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, aes(weight = size)) +
  xlab("Proportion Other") +
  ylab("Onset of Work-Limiting Disability") +
  geom_hline(yintercept = 0, color = "gray") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(face = "bold")) +
  scale_size_continuous(range = c(.2,3)) +
  xlim(c(0,.65)) +
  ylim(c(0,.056)) +
  annotate(geom = "text", x = .65, y = .002, hjust = 1,
           label = paste0("Slope of Best Fit Line: ",slopes["Other"]),
           size = 3)
plot +
  ggsave("figures/scatter_factual_outcome_re_Other.pdf",
         height = 3, width = 3.5)
plot +
  geom_text_repel(aes(label = case_when(occ_title == "Chief executives and legislators/public administration" ~ "CEOs",
                                        occ_title == "Physicians and Surgeons" ~ "Physicians",
                                        occ_title == "Combined Food Preparation and Serving Workers, Including Fast Food" ~ "Fast Food",
                                        occ_title == "Taxi Drivers and Chauffeurs" ~ "Taxi Drivers",
                                        occ_title == "Software Developers, Applications and Systems Software" ~ "Software\nDevelopers",
                                        occ_title == "Cargo and Freight Agents" ~ "Cargo and Freight Agents",
                                        occ_title == "Personal Appearance Workers, nec" ~ "Personal\nAppearance\nWorkers",
                                        y == max(y) ~ occ_title,
                                        T ~ "")),
                  size = 2) +
  geom_text_repel(aes(label = case_when(occ_title == "Gaming Services Workers" ~ "Gaming\nServices\nWorkers",
                                        T ~ "")),
                  size = 2, nudge_y = .01, segment.size = .2) +
  ggsave("figures/scatter_factual_outcome_re_Other_annotated.pdf",
         height = 3, width = 3.5)


# Note specific occupations that appear in the main text
sink("figures/opening_example_occupations.txt")
print(
  for_scatter %>%
    filter(occ_title %in% c("Nursing, Psychiatric, and Home Health Aides",
                            "Agricultural workers, nec",
                            "Industrial Truck and Tractor Operators",
                            "Personal Care Aides",
                            "Roofers")) %>%
    select(occ_title, category, proportion) %>%
    spread(key = category, value = proportion)
)
sink()

##################
# SLIDE VERSIONS #
##################

# These build up the non-Hispanic Black plot
plot <- for_scatter %>%
  filter(category == "Non-Hispanic Black") %>%
  ggplot(aes(x = proportion, y = y, size = size)) +
  xlab("Proportion Non-Hispanic Black") +
  ylab("Onset of Work-Limiting Disability") +
  xlim(c(0,.4)) +
  ylim(c(0,.056)) +
  geom_hline(yintercept = 0, color = "gray") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(face = "bold")) +
  scale_size_continuous(range = c(.2,3))

plot +
  ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide1.pdf",
         height = 3, width = 4)
plot2 <- plot +
  geom_point(aes(alpha = occ_title %in% c("Physicians and Surgeons"))) +
  geom_text(aes(label = case_when(occ_title == "Physicians and Surgeons" ~ "Physicians")),
            size = 2, vjust = 2) +
  scale_alpha_manual(values = c(0,1)) +
  theme(legend.position = "none") +
  ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide2.pdf",
         height = 3, width = 4)

plot3 <- plot2 +
  geom_point(aes(alpha = occ_title %in% c("Nursing, Psychiatric, and Home Health Aides"))) +
  geom_text(aes(label = case_when(occ_title == "Nursing, Psychiatric, and Home Health Aides" ~ "Home Health\nAide")),
            size = 2,
            vjust = 1.5) +
  theme(legend.position = "none") +
  ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide3.pdf",
         height = 3, width = 4)

plot4 <- plot3 +
  geom_point(aes(alpha = occ_title %in% c("Taxi Drivers and Chauffeurs"))) +
  geom_text(aes(label = case_when(occ_title == "Taxi Drivers and Chauffeurs" ~ "Taxi Drivers")),
            size = 2,
            vjust = -1) +
  theme(legend.position = "none") +
  ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide4.pdf",
         height = 3, width = 4)

plot5 <- plot4 +
  geom_point() +
  geom_text_repel(aes(label = case_when(occ_title == "Personal Care Aides" ~ "Personal\nCare Aides",
                                        occ_title == "Combined Food Preparation and Serving Workers, Including Fast Food" ~ "Fast Food",
                                        occ_title == "Dishwashers" ~ "Dishwashers",
                                        occ_title == "Carpet, Floor, and Tile Installers and Finishers" ~ "Carpet\nInstallers",
                                        T ~ "")),
                  size = 2) +
  theme(legend.position = "none") +
  ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide5.pdf",
         height = 3, width = 4)

plot6 <- plot5 +
  geom_point() +
  geom_smooth(method = "lm", se = F, aes(weight = size)) +
  annotate(geom = "text", x = .4, y = .002, hjust = 1,
           label = paste0("Slope of Best Fit Line: ",slopes["Non-Hispanic Black"]),
           size = 3) +
  theme(legend.position = "none") +
  ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide6.pdf",
         height = 3, width = 4)
