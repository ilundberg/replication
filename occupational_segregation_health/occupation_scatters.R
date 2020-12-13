# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A gap-closing perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This code completes two related tasks
# 1. Scatter plots of occupation-specific onset and racial/ethnic composition
# 2. Illustration of the assignment rule in subgroups with selected occupations

###########################################
# 1. Scatter plots of occupation-specific #
# onset and racial/ethnic composition     #
###########################################

# Slide versions are at the end of this section

source("code/prepare_data.R")
all_data <- prepare_data(2005:2020)
for_predictions <- all_data$d_onset
load("intermediate/prop_NonHispanicBlack_fit.Rdata")
for_predictions$prop_NonHispanicBlack <- fitted(fit)
load("intermediate/prop_Hispanic_fit.Rdata")
for_predictions$prop_Hispanic <- fitted(fit)
load("intermediate/prop_Other_fit.Rdata")
for_predictions$prop_Other <- fitted(fit)
rm(fit)

# The main results leave non-Hispanic white as the reference category, so that model is not yet fitted.
# Fit that model for making the scatter.
fit_prop_nonHispanicWhite <- gam(as.numeric(RACE == "Non-Hispanic White") ~ s(OCC2010, bs = "re"),
                                 data = all_data$d_onset,
                                 weights = ASECWT)
for_predictions$prop_NonHispanicWhite = fitted(fit_prop_nonHispanicWhite)

# Extract the occupational titles to facilitate labeling points
occupation_titles <- all_data$full_population %>%
  select(OCC2010) %>%
  group_by(OCC2010) %>%
  filter(1:n() == 1) %>%
  group_by() %>%
  mutate(occ_title = as.character(to_factor(OCC2010)),
         OCC2010 = factor(OCC2010))

# Fit a model to describe the mean outcome in each occupation
factual_outcome_fit_re <- bam(y ~ s(OCC2010, bs = "re"),
                              data = for_predictions,
                              weights = ASECWT)

# Prepare data for the plots
for_factual_plot_re <- for_predictions %>%
  mutate(fitted_factual = fitted(factual_outcome_fit_re)) %>%
  group_by(OCC2010, prop_NonHispanicBlack, prop_Hispanic, prop_NonHispanicWhite, prop_Other) %>%
  summarize(average_factual_outcome = weighted.mean(fitted_factual, w = ASECWT),
            size = sum(ASECWT)) %>%
  group_by() %>%
  select(OCC2010, prop_NonHispanicBlack, prop_Hispanic, prop_NonHispanicWhite, prop_Other, average_factual_outcome, size) %>%
  left_join(occupation_titles, by = "OCC2010")

# Get the slopes of the best-fit lines
slope_NonHispanicBlack_re <- coef(lm(average_factual_outcome ~ prop_NonHispanicBlack,
                                     weights = size,
                                     data = for_factual_plot_re))[2]
slope_Hispanic_re <- coef(lm(average_factual_outcome ~ prop_Hispanic,
                             weights = size,
                             data = for_factual_plot_re))[2]
slope_NonHispanicWhite_re <- coef(lm(average_factual_outcome ~ prop_NonHispanicWhite,
                                     weights = size,
                                     data = for_factual_plot_re))[2]
slope_Other_re <- coef(lm(average_factual_outcome ~ prop_Other,
                          weights = size,
                          data = for_factual_plot_re))[2]

# Plot as a function of non-Hispanic black
plot <- for_factual_plot_re %>%
  ggplot(aes(x = prop_NonHispanicBlack, y = average_factual_outcome, size = size)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, aes(weight = size)) +
  xlab("Proportion Non-Hispanic Black") +
  ylab("Onset of Work-Limiting Disability") +
  #xlim(c(0,.47)) +
  xlim(c(0,.65)) +
  ylim(c(0,.056)) +
  geom_hline(yintercept = 0, color = "gray") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(face = "bold")) +
  scale_size_continuous(range = c(.2,3)) +
  annotate(geom = "text", x = .65, y = .002, hjust = 1,
           label = paste0("Slope of Best Fit Line: ",format(round(slope_NonHispanicBlack_re,2),nsmall = 2)),
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
# Look at a larger set of them to pick the example labels selected above
plot + 
  geom_text_repel(aes(label = case_when(average_factual_outcome > .04 ~ occ_title,
                                        prop_NonHispanicBlack < .05 & average_factual_outcome > .03 ~ occ_title,
                                        prop_NonHispanicBlack == min(prop_NonHispanicBlack) ~ occ_title,
                                        average_factual_outcome == min(average_factual_outcome) ~ occ_title)),
                  size = 3)

# Plot as a function of Hispanic
plot <- for_factual_plot_re %>%
  ggplot(aes(x = prop_Hispanic, y = average_factual_outcome, size = size)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, aes(weight = size)) +
  xlab("Proportion Hispanic") +
  ylab("Onset of Work-Limiting Disability") +
  #xlim(c(0,.47)) +
  xlim(c(0,.65)) +
  ylim(c(0,.056)) +
  scale_size_continuous(range = c(.2,3)) +
  geom_hline(yintercept = 0, color = "gray") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(face = "bold")) +
  annotate(geom = "text", x = .65, y = .002, hjust = 1,
           label = paste0("Slope of Best Fit Line: ",format(round(slope_Hispanic_re,2),nsmall = 2)),
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

# Plot as a function of Non-Hispanic White
plot <- for_factual_plot_re %>%
  ggplot(aes(x = prop_NonHispanicWhite, y = average_factual_outcome, size = size)) +
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
           label = paste0("Slope of Best Fit Line: ",format(round(slope_NonHispanicWhite_re,2),nsmall = 2)),
           size = 3)

plot +
  geom_text_repel(aes(label = case_when(occ_title == "Chief executives and legislators/public administration" ~ "CEOs",
                                        occ_title == "Physicians and Surgeons" ~ "Physicians",
                                        occ_title == "Combined Food Preparation and Serving Workers, Including Fast Food" ~ "Fast Food",
                                        occ_title == "Nonfarm Animal Caretakers" ~ "Nonfarm\nAnimal\nCaretakers",
                                        occ_title == "Sheet Metal Workers, metal-working" ~ "Sheet\nMetal\nWorkers",
                                        occ_title == "Farmers, Ranchers, and Other Agricultural Managers" ~ "Farmers",
                                        occ_title == "Taxi Drivers and Chauffeurs" ~ "Taxi Drivers",
                                        occ_title == "Software Developers, Applications and Systems Software" ~ "Software\nDevelopers",
                                        average_factual_outcome == max(average_factual_outcome) ~ occ_title,
                                        T ~ "")),
                  size = 2) +
  geom_text_repel(aes(label = case_when(occ_title == "Personal Appearance Workers, nec" ~ "Personal\nAppearance\nWorkers",
                                        T ~ "")),
                  size = 2, nudge_y = -.001, nudge_x = -.02) +
  ggsave("figures/scatter_factual_outcome_re_NonHispanicWhite_annotated.pdf",
         height = 3, width = 3.5)

# Plot as a function of Other
plot <- for_factual_plot_re %>%
  ggplot(aes(x = prop_Other, y = average_factual_outcome, size = size)) +
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
           label = paste0("Slope of Best Fit Line: ",format(round(slope_Other_re,2),nsmall = 2)),
           size = 3)

plot +
  geom_text_repel(aes(label = case_when(occ_title == "Chief executives and legislators/public administration" ~ "CEOs",
                                        occ_title == "Physicians and Surgeons" ~ "Physicians",
                                        occ_title == "Combined Food Preparation and Serving Workers, Including Fast Food" ~ "Fast Food",
                                        occ_title == "Taxi Drivers and Chauffeurs" ~ "Taxi Drivers",
                                        occ_title == "Software Developers, Applications and Systems Software" ~ "Software\nDevelopers",
                                        occ_title == "Cargo and Freight Agents" ~ "Cargo and Freight Agents",
                                        occ_title == "Personal Appearance Workers, nec" ~ "Personal\nAppearance\nWorkers",
                                        average_factual_outcome == max(average_factual_outcome) ~ occ_title,
                                        T ~ "")),
                  size = 2) +
  geom_text_repel(aes(label = case_when(occ_title == "Gaming Services Workers" ~ "Gaming\nServices\nWorkers",
                                        T ~ "")),
                  size = 2, nudge_y = .01, segment.size = .2) +
  ggsave("figures/scatter_factual_outcome_re_Other_annotated.pdf",
         height = 3, width = 3.5)




##################
# SLIDE VERSIONS #
##################

# Plot as a function of non-Hispanic black
plot <- for_factual_plot_re %>%
  ggplot(aes(x = prop_NonHispanicBlack, y = average_factual_outcome, size = size)) +
  xlab("Proportion Non-Hispanic Black") +
  ylab("Onset of Work-Limiting Disability") +
  #xlim(c(0,.47)) +
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
           label = paste0("Slope of Best Fit Line: ",format(round(slope_NonHispanicBlack_re,2),nsmall = 2)),
           size = 3) +
  theme(legend.position = "none") +
  ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide6.pdf",
         height = 3, width = 4)

##########################################
# 2. Illustration of the assignment rule #
# in subgroups with selected occupations #
##########################################

# Need cases with very different proportions across education and across race
factual <- all_data$d_onset %>%
  group_by(OCC2010, EDUC, RACE) %>%
  summarize(weight = sum(ASECWT)) %>%
  group_by(EDUC, RACE) %>%
  mutate(factual = weight / sum(weight)) %>%
  group_by() %>%
  # Restrict to HS and college
  # and to black and white
  filter(EDUC %in% c("High school","College") & RACE %in% c("Non-Hispanic Black","Non-Hispanic White")) %>%
  # Get the most variable by race within education
  group_by(OCC2010, EDUC) %>%
  mutate(variance_factual = var(factual)) %>%
  group_by(EDUC, RACE) %>%
  arrange(-variance_factual) %>%
  mutate(variance_index = 1:n()) %>%
  group_by() %>%
  left_join(occupation_titles, by = "OCC2010") %>%
  mutate(RACE = gsub(" ","\n",RACE))

counterfactual <- all_data$d_onset %>%
  group_by(OCC2010, EDUC) %>%
  summarize(weight = sum(ASECWT)) %>%
  group_by(EDUC) %>%
  mutate(counterfactual = weight / sum(weight)) %>%
  group_by() %>%
  select(OCC2010, EDUC, counterfactual)

unique((factual %>%
          filter(variance_index <= 3))$occ_title)

factual %>%
  filter(variance_index <= 3) %>%
  group_by(RACE) %>%
  mutate(factual = factual / sum(factual)) %>%
  mutate(EDUC = case_when(EDUC == "High school" ~ "Among those with exactly\na high school degree",
                          EDUC == "College" ~ "Among those with exactly\na college degree"),
         EDUC = fct_rev(EDUC)) %>%
  mutate(occ_title = gsub("and","and\n",occ_title),
         occ_title = gsub("of","\nof",occ_title)) %>%
  ggplot(aes(x = RACE, y = occ_title, size = factual)) +
  geom_point() +
  facet_wrap(~EDUC, nrow = 2, scales = "free") +
  ggtitle("Factual\nAssignment\nRule") +
  annotate(geom = "rect", xmin = .8, xmax = 1.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  annotate(geom = "rect", xmin = 1.8, xmax = 2.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, face = "bold")) +
  ggsave("figures/pi_bucket_factual.pdf",
         height = 5, width = 4)

factual %>%
  filter(variance_index <= 3) %>%
  left_join(counterfactual, by = c("OCC2010","EDUC")) %>%
  mutate(counterfactual = counterfactual / sum(counterfactual)) %>%
  mutate(EDUC = case_when(EDUC == "High school" ~ "Among those with exactly\na high school degree",
                          EDUC == "College" ~ "Among those with exactly\na college degree"),
         EDUC = fct_rev(EDUC)) %>%
  mutate(occ_title = gsub("and","and\n",occ_title),
         occ_title = gsub("of","\nof",occ_title)) %>%
  ggplot(aes(x = RACE, y = occ_title, size = counterfactual)) +
  geom_point() +
  facet_wrap(~EDUC, nrow = 2, scales = "free") +
  ggtitle("Counterfactual\nAssignment\nRule") +
  annotate(geom = "rect", xmin = .8, xmax = 1.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  annotate(geom = "rect", xmin = 1.8, xmax = 2.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, face = "bold")) +
  ggsave("figures/pi_bucket_counterfactual.pdf",
         height = 5, width = 4)

factual %>%
  filter(variance_index <= 3) %>%
  left_join(counterfactual, by = c("OCC2010","EDUC")) %>%
  mutate(counterfactual = counterfactual / sum(counterfactual)) %>%
  mutate(EDUC = case_when(EDUC == "High school" ~ "Among those with exactly\na high school degree",
                          EDUC == "College" ~ "Among those with exactly\na college degree"),
         EDUC = fct_rev(EDUC)) %>%
  mutate(occ_title = gsub("and","and\n",occ_title),
         occ_title = gsub("of","\nof",occ_title)) %>%
  ggplot(aes(x = RACE, y = occ_title, size = counterfactual)) +
  geom_point() +
  facet_wrap(~EDUC, nrow = 2, scales = "free") +
  ggtitle("Counterfactual\nAssignment\nRule") +
  annotate(geom = "rect", xmin = .8, xmax = 1.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  annotate(geom = "rect", xmin = 1.8, xmax = 2.2, 
           ymin = .5, ymax = 3.5, fill = "gray", alpha = .4) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = .5, face = "bold")) +
  ggsave("figures/pi_bucket_counterfactual_noLabels.pdf",
         height = 5, width = 2)


