# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))

# Record printed output in a text file
sink("logs/fig01.txt")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load packages
library(tidyverse)
library(reshape2)
library(ggrepel)

# Load the data
for_scatter <- readRDS("intermediate/for_scatter.Rds")
scatter_slopes <- readRDS("intermediate/scatter_slopes.Rds")

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
           label = paste0("Slope of Best Fit Line: ",scatter_slopes["Non-Hispanic Black"]),
           size = 3)

ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack.pdf",
       height = 3, width = 3.5, plot = plot)

annotated <- plot +
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
                  size = 2)

ggsave(filename = "figures/scatter_factual_outcome_re_NonHispanicBlack_annotated.pdf",
       plot = annotated,
       device = "pdf",
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
           label = paste0("Slope of Best Fit Line: ",scatter_slopes["Hispanic"]),
           size = 3)

ggsave("figures/scatter_factual_outcome_re_Hispanic.pdf",
       height = 3, width = 3.5,
       plot = plot, device = "pdf")

annotated <- plot +
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
                  size = 2)

ggsave("figures/scatter_factual_outcome_re_Hispanic_annotated.pdf",
       height = 3, width = 3.5, plot = annotated, device = "pdf")

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
           label = paste0("Slope of Best Fit Line: ",scatter_slopes["Non-Hispanic White"]),
           size = 3)

ggsave("figures/scatter_factual_outcome_re_NonHispanicWhite.pdf",
       height = 3, width = 3.5, plot = plot, device = "pdf")

annotated <- plot +
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
                  size = 2, nudge_y = -.001, nudge_x = -.02)
ggsave("figures/scatter_factual_outcome_re_NonHispanicWhite_annotated.pdf",
       height = 3, width = 3.5, plot = annotated, device = "pdf")

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
           label = paste0("Slope of Best Fit Line: ",scatter_slopes["Other"]),
           size = 3)
ggsave("figures/scatter_factual_outcome_re_Other.pdf",
       height = 3, width = 3.5, plot = plot, device = "pdf")
annotated <- plot +
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
                  size = 2, nudge_y = .01, segment.size = .2)
ggsave("figures/scatter_factual_outcome_re_Other_annotated.pdf",
       height = 3, width = 3.5, plot = annotated, device = "pdf")

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

ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide1.pdf",
       height = 3, width = 4, plot = plot, device = "pdf")
plot2 <- plot +
  geom_point(aes(alpha = occ_title %in% c("Physicians and Surgeons"))) +
  geom_text(aes(label = case_when(occ_title == "Physicians and Surgeons" ~ "Physicians")),
            size = 2, vjust = 2) +
  scale_alpha_manual(values = c(0,1)) +
  theme(legend.position = "none")
ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide2.pdf",
       height = 3, width = 4, plot = plot2, device = "pdf")

plot3 <- plot2 +
  geom_point(aes(alpha = occ_title %in% c("Nursing, Psychiatric, and Home Health Aides"))) +
  geom_text(aes(label = case_when(occ_title == "Nursing, Psychiatric, and Home Health Aides" ~ "Home Health\nAide")),
            size = 2,
            vjust = 1.5) +
  theme(legend.position = "none")
ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide3.pdf",
       height = 3, width = 4, plot = plot3, device = "pdf")

plot4 <- plot3 +
  geom_point(aes(alpha = occ_title %in% c("Taxi Drivers and Chauffeurs"))) +
  geom_text(aes(label = case_when(occ_title == "Taxi Drivers and Chauffeurs" ~ "Taxi Drivers")),
            size = 2,
            vjust = -1) +
  theme(legend.position = "none")
ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide4.pdf",
       height = 3, width = 4, plot = plot4, device = "pdf")

plot5 <- plot4 +
  geom_point() +
  geom_text_repel(aes(label = case_when(occ_title == "Personal Care Aides" ~ "Personal\nCare Aides",
                                        occ_title == "Combined Food Preparation and Serving Workers, Including Fast Food" ~ "Fast Food",
                                        occ_title == "Dishwashers" ~ "Dishwashers",
                                        occ_title == "Carpet, Floor, and Tile Installers and Finishers" ~ "Carpet\nInstallers",
                                        T ~ "")),
                  size = 2) +
  theme(legend.position = "none")
ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide5.pdf",
       height = 3, width = 4, plot = plot5, device = "pdf")

plot6 <- plot5 +
  geom_point() +
  geom_smooth(method = "lm", se = F, aes(weight = size)) +
  annotate(geom = "text", x = .4, y = .002, hjust = 1,
           label = paste0("Slope of Best Fit Line: ",scatter_slopes["Non-Hispanic Black"]),
           size = 3) +
  theme(legend.position = "none")
ggsave("figures/scatter_factual_outcome_re_NonHispanicBlack_slide6.pdf",
       height = 3, width = 4, plot = plot6, device = "pdf")

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))
