# Supporting code file for
# Occupational segregation contributes to racial disparities in health: A counterfactual perspective
# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R to see how this file is called

# This code illustrates the notion of occupational lotteries
# by assignment rules in subgroups with selected occupations

source("code/prepare_data.R")
all_data <- prepare_data(2005:2020)

# Find occupations with very different proportions across education and across race
factual <- all_data$d_onset %>%
  group_by(OCC2010, EDUC, RACE) %>%
  summarize(weight = sum(ASECWT)) %>%
  group_by(EDUC, RACE) %>%
  mutate(factual = weight / sum(weight)) %>%
  group_by() %>%
  # Restrict to HS and college
  # and to black and white
  filter(EDUC %in% c("High school","College") & RACE %in% c("Non-Hispanic Black","Non-Hispanic White")) %>%
  # Find the occupations that are allocated very unequally by race
  group_by(OCC2010, EDUC) %>%
  mutate(variance_factual = var(factual)) %>%
  group_by(EDUC, RACE) %>%
  arrange(-variance_factual) %>%
  mutate(variance_index = 1:n()) %>%
  group_by() %>%
  left_join(occupation_titles, by = "OCC2010") %>%
  mutate(RACE = gsub(" ","\n",RACE))

# Find the assignment probabilities if assigned by education alone
counterfactual <- all_data$d_onset %>%
  group_by(OCC2010, EDUC) %>%
  summarize(weight = sum(ASECWT)) %>%
  group_by(EDUC) %>%
  mutate(counterfactual = weight / sum(weight)) %>%
  group_by() %>%
  select(OCC2010, EDUC, counterfactual)

# Look at some of those occupations
unique((factual %>%
          filter(variance_index <= 3))$occ_title)

# Produce a plot of the factual assignment rule
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