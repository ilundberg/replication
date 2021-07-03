# Quantifying the contribution of occupational
# segregation to racial disparities in health:
# A gap-closing perspective

# Ian Lundberg
# ilundberg@princeton.edu

# See run_all.R where this file is called.

# Start with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))

# Record printed output in a text file
sink("logs/fig07.txt")

# Record the time that this code file begins.
t0 <- Sys.time()
print("Date and time of code run:")
print(t0)

# Load packages
library(tidyverse)
library(reshape2)


counterfactual_results <- readRDS("intermediate/counterfactual_results.Rds")

# Panel A: Factual and counterfactual proportions
plot <- counterfactual_results$counterfactual_estimate %>%
  filter(target == "proportion" & estimand %in% c("factual","counterfactual_within_educ")) %>%
  group_by() %>%
  rename(Estimand = estimand) %>%
  mutate(Estimand = case_when(Estimand == "factual" ~ "Factual",
                              Estimand == "counterfactual_within_educ" ~ "Counterfactual"),
         Estimand = fct_rev(Estimand),
         RACE = gsub(" ","\n",RACE),
         RACE = fct_relevel(RACE,"Non-Hispanic\nBlack","Non-Hispanic\nWhite","Hispanic","Other")) %>%
  ggplot(aes(x = RACE, y = point, color = Estimand, shape = Estimand,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"))) +
  geom_errorbar(position = position_dodge(width = .5),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .5)) +
  geom_text(position = position_dodge(width = .5),
            aes(hjust = ifelse(Estimand == "Factual", 1.2, -.2)),
            size = 4,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold"))
ggsave("figures/proportions.pdf",
       height = 3.75, width  = 6.5, plot = plot)

# Panel B: Factual and counterfactual disparities
plot <- counterfactual_results$counterfactual_estimate %>%
  filter(target == "disparity_vs_white" & estimand %in% c("factual","counterfactual_within_educ")) %>%
  group_by() %>%
  rename(Estimand = estimand) %>%
  mutate(Estimand = case_when(Estimand == "factual" ~ "Factual Disparity",
                              Estimand == "counterfactual_within_educ" ~ "Counterfactual Disparity"),
         Estimand = fct_rev(Estimand),
         RACE = paste0(RACE,"\n- Non-Hispanic White"),
         RACE = fct_relevel(RACE,"Non-Hispanic Black\n- Non-Hispanic White","Hispanic\n- Non-Hispanic White","Other\n- Non-Hispanic White")) %>%
  ggplot(aes(x = RACE, y = point, color = Estimand, shape = Estimand,
             ymin = point - qnorm(.975) * se,
             ymax = point + qnorm(.975) * se,
             label = paste0(format(round(100*point,1),nsmall = 1),"%"))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = position_dodge(width = .5),
                width = .2, size = .2) +
  geom_point(position = position_dodge(width = .5)) +
  geom_text(position = position_dodge(width = .5),
            aes(hjust = ifelse(Estimand == "Factual Disparity", 1.15, -.15)),
            size = 4,
            show.legend = F, fontface = "bold") +
  theme_bw() +
  scale_y_continuous(name = "Disparity in Onset of\nWork-Limiting Disability",
                     labels = function(x) paste0(format(round(100 * x, 1), nsmall = 1),"%")) +
  xlab("Race / Ethnicity Contrast") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.title = element_text(face = "bold"))
ggsave("figures/disparity.pdf",
       height = 3.75, width  = 6.5, plot = plot)

print("Note the proportional change in the disparity")
print("These are added to the figure in LaTeX")
print(
  data.frame(counterfactual_results$counterfactual_estimate %>% 
               filter(estimand %in% c("counterfactual_within_educ","factual")) %>%
               filter(target == "disparity_vs_white") %>%
               select(target, RACE, estimand, point) %>%
               spread(key = estimand, value = point) %>%
               mutate(factor_change = counterfactual_within_educ / factual))
)

print("Time spent")
print(difftime(Sys.time(),t0))

# Close the text output file
sink()

# End with an environment with no objects so this code can be run independently
rm(list = ls(all = TRUE))