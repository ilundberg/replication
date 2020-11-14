
# Replication code for:
# What is Your Estimand? Defining the Target Quantity Connects Statistical Evidence to Theory
# Ian Lundberg, Rebecca Johnson, and Brandon Stewart
# Email: ilundberg@princeton.edu

# This file carries out the analysis for the illustration in
# For Analysts: Estimands Ground Methodological Choices

# This file does not require any data.

library(tidyverse)
library(reshape2)

set.seed(08544)
n <- 100

data.frame(x = seq(.2,.4,.01)) %>%
  mutate(Under_Treatment = 2*x,
         Under_Control = x,
         Risk_Ratio = 2,
         Risk_Difference = x) %>%
  melt(id = "x", 
       variable.name = "Estimand", 
       value.name = "y") %>%
  mutate(set = case_when(Estimand %in% c("Under_Treatment","Under_Control") ~ "Probability of Y = 1",
                         Estimand == "Risk_Ratio" ~ "Causal Risk Ratio",
                         Estimand == "Risk_Difference" ~ "Causal Risk Difference"),
         set = fct_rev(set),
         Estimand = str_replace(Estimand,"\\_","")) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(group = Estimand), size = .5) +
  scale_y_continuous(name = element_blank(), labels = function(x) format(round(x,2), nsmall = 2, digits = 2)) +
  xlab("Pre-Treatment Covariate X") +
  facet_wrap(~set, scales = "free_y", strip.position = "left") +
  theme_minimal() +
  theme(strip.placement = "outside") +
  geom_text(data = data.frame(set = c(rep("Probability of Y = 1",2),
                                      "Causal Risk Ratio", "Causal Risk Difference"),
                              x = c(.35,.35,.3, .3), y = c(.35, .7, 2, .28),
                              label = c("(2) Under\nControl","(1) Under\nTreatment",
                                        "(1) / (2)\n\nNo Interaction","(1) - (2)\n\nInteraction Visible")),
            aes(label = label, 
                vjust = c(-.1,-.1,.5,.5),
                hjust = c(1.1,1.1,.5,.5)),
            size = 3) +
  # Fix the scales by adding invisible points
  geom_point(data = data.frame(set = c(rep("Probability of Y = 1",4),
                                       rep("Causal Risk Ratio",2),rep("Causal Risk Difference",2)),
                               x = rep(.35,8), y = c(0,1,0,1,1,3,0,1),
                               label = c("(2) Under\nControl","(1) Under\nTreatment",
                                         "(1) / (2)","(1) - (2)")),
             alpha = 0) +
  ggsave("output/risk_ratio_difference.pdf",
         height = 2.5, width = 6.5)
