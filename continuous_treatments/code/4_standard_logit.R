
sink("logs/4_standard_logit.txt")

print("This file carries out the analysis as it might be done by someone using a standard logit approach. It serves a mostly pedagogical purpose for our paper as a foil for what we wil do.")

library(tidyverse)
theme_set(theme_bw())

prepared <- readRDS("intermediate/prepared.RDS") %>%
  mutate(educJoint = fct_rev(educJoint))

unadjusted <- glm(enrolled ~ log(income), 
                  data = prepared,
                  family = binomial,
                  weights = w)
adjusted <- glm(enrolled ~ log(income) + race + educJoint + log(wealth),
                data = prepared,
                family = binomial,
                weights = w)

# note log odds change the same for everybody
coef(unadjusted)["log(income)"]
coef(adjusted)["log(income)"]

stargazer::stargazer(unadjusted,adjusted,
                     star.cutoffs = c(.05,.01,.001))

# note conversion to average predicted probability
d1 <- prepared %>% mutate(income = income + 25e3)
forplot_classic <- prepared %>%
  mutate(Unadjusted = predict(unadjusted, newdata = d1, type = "response") -
           predict(unadjusted, type = "response"),
         Adjusted = predict(adjusted, newdata = d1, type = "response") -
           predict(adjusted, type = "response")) %>%
  select(PUBID, Unadjusted, Adjusted, w) %>%
  pivot_longer(cols = c("Unadjusted","Adjusted"), names_to = "estimand", values_to = "estimate") %>%
  mutate(estimand = fct_rev(estimand))
averaged <- forplot_classic %>% group_by(estimand) %>% summarize(estimate = weighted.mean(estimate, w = w))

forplot_classic %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(aes(weight = w), fill = "gray", alpha = .8) +
  geom_vline(data = averaged,
             aes(xintercept = estimate)) +
  geom_text(data = averaged,
            y = 600, hjust = 0,
            aes(label = paste("  Average:", format(round(estimate, 2), nsmall = 2)))) +
  facet_wrap(~estimand, ncol = 1) +
  xlab("Change in P(College Enrollment)\nfor $25,000 Change in Income") +
  ylab("Weighted Count")
ggsave("figures/classic_approach.pdf",
       height = 5, width = 6.5)

sessionInfo()
sink()
