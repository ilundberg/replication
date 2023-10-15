
sink("../logs/visualize_motherhood.txt")

t0 <- Sys.time()
print(t0)

library(tidyverse)

motherhood_result <- readRDS("../intermediate/motherhood_result.RDS")
fatherhood_result <- readRDS("../intermediate/fatherhood_result.RDS")


forplot <- motherhood_result %>%
  filter(grepl("effect_m",estimand)) %>%
  #mutate(treatment = "'Effect of'~bold('Motherhood')") %>%
  mutate(treatment = "Effect of Motherhood") %>%
  bind_rows(
    fatherhood_result %>%
      filter(grepl("effect_m",estimand)) %>%
      #mutate(treatment = "'Effect of'~bold('Fatherhood')")
      mutate(treatment = "Effect of Fatherhood")
  ) %>%
  mutate(treatment = fct_rev(treatment))
p <- forplot %>%
  ggplot(aes(y = treatment, x = estimate,
             xmin = ci.min, xmax = ci.max,
             label = label, alpha = treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .2) +
  theme_bw() +
  scale_y_discrete(name = element_blank()) +
  xlab("Effect on Employment") +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12))
p + scale_alpha_manual(values = c(0,0))
ggsave("../figures/parenthood_employment_1.pdf",
       height = 2, width = 5)
p + geom_label(data = forplot %>% filter(grepl("Fatherhood",treatment))) +
  scale_alpha_manual(values = c(0,1))
ggsave("../figures/parenthood_employment_2.pdf",
       height = 2, width = 5)
p + geom_label(data = forplot) +
  scale_alpha_manual(values = c(1,1))
ggsave("../figures/parenthood_employment_3.pdf",
       height = 2, width = 5)

forplot <- motherhood_result %>%
  filter(grepl("effect_y",estimand))
p <- forplot %>%
  ggplot(aes(y = estimand, x = estimate,
             xmin = ci.min, xmax = ci.max,
             label = label, color = estimand,
             alpha = estimand)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .2) +
  theme_bw() +
  scale_y_discrete(name = "Estimate",
                   labels = function(x) {
                     case_when(x == "effect_y_lower" ~ "Lower Bound",
                               x == "effect_y_naive" ~ "Estimate if\nNo Selection",
                               x == "effect_y_upper" ~ "Upper Bound")
                   }) +
  xlab("Effect of a Motherhood on Log Wage") +
  theme(plot.margin = unit(c(0,.2,0,0),"in"),
        legend.position = "none") +
  scale_alpha_manual(values = c(0,0,0)) +
  scale_color_manual(values = c("blue","black","seagreen4"))
ggsave("../figures/motherhood_1.pdf",
       height = 2, width = 4.5)
p +
  geom_label(data = forplot %>% filter(grepl("naive",estimand))) +
  scale_alpha_manual(values = c(0,1,0))
ggsave("../figures/motherhood_2.pdf",
       height = 2, width = 4.5)
p +
  geom_label(data = forplot %>% filter(grepl("naive|lower",estimand))) +
  scale_alpha_manual(values = c(1,1,0))
ggsave("../figures/motherhood_3.pdf",
       height = 2, width = 4.5)
p4 <- p +
  geom_label(data = forplot) +
  scale_alpha_manual(values = c(1,1,1))
p4
ggsave("../figures/motherhood_4.pdf",
       height = 2, width = 4.5)
p5 <- p4 +
  annotate(geom = "segment",
           x = motherhood_result$ci.min[motherhood_result$estimand == "effect_y_lower"],
           xend = motherhood_result$ci.max[motherhood_result$estimand == "effect_y_naive"],
           y = 1.5, yend = 1.5,
           linewidth = 5,
           color = "blue")
p5
ggsave("../figures/motherhood_5.pdf",
       height = 2, width = 4.5)
p6 <- p5 +
  annotate(geom = "segment",
           x = motherhood_result$ci.min[motherhood_result$estimand == "effect_y_naive"],
           xend = motherhood_result$ci.max[motherhood_result$estimand == "effect_y_upper"],
           y = 2.5, yend = 2.5,
           linewidth = 5,
           color = "seagreen4")
p6
ggsave("../figures/motherhood_6.pdf",
       height = 2, width = 4.5)
  
ggsave("../figures/motherhood.pdf",
       height = 2, width = 4.5)
p1 <- p +
  annotate(geom = "errorbar",
           xmin = motherhood_result$ci.min[motherhood_result$estimand == "effect_y_lower"],
           xmax = motherhood_result$ci.max[motherhood_result$estimand == "effect_y_naive"],
           y = 1.5,
           size = 1.2,
           width = .15,
           color = "blue")
p1
ggsave("../figures/motherhood_annotated_1.pdf",
       height = 2, width = 4.5)
p2 <- p1 +
  annotate(geom = "errorbar",
           xmin = motherhood_result$ci.min[motherhood_result$estimand == "effect_y_naive"],
           xmax = motherhood_result$ci.max[motherhood_result$estimand == "effect_y_upper"],
           y = 2.5,
           size = 1.2,
           width = .15,
           color = "seagreen4")
p2
ggsave("../figures/motherhood_annotated_2.pdf",
       height = 2, width = 4.5)

p <- fatherhood_result %>%
  filter(grepl("effect_y",estimand)) %>%
  ggplot(aes(y = estimand, x = estimate,
             xmin = ci.min, xmax = ci.max,
             label = label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .2) +
  geom_label(size = 2.5, label.padding = unit(.18,"lines")) +
  theme_bw() +
  scale_y_discrete(name = "Estimate",
                   labels = function(x) {
                     case_when(x == "effect_y_lower" ~ "Lower Bound",
                               x == "effect_y_naive" ~ "Estimate if\nNo Selection",
                               x == "effect_y_upper" ~ "Upper Bound")
                   }) +
  xlab("Effect of a Fatherhood on Log Wage") +
  theme(plot.margin = unit(c(0,.2,0,0),"in"))
p
ggsave("../figures/fatherhood.pdf",
       height = 2, width = 4.5)
p1 <- p +
  annotate(geom = "errorbar",
           xmin = fatherhood_result$ci.min[fatherhood_result$estimand == "effect_y_lower"],
           xmax = fatherhood_result$ci.max[fatherhood_result$estimand == "effect_y_naive"],
           y = 2.5,
           size = 1.2,
           width = .15,
           color = "seagreen4")
p1
ggsave("../figures/fatherhood_annotated_1.pdf",
       height = 2, width = 4.5)
p2 <- p1 +
  annotate(geom = "errorbar",
           xmin = fatherhood_result$ci.min[fatherhood_result$estimand == "effect_y_naive"],
           xmax = fatherhood_result$ci.max[fatherhood_result$estimand == "effect_y_upper"],
           y = 1.5,
           size = 1.2,
           width = .15,
           color = "blue")
p2
ggsave("../figures/fatherhood_annotated_2.pdf",
       height = 2, width = 4.5)

sessionInfo()

print("Finish time")
print(Sys.time())
print("Time spent")
difftime(Sys.time(),t0)

sink()