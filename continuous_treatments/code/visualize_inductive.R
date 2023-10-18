library(tidyverse)

source("code/label_outcomes_treatments.R")

inductive <- readRDS("intermediate/inductive.RDS")

# TEMP TESTING
inductive <- inductive %>%
  arrange(ci.max_learning) %>%
  mutate(rank_small = 1:n())

for (estimand_value in c("big","small")) {
  for (outcome_value in unique(inductive$outcome)) {
    for (delta_value in unique(inductive$delta)) {
      if (estimand_value == "big") {
        forplot <- inductive %>%
          filter(num_learning >= 100 & num_estimation >= 100) %>%
          arrange(rank_big) %>%
          slice_head(n = 3)
      } else if (estimand_value == "small") {
        forplot <- inductive %>%
          filter(num_learning >= 100 & num_estimation >= 100) %>%
          arrange(rank_small) %>%
          slice_head(n = 3)
      }
      forplot %>%
        filter(outcome == outcome_value & delta == delta_value) %>%
        mutate(yval = paste0(gsub("[\n]"," ",educJoint),"\n",
                             race,"\n",
                             "Wealth ",gsub("[\n]"," ",tolower(label_wealth)),"\n",
                             "Income ",gsub("[\n]"," ",tolower(label_income))),
               yval = fct_reorder(yval, estimate_learning)) %>%
        ungroup() %>%
        select(yval, estimate_estimation, estimate_learning,
               ci.min_estimation, ci.min_learning, ci.max_estimation, ci.max_learning) %>%
        pivot_longer(cols = -yval) %>%
        separate(name, into = c("quantity","set"), sep = "_") %>%
        pivot_wider(names_from = "quantity", values_from = "value") %>%
        mutate(label = format(round(estimate, 3),nsmall = 3)) %>%
        mutate(set = ifelse(set == "estimation", "Estimation", "Learning")) %>%
        rename(Set = set) %>%
        ggplot(aes(y = yval, color = Set,
                   x = estimate, 
                   xmin = ci.min, xmax = ci.max,
                   label = label)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
        geom_errorbar(position = position_dodge(width = -.2),
                      width = .1) +
        geom_point(position = position_dodge(width = -.2)) +
        geom_text(aes(vjust = ifelse(Set == "Estimation",-1,2)), 
                  position = position_dodge(width = -.2),
                  size = 3, show.legend = F) +
        ylab("Population Subgroup") +
        xlab(paste0("Conditional Average Causal Effect\nof a $",
                    prettyNum(delta_value, big.mark = ","),
                    " Increase in Parent Income"))  +
        ggtitle(paste0("Outcome: ",gsub("\n"," ",label_outcomes_treatments(outcome_value)))) +
        theme(plot.title = element_text(size = 10))
      ggsave(paste0("figures/inductive_",estimand_value,"_",outcome_value,"_",delta_value,".pdf"),
             height = 3.5, width = 6.5)
    }
  }
}
