
source("code/weighted.quantile.R")
for (outcome_name in outcomes) {
  for (delta_value in delta_values) {
    print(paste(toupper(outcome_name),delta_value))
    # Load this result
    estimate.out <- readRDS(paste0("intermediate/causal_",
                                   outcome_name,".RDS"))
    
    # Categorize effect size groups
    effect_groups <- estimate.out$estimate %>%
      filter(delta == delta_value) %>%
      left_join(estimate.out$data %>%
                  select(-w),
                by = "PUBID") %>%
      (function(.data) {
        cutoffs <- c(
          weighted.quantile(.data$effect, q = .25, w = .data$w),
          weighted.quantile(.data$effect, q = .75, w = .data$w)
        )
        .data %>%
          mutate(effect_group = factor(case_when(effect < cutoffs[1] ~ 1,
                                                 effect < cutoffs[2] ~ 2,
                                                 effect >= cutoffs[2] ~ 3),
                                       labels = c("Bottom Quartile of Effect Size",
                                                  "Middle 50% of Effect Size",
                                                  "Top Quartile of Effect Size")))
      })
    
    # Function to summarize proportions in effect size groups
    summarize_proportion <- function(.data, variable) {
      .data %>%
        select(effect_group, matches(variable), w) %>%
        group_by(effect_group) %>%
        mutate(denominator = sum(w)) %>%
        group_by_at(vars(all_of(c("effect_group",variable)))) %>%
        mutate(numerator = sum(w)) %>%
        summarize(proportion = mean(numerator / denominator),
                  .groups = "drop") %>%
        mutate(proportion = format(round(proportion,2),nsmall = 2)) %>%
        pivot_wider(names_from = effect_group, values_from = proportion) %>%
        rename_at(vars(matches(variable)),function(x) "label") %>%
        mutate(label = paste("Proportion",label))
    }
    # Function to summarize median in effect size groups
    summarize_median <- function(.data, variable) {
      .data %>%
        group_by(effect_group) %>%
        rename_at(vars(matches(paste0("^",variable,"$"))),
                  function(x) "focal_variable") %>%
        summarize(focal_variable = weighted.quantile(focal_variable, w = w, q = .5)) %>%
        rename_at(vars(matches("focal_variable")), function(x) variable) %>%
        mutate_at(vars(matches(variable)), scales::label_dollar()) %>%
        mutate(label = paste("Median",variable)) %>%
        pivot_wider(names_from = effect_group, values_from = matches(variable))
    }
    
    myLabeler <- function(number) {
      format(round(number,2), nsmall = 2)
    }
    
    print(xtable::xtable(
      effect_groups %>%
        group_by(effect_group) %>%
        summarize(min_effect = min(effect),
                  max_effect = max(effect),
                  .groups = "drop") %>%
        mutate(range = paste(myLabeler(min_effect),"to",myLabeler(max_effect)),
               label = "Range of effect sizes") %>%
        select(label, effect_group, range) %>%
        pivot_wider(names_from = effect_group, values_from = range) %>%
        bind_rows(effect_groups %>%
                    summarize_proportion("y") %>%
                    filter(!grepl("FALSE",label)) %>%
                    mutate(label = "Factual outcome: Proportion enrolled in college")) %>%
        bind_rows(effect_groups %>%
                    summarize_median("income")) %>%
        bind_rows(effect_groups %>%
                    summarize_median("wealth")) %>%
        bind_rows(effect_groups %>%
                    summarize_proportion("educJoint")) %>%
        bind_rows(effect_groups %>%
                    summarize_proportion("race")),
      caption = "Characteristics of those with low, middle, and high causal effects"
    ), include.rownames = F)
  }
}
  
