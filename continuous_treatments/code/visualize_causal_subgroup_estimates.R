
source("code/shorten_race.R")

for (outcome in outcomes) {
  print(toupper(outcome))
  
  # Load results for this outcome
  estimate.out <- readRDS(paste0("intermediate/causal_",
                                 outcome,".RDS"))
  
  # Notes about the severity of looking at subgroups
  print("Subgroups that are never seen")
  
  all_subgroups <- estimate.out$data %>%
    select(race, educJoint, 
           label_wealth, label_income) %>%
    expand(race, educJoint, label_wealth, label_income)
  counts <- estimate.out$data %>%
    group_by(race, educJoint, label_wealth, label_income) %>%
    count()
  print(
    all_subgroups %>%
      left_join(counts) %>%
      filter(is.na(n))
  )
  print("Subgroups that are very sparse:")
  print(
    estimate.out$data %>%
      group_by(race, educJoint, label_wealth, label_income) %>%
      count() %>%
      arrange(n)
  )
  print("Number of subgroups with at least 100:")
  print(
    estimate.out$data %>%
      group_by(race, educJoint, label_wealth, label_income) %>%
      summarize(num = n(), .groups = "drop") %>%
      filter(num >= 100) %>%
      summarize(num_subgroups = n(), .groups = "drop")
  )
  
  print("Prop of sample in those subgroups:")
  print(
    estimate.out$data %>%
      group_by(race, educJoint, label_wealth, label_income) %>%
      mutate(populated = n() >= 100) %>%
      ungroup() %>%
      summarize(prop_sample = mean(populated),
                weighted = weighted.mean(populated, w = w))
  )
  
  for (delta_value in delta_values) {
    
    subgroup_estimates <- estimate.out$estimate %>%
      filter(delta == delta_value) %>%
      left_join(estimate.out$data %>% select(-w), by = "PUBID") %>%
      group_by(race, educJoint, label_wealth, label_income) %>%
      summarize(estimate = weighted.mean(effect, w = w),
                num = n(),
                .groups = "drop") %>%
      left_join(estimate.out$bootstrap%>%
                  filter(delta == 10e3) %>%
                  left_join(estimate.out$data %>% select(-w), by = "PUBID") %>%
                  group_by(race, educJoint, label_wealth, label_income, bs) %>%
                  summarize(estimate = weighted.mean(effect, w = w),
                            .groups = "drop_last") %>%
                  summarize(se = sd(estimate),
                            .groups = "drop"),
                by = c("race","educJoint","label_wealth","label_income")) %>%
      mutate(ci.min = estimate - qnorm(.975) * se,
             ci.max = estimate + qnorm(.975) * se) %>%
      filter(num >= 100) %>%
      arrange(estimate) %>%
      mutate(rank = 1:n()) %>%
      mutate(
        group_label = paste0(
          gsub("[\n]"," ",educJoint),"\n",
          shorten_race(race),"\n",
          "Wealth ",gsub("[\n]"," ",tolower(label_wealth)),"\n",
          "Income ",gsub("[\n]"," ",tolower(label_income))
        ),
        group_label = fct_reorder(group_label, estimate)
      )
    
    subgroup_estimates %>%
      ggplot(aes(x = estimate, xmin = ci.min, xmax = ci.max,
                 y = group_label, label = format(round(estimate,3),nsmall = 3))) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
      geom_errorbar(width = .2) +
      geom_label(size = 3) +
      theme(axis.text.y = element_text(size = 8)) +
      ylab("Population Subgroup") +
      xlab(paste0("Effect on ",
                  case_when(outcome == "enrolled_any" ~ "College Enrollment",
                            outcome == "enrolled_4yr" ~ "4-Year College Enrollment",
                            outcome == "completed_25" ~ "BA Completion by Age 25",
                            outcome == "completed_30" ~ "BA Completion by Age 30"),
                  " of\na $",
                  prettyNum(delta_value, big.mark = ","),
                  " Increase in Parent Income"))
    ggsave(paste0("figures/many_subgroups_",outcome,"_",delta_value,".pdf"),
           height = 12, width = 6.5)
  }
}
