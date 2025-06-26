
visualize_causal <- function(model = "logit") {
  
  source("code/label_outcomes_treatments.R")
  
  print(model)
  
  # Function to aggregate individual estimates to population
  # and subpopulation summaries
  aggregator <- function(groups) {
    
    to_return <- foreach(outcome_name = outcomes, .combine = "rbind") %do% {
      
      # Load this result
      if (model == "logit") {
        estimate.out <- readRDS(paste0("intermediate/causal_",
                                       outcome_name,".RDS"))
      } else if (model == "forest") {
        estimate.out <- readRDS(paste0("intermediate/causal_",
                                       outcome_name,"_forest.RDS"))
      } else if (model == "gam") {
        estimate.out <- readRDS(paste0("intermediate/causal_",
                                       outcome_name,"_gam.RDS"))
      } else {
        stop("model object should be the name of a model you have estimated")
      }
      
      # Create data frame of group information
      if (is.null(groups)) {
        group_info <- estimate.out$data %>%
          select(PUBID)
      } else {
        group_info <- estimate.out$data %>%
          select(PUBID, matches(paste0("^",groups,"$")))
      }
      
      # Calculate point estimate
      point <- estimate.out$estimate %>%
        left_join(group_info, by = "PUBID") %>%
        group_by_at(vars(all_of(c("delta",groups)))) %>%
        summarize(estimate = weighted.mean(effect, w = w),
                  .groups = "drop")
      se <- estimate.out$bootstrap %>%
        left_join(group_info, by = "PUBID") %>%
        group_by_at(vars(c("delta",all_of(groups),"bs"))) %>%
        summarize(estimate = weighted.mean(effect, w = w),
                  .groups = "drop_last") %>%
        summarize(se = sd(estimate),
                  .groups = "drop")
      point %>%
        left_join(se, by = c("delta",groups)) %>%
        mutate(ci.min = estimate - qnorm(.975) * se,
               ci.max = estimate + qnorm(.975) * se) %>%
        mutate(outcome = outcome_name,
               ybar0 = weighted.mean(estimate.out$data$y,
                                     w = estimate.out$data$w),
               outcome = outcome_name)
    }
    return(to_return)
  }
  
  plot_outcomes_treatments <- function(
    data,
    xvar,
    xlab = "Population Subgroup",
    ylab = "Outcome",
    height = 10,
    width = 10
  ) {
    source("code/label_outcomes_treatments.R")
    
    # Plot with all outcomes and treatments
    this_plot <- data %>%
      rename_with(.fn = function(x) str_replace_all(x,xvar,"xvar")) %>%
      ggplot(aes(x = xvar, y = estimate,
                 ymin = ci.min, ymax = ci.max)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      geom_point() +
      geom_errorbar(width = .2) +
      geom_text(aes(label = format(round(estimate,3),nsmall=3)),
                nudge_x = .2,
                size = 3) +
      facet_grid(outcome ~ delta,
                 switch = "y",
                 labeller = label_outcomes_treatments) +
      ylab(ylab) +
      xlab(xlab) +
      ggtitle("Treatment") +
      theme(strip.placement = "outside",
            axis.title = element_text(size = 12, hjust = .5, face = "bold"),
            plot.title = element_text(size = 12, hjust = .5, face = "bold"))
    ggsave(file = paste0("figures/",ifelse(model == "logit", "", paste0(model,"_")),"by_",xvar,"_all.pdf"),
           plot = this_plot,
           height = height, width = width)
    
    # Plot with focal outcome and treatment
    this_plot <- data %>%
      filter(outcome == "enrolled_any" & delta == 10e3) %>%
      rename_with(.fn = function(x) str_replace_all(x,xvar,"xvar")) %>%
      ggplot(aes(x = xvar, y = estimate,
                 ymin = ci.min, ymax = ci.max,
                 label = format(round(estimate,3),nsmall=3))) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      geom_errorbar(width = .2) +
      geom_label() +
      ylab("Effect on College Enrollment") +
      xlab(xlab) +
      ylim(c(-.0025,.04))
    ggsave(file = paste0("figures/",ifelse(model == "logit", "", paste0(model,"_")),"by_",xvar,".pdf"),
           plot = this_plot,
           height = 3, width = 4.5)
  }
  
  print("POPULATION AVERAGE EFFECT")
  aggregator(groups = NULL) %>%
    print() %>%
    mutate(average = "Population\nAverage") %>%
    plot_outcomes_treatments(
      xvar = "average",
      xlab = NULL
    )
  print("WITHIN EDUCATION")
  aggregator(groups = "educJoint") %>%
    # print, shortening names so output not truncated
    (function(.data) {
      print(.data %>%
              mutate(educJoint = case_when(grepl("Two",educJoint) ~ 2,
                                           grepl("One",educJoint) ~ 1,
                                           grepl("No",educJoint) ~ 0 )))
      return(.data)
    }) %>%
    mutate(educJoint = fct_rev(educJoint)) %>%
    plot_outcomes_treatments(
      xvar = "educJoint",
      xlab = "Parent Education"
    )
  print("WITHIN INCOME TERCILE")
  aggregator(groups = "label_income") %>%
    # print, shortening names so output not truncated
    (function(.data) {
      print(.data %>%
              mutate(label_income = case_when(grepl("Low", label_income) ~ "Low",
                                              grepl("Middle", label_income) ~ "Mid",
                                              grepl("High", label_income) ~ "High")))
      return(.data)
    }) %>%
    plot_outcomes_treatments(
      xvar = "label_income",
      xlab = "Parent Income Tercile"
    )
  print("WITHIN WEALTH TERCILE")
  aggregator(groups = "label_wealth") %>%
    # print, shortening names so output not truncated
    (function(.data) {
      print(.data %>%
              mutate(label_wealth= case_when(grepl("Low", label_wealth) ~ "Low",
                                             grepl("Middle", label_wealth) ~ "Mid",
                                             grepl("High", label_wealth) ~ "High")))
      return(.data)
    }) %>%
    plot_outcomes_treatments(
      xvar = "label_wealth",
      xlab = "Parent Wealth Tercile"
    )
  
  
  # Visualize CATE estimates by income
  forplot <- aggregator(groups = c("income","PUBID")) |>
    filter(delta == 10e3 & outcome == "enrolled_any")
  forplot |>
    ggplot(aes(x = income, y = estimate)) + 
    geom_point(color = "gray", size = .2, alpha = .2) + 
    scale_x_continuous(
      labels = scales::label_currency(),
      name = "Family Income at Age 17",
      limits = c(0,325e3)
    ) +
    ylab(
      "Effect of Additional $10,000\nOn Any College Enrollment"
    )
  ggsave(paste0("figures/",ifelse(model == "logit", "", paste0(model,"_")),"effect_by_income.pdf"),
         height = 3, width = 4)
  
  # Visualize the histogram of CATE estimates
  cate_all <- foreach(outcome_name = outcomes, .combine = "rbind") %do% {
    estimate.out <- readRDS(paste0("intermediate/causal_",
                                   outcome_name,".RDS"))
    return(estimate.out$estimate %>%
             mutate(outcome = outcome_name))
  }
  cate_all %>%
    filter(delta == 10e3 & outcome == "enrolled_any") %>%
    (function(.data) {
      print(paste0("Below 0 dropped from histogram. Weighted prop: ",
                   weighted.mean(.data$effect < 0, w = .data$w)))
      return(.data %>%
               filter(effect >= 0))
    }) %>%
    ggplot(aes(x = effect, weight = w)) +
    geom_histogram(bins = 20, boundary = 0) +
    xlab("Conditional Average Causal Effect\nof Additional $10,000 on College Enrollment") +
    ylab("Weighted\nCount")
  ggsave(paste0("figures/",ifelse(model == "logit", "", paste0(model,"_")),"effect_histogram.pdf"),
         height = 2.5, width = 4)
  
  cate_all %>%
    ggplot(aes(x = effect, weight = w)) +
    geom_histogram() +
    xlab("Conditional Average Causal Effect") +
    ylab("Weighted Count") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_grid(outcome ~ delta,
               labeller = label_outcomes_treatments)
  ggsave(paste0("figures/",ifelse(model == "logit", "", paste0(model,"_")),"effect_histogram_all.pdf"),
         height = 7.5, width = 6.5)
  
  # VISUALIZE CAUSAL VS DESCRIPTIVE
  ate_descriptive <- readRDS("intermediate/descriptive_smooths.RDS") %>%
    filter(grepl("delta",estimand)) %>%
    mutate(delta = as.numeric(gsub("delta_","",estimand))) %>%
    select(outcome, delta, estimate, ci.min, ci.max) %>%
    mutate(estimand = "Unadjusted")
  
  ate_comparison <- aggregator(NULL) %>%
    mutate(estimand = "Adjusted for\nConfounders") %>%
    bind_rows(ate_descriptive) %>%
    mutate(estimand = fct_rev(estimand)) %>%
    mutate(label = format(round(estimate,3), nsmall = 3))
  
  ate_comparison %>%
    ggplot(aes(x = estimand, y = estimate,
               ymin = ci.min, ymax = ci.max,
               label = label)) +
    geom_point() +
    geom_errorbar(width = .2) +
    geom_text(nudge_x = .3, size = 3) +
    facet_grid(outcome ~ delta,
               labeller = label_outcomes_treatments) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ylab("Average Causal Effect Estimate") +
    xlab("Estimator")
  ggsave(paste0("figures/",ifelse(model == "logit", "", paste0(model,"_")),"ate_comparison_all.pdf"),
         height = 7.5, width = 6.5)
  
  ate_comparison %>%
    filter(outcome == "enrolled_any" & delta == 10e3) %>%
    ggplot(aes(x = estimand, y = estimate,
               ymin = ci.min, ymax = ci.max,
               label = label)) +
    geom_errorbar(width = .2) +
    geom_label() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ylab("Effect on College Enrollment") +
    xlab("Estimator")
  ggsave(paste0("figures/",ifelse(model == "logit", "", paste0(model,"_")),"ate_comparison.pdf"),
         height = 3, width = 3)
  
}