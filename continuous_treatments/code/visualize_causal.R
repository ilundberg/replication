
visualize_causal <- function(model = "logit") {
  
  print(model)
  
  # Function to aggregate individual estimates to population
  # and subpopulation summaries
  aggregator <- function(groups) {
    
    to_return <- foreach(outcome_name = outcome_name, .combine = "rbind") %do% {
      
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
  
}