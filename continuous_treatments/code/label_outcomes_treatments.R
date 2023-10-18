label_outcomes_treatments <- as_labeller(function(x) case_when(
  x == "completed_25" ~ "Completed 4-year degree\nby age 25",
  x == "completed_30" ~ "Completed 4-year degree\nby age 30",
  x == "enrolled_4yr" ~ "Enrolled in 4-year college\nby age 21",
  x == "enrolled_any" ~ "Enrolled in any college\nby age 21",
  x == "delta_10000" ~ "Effect of $10,000",
  x == "delta_25000" ~ "Effect of $25,000",
  x == "10000" ~ "Effect of $10,000",
  x == "25000" ~ "Effect of $25,000",
  T ~ x
))
