# function to shorten race / ethnicity labels for figures
shorten_race <- function(value) {
  case_when(value == "Hispanic" ~ "Hispanic",
            value == "Non-Hispanic Black" ~ "Black",
            value == "Non-Black Non-Hispanic" ~ "White or other")
}
