# Simulation showing the four possible settings

library(tidyverse)

tibble(a = rep(seq(0,1,.01), 2)) |>
  mutate(
    x = rep(1:2, each = 101)
  ) |>
  mutate(
    linear_homogeneous = x + a,
    linear_heterogeneous = 1 + .2 * x + ifelse(x == 1, 1, -.5) * a,
    nonlinear_homogeneous = .1 + .5 * x + .8*(log(a + .1) - log(.1)),
    nonlinear_heterogeneous = case_when(
      x == 1 ~ .1 + .5 * x + .8*(log(a + .1) - log(.1)),
      x == 2 ~ .1 + .5 * x + 2*(a - .5) ^ 2
    )
  ) |>
  pivot_longer(cols = -c("x","a"), values_to = "y") |>
  separate(name, into = c("linearity","homogeneity")) |>
  mutate(homogeneity = fct_rev(homogeneity)) |>
  ggplot(aes(x = a, y = y, color = paste0("X = ",x), linetype = paste0("X = ",x))) +
  geom_line() +
  facet_grid(
    homogeneity ~ linearity,
    labeller = as_labeller(function(x) {
      case_when(
        x == "homogeneous" ~ "Homogeneous\nEffects Across X",
        x == "heterogeneous" ~ "Heterogeneous\nEffects Across X",
        x == "linear" ~ "Linear Function\nof Treatment A",
        x == "nonlinear" ~ "Nonlinear Function\nof Treatment A"
      )
    })
  ) +
  labs(
    color = "Subgroup",
    linetype = "Subgroup",
    x = "Treatment A",
    y = "Outcome Y"
  ) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave("figures/nonlinear_heterogeneous_conceptual.pdf", height = 3, width = 4.5)