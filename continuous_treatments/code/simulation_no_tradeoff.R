
theme_set(theme_bw())

curve_linear <- function(x,a) {
  (x == 1) * (0 + a) +
    (x == 2) * (.15 + .5 * a) +
    (x == 3) * (.45)
}
curve_nonlinear <- function(a) {
  (a <= .3) * (0 + a) +
    (a > .3 & a <= .6) * (.15 + .5 * a) +
    (a > .6) * (.45)
}

# Show the data
data <- tibble(
  x = rep(1:3, each = 3), 
  a = c(.2, .25, .3, .45, .5, .55, .7, .75, .8)
) |>
  mutate(
    y = curve_linear(x, a)
  )
data |>
  ggplot(aes(x = a, y = y, color = factor(x), shape = factor(x))) +
  geom_point() +
  scale_color_discrete(
    labels = \(x) paste0("X = ",x),
    name = "Pre-Treatment\nPopulation\nSubgroup"
  ) +
  scale_shape_discrete(
    labels = \(x) paste0("X = ",x),
    name = "Pre-Treatment\nPopulation\nSubgroup"
  ) +
  labs(
    x = "Treatment Value",
    y = "\nOutcome Value"
  ) +
  ylim(c(0,1)) +
  scale_x_continuous(
    labels = as.character,
    limits = c(0,1)
  )

ggsave(
  "figures/nonlinear_heterogeneous_data.pdf",
  height = 1.7, width = 3.5
)

# Show the data generating processes
dgp <- tibble(
  x = rep(1:3, each = 81),
  a = rep(seq(.1,.9,.01),3)
) |>
  mutate(
    linear = curve_linear(x,a),
    nonlinear = curve_nonlinear(a)
  ) |>
  pivot_longer(
    cols = contains("linear"),
    names_to = "dgp",
    values_to = "y"
  )
dgp |>
  ggplot(
    aes(
      x = a, y = y, color = factor(x), linetype = factor(x), shape = factor(x)
    )
  ) +
  geom_line(
    data = dgp |>
      mutate(x = ifelse(dgp == "nonlinear","Same curve\nfor all three\nsubgroups",x))
  ) +
  geom_point(
    data = dgp |> 
      mutate(a = round(a,2)) |>
      inner_join(
        data |> select(x,a),
        by = join_by(x,a)
      )
  ) +
  scale_shape_discrete(
    #values = c(scales::hue_pal()(3),"gray"),
    labels = \(x) ifelse(x %in% 1:3,paste0("X = ",x),x),
    name = "Pre-Treatment\nPopulation\nSubgroup"
  ) +
  scale_color_manual(
    values = c(scales::hue_pal()(3),"black"),
    labels = \(x) ifelse(x %in% 1:3,paste0("X = ",x),x),
    name = "Pre-Treatment\nPopulation\nSubgroup"
  ) +
  scale_linetype_manual(
    values = c(rep("solid",3),"dashed"),
    labels = \(x) ifelse(x %in% 1:3,paste0("X = ",x),x),
    name = "Pre-Treatment\nPopulation\nSubgroup"
  ) +
  facet_wrap(
    ~dgp,
    labeller = as_labeller(\(x) {
      ifelse(
        x == "linear",
        "Linear Heterogeneous\nData Generating Process",
        "Nonlinear Homogeneous\nData Generating Process"
      )
    })
  ) +
  labs(
    x = "Treatment Value",
    y = "\nOutcome Value"
  ) +
  ylim(c(0,1)) +
  scale_x_continuous(
    labels = as.character,
    limits = c(0,1)
  )
ggsave(
  "figures/nonlinear_heterogeneous_dgp.pdf",
  height = 2, width = 5.2
)

# Different implications for E(Y^.9)
tibble(x = 1:3, a = .9) |>
  mutate(
    y_nonlinear = curve_nonlinear(a),
    y_linear = curve_linear(x,a)
  ) |>
  select(a, y_nonlinear, y_linear) |>
  summarize_all(mean)

# Different implications for E(Y^.1)
tibble(x = 1:3, a = .1) |>
  mutate(
    y_nonlinear = curve_nonlinear(a),
    y_linear = curve_linear(x,a)
  ) |>
  select(a, y_nonlinear, y_linear) |>
  summarize_all(mean)

# Different across the whole dose response curve
tibble(
  x = rep(1:3, each = 81),
  a = rep(seq(.1,.9,.01), 3)
) |>
  mutate(
    nonlinear = curve_nonlinear(a),
    linear = curve_linear(x,a)
  ) |>
  group_by(a) |>
  select(-x) |>
  summarize_all(mean) |>
  pivot_longer(cols = contains("linear"), names_to = "dgp") |>
  ggplot(aes(x = a, y = value)) +
  geom_line() +
  labs(
    y = "Population Average\nDose-Response Curve",
    x = "Treatment Value"
  ) +
  facet_wrap(
    ~dgp,
    labeller = as_labeller(\(x) {
      ifelse(
        x == "linear",
        "Linear Heterogeneous\nData Generating Process",
        "Nonlinear Homogeneous\nData Generating Process"
      )
    })
  ) +
  ylim(c(0,1)) +
  scale_x_continuous(
    labels = as.character,
    limits = c(0,1)
  )
ggsave(
  "figures/nonlinear_heterogeneous_dose_response.pdf",
  height = 2, width = 4
)




