
theme_set(theme_bw())

data <- tibble(x = rep(1:3, each = 6)[-1]) |>
  mutate(
    a = c(.18,.2,.25,.3,.32,
          .375,
          .43,.45,.5,.55,.57,
          .625,
          .68,.7,.75,.8,.82),
    set = rep(c("junction","line","point","point","point","line"),3)[-1],
    y = a - .4 * ifelse(a > .4, a - .4, 0) - .4 * ifelse(a > .65, a - .65, 0)
  )
data |>
  ggplot(aes(x = a, y = y, group = x)) +
  geom_point(aes(x = ifelse(set == "point", a, NA))) +
  geom_line(aes(x = ifelse(set %in% c("point","line"), a , NA))) +
  annotate(
    geom = "rect", fill = "gray", alpha = .2,
    xmin = -Inf, xmax = .375, ymin = -Inf, ymax = Inf
  ) +
  annotate(
    geom = "rect", fill = "gray", alpha = .4,
    xmin = .375, xmax = .625, ymin = -Inf, ymax = Inf
  ) +
  annotate(
    geom = "rect", fill = "gray", alpha = .2,
    xmin = .625, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  theme(panel.grid = element_blank()) +
  xlim(c(.15,.85)) +
  ylim(c(.15,.9)) +
  annotate(geom = "text", x = c(.25,.5,.75), y = .75, label = paste0("Subgroup\nX = ",1:3), size = 3) +
  labs(
    x = "Treatment Value A",
    y = "Outcome Value Y"
  )
ggsave("figures/tradeoff_heterogeneity.pdf", height = 2.5, width = 3)

data |>
  ggplot(aes(x = a, y = y)) +
  geom_point(aes(x = ifelse(set == "point", a, NA))) +
  geom_line() +
  annotate(
    geom = "rect", fill = "gray", alpha = .2,
    xmin = -Inf, xmax = .375, ymin = -Inf, ymax = Inf
  ) +
  annotate(
    geom = "rect", fill = "gray", alpha = .4,
    xmin = .375, xmax = .625, ymin = -Inf, ymax = Inf
  ) +
  annotate(
    geom = "rect", fill = "gray", alpha = .2,
    xmin = .625, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  theme(panel.grid = element_blank()) +
  xlim(c(.15,.85)) +
  ylim(c(.15,.9)) +
  annotate(geom = "text", x = c(.25,.5,.75), y = .75, label = paste0("Subgroup\nX = ",1:3), size = 3) +
  labs(
    x = "Treatment Value A",
    y = "Outcome Value Y"
  )
ggsave("figures/tradeoff_nonlinearity.pdf", height = 2.5, width = 3)

