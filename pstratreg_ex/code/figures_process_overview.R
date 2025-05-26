
library(tidyverse)
theme_set(theme_bw())
library(foreach)

uclablue <- "#2774AE"
uclagold <- "#FFD100"
darkestblue <- "#003B5C"

# for other presentations
#uclablue <- "#0000FF"
#uclagold <- "#90EE90"
#darkestblue <- "#003B5C"

# REVISED FIGURES FOR PAPER

# Visualize logistic regression for survival

tibble(x = seq(-2,2,.01)) |>
  mutate(among_untreated = plogis(x - .5),
         among_treated = plogis(x + .5)) |>
  pivot_longer(cols = -x) |>
  ggplot(aes(x = x, y = value, linetype = name)) +
  geom_line() +
  labs(
    x = "Confounder X",
    y = "Probability of Survival",
    linetype = "Potential Outcome"
  ) +
  scale_linetype_discrete(
    name = "Potential Survival",
    labels = function(x) str_to_title(str_replace(x,"_"," "))
  ) +
  annotate(geom = "segment", x = c(-.5,0,.5), y = c(.3,.42,.53), yend = c(.45,.57,.68), arrow = arrow(length = unit(.07,"in"))) +
  annotate(geom = "text", x = 0.1, y = .25, label = "Each arrow is a conditional\naverage causal effect", hjust = 0, size = 2) +
  ggtitle("Example:\nLogistic regression for survival\n")
ggsave("../figures/survival_model_illustration.pdf", height = 2.5, width = 5)

# Visualize OLS regression for outcome

density_case <- data.frame(resid = qnorm(seq(.01,.99,.01))) %>%
  mutate(f = dnorm(resid))

tibble(x = seq(-2,2,.01)) |>
  mutate(among_untreated = .25 * x, among_treated = x + 5) |>
  pivot_longer(cols = -x) |>
  ggplot() +
  geom_line(aes(x = x, y = value, linetype = name)) +
  # Densities under treatment
  geom_ribbon(
    data = density_case,
    aes(xmin = -1, xmax = -1 + f, y = 4 + .3*resid),
    alpha = .4
  ) +
  geom_ribbon(
    data = density_case,
    aes(xmin = 0, xmax =  f, y = 5 + .6*resid),
    alpha = .4
  ) +
  geom_ribbon(
    data = density_case,
    aes(xmin = 1, xmax = 1 + f, y = 6 + .9*resid),
    alpha = .4
  ) +
  # Densities under control
  geom_ribbon(
    data = density_case,
    aes(xmin = -1, xmax = -1 + f, y = -.25 + .4*resid),
    alpha = .4
  ) +
  geom_ribbon(
    data = density_case,
    aes(xmin = 0, xmax =  f, y = 0 + .45*resid),
    alpha = .4
  ) +
  geom_ribbon(
    data = density_case,
    aes(xmin = 1, xmax = 1 + f, y = .25 + .5*resid),
    alpha = .4
  ) +
  scale_linetype_discrete(
    name = "Potential Outcome\nAmong Survivors",
    labels = function(x) str_to_title(str_replace(x,"_"," "))
  ) +
  labs(
    x = "Confounder X",
    y = "Outcome Y\nAmong Survivors"
  ) +
  ggtitle("Example:\nHeteroskedastic regression for conditionally\nnormal outcomes among survivors")
ggsave("../figures/outcome_model_illustration.pdf", height = 2.5, width = 5)

# Simulation step
density_case %>%
  ggplot(aes(x = resid,
             ymin = 0, ymax = f)) +
  geom_ribbon(alpha = .4) +
  theme_void() +
  annotate(geom = "label",
           x = 0, y = .05,
           label = "bold(Y)~bold(' | ')~bold(S==1)~','~bold(~A==1)~','~bold(X == x[i])",
           parse = T,
           fontface = "bold",
           size = 4,
           label.size = NA,
           color = "white", fill = NA)
ggsave("../figures/sim_y1.pdf", height = .8, width = 2.7)
density_case %>%
  ggplot(aes(x = resid,
             ymin = 0, ymax = f)) +
  geom_ribbon(alpha = .4) +
  theme_void() +
  annotate(geom = "label",
           x = 0, y = .05,
           label = "bold(Y)~bold(' | ')~bold(S==1)~','~bold(~A==0)~','~bold(X == x[i])",
           parse = T,
           fontface = "bold",
           size = 4,
           label.size = NA,
           color = "white", fill = NA)
ggsave("../figures/sim_y0.pdf", height = .8, width = 2.7)

density_case %>%
  ggplot() +
  geom_ribbon(
    data = density_case[1:20,],
    aes(x = resid,
        ymin = 0, ymax = f),
    alpha = .4
  ) +
  geom_ribbon(
    data = density_case[20:99,],
    aes(x = resid,
        ymin = 0, ymax = f),
    fill = uclablue
  ) +
  theme_void()
ggsave("../figures/sim_y1_upper.pdf", height = .8, width = 2.7)

density_case %>%
  ggplot() +
  geom_ribbon(
    data = density_case[80:99,],
    aes(x = resid,
        ymin = 0, ymax = f),
    alpha = .4
  ) +
  geom_ribbon(
    data = density_case[1:80,],
    aes(x = resid,
        ymin = 0, ymax = f),
    fill = uclablue
  ) +
  theme_void()
ggsave("../figures/sim_y1_lower.pdf", height = .8, width = 2.7)

density_case %>%
  ggplot() +
  geom_ribbon(
    data = density_case,
    aes(x = resid,
        ymin = 0, ymax = f),
    fill = uclablue
  ) +
  theme_void()
ggsave("../figures/sim_y0_all.pdf", height = .8, width = 2.7)