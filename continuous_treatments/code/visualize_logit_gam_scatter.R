
# Scatterplot comparison

logit_estimates <- readRDS(paste0("intermediate/causal_enrolled_any.RDS"))
gam_estimates <- readRDS(paste0("intermediate/causal_enrolled_any_gam.RDS"))

forplot <- logit_estimates$estimate |>
  mutate(method = "logit") |>
  bind_rows(
    gam_estimates$estimate |>
      mutate(method = "gam")
  ) |>
  filter(delta == 10e3) |>
  select(PUBID, effect, method, w) |>
  pivot_wider(names_from = "method", values_from = "effect")

forplot |>
  filter(logit < .05) |>
  ggplot(aes(x = logit, y = gam)) +
  geom_point(color = "gray", alpha = .2, size = .15) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  xlab("Logit with Interactions\n(modeling effect heterogeneity)") +
  ylab("Generalized Additive Model\n(model effect nonlinearity)") +
  #ylim(c(-.01,.1)) +
  #xlim(c(-.01,.1)) +
  coord_fixed() +
  annotate(
    geom = "text", x = -.01, y = .0375, 
    label = paste0(
      "Correlation = ",
      forplot |> 
        summarize(estimate = format(round(cor(logit, gam),2),nsmall = 2)) |> 
        pull(estimate)
    ),
    size = 3,
    hjust = 0
  ) +
  annotate(geom = "text", x = .015, y = .015, label = "45-Degree Line of Equal Estimates", size = 2.5, angle = 45, vjust = -.5, fontface = "bold") +
  annotate(geom = "text", x = .05, y = .009,
           label = paste0("Figure omits\n",scales::label_percent(accuracy = .1)(mean(forplot$logit >= .05))," of estimates\nthat are above 0.05"), 
           hjust = 1, size = 2.5) +
  annotate(geom = "segment", x = .045, xend = .05, y = .004, arrow = arrow(length = unit(.05,"in")))
ggsave("figures/logit_gam_scatter.pdf", height = 3, width = 6.5)
