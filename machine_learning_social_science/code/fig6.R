
# Code file from the replication package for
# Lundberg, Ian, Jennie E. Brand, and Nanum Jeon
# "Researcher reasoning meets computational capacity: Machine learning for social science"

# With questions, contact Ian Lundberg, ilundberg@cornell.edu

# Replication code for Figure 6.
# This figure illustrates the idea of an average partial effect.

# Store output
sink("logs/fig6.txt")
t0 <- Sys.time()

# Load packages
library(tidyverse)
theme_set(theme_bw())
library(mgcv)

# Record the session information for reproducibility
print(sessionInfo())

# Set seed for reproducibility
set.seed(90095)

# Set parameters for simulation
xvals <- exp(seq(log(.01),log(1),length.out = 10))
response <- function(x) 1 - (1 - x) ^ 2
delta <- .1

# Generate data
d <- data.frame(x = xvals) %>%
  mutate(y = response(x))

# Estimate two models: OLS and GAM
lm.out <- lm(y ~ x, data = d)
gam.out <- gam(y ~ s(x, bs = "tp"), data = d)

# Make predictions
d.pred <- d %>%
  mutate(x_delta = x + delta,
         y_delta = response(x + delta),
         yhat = predict(lm.out),
         yhat_delta = predict(lm.out, newdata = d %>% mutate(x = x + delta)),
         yhat_gam = predict(gam.out),
         yhat_gam_delta = predict(gam.out, newdata = d %>% mutate(x = x + delta)))
d.pred.long <- data.frame(x = seq(0,1,.005)) %>%
  mutate(y = response(x),
         yhat_gam = predict(gam.out, newdata = data.frame(x = seq(0,1,.005))))

# Visualize the data and the linear model
d.pred %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = yhat), color = "gray") +
  geom_segment(data = d.pred[8,],
               aes(x = x, y = ,
                   xend = x, yend = yhat),
               linetype = "dotted") +
  geom_segment(data = d.pred[8,],
               aes(x = x, y = yhat,
                   xend = x_delta, yend = yhat)) +
  geom_segment(data = d.pred[8,],
               aes(x = x_delta, y = yhat,
                   xend = x_delta, yend = yhat_delta)) +
  geom_text(data = d.pred[8,],
            aes(x = .5 * (x + x_delta),
                y = yhat),
            vjust = 1.5, label = "Delta", parse = T, size = 3.5) +
  geom_text(data = d.pred[8,],
            aes(x = x_delta,
                y = .5 * (yhat + yhat_delta)),
            hjust = -.1, label = "hat(f)(x + Delta ) - hat(f)(x)", parse = T, size = 3.5) +
  annotate(geom = "text",
           x = xvals[8], y = .2,
           label = "{frac(partialdiff,partialdiff*x)*hat(f)(x)==lim(frac(hat(f)(x + Delta ) - hat(f)(x),Delta),Delta %->% 0)}==hat(beta)",
           parse = T, hjust = 0, size = 3.5) +
  annotate(geom = "text", x = .05, y = .9,
           label = "Average Partial Effect\nfrom Best Fit Line:",
           vjust = -.2, hjust = 0, size = 3.5) +
  annotate(geom = "text", x = .05, y = .9,
           label = paste0("{frac(1,n)~sum(frac(partialdiff,partialdiff*x)~hat(f)(x[i]),i==1,n)==hat(beta)}==",
                          round(coef(lm.out)[2],3)),
           vjust = 1.2, hjust = 0, parse = T, size = 3.5) +
  xlab("Predictor Variable X") +
  scale_y_continuous(name = "Outcome Variable Y",
                     limits = c(0,1.15),
                     breaks = seq(0,1,.2)) +
  annotate(geom = "text",
           x = .8, y = predict(lm.out, newdata = data.frame(x = .8)),
           label = "Best Fit Line", size = 3, color = "gray",
           angle = 25, vjust = -.5)
ggsave("figures/partial_effects_linear.pdf",
       height = 3, width = 5)

# Visualize the data and the GAM
d.pred %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(data = d.pred.long, 
            aes(y = yhat_gam),
            color = "gray") +
  geom_point() +
  geom_segment(data = d.pred[8,],
               aes(x = x, y = yhat_gam,
                   xend = x_delta, yend = yhat_gam)) +
  geom_segment(data = d.pred[8,],
               aes(x = x_delta, y = yhat_gam,
                   xend = x_delta, yend = yhat_gam_delta)) +
  geom_text(data = d.pred[8,],
            aes(x = .5 * (x + x_delta),
                y = y),
            vjust = 1.5, label = "Delta", parse = T, size = 3.5) +
  geom_text(data = d.pred[8,],
            aes(x = x_delta,
                y = .5 * (y + yhat_gam_delta)),
            hjust = -.1, label = "hat(f)(x + Delta ) - hat(f)(x)", parse = T, size = 3.5) +
  annotate(geom = "text",
           x = xvals[8], y = .2, size = 3.5,
           label = "frac(partialdiff,partialdiff*x)*hat(f)(x)==lim(frac(hat(f)(x + Delta ) - hat(f)(x),Delta),Delta %->% 0)",
           parse = T, hjust = 0) +
  annotate(geom = "text", x = .05, y = .9, size = 3.5,
           label = "Average Partial Effect\nfrom GAM Estimate:",
           vjust = -.2, hjust = 0) +
  annotate(geom = "text", x = .05, y = .9, size = 3.5,
           label = paste0("frac(1,n)~sum(frac(partialdiff,partialdiff*x)~hat(f)(x[i]),i==1,n)==",
                          round(100 * mean(predict(gam.out, newdata = d %>% mutate(x = x + .01)) -
                                             predict(gam.out)),3)),
           vjust = 1.2, hjust = 0, parse = T) +
  xlab("Predictor Variable X") +
  scale_y_continuous(name = "Outcome Variable Y",
                     limits = c(0,1.15),
                     breaks = seq(0,1,.2)) +
  annotate(geom = "text",
           x = 1, y = 1,
           label = "Estimated Curve", size = 3, color = "gray",
           vjust = -.5, hjust = 1.1)
ggsave("figures/partial_effects_gam.pdf",
       height = 3, width = 5)


print(paste("Began:",t0))
print(paste("Finished:",Sys.time()))
print(paste("Spent:",round(difftime(Sys.time(),t0, units = "secs"),2),"seconds"))