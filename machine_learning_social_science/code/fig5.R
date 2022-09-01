
# Code file from the replication package for
# Lundberg, Ian, Jennie E. Brand, and Nanum Jeon
# "Researcher reasoning meets computational capacity: Machine learning for social science"

# With questions, contact Ian Lundberg, ilundberg@cornell.edu

# Replication code for Figure 5.
# This figure illustrates data-driven estimator selection.

# Store output
sink("logs/fig5.txt")
t0 <- Sys.time()

# Load packages
library(tidyverse)
theme_set(theme_bw())
library(foreach)
library(mgcv)
library(rpart)

# Record the session information for reproducibility
print(sessionInfo())

# Set seed for reproducibility
set.seed(90095)

# Set parameters for simulation
n <- 1000

# Generate data
d <- data.frame(x = seq(1,10,length.out = n)) %>%
  mutate(mu = ifelse(x <= 4, 
                     sin(pi / 2 * x / 4),
                     1),
         y = rnorm(n, mean = mu, sd = .1))

# Visualize the generated data
d %>%
  ggplot(aes(x = x, y = mu)) +
  geom_point(aes(y = y),
             color = "gray", size = .05) +
  geom_line(color = "black") +
  scale_x_continuous(name = "Predictor Variable: X",
                     breaks = c(1,4,7,10)) +
  ylab("Outcome Variable: Y")
ggsave("figures/sim_learners_data.pdf",
       height = 2, width = 3)

# Function to fit models and make predictions
fit_and_predict <- function(train, test = train) {
  fits <- list(
    `OLS: Linear` = lm(y ~ x, data = train),
    `OLS: Quadratic` = lm(y ~ poly(x,2), data = train),
    `OLS: Cubic` = lm(y ~ poly(x,3), data = train),
    `OLS: Log` = lm(y ~ log(x), data = train),
    GAM = gam(y ~ s(x), data = train),
    Tree = rpart(y ~ x, data = train)
  )
  fitted <- foreach(i = 1:length(fits), .combine = "rbind") %do% {
    test %>%
      mutate(Method = names(fits)[i],
             Fitted = predict(fits[[i]],
                              newdata = test))
  }
  return(fitted)
}
# Get fitted values, using the full data
fitted <- fit_and_predict(d)

# Visualize those fits
fitted %>%
  mutate(Method = fct_relevel(Method,"OLS: Linear","OLS: Log","OLS: Quadratic","OLS: Cubic","Tree","GAM")) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = mu), color = "gray") +
  geom_line(aes(y = Fitted), linetype = "dashed") +
  facet_wrap(~Method, ncol = 2) +
  scale_x_continuous(name = "Predictor Variable: X",
                     breaks = c(1,4,7,10)) +
  ylab("Conditional Outcome Mean: E(Y | X)")
ggsave("figures/sim_learners.pdf",
       height = 4, width = 3)

# Train-test split to assess predictive performance
d_split <- d %>%
  mutate(set = rep(c("train","test"), ceiling(n() / 2))[1:n()],
         set = sample(set))

# Fit on training set and evaluate mean squared error on the holdout set
holdout_mse <- fit_and_predict(train = d_split %>% filter(set == "train"),
                               test = d_split %>% filter(set == "test")) %>%
  group_by(Method) %>%
  summarize(mse = mean((y - Fitted) ^ 2),
            se = sd((y - Fitted) ^ 2) / sqrt(n()))

# Visualize the out-of-sample predictive performance
holdout_mse %>%
  mutate(Method = fct_reorder(Method, -mse)) %>%
  ggplot(aes(x = Method, y = mse,
             ymin = mse - qnorm(.975) * se,
             ymax = mse + qnorm(.975) * se)) +
  geom_point() +
  geom_errorbar(width = .2) +
  coord_flip() +
  ylim(c(.005,.03)) +
  ylab("Mean Squared Error\n(Test Set)") +
  xlab("Method")
ggsave("figures/sim_learners_test.pdf",
       height = 2, width = 3)


print(paste("Began:",t0))
print(paste("Finished:",Sys.time()))
print(paste("Spent:",round(difftime(Sys.time(),t0, units = "secs"),2),"seconds"))