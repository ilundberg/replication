
# Code file from the replication package for
# Lundberg, Ian, Jennie E. Brand, and Nanum Jeon
# "Researcher reasoning meets computational capacity: Machine learning for social science"

# With questions, contact Ian Lundberg, ilundberg@cornell.edu

# Replication code for Figure 4.
# This figure illustrates the bias-variance tradeoff with a hierarchical linear model.

# Store output
sink("logs/fig4.txt")
t0 <- Sys.time()

# Load packages
library(tidyverse)
theme_set(theme_bw())
library(foreach)

# Record the session information for reproducibility
print(sessionInfo())

# Set seed for reproducibility
set.seed(90095)

# Set parameters of the simulation
# Simulate for 100 groups with varying sizes
num_groups <- 100
group_sizes <- rep(5, 100)
within_variance <- 10
between_variance <- 3
R <- 100
true_means <- data.frame(group = 1:num_groups,
                         theta = qnorm(seq(.005,.995,.01), 
                                       sd = sqrt(between_variance)))

# Create simulations of the estimator applied to simulated data
sims <- foreach(r = 1:R, .combine = "rbind") %do% {
  # Simulate data
  sim <- data.frame(group = rep(1:num_groups, group_sizes)) %>%
    left_join(true_means, by = "group") %>%
    # Generate unit-level outcomes by a normal draw around that mean
    mutate(y = rnorm(n(), mean = theta, sd = sqrt(within_variance)))
  # Calculate the estimator and return estimates
  sim %>%
    group_by(group) %>%
    summarize(ybar_j = mean(y),
              sigma2_j = var(y),
              theta = mean(theta),
              num = n(),
              .groups = "drop") %>%
    mutate(delta2 = var(ybar_j),
           ybar = mean(ybar_j),
           shrinkage_ratio = (sigma2_j / num) / (sigma2_j / num + delta2),
           pooled = ybar_j - (ybar_j - ybar) * shrinkage_ratio) %>%
    select(group, theta, ybar, ybar_j, shrinkage_ratio, pooled)
}

# Create data frame showing the simulated shrinkage curve
shrinkage_curve <- foreach(ratio = seq(0,1,.01), .combine = "rbind") %do% {
  sims %>%
    mutate(estimate = ybar_j - (ybar_j - ybar) * ratio) %>%
    group_by(group) %>%
    summarize(Bias_Sq = (mean(estimate - theta)) ^ 2,
              Variance = var(estimate),
              MSE = mean((estimate - theta) ^ 2),
              .groups = "drop") %>%
    mutate(shrinkage_ratio = ratio)
}

# Prepare results data for the plot
forplot <- shrinkage_curve %>%
  group_by(shrinkage_ratio) %>%
  summarize_all(.funs = mean) %>%
  pivot_longer(cols = c("Bias_Sq","Variance","MSE")) %>%
  mutate(name = factor(case_when(name == "MSE" ~ 1,
                                 name == "Bias_Sq" ~ 2,
                                 name == "Variance" ~ 3),
                       labels = c("Expected\nSquared Error","Bias Squared","Variance")))
# Generate the plot
forplot %>%
  ggplot(aes(x = shrinkage_ratio, y = value)) +
  geom_line() +
  facet_wrap(~name, ncol = 1, scales = "free",
             strip.position = "left") +
  geom_point(data = forplot %>% filter(shrinkage_ratio == .4)) +
  xlab("Shrinkage Factor") +
  scale_y_continuous(name = element_blank()) +
  geom_vline(xintercept = c(0,.4,1), linetype = "dashed") +
  geom_text(data = forplot %>%
              filter(name == "Expected\nSquared Error" & shrinkage_ratio == .42) %>%
              mutate(value = 3.3),
            label = "Shrinkage\nChosen\nby HLM",
            #angle = 270, size = 2.5, hjust = 0, vjust = -1,
            size = 2, hjust = 0, vjust = 1) +
  geom_text(data = forplot %>%
              filter(name == "Expected\nSquared Error" & shrinkage_ratio == .01) %>%
              mutate(value = 3.3),
            label = "No Shrinkage:\nWithin-Group\nMeans",
            #angle = 270, size = 2.5, hjust = 0, vjust = -1,
            size = 2, hjust = 0, vjust = 1) +
  geom_text(data = forplot %>%
              filter(name == "Expected\nSquared Error" & shrinkage_ratio == .99) %>%
              mutate(value = 3.3),
            label = "Full Shrinkage:\nOverall Mean",
            #angle = 270, size = 2.5, hjust = 0, vjust = -1,
            size = 2, hjust = 1, vjust = 1) +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10))
# Save the plot
ggsave("figures/hlm_simulation.pdf",
       height = 5, width = 3.5)

print(paste("Began:",t0))
print(paste("Finished:",Sys.time()))
print(paste("Spent:",round(difftime(Sys.time(),t0, units = "secs"),2),"seconds"))
