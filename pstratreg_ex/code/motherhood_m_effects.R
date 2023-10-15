
sink("../logs/motherhood_m_effects.txt")

t0 <- Sys.time()
print(t0)

library(tidyverse)
theme_set(theme_bw())
library(Amelia)
library(foreach)
library(doParallel)
library(doRNG)
cl <- makeCluster(6)
registerDoParallel(cl)

bs_reps <- 100

set.seed(14850)

# Load the prepared data
d_prepared <- readRDS("../intermediate/motherhood.RDS")

# Look at terciles of baseline wage among parents
cutoffs <- data %>%
  filter(treated & employed_baseline & !is.na(wage_baseline)) %>%
  arrange(wage_baseline) %>%
  mutate(cdf = cumsum(w) / sum(w)) %>%
  mutate(tercile = case_when(cdf <= 1/3 ~ 1,
                             cdf <= 2/3 ~ 2,
                             T ~ 3)) %>%
  group_by(tercile) %>%
  summarize(cutoff = max(wage_baseline)) %>%
  filter(tercile %in% 1:2) %>%
  mutate(in_dollars = exp(cutoff),
         label = paste0("$",round(exp(cutoff)))) %>%
  print()

wage_categorizer <- function(wage_val, emp_val) {
  factor(
    case_when(
      !emp_val ~ 1,
      wage_val <= cutoffs$cutoff[1] ~ 2,
      wage_val <= cutoffs$cutoff[2] ~ 3,
      wage_val > cutoffs$cutoff[2] ~ 4
    ),
    labels = c("Not Employed",
               paste0("Low Wage\n< ",cutoffs$label[1]),
               paste0("Mid Wage\n",cutoffs$label[1]," - ",cutoffs$label[2]),
               paste0("High Wage\n",cutoffs$label[2]," +"))
  )
}

estimator <- function(data, bs = F) {
  
  # Bootstrap if requested
  if (bs) {
    # choose people, not rows, for a cluster bootstrap at the person level
    # to mimic the sampling process where people are sampled
    original_people <- unique(data$PUBID)
    chosen <- sample(original_people, replace = T)
    data_star <- foreach(case = chosen, .combine = "rbind") %do% {
      data %>%
        filter(PUBID == case)
    }
    data_star <- sample_frac(data, replace = T)
  } else {
    data_star <- data
  }
  
  # Impute missing values
  amelia.out <- amelia(
    x = data_star %>% 
      select(-year_1,-year_2,
             -age_at_birth,-age_2,
             -sex, # since will be separate by sex anyhow
             -birth_year),
    m = 1,
    idvars = c("PUBID","w"),
    noms = c("race","marital","fulltime"),
    ords = "educ",
    boot.type = "none"
  )
  
  # Keep original unimputed outcome
  data_imp <- amelia.out$imputations$imp1 %>%
    mutate(wage = data_star$wage)
  
  # and enforce original bounds on numeric variables
  numeric_vars <- c("age_1","wage_baseline","tenure","experience")
  for (varname in numeric_vars) {
    original_range <- range(data[[varname]], na.rm = T)
    x <- data_imp[[varname]]
    if (any(x < original_range[1])) {
      x[x < original_range[1]] <- original_range[1]
    }
    if (any(x > original_range[2])) {
      x[x > original_range[2]] <- original_range[2]
    }
    data_imp[[varname]] <- x
  }
  
  fit_m <- glm(employed ~ treated*(race + poly(age_1,2) + educ + marital + fulltime + log(tenure + 1) + 
                                     log(experience + 1) + wage_baseline + employed_baseline),
               data = data_imp,
               weights = w,
               family = binomial)
  
  each_unit <- data_imp %>%
    mutate(mhat1 = predict(fit_m, newdata = data_imp %>% mutate(treated = T), type = "response"),
           mhat0 = predict(fit_m, newdata = data_imp %>% mutate(treated = F), type = "response")) %>%
    # make the ATT
    filter(treated)
  marginal <- each_unit %>%
    summarize(estimate = weighted.mean(mhat1 - mhat0, w = w)) %>%
    mutate(category = "everyone")
  conditional <- each_unit %>%
    mutate(category = wage_categorizer(wage_val = wage_baseline, emp_val = employed_baseline)) %>%
    group_by(category) %>%
    summarize(estimate = weighted.mean(mhat1 - mhat0, w = w))
  return(marginal %>%
           bind_rows(conditional))
}

point_women <- estimator(data = d_prepared %>% filter(sex == "Women"), bs = F)
point_men <- estimator(data = d_prepared %>% filter(sex == "Men"), bs = F)
bs_women <- foreach(r = 1:bs_reps, .combine = "rbind") %dorng% {
  estimator(d_prepared %>% filter(sex == "Women"), bs = T)
}
bs_men <- foreach(r = 1:bs_reps, .combine = "rbind") %dorng% {
  estimator(d_prepared %>% filter(sex == "Men"), bs = T)
}

m_effect <- point_women %>%
  mutate(sex = "Women") %>%
  bind_rows(point_men %>%
              mutate(sex = "Men")) %>%
  left_join(bs_women %>%
              mutate(sex = "Women") %>%
              bind_rows(bs_men %>%
                          mutate(sex = "Men")) %>%
              group_by(category, sex) %>%
              summarize(se = sd(estimate),
                        .groups = "drop"),
            by = c("sex","category")) %>%
  mutate(ci.min = estimate - qnorm(.975) * se,
         ci.max = estimate + qnorm(.975) * se,
         label = format(round(estimate,2),nsmall = 2))

# Plot the marginal effect
forplot <- m_effect %>%
  filter(category == "everyone")
p <- forplot %>%
  ggplot(aes(y = sex, x = estimate,
             xmin = ci.min, xmax = ci.max,
             label = label, alpha = sex)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .2) +
  theme_bw() +
  scale_y_discrete(name = element_blank()) +
  xlab("Effect of Parenthood on Employment") +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12))
p + scale_alpha_manual(values = c(0,0))
ggsave("../figures/parenthood_employment_1.pdf",
       height = 2, width = 5)
p + geom_label(data = forplot %>% filter(sex == "Women")) +
  scale_alpha_manual(values = c(0,1))
ggsave("../figures/parenthood_employment_2.pdf",
       height = 2, width = 5)
p + geom_label(data = forplot) +
  scale_alpha_manual(values = c(1,1))
ggsave("../figures/parenthood_employment_3.pdf",
       height = 2, width = 5)

# Plot the conditional effects
forplot <- m_effect %>%
  filter(category != "everyone") %>%
  mutate(category = gsub("\n",": ",category),
         category = gsub(" Wage","",category)) %>%
  mutate(category = fct_relevel(category,c("Not Employed","Low: < $15","Mid: $15 - $25","High: $25 +"))) %>%
  mutate(sex = fct_rev(sex))
p <- forplot %>%
  ggplot(aes(y = category, x = estimate,
             xmin = ci.min, xmax = ci.max,
             label = label, alpha = sex)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .2) +
  facet_wrap(~sex, ncol = 1) +
  scale_y_discrete(name = "Wage Pre-Birth") +
  xlab("Effect of Parenthood on Employment") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text = element_text(size = 12))
p +
  scale_alpha_manual(values = c(0,0))
ggsave("../figures/parenthood_employment_conditional_1.pdf",
       height = 3.5, width = 5)
p +
  scale_alpha_manual(values = c(1,0)) +
  geom_label(data = forplot %>% filter(sex == "Women"),
             size = 3)
ggsave("../figures/parenthood_employment_conditional_2.pdf",
       height = 3.5, width = 5)
p +
  scale_alpha_manual(values = c(1,1)) +
  geom_label(data = forplot,
             size = 3)
ggsave("../figures/parenthood_employment_conditional.pdf",
       height = 3.5, width = 5)

sessionInfo()

print("Finish time")
print(Sys.time())
print("Time spent")
difftime(Sys.time(),t0)


sink()
