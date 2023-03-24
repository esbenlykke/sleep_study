library(tidyverse)
library(arrow)
library(lme4)
library(car)

crude_stats <-
  read_parquet("data/processed/crude_stats.parquet") %>%
  mutate(
    id = as_factor(id)
  ) %>%
  split(.$model) %>%
  map(drop_na)

crude_dc_data <-
  crude_stats[[1]] %>% 
  ungroup()

library(lme4)
library(car)


sleep_data <- 
  crude_dc_data %>% 
  select(id, spt_hrs)

# Fit a linear mixed effects model to estimate the within-subject standard deviation for each subject
sleep_model <- lmer(spt_hrs ~ (1 | id), data = sleep_data)

# Extract sd from model
sd <- sigma(sleep_model)

# Perform Levene's test
levene_test <- leveneTest(resid(sleep_model) ~ sleep_data$id) %>% broom::tidy()

# Levenes test is non sign. No need to use ICC

# Calculate SEM
n <- length(unique(sleep_data$id)) # number of groups
k <- length(sleep_data$spt_hrs) / n # sample size per group
SEM <- sd / sqrt(n * k)

# MDC
MDC <- 1.96 * SEM * sqrt(2)
MDC * 60

# 95%CI
# Calculate degrees of freedom
df <- n - 1

# Calculate critical value for alpha = 0.05
t_critical <- qt(1 - 0.05/2, df)

# Calculate confidence interval
CI <- c(MDC - t_critical * SEM * sqrt(2) * 60, MDC + t_critical * SEM * sqrt(2) * 60)

# Print MDC and CI
cat("MDC:", round(MDC, 2), "\n")
cat("CI:", round(CI[1], 2), "-", round(CI[2], 2))
