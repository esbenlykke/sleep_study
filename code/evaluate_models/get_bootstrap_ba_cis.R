#!/usr/bin/env Rscript

library(tidyverse)
library(boot)
library(furrr)

# Set up a parallel backend using the future package
# Adjust the number of workers as necessary
plan(multisession, workers = 5)

# Define a function to calculate the mean difference and LoA
get_ba <- function(d, i) {
  d <- d[i] # resample with replacement
  mean_diff <- mean(d)
  sd_diff <- sd(d)
  loa_upper <- mean_diff + 1.96 * sd_diff
  loa_lower <- mean_diff - 1.96 * sd_diff
  return(c(mean_diff, loa_upper, loa_lower))
}

# Define a function to calculate bootstrap confidence intervals
get_bootstrap_ci <- function(data, var) {
  # Run the bootstrap
  results <- boot(data[[var]], get_ba, R = 10000)
  
  # Calculate the 95% confidence intervals
  bias_ci <- boot.ci(results, type = "bca", index = 1)$bca
  loa_upper_ci <- boot.ci(results, type = "bca", index = 2)$bca
  loa_lower_ci <- boot.ci(results, type = "bca", index = 3)$bca
  
  # Return a tibble with the results
  return(tibble(
    variable = var,
    bias = results$t0[1],
    loa_upper = results$t0[2],
    loa_lower = results$t0[3],
    bias_ci_lower = bias_ci[4],
    bias_ci_upper = bias_ci[5],
    loa_upper_ci_lower = loa_upper_ci[4],
    loa_upper_ci_upper = loa_upper_ci[5],
    loa_lower_ci_lower = loa_lower_ci[4],
    loa_lower_ci_upper = loa_lower_ci[5]
  ))
}

# List of variables to bootstrap
vars_to_bootstrap <- c("diff_spt", "diff_tst", "diff_se", "diff_lps", "diff_waso")

# Read in the data
stats <-
  list.files("data/data_for_modelling/chained_classifiers/sleep_stats", full.names = TRUE) %>%
  read_csv(id = "model") %>%
  mutate(
    model = str_extract(model, "(?<=sleep_stats\\/).*(?=_stats.csv)")
  ) %>%
  mutate(
    diff_spt = (spt_hrs - zm_spt) * 60,
    diff_tst = (tst_hrs - zm_tst) * 60,
    diff_se = se_percent - zm_se,
    diff_lps = lps_min - zm_lps,
    diff_waso = waso_min - zm_waso
  ) %>% 
  relocate(sleep_type)

# Apply get_bootstrap_ci to each variable within each model group and sleep type, in parallel
bootstrap_results <- stats %>%
  group_by(sleep_type, model) %>%
  do(bind_rows(future_map(vars_to_bootstrap, get_bootstrap_ci, data = ., 
                          .options = furrr_options(seed = 123), .progress = TRUE))) %>%
  ungroup()

# Save the results
write_csv(bootstrap_results, "data/processed/bootstrap_ba_results_ml.csv")

beepr::beep(9)
