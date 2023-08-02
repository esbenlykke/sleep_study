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
ml_stats <-
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
  relocate(type = sleep_type)

# Apply get_bootstrap_ci to each variable within each model group and sleep type, in parallel
ml_ba_res <- ml_stats %>%
  group_by(type, model) %>%
  group_modify(~ future_map_dfr(vars_to_bootstrap, get_bootstrap_ci,
    data = .x,
    .options = furrr_options(seed = 123)
  ), .keep = TRUE)

# Save the results
write_csv(ml_ba_res, "data/processed/bootstrap_ba_results_ml.csv")

lstm_stats <- 
  read_csv("data/data_for_modelling/lstm/stats/biLSTM_stats.csv") %>%
  mutate(
    diff_spt = (spt - zm_spt) * 60,
    diff_tst = (tst - zm_tst) * 60,
    diff_se = se - zm_se,
    diff_lps = lps - zm_lps,
    diff_waso = waso - zm_waso
  ) %>%
  relocate(type, model, .after = 1) %>% 
  filter(!id %in% c(255704, 649105)) %>% 
  drop_na()

lstm_ba_res <- 
  lstm_stats %>%
  group_by(type, model) %>%
  group_modify(~ future_map_dfr(vars_to_bootstrap, get_bootstrap_ci,
                                data = .x,
                                .options = furrr_options(seed = 123)
  ), .keep = TRUE, )

# Save the results
write_csv(lstm_ba_res, "data/processed/bootstrap_ba_results_lstm.csv")

beepr::beep(9)

bind_rows(lstm_ba_res, ml_ba_res) %>% 
  arrange(desc(type)) %>%
  write_csv("data/processed/all_boostrap_ba_cis.csv")


# PEARSON CORRELATIONS
ml_stats <-
  ml_stats %>%
  rename(
    lps = lps_min, se = se_percent,
    spt = spt_hrs, tst = tst_hrs, waso = waso_3, type = sleep_type
  )

# Define function to perform cor.test and return estimate and confidence interval
perform_cor_test <- function(x, y) {
  result <- cor.test(x, y, method = "pearson", conf.level = 0.95)
  tibble(
    estimate = result$estimate,
    lower.ci = result$conf.int[1],
    upper.ci = result$conf.int[2]
  )
}

cors <- 
  ml_stats %>% 
  bind_rows(lstm_stats) %>% 
  group_by(model, type) %>% 
  reframe(
    spt = list(perform_cor_test(spt, zm_spt)),
    tst = list(perform_cor_test(tst, zm_tst)),
    se = list(perform_cor_test(se, zm_se)),
    lps = list(perform_cor_test(lps, zm_lps)),
    waso = list(perform_cor_test(waso, zm_waso))
  ) %>%
  pivot_longer(cols = -c(1:2), 
               names_to = "correlation", 
               values_to = "stats") %>% 
  unnest(stats)

write_csv(cors, "data/processed/all_cors.csv")