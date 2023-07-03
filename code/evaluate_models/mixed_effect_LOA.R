#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)
library(boot)

stats <-
  list.files("data/data_for_modelling/chained_classifiers/sleep_stats", full.names = TRUE) %>% 
  read_csv(id = "model") %>% 
  mutate(
    model = str_extract(model, "(?<=sleep_stats\\/).*(?=_stats.csv)")
  ) %>% 
  mutate(
    diff_spt = spt_hrs - zm_spt,
    diff_tst = tst_hrs - zm_tst,
    diff_se = se_percent - zm_se,
    diff_lps = lps_min - zm_lps,
    diff_waso = waso_min - zm_waso
  )

stats %>% 
  group_by(model) %>% 
  summarise(
    bias_spt = mean(diff_spt) * 60,
    upper_spt = bias_spt + (1.96 * sd(diff_spt)),
    lower_spt = bias_spt - (1.96 * sd(diff_spt)),
    bias_tst = mean(diff_tst) * 60,
    upper_tst = bias_tst + (1.96 * sd(diff_tst)),
    lower_tst = bias_tst - (1.96 * sd(diff_tst)),
    bias_se = mean(diff_se),
    upper_se = bias_se + (1.96 * sd(diff_se)),
    lower_se = bias_se - (1.96 * sd(diff_se)),
    bias_lps = mean(diff_lps),
    upper_lps = bias_lps + (1.96 * sd(diff_lps)),
    lower_lps = bias_lps - (1.96 * sd(diff_lps)),
    bias_waso = mean(diff_waso),
    upper_waso = bias_waso + (1.96 * sd(diff_waso)),
    lower_waso = bias_waso - (1.96 * sd(diff_waso)),
  )


# Define a function to calculate the mean difference and LoA
get_ba <- function(d, i) {
  d <- d[i]  # resample with replacement
  mean_diff <- mean(d)
  sd_diff <- sd(d)
  loa_upper <- mean_diff + 1.96 * sd_diff
  loa_lower <- mean_diff - 1.96 * sd_diff
  return(c(mean_diff, loa_upper, loa_lower))
}

# Run the bootstrap
results <- boot(stats$diff_spt, get_ba, R=10000)

# Extract the bias and LoA
bias <- results$t0[1]
loa_upper <- results$t0[2]
loa_lower <- results$t0[3]

# Calculate the 95% confidence intervals
bias_ci <- boot.ci(results, type="bca", index=1)$bca
loa_upper_ci <- boot.ci(results, type="bca", index=2)$bca
loa_lower_ci <- boot.ci(results, type="bca", index=3)$bca


# This method is nonparametric and accounts for repeated measures. Double check with Jan!

# Mixed Effects Limits of Agreement
# This function allows for the calculation of bootstrapped limits of agreement
# when there are multiple observations per subject


get_agree <- function(df, diff, delta) {
  df %>%
    loa_mixed(
      diff = diff,
      condition = "noon_day",
      id = "id",
      data = .,
      delta = delta,
      replicates = 1000
    ) %>% 
    pluck("loa") %>%
    rownames_to_column(var = "ba_metric") |>
    mutate(
      ba_metric = str_remove(ba_metric, "...\\d"),
      ba_metric = str_to_lower(str_replace(ba_metric, " ", "_")),
      type = diff,
      type = str_remove(type, "diff_")
    ) |>
    as_tibble()
}

stats %>% 
  loa_mixed(condition = "noon_day", id = "id", data = ., diff = "diff_spt", delta = 2)

# Run in parallel

doParallel::registerDoParallel(cores = 6)

# spt
crude_spt <-
  crude_all_diffs %>%
  map(~ filter(.x, !abs(diff_spt_hrs) > 6 & spt_hrs >= 0)) %>%
  future_map_dfr(~ get_agree(
    df = .x,
    diff = "diff_spt_hrs",
    delta = 2,
  ),
  .progress = TRUE,
  .id = "model",
  .options = furrr_options(seed = 123)
  )


# tst
crude_tst <-
  crude_all_diffs %>%
  map(~ filter(.x, !abs(diff_tst_hrs) > 5 & tst_hrs >= 0)) %>%
  future_map_dfr(~ get_agree(
    df = .x,
    diff = "diff_tst_hrs",
    delta = 2,
  ),
  .progress = TRUE,
  .id = "model",
  .options = furrr_options(seed = 123)
  )

# se_percent
crude_se_percent <-
  crude_all_diffs %>%
  map(~ filter(.x, !abs(diff_se_percent) > 30 & se_percent >= 0)) %>%
  future_map_dfr(~ get_agree(
    df = .x,
    diff = "diff_se_percent",
    delta = 2,
  ),
  .progress = TRUE,
  .id = "model",
  .options = furrr_options(seed = 123)
  )

# lps_min
crude_lps <-
  crude_all_diffs %>%
  map(~ filter(.x, !abs(diff_lps_min) > 100 & lps_min >= 0)) %>%
  future_map_dfr(~ get_agree(
    df = .x,
    diff = "diff_lps_min",
    delta = 2,
  ),
  .progress = TRUE,
  .id = "model",
  .options = furrr_options(seed = 123)
  )

# waso
crude_waso <-
  crude_all_diffs %>%
  map(~ filter(.x, !abs(diff_waso_min) > 100 & waso_min >= 0)) %>%
  future_map_dfr(~ get_agree(
    df = .x,
    diff = "diff_waso_min",
    delta = 2,
  ),
  .progress = TRUE,
  .id = "model",
  .options = furrr_options(seed = 123)
  )


bind_rows(crude_spt, crude_tst, crude_se_percent, crude_lps, crude_waso) |>
  janitor::clean_names() |>
  write_csv("data/processed/crude_mixed_effect_ba.csv")

beepr::beep()


### Multiclass
# spt
multi_spt <-
  multi_all_diffs %>%
  map(~ filter(.x, !abs(diff_spt_hrs) > 6 & spt_hrs >= 0)) %>% 
  future_map_dfr(~ get_agree(
    df = .x,
    diff = "diff_spt_hrs",
    delta = 2,
  ),
  .progress = TRUE,
  .id = "model",
  .options = furrr_options(seed = 123)
  )


# tst
multi_tst <- 
  multi_all_diffs %>%
  map(~ filter(.x, !abs(diff_tst_hrs) > 5 & tst_hrs >= 0)) %>% 
  future_map_dfr(~ get_agree(
    df = .x,
    diff = "diff_tst_hrs",
    delta = 2,
  ),
  .progress = TRUE,
  .id = "model",
  .options = furrr_options(seed = 123)
  )

# se_percent
multi_se_percent <-  
  multi_all_diffs %>%
  map(~ filter(.x, !abs(diff_se_percent) > 30 & se_percent >= 0)) %>% 
  future_map_dfr(~ get_agree(
    df = .x,
    diff = "diff_se_percent",
    delta = 2,
  ),
  .progress = TRUE,
  .id = "model",
  .options = furrr_options(seed = 123)
  )

# lps_min
multi_lps <- 
  multi_all_diffs %>%
  map(~ filter(.x, !abs(diff_lps_min) > 100 & lps_min >= 0)) %>% 
  future_map_dfr(~ get_agree(
    df = .x,
    diff = "diff_lps_min",
    delta = 2,
  ),
  .progress = TRUE,
  .id = "model",
  .options = furrr_options(seed = 123)
  )

# waso
multi_waso <- 
  multi_all_diffs %>%
  map(~ filter(.x, !abs(diff_waso_min) > 100 & waso_min >= 0)) %>% 
  future_map_dfr(~ get_agree(
    df = .x,
    diff = "diff_waso_min",
    delta = 2,
  ),
  .progress = TRUE,
  .id = "model",
  .options = furrr_options(seed = 123)
  )


bind_rows(multi_spt, multi_tst, multi_se_percent, multi_lps, multi_waso) |>
  janitor::clean_names() |>
  write_csv("data/processed/multiclass_mixed_effect_ba.csv")

beepr::beep()