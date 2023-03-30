#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)
library(SimplyAgree)
library(furrr)


crude_stats <-
  read_parquet("data/processed/crude_stats.parquet")

multi_stats <- 
  read_parquet("data/processed/multiclass_stats.parquet")


get_diff_stats <-
  function(tbl) {
    tbl |>
      mutate(
        diff_spt_hrs = spt_hrs - zm_spt_hrs,
        diff_tst_hrs = tst_hrs - zm_tst_hrs,
        diff_se_percent = se_percent - zm_se_percent,
        diff_lps_min = lps_min - zm_lps_min,
        diff_waso_min = waso_min - zm_waso_min
      ) |>
      rowwise() |>
      mutate(
        avg_spt_hrs = mean(c(spt_hrs, zm_spt_hrs)),
        avg_tst_hrs = mean(c(tst_hrs, zm_tst_hrs)),
        avg_se_percent = mean(c(se_percent, zm_se_percent)),
        avg_lps_min = mean(c(lps_min, zm_lps_min)),
        avg_waso_min = mean(c(waso_min, zm_waso_min))
      ) |>
      ungroup()
  }


crude_all_diffs <-
  get_diff_stats(crude_stats) %>%
  group_split(model) %>%
  set_names(c("decision_tree", "logistic_regression", "neural_network", "xgboost")) %>%
  map(drop_na) 

multi_all_diffs <- 
  get_diff_stats(multi_stats) %>%
  group_split(model) %>%
  set_names(c("decision_tree", "decision_tree_SMOTE", "logistic_regression", "neural_network", "xgboost")) %>%
  map(drop_na) 


# This method is nonparametric and accounts for repeated measures. Double check with Jan!

# Mixed Effects Limits of Agreement
# This function allows for the calculation of bootstrapped limits of agreement
# when there are multiple observations per subject


get_agree <- function(df, diff, delta, x_axis) {
  df %>%
    loa_mixed(
      diff = diff,
      condition = "noon_day",
      id = "id",
      data = .,
      delta = delta,
      replicates = 1000
    ) |>
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