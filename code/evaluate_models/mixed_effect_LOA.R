library(tidyverse)
library(arrow)
library(SimplyAgree)


stats_files <-
  list.files("~/sleep_study/data/processed/stats_predictions", full.names = TRUE)

all_stats <-
  map(stats_files, read_parquet) |>
  set_names(c("logistic_regression", "neural_net", "decision_tree", "xgboost"))


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


all_diffs <-
  all_stats |>
  map(get_diff_stats)


# This method is nonparametric and accounts for repeated measures. Double check with Jan!

# Mixed Effects Limits of Agreement
# This function allows for the calculation of bootstrapped limits of agreement 
# when there are multiple observations per subject

get_agree <- function(df = all_diffs, diff, condition = "noon_day", delta) {
  map(df, ~ loa_mixed(
    diff = diff,
    condition = condition,
    id = "id",
    data = .,
    delta = delta,
    replicates = 1000
  )) |>
    map_dfr("loa", .id = "model") |>
    rownames_to_column(var = "ba_metric") |>
    mutate(
      ba_metric = str_remove(ba_metric, "...\\d"),
      ba_metric = str_to_lower(str_replace(ba_metric, " ", "_")),
      type = pred
    )
}
# spt
spt <- get_agree(all_diffs, "spt_hrs", "zm_spt_hrs", 2)

# tst
tst <- get_agree(all_diffs, "tst_hrs", "zm_tst_hrs", 2)

# se_percent
se_percent <- get_agree(all_diffs, "se_percent", "zm_se_percent", 20)

# lps_min
lps <- get_agree(all_diffs, "lps_min", "zm_lps_min", 20)

# waso
waso <- get_agree(all_diffs, "waso_min", "zm_waso_min", 10)

ba_metrics <-
  as_tibble(bind_rows(spt, tst, se_percent, lps, waso))

beepr::beep(3)