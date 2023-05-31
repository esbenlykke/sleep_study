library(tidyverse)
library(slider)
library(arrow)


create_stats <- function(tbl){
  tbl %>% 
    mutate(
      .pred_class = as.numeric(.pred_class) - 1,
      sleep_12_min = slide_sum(.pred_class, before = 12, after = 12)
    ) %>%
    select(-contains("in_bed")) %>% 
    group_by(id, noon_day, month) %>%
    summarise(
      spt_hrs = n() * 30 / 60 / 60,
      tst_hrs = (sum(.pred_class) * 30) / 60 / 60,
      se_percent = 100 * (tst_hrs / spt_hrs),
      lps_min = (((min(row_number()[sleep_12_min == 20])) - 1) * 30) / 60,
      waso_min = ((sum(.pred_class == 0) - lps_min) * 30) / 60,
      .groups = "drop"
    )
}

# Define a function to count zeros in runs of at least three consecutive zeros (WASO)
count_each_zero_in_consecutive_zeros <-
  function(x, n = 3) {
    rle_x <- rle(x == 0)
    sum(rle_x$lengths[rle_x$values & rle_x$lengths >= n])
  }

preds_30_sec %>% 
  map_depth(1, create_stats)


preds_30_sec$in_bed_by_xgboost$median_5[[1]] %>% 
  filter(id == 8504) %>%
  group_by(id, noon_day, month) %>%
  mutate(
    .pred_class = as.numeric(.pred_class) - 1,
    sleep_12_min = slide_sum(.pred_class, before = 12, after = 12)
  ) %>%
  select(-contains("in_bed")) %>% 
  summarise(
    spt_hrs = n() * 30 / 60 / 60,
    tst_hrs = (sum(.pred_class) * 30) / 60 / 60,
    se_percent = 100 * (tst_hrs / spt_hrs),
    lps_min = (((min(row_number()[sleep_12_min == 20])) - 1) * 30) / 60,
    waso_min = ((sum(.pred_class == 0) - lps_min) * 30) / 60,
    waso_3 = count_each_zero_in_consecutive_zeros(.pred_class),
    .groups = "drop"
  )

# Read in the summary statistics for all participants
zm_stats <- read_csv("data/processed/all_zm_stats.csv") %>% 
  janitor::clean_names()

zm_stats %>% 
  filter(id == 8504)










create_stats <- function(tbl, pred_class_col) {
  tbl %>%
    mutate(
      !TODO = as.numeric(!TODO) - 1,
      sleep_12_min = slide_sum(!TODO, before = 12, after = 12)
    ) %>%
    select(-contains("in_bed")) %>%
    group_by(id, noon_day, month) %>%
    summarise(
      spt_hrs = n() * 30 / 60 / 60,
      tst_hrs = (sum(.data[[pred_class_col]]) * 30) / 60 / 60,
      se_percent = 100 * (tst_hrs / spt_hrs),
      lps_min = (((min(row_number()[sleep_12_min == 60])) - min(row_number() - 30)) * 30) / 60,
      waso_min = ((sum(.data[[pred_class_col]] == 1) - lps_min) * 30) / 60,
      .groups = "drop"
    )
}

# pred_colnames <- c(
#   "pred_class_sleep_10_min_median_decision_tree_30_sec_epochs",
#   "pred_class_sleep_5_min_median_decision_tree_30_sec_epochs",
#   "pred_class_sleep_raw_decision_tree_30_sec_epochs",
#   "pred_class_sleep_10_min_median_logistic_regression_30_sec_epochs",
#   "pred_class_sleep_5_min_median_logistic_regression_30_sec_epochs",
#   "pred_class_sleep_raw_logistic_regression_30_sec_epochs",
#   "pred_class_sleep_10_min_median_neural_network_30_sec_epochs",
#   "pred_class_sleep_5_min_median_neural_network_30_sec_epochs",
#   "pred_class_sleep_raw_neural_network_30_sec_epochs",
#   "pred_class_sleep_10_min_median_xgboost_30_sec_epochs",
#   "pred_class_sleep_5_min_median_xgboost_30_sec_epochs",
#   "pred_class_sleep_raw_xgboost_30_sec_epochs"
# )















create_stats <- function(tbl) {
  tbl %>%
    mutate(across(c(pred_in_bed, pred_sleep), as.integer) - 1) |>
    mutate(
      month = month(datetime - hours(12)),
      in_bed_72 = slide_sum(pred_in_bed, before = 36, after = 36),
      sleep_72 = slide_sum(pred_sleep, before = 36, after = 36),
      in_bed_sleep = if_else(in_bed_72 > 60 & sleep_72 > 60, 1, 0),
      in_bed_no_sleep = if_else(in_bed_72 > 60 & sleep_72 < 60, 1, 0)
    ) |>
    group_by(id, noon_day, month) |>
    summarise(
      spt_hrs = ((max(row_number()[in_bed_72 == 60] + 30) - min(row_number()[in_bed_72 == 60] - 30)) * 10) / 60 / 60,
      tst_hrs = (sum(in_bed_sleep) * 10) / 60 / 60,
      se_percent = 100 * (tst_hrs / spt_hrs),
      lps_min = (((min(row_number()[sleep_72 == 60])) - min(row_number()[in_bed_72 == 60] - 30)) * 10) / 60,
      waso_min = ((sum(in_bed_no_sleep) - lps_min) * 10) / 60,
      .groups = "drop"
    ) |>
    inner_join(zm_stats, by = c("id", "noon_day" = "day", "month"))
}

preds %>%
  group_by(model) %>%
  group_modify(~ create_crude_stats(.x), .keep = TRUE)

tbl %>%
  mutate(
    noon_day = day(datetime - hours(12)),
    month = month(datetime - hours(12)),
    score_filtered = slide_dbl(score, median, .after = 10), # 5 min window
    sleep_raw = if_else(score != 0, 1, 0),
    sleep_filter_5 = if_else(score_filtered != 0, 1, 0),
    sleep_12_cumsum_raw = slide_dbl(sleep_raw, sum, .after = 24),
    sleep_12_cumsum_filter_5 = slide_dbl(sleep_filter_5, sum, .after = 24)
  ) %>%
  group_by(id, noon_day, month) |>
  summarise(
    median_5_spt = n() * 30 / 60 / 60,
    median_5_tst = sum(sleep_filter_5) * 30 / 60 / 60,
    median_5_se = 100 * (median_5_tst / median_5_spt),
    median_5_lps = (min(row_number()[sleep_12_cumsum_filter_5 == 20]) + 4) * 30 / 60,
    median_5_waso = count_each_zero_in_consecutive_zeros(sleep_filter_5, 3) * 30 / 60 - median_5_lps,
    raw_tst = sum(sleep_raw) * 30 / 60 / 60,
    raw_se_percent = 100 * (raw_tst / median_5_spt),
    raw_lps = (min(row_number()[sleep_12_cumsum_raw == 20]) + 4) * 30 / 60,
    raw_waso = count_each_zero_in_consecutive_zeros(sleep_raw, 3) * 30 / 60 - raw_lps
  )




zm_stats <-
  read_csv("data/processed/all_zm_stats.csv") |>
  janitor::clean_names() |>
  mutate(
    day = day(start_date),
    month = month(start_date)
  ) |>
  select(id, day, month, spt_hrs, tst_hrs, se_percent, lps_min, waso_min) |>
  rename_with(.cols = -c(id, day, month), ~ paste0("zm_", .))
