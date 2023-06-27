#!/usr/bin/env Rscript
library(tidyverse)
library(slider)
library(arrow)
library(stringr)

# Function to count zeros in runs of at least three consecutive zeros (WASO)
count_each_zero_in_consecutive_zeros <- function(x, n = 3) {
  rle_x <- rle(x == 0)
  sum(rle_x$lengths[rle_x$values & rle_x$lengths >= n])
}

# File path
all_preds_filenames <- list.files("data/data_for_modelling/chained_classifiers/sleep_predictions", full.names = TRUE)

# Extract model names
model_names <- str_extract(all_preds_filenames, "(?<=/)[^/]+(?=_sleep_prediction_complete.parquet)")

# Different measures of sleep
sleep_types <- c("raw", "median_5", "median_10")

# Use map from purrr package to apply function over sleep measure
all_sleep_stats <- all_preds_filenames %>%
  set_names(model_names) %>%
  map(function(file) {
    test_dc <- read_parquet(file) %>%
      select(id, noon_day, month, datetime, matches("in_bed|sleep")) %>%
      # filter(id == 8504) %>%
      group_by(id, noon_day, month) %>%
      mutate(
        across(where(is.factor), as.numeric) - 1,
        across(matches("sleep|in_bed"), ~ slide_sum(.x, before = 12, after = 12), .names = "{.col}_slide_sum_12")
      )

    map_dfr(sleep_types, function(st) {
      test_dc %>%
        summarise(
          spt_hrs = if (any(in_bed_class_slide_sum_12 >= 20)) {
            (max(row_number()[in_bed_class_slide_sum_12 >= 20]) - min(row_number()[in_bed_class_slide_sum_12 >= 20])) * 30 / 60 / 60
          } else {
            NA
          },
          tst_hrs = sum(get(paste0("sleep_", st, "_pred_class"))[get(paste0("sleep_", st, "_pred_class_slide_sum_12")) >= 20]) * 30 / 60 / 60,
          se_percent = ifelse(spt_hrs != 0, 100 * (tst_hrs / spt_hrs), NA),
          lps_min = if (any(get(paste0("sleep_", st, "_pred_class_slide_sum_12")) >= 20) & any(in_bed_class_slide_sum_12 > 0)) {
            ((min(row_number()[get(paste0("sleep_", st, "_pred_class_slide_sum_12")) >= 20]) -
              min(row_number()[in_bed_class_slide_sum_12 > 0])) * 30) / 60
          } else {
            NA
          },
          waso_min = sum(get(paste0("sleep_", st, "_pred_class")) == 0 &
            get(paste0("sleep_", st, "_pred_class_slide_sum_12")) > 0 &
            get(paste0("sleep_", st, "_pred_class_slide_sum_12")) < 20) / 2,
          waso_3 = (count_each_zero_in_consecutive_zeros(get(paste0("sleep_", st, "_pred_class"))[get(paste0("sleep_", st, "_pred_class_slide_sum_12")) > 0 & get(paste0("sleep_", st, "_pred_class_slide_sum_12")) < 20]) * 30 / 60),
          sleep_type = st,
          .groups = "drop"
        )
    })
  })


############## Look at ZM predictions
zm_score <-
  read_tsv("data/processed/zm_scores.tsv") %>%
  mutate(
    noon_day = day(datetime - hours(12)),
    month = month(datetime - hours(12)),
    in_bed = 1,
    sleep_raw = if_else(score %in% c(2, 3, 5, -5), 1, 0),
    sleep_raw_slide_12 = slide_sum(sleep_raw, after = 24),
    sleep_median_5 = slide_dbl(sleep_raw, median, .before = 5, .after = 5),
    sleep_slide_12_median_5 = slide_sum(sleep_median_5, after = 24),
    sleep_median_10 = slide_dbl(sleep_raw, median, .before = 10, .after = 10),
    sleep_slide_12_median_10 = slide_sum(sleep_median_10, after = 24),
    .after = 1
  )

# Extract no edge SP days
no_edge_SP_zm_data <-
  zm_score %>%
  group_by(id, noon_day, month) %>%
  # For each group, perform the following steps
  group_modify(~ {
    # Store the data of the current group
    current_date_data <- .x

    # Check if the first or last recording of the night has a sensor problem (score == -5)
    first_problem <- current_date_data$score[1] != -5
    last_problem <- tail(current_date_data$score, n = 1) != -5

    # If there's no sensor problem at the beginning or end of the night, return the data
    if (first_problem & last_problem) {
      return(current_date_data)
    } else {
      # Otherwise, return an empty tibble, effectively removing this group from the final output
      return(tibble())
    }
  })

# Calculate precise sleep stats from raw zm predictions
calculated_zm_stats <-
  no_edge_SP_zm_data %>%
  group_by(id, noon_day, month) %>%
  summarise(
    spt_hrs_raw = n() * 30 / 60 / 60,
    tst_hrs_raw = sum(sleep_raw, na.rm = TRUE) * 30 / 60 / 60,
    se_percent_raw = ifelse(spt_hrs_raw != 0, 100 * (tst_hrs_raw / spt_hrs_raw), NA),
    lps_min_raw = if (any(sleep_raw_slide_12 == 20)) {
      (min(row_number()[sleep_raw_slide_12 == 20], na.rm = TRUE) + 3) * 30 / 60
    } else {
      NA
    },
    waso_3_min_raw = (count_each_zero_in_consecutive_zeros(sleep_raw) * 30 / 60) - lps_min_raw,
    spt_hrs_median_5 = n() * 30 / 60 / 60,
    tst_hrs_median_5 = sum(sleep_median_5, na.rm = TRUE) * 30 / 60 / 60,
    se_percent_median_5 = ifelse(spt_hrs_raw != 0, 100 * (tst_hrs_median_5 / spt_hrs_raw), NA),
    lps_min_median_5 = if (any(sleep_slide_12_median_5 == 20)) {
      (min(row_number()[sleep_slide_12_median_5 == 20], na.rm = TRUE) + 3) * 30 / 60
    } else {
      NA
    },
    waso_3_min_median_5 = (count_each_zero_in_consecutive_zeros(sleep_median_5) * 30 / 60) - lps_min_median_5,
    spt_hrs_median_10 = n() * 30 / 60 / 60,
    tst_hrs_median_10 = sum(sleep_median_10, na.rm = TRUE) * 30 / 60 / 60,
    se_percent_median_10 = ifelse(spt_hrs_raw != 0, 100 * (tst_hrs_median_10 / spt_hrs_raw), NA),
    lps_min_median_10 = if (any(sleep_slide_12_median_10 == 20)) {
      (min(row_number()[sleep_slide_12_median_10 == 20], na.rm = TRUE) + 3) * 30 / 60
    } else {
      NA
    },
    waso_3_min_median_10 = (count_each_zero_in_consecutive_zeros(sleep_median_10) * 30 / 60) - lps_min_median_10,
    .groups = "drop"
  ) %>%
  rename_with(.cols = -c(id, noon_day, month), ~ paste0("zm_", .))

all_sleep_stats_incl_zm <- all_sleep_stats %>%
  map(~ inner_join(.x, calculated_zm_stats, by = c("id", "noon_day", "month")) %>%
    drop_na())

walk2(
  all_sleep_stats_incl_zm, model_names,
  ~ write_csv(.x, paste0("data/data_for_modelling/chained_classifiers/sleep_stats/", .y, "_stats.csv"))
)

