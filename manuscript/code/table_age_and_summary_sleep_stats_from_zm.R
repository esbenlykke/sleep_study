#!/usr/bin/env Rscript
library(tidyverse)
library(slider)
library(arrow)
library(stringr)
library(gt)


count_each_zero_in_consecutive_zeros <- function(x, n = 3) {
  rle_x <- rle(x == 0)
  sum(rle_x$lengths[rle_x$values & rle_x$lengths >= n])
}


acc_ids <-
  read_parquet("data/data_for_modelling/no_edge_sp_incl_features_30_sec_epochs.parquet") %>%
  pull(id) %>%
  unique()

age <-
  read_parquet("data/data_for_modelling/no_edge_sp_incl_features_30_sec_epochs.parquet") %>%
  select(id, noon_day, month, age) %>%
  distinct()

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
  }) %>%
  ungroup()

# Calculate precise sleep stats from raw zm predictions
calculated_zm_stats <-
  no_edge_SP_zm_data %>%
  filter(id %in% acc_ids) %>%
  group_by(id, noon_day, month) %>%
  summarise(
    # age_mean = mean(age),
    # age_sd = sd(age),
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


# tab <-
bind_rows(
  zm_score %>%
    summarise(across(c(sleep_raw, sleep_median_5, sleep_median_10), list(
      sum = ~ sum(.x == 1),
      prop = ~ sum(.x == 1) / length(.x)
    )), .groups = "drop") %>%
    mutate(
      row = "sleep_count_and_ratio", .before = 1
    ) %>%
    rename(
      raw_stat = sleep_raw_sum, raw_stat2 = sleep_raw_prop,
      median_5_stat = sleep_median_5_sum, median_5_stat2 = sleep_median_5_prop,
      median_10_stat = sleep_median_10_sum, median_10_stat2 = sleep_median_10_prop
    ), calculated_zm_stats %>%
    # left_join(age, by = c("id", "noon_day", "month")) %>%
    select(contains("zm")) %>%
    reframe(across(everything(), list(
      mean = ~ mean(.x, na.rm = TRUE),
      sd = ~ sd(.x, na.rm = TRUE)
    ))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "type",
      values_to = "value"
    ) %>%
    mutate(type = str_replace(type, "zm_", "")) %>%
    separate(type, into = c("row", "column"), "_hrs_|_percent_|_3_min_|_min_|ge_") %>%
    pivot_wider(names_from = column, values_from = value) %>%
    rename(
      raw_stat = raw_mean, raw_stat2 = raw_sd, median_5_stat = median_5_mean,
      median_5_stat2 = median_5_sd,
      median_10_stat = median_10_mean, median_10_stat2 = median_10_sd
    )
) %>%
  gt() %>%
  fmt_number(rows = 1, decimals = 0) %>%
  fmt_percent(rows = 1, columns = c(3, 5, 7), decimals = 1) %>%
  fmt_number(rows = 2:6, decimals = 1)
  
  
# gtExtras::gtsave_extra(tab, "manuscript/visuals/table_stats_summary.html")
