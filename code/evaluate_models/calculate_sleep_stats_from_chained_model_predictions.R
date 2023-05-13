#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)
library(lubridate)

# Function to process data
extract_in_bed_data <- function(data_path, model_filenames, epoch_length) {
  # Read data
  data <- read_parquet(data_path)

  # Extract model names from filenames
  model_names <- str_match(model_filenames, ".*/(.*).rds$")

  # Read fitted models
  fits <- map(model_filenames, read_rds)

  # Generate predictions
  preds <- map(fits, ~ .x %>%
    augment(data) %>%
    rename_with(~ str_replace(.x, ".pred", "in_bed")))

  # Further processing
  preds <- preds %>%
    map(~ .x %>%
      mutate(
        in_bed_filtered = slide_dbl(as.numeric(in_bed_class) - 1, median,
          .after = (7.5 * 60) / epoch_length, .before = (7.5 * 60) / epoch_length
        )
      ) %>%
      group_by(id, noon_day, month) %>%
      group_modify(~ .x %>%
        # Filter rows where row_number is within the in_bed_filtered range
        filter(row_number() > min(row_number()[in_bed_filtered == 1]) &
          row_number() < max(row_number()[in_bed_filtered == 1]))) %>%
      ungroup()) %>%
    set_names(model_names[, 2])

  preds
}

# Set common variables
models_dir <- "/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed"
model_filenames <- list.files(models_dir, full.names = TRUE)
data_dir <- list.files("data/data_for_modelling", full.names = TRUE) %>%
  str_subset("no_edge")

# Process data
all_data_10_sec_epochs_preds <-
  extract_in_bed_data(data_dir[[1]], model_filenames, 10)
all_data_30_sec_epochs_preds <-
  extract_in_bed_data(data_dir[[2]],model_filenames, 30)


test$in_bed_median5_xgboost_30_sec_epoch_fit %>%
  filter(id == 8505) %>%
  ggplot(aes(datetime)) +
  geom_line(aes(y = scale(sd_max) - 3), color = "pink", alpha = .8) +
  geom_line(aes(y = scale(incl) - 3), color = "grey70") +
  geom_line(aes(y = in_bed + 3), color = "grey20") +
  geom_line(aes(y = as.numeric(in_bed_class) + .5), color = "brown") +
  geom_line(aes(y = as.numeric(in_bed_class) + .5), color = "") +
  geom_line(aes(y = in_bed_filtered - .5), color = "darkblue") +
  scale_x_datetime(
    breaks = "1 hour",
    date_labels = "%H"
  ) +
  facet_wrap(~noon_day, scales = "free", ncol = 1)

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

preds_crude %>%
  group_by(model) %>%
  group_modify(~ create_crude_stats(.x), .keep = TRUE) %>%
  write_parquet("data/processed/crude_stats.parquet")

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

# Define a function to count zeros in runs of at least three consecutive zeros (WASO)
count_each_zero_in_consecutive_zeros <-
  function(x, n = 3) {
    rle_x <- rle(x == 0)
    sum(rle_x$lengths[rle_x$values & rle_x$lengths >= n])
  }


zm_stats <-
  read_csv("data/processed/all_zm_stats.csv") |>
  janitor::clean_names() |>
  mutate(
    day = day(start_date),
    month = month(start_date)
  ) |>
  select(id, day, month, spt_hrs, tst_hrs, se_percent, lps_min, waso_min) |>
  rename_with(.cols = -c(id, day, month), ~ paste0("zm_", .))


# Your string
s <- "/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed/in_bed_median5_decision_tree_30_sec_epoch_fit.rds"

# Use str_extract to match everything after the last slash and before the .rds
match <- str_match(s, ".*/(.*).rds$")

print(filename_no_ext)

