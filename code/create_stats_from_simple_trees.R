#!/usr/bin/env RscriptsuppressMessages(library(tidyverse))

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))
library(slider)
library(lubridate)

test <-
  read_parquet("data/processed/screens_test_data.parquet")

# Load models -------------------------------------------------------------


in_bed_CART_fit <- read_rds("data/models/fitted_models/in_bed_simple_CART_fit")
sleep_CART_fit <- read_rds("data/models/fitted_models/sleep_simple_CART_fit")

zm_stats <-
  read_csv("data/processed/all_zm_stats.csv") |>
  janitor::clean_names() |>
  mutate(
    day = day(start_date),
    month = month(start_date)
  ) |>
  select(id, day, month, spt_hrs, tst_hrs, se_percent, lps_min, waso_min) |>
  rename_with(.cols = -c(id, day, month), ~ paste0("zm_", .))

# create stats ------------------------------------------------------------


pred_stats <-
  in_bed_CART_fit |>
  augment(test) |>
  select(id, datetime, noon_day, in_bed_pred = .pred_class) |>
  bind_cols(
    sleep_CART_fit |>
      predict(test) |>
      rename(sleep_pred = .pred_class)
  ) |>
  mutate(across(in_bed_pred:sleep_pred, as.integer) - 1) |>
  mutate(
    month = month(datetime),
    in_bed_72 = slide_sum(in_bed_pred, before = 36, after = 36),
    sleep_72 = slide_sum(sleep_pred, before = 36, after = 36),
    in_bed_sleep = if_else(in_bed_72 > 60 & sleep_72 > 60, 1, 0),
    in_bed_no_sleep = if_else(in_bed_pred == 1 & sleep_pred == 0, 1, 0)
  ) |>
  group_by(id, noon_day, month) |>
  summarise(
    spt_hrs = ((max(row_number()[in_bed_72 == 1]) - min(row_number()[in_bed_72 == 36])) * 10) / 60 / 60,
    tst_hrs = (sum(in_bed_sleep) * 10) / 60 / 60,
    se_percent = 100 * (tst_hrs / spt_hrs),
    lps_min = (((min(row_number()[sleep_72 == 36])) - min(row_number()[in_bed_72 == 36])) * 10) / 60,
    waso_min = (sum(in_bed_no_sleep) * 10) / 60,
    .groups = "drop"
  )


pred_stats |>
  inner_join(zm_stats, by = c("id", "noon_day" = "day", "month")) |> 
  write_parquet("data/processed/pred_stats_zm_stats.parquet")

