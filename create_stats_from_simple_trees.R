#!/usr/bin/env RscriptsuppressMessages(library(tidyverse))

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))
library(slider)

test <-
  read_parquet("data/processed/screens_test_data.parquet") |> 
  filter(id == 8504)

# Load models -------------------------------------------------------------


in_bed_CART_fit <- read_rds("data/models/fitted_models/in_bed_simple_CART_fit")
sleep_CART_fit <- read_rds("data/models/fitted_models/sleep_simple_CART_fit")

zm_stats <-
  read_csv("data/processed/all_zm_stats.csv") |> 
  janitor::clean_names()

zm_stats |> filter(id == 8504) |> 
  mutate(day = lubridate::day(start_date)) |> 
  select(id, day, spt_hrs, tst_hrs, se_percent, lps_min, waso_min)

# create stats ------------------------------------------------------------

in_bed_CART_fit |>
  augment(test) |>
  select(id, datetime, noon_day, in_bed_pred = .pred_class) |>
  bind_cols(
    sleep_CART_fit |>
      predict(test) |>
      rename(sleep_pred = .pred_class)
  ) |>
  mutate(
    in_bed_sleep = if_else(in_bed_pred == 1 & sleep_pred == 1, 1, 0),
    in_bed_no_sleep = if_else(in_bed_pred == 1 & sleep_pred == 0, 1, 0)
  ) |>
  mutate(across(in_bed_pred:sleep_pred, as.integer) - 1) |>
  group_by(id, noon_day) |>
  summarise(
    # this is the most non-DRY shit ever!! How to fix???
    spt_hrs = ((max(row_number()[in_bed_pred == 1 &
      lag(in_bed_pred, n = 1) == 1 &
      lag(in_bed_pred, n = 2) == 1 &
      lag(in_bed_pred, n = 3) == 1 &
      lag(in_bed_pred, n = 4) == 1 &
      lag(in_bed_pred, n = 5) == 1 &
      lag(in_bed_pred, n = 6) == 1 &
      lag(in_bed_pred, n = 7) == 1 &
      lag(in_bed_pred, n = 8) == 1 &
      lag(in_bed_pred, n = 9) == 1 &
      lag(in_bed_pred, n = 10) == 1 &
      lag(in_bed_pred, n = 11) == 1 &
      lag(in_bed_pred, n = 12) == 1 &
      lag(in_bed_pred, n = 13) == 1 &
      lag(in_bed_pred, n = 14) == 1 &
      lag(in_bed_pred, n = 15) == 1 &
      lag(in_bed_pred, n = 16) == 1 &
      lag(in_bed_pred, n = 17) == 1 &
      lag(in_bed_pred, n = 18) == 1 &
      lag(in_bed_pred, n = 19) == 1 &
      lag(in_bed_pred, n = 20) == 1 &
      lag(in_bed_pred, n = 21) == 1 &
      lag(in_bed_pred, n = 22) == 1 &
      lag(in_bed_pred, n = 23) == 1 &
      lag(in_bed_pred, n = 24) == 1 &
      lag(in_bed_pred, n = 25) == 1 &
      lag(in_bed_pred, n = 26) == 1 &
      lag(in_bed_pred, n = 27) == 1 &
      lag(in_bed_pred, n = 28) == 1 &
      lag(in_bed_pred, n = 29) == 1 &
      lag(in_bed_pred, n = 30) == 1
      ]) -
      min(row_number()[in_bed_pred == 1 &
        lead(in_bed_pred, n = 1) == 1 &
        lead(in_bed_pred, n = 2) == 1 &
        lead(in_bed_pred, n = 3) == 1 &
        lead(in_bed_pred, n = 4) == 1 &
        lead(in_bed_pred, n = 5) == 1 &
        lead(in_bed_pred, n = 6) == 1 &
        lead(in_bed_pred, n = 7) == 1 &
        lead(in_bed_pred, n = 8) == 1 &
        lead(in_bed_pred, n = 9) == 1 &
        lead(in_bed_pred, n = 10) == 1 &
        lead(in_bed_pred, n = 11) == 1 &
        lead(in_bed_pred, n = 12) == 1 &
        lead(in_bed_pred, n = 13) == 1 &
        lead(in_bed_pred, n = 14) == 1 &
        lead(in_bed_pred, n = 15) == 1 &
        lead(in_bed_pred, n = 16) == 1 &
        lead(in_bed_pred, n = 17) == 1 &
        lead(in_bed_pred, n = 18) == 1 &
        lead(in_bed_pred, n = 19) == 1 &
        lead(in_bed_pred, n = 20) == 1 &
        lead(in_bed_pred, n = 21) == 1 &
        lead(in_bed_pred, n = 22) == 1 &
        lead(in_bed_pred, n = 23) == 1 &
        lead(in_bed_pred, n = 24) == 1 &
        lead(in_bed_pred, n = 25) == 1 &
        lead(in_bed_pred, n = 26) == 1 &
        lead(in_bed_pred, n = 27) == 1 &
        lead(in_bed_pred, n = 28) == 1 &
        lead(in_bed_pred, n = 29) == 1 &
        lead(in_bed_pred, n = 30) == 1
        ])) * 10) / 60 / 60,
    tst_hrs = (sum(sleep_pred) * 10) / 60 / 60,
    se_percent = 100 * (tst_hrs / spt_hrs),
    lps_min = NA_real_,
    waso_min = (sum(in_bed_no_sleep) * 10) / 60
  )

zm_stat <- zm_test |> filter(day %in% c(9, 13, 14, 15)) 
tree_pred_stat <- test_stat |> rename(day = noon_day)