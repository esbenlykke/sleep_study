#!/usr/bin/env Rscript

library(tidyverse)
library(hms)
library(lubridate)
library(arrow)

bsl_int <-
  read_parquet("data/processed/screens_baseline.parquet") |>
  rename(datetime = time) %>%
  mutate(
    day = day(datetime),
    datetime = as.integer(datetime) %/% 5 * 5,
    .after = 1
  ) |>
  select(-sensor_code)

bsl_days <-
  bsl_int |>
  distinct(id, day) |>
  arrange(id)

zm_int <-
  read_tsv("data/processed/zm_scores.tsv") |>
  # filter(score != -5) |>
  slice(rep(1:n(), each = 30 / 5)) %>%
  mutate(
    day = day(datetime),
    datetime = as.integer(datetime + rep_len(seq(0, 25, 5), length.out = nrow(.))) %/% 5 * 5,
    .after = 1
  ) |>
  select(id, datetime, day, score) |>
  semi_join(bsl_days)

# valid zm days are determined by as a minimum to be including sleep
# (i.e., score %in% c(2, 3, 5))
valid_zm_days <-
  zm_int |>
  count(id, day, score) |>
  group_by(day) |>
  filter(score %in% c(2, 3, 5)) |>
  distinct(id, day)

# the case_when() replaces up to 20 consecutive NAs with in-bed times (blips of NAs
# is random noise produced by clock synchronization errors, I believe)
temp <-
  bsl_int |>
  semi_join(valid_zm_days) |>
  filter(placement == "thigh") |>
  left_join(zm_int, by = c("id", "datetime", "day")) |>
  mutate(
    score = if_else(score == -5, NA_real_, score),
    in_bed = if_else(is.na(score), 0, 1),
    sleep = if_else(score %in% c(NA, 0), 0, 1),
    in_bed = case_when(
      is.na(score) & (lag(in_bed == 1, default = 0) & lead(in_bed == 1, default = 0)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 2) & lead(in_bed == 1, default = 0, n = 2)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 3) & lead(in_bed == 1, default = 0, n = 3)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 4) & lead(in_bed == 1, default = 0, n = 4)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 5) & lead(in_bed == 1, default = 0, n = 5)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 6) & lead(in_bed == 1, default = 0, n = 6)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 7) & lead(in_bed == 1, default = 0, n = 7)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 8) & lead(in_bed == 1, default = 0, n = 8)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 9) & lead(in_bed == 1, default = 0, n = 9)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 10) & lead(in_bed == 1, default = 0, n = 10)) ~ 1,
      TRUE ~ in_bed
    )
  ) |>
  group_by(group = cumsum(in_bed == 0), id) |>
  mutate(
    in_bed_cumsum = cumsum(in_bed)
  )

rm(bsl_int, zm_int)
gc()

# Find in-bed periods longer than 7 hrs and shorter than 12 hrs
# see Hirshkowitz, 2014.
bad_in_bed_periods <-
  temp |>
  summarise(max_cumsum = max(in_bed_cumsum), .groups = "drop") |>
  filter(max_cumsum != 0 & (max_cumsum < 5040 | max_cumsum > 8640))

temp |>
  ungroup() |>
  anti_join(bad_in_bed_periods, by = c("id", "group")) |>
  select(-c(score, group, in_bed_cumsum)) |>
  write_parquet("data/processed/bsl_thigh_no_bad_zm.parquet")

