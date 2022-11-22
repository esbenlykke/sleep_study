#!/usr/bin/env Rscript

library(tidyverse)
library(hms)
library(lubridate)
library(arrow)

epoch_length <- 10

bsl_int <-
  read_parquet("data/processed/screens_baseline.parquet") |>
  rename(datetime = time) %>%
  mutate(
    day = day(datetime),
    noon_day = day(datetime - hours(12)),
    unix_time = as.integer(datetime) %/% epoch_length * epoch_length,
    datetime = as_datetime(unix_time),
    .after = 1
  ) |>
  arrange(id) |>
  select(id, datetime, unix_time, day, noon_day, age:sd_max, -sensor_code)

# filter join the bsl_days from the zm data
bsl_noon_days <-
  bsl_int |>
  distinct(id, noon_day) |>
  arrange(id)

zm_int <-
  read_tsv("data/processed/zm_scores.tsv") |>
  # filter(score != -5) |>
  slice(rep(1:n(), each = 30 / epoch_length)) %>%
  mutate(
    unix_time = as.integer(datetime + rep_len(seq(0, 25, epoch_length), length.out = nrow(.))) %/% epoch_length * epoch_length,
    datetime = as_datetime(unix_time),
    noon_day = day(datetime - hours(12)),
    day = day(datetime),
    .after = 1
  ) |>
  select(id, datetime, unix_time, day, noon_day, score, -unix_time, -day) |>
  semi_join(bsl_noon_days)

# valid zm days are determined by as a minimum to be including sleep
# (i.e., score %in% c(2, 3, 5))
valid_zm_days <-
  zm_int |>
  count(id, noon_day, score) |>
  group_by(noon_day) |>
  filter(score %in% c(2, 3, 5)) |>
  distinct(id, noon_day)

# the case_when() replaces up to 20 consecutive NAs with in-bed times (blips of NAs
# is random noise produced by clock synchronization errors, I believe)
temp <-
  bsl_int |>
  semi_join(valid_zm_days, by = c("id", "noon_day")) |>
  filter(placement == "thigh") |>
  left_join(zm_int, by = c("id", "datetime", "noon_day")) |>
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
      is.na(score) & (lag(in_bed == 1, default = 0, n = 11) & lead(in_bed == 1, default = 0, n = 11)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 12) & lead(in_bed == 1, default = 0, n = 12)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 13) & lead(in_bed == 1, default = 0, n = 13)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 14) & lead(in_bed == 1, default = 0, n = 14)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 15) & lead(in_bed == 1, default = 0, n = 15)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 16) & lead(in_bed == 1, default = 0, n = 16)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 17) & lead(in_bed == 1, default = 0, n = 17)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 18) & lead(in_bed == 1, default = 0, n = 18)) ~ 1,
      is.na(score) & (lag(in_bed == 1, default = 0, n = 19) & lead(in_bed == 1, default = 0, n = 19)) ~ 1,
      TRUE ~ in_bed
    )
  ) |>
  group_by(group = cumsum(in_bed == 0), id) |>
  mutate(
    in_bed_cumsum = cumsum(in_bed)
  )

rm(bsl_int, zm_int)
gc()

# Find in-bed periods shorter than 7 hrs and longer than 12 hrs
# see Hirshkowitz, 2014.
bad_in_bed_periods <-
  temp |>
  group_by(id, noon_day) |>
  summarise(max_cumsum = max(in_bed_cumsum), .groups = "drop") |>
  filter(
    max_cumsum != 0 & (max_cumsum < (7 * 60 * 60) / epoch_length | max_cumsum > (12 * 60 * 60) / epoch_length)
  )

# ID's with no in_bed time. Not necessary cf. "only_in_bed_days"
# no_in_bed <-
# temp |>
# ungroup() |>
# anti_join(bad_in_bed_periods, by = c("id", "noon_day")) |>
# count(id, in_bed) |>
# pivot_wider(id, names_from = in_bed, values_from = n, names_prefix = "in_bed_") |>
# filter(is.na(in_bed_1)) |>
# pull(id)

# filter only for noon_days containing in-bed
only_in_bed_days <-
  temp |>
  ungroup() |>
  anti_join(bad_in_bed_periods, by = c("id", "noon_day")) |>
  count(id, noon_day, group) |>
  filter(n > 1)

# write data
temp |>
  ungroup() |>
  anti_join(bad_in_bed_periods, by = c("id", "noon_day")) |>
  semi_join(only_in_bed_days, by = c("id", "noon_day")) |>
  select(-c(score, group, in_bed_cumsum)) |>
  arrange(id) |>
  write_parquet("data/processed/bsl_thigh_no_bad_zm.parquet")
