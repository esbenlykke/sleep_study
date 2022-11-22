#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(arrow)

data <-
  read_parquet("data/processed/model_data/bsl_thigh.parquet")

## sensor independent features basded on static clock time
data |>
  group_by(id, noon_day) |>
  mutate(
    clock_group = if_else((hms::as_hms(datetime) > hms("18:00:00") | hms::as_hms(datetime) < hms("08:00:00")), 1, 0),
    .after = 1
  ) |>
  group_by(id, noon_day, clock_group) |>
  mutate(
    clock_proxy_cos = if_else(hms::as_hms(datetime) > hms("18:00:00") | hms::as_hms(datetime) < hms("08:00:00"),
      cos(seq(-(pi / 2), pi / 2, length.out = n())), 0
    ),
    clock_proxy_linear = if_else(hms::as_hms(datetime) > hms("18:00:00") | hms::as_hms(datetime) < hms("08:00:00"),
      seq(0, 1, length.out = n()), 0
    ),
    .after = 1
  ) |> 
  ungroup() |> 
  write_parquet("data/processed/model_data/bsl_thigh_sensor_independent_features.parquet")

# TODO make start and stop for sensor independent features based on sdacc_x (Skotte 2014)

# data |>
#   filter(id == 8505) |>
#   rowid_to_column() |> 
#   group_by(id, noon_day) |>
#   mutate(
#     threshold = sdacc_x > .01 & incl < 45 &
#       (hms::as_hms(datetime) < hms("23:00:00") & hms::as_hms(datetime) > hms("17:00:00")),
#     .after = 1
#   ) |>
#   filter(threshold == TRUE) |>
#   slice_max(n = 1, order_by = rowid) |> 
#   pull(rowid)

