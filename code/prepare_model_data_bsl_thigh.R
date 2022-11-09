#!/usr/bin/env Rscript

library(tidyverse)
library(hms)
library(lubridate)
library(arrow)

dir.create("data/processed/model_data")

# args <- R.utils::commandArgs(trailingOnly = TRUE)
# acc <- args[1]
# zm <- args[2]
# dest <- args[3]
# placement <- args[4]


bsl_int <-
  read_parquet("data/processed/screens_baseline.parquet") |>
  filter(placement == "thigh") |>
  rename(datetime = time) %>%
  mutate(
    day = day(datetime),
    datetime = as.integer(datetime) %/% 5 * 5,
    .after = 1
  ) |>
  select(-sensor_code)


zm_int <-
  read_tsv("data/processed/zm_scores.tsv") |>
  filter(score != -5) |>
  slice(rep(1:n(), each = 30 / 5)) %>%
  mutate(
    day = day(datetime),
    datetime = as.integer(datetime + rep_len(seq(0, 25, 5), length.out = nrow(.))) %/% 5 * 5,
    .after = 1
  ) |>
  select(id, datetime, day, score)

bsl_int |>
  left_join(zm_int, by = c("id", "datetime", "day")) |>
  mutate(
    in_bed = if_else(is.na(score), 0, 1),
    sleep = if_else(score %in% c(NA, 0), 0, 1)
  ) |>
  select(id, datetime, age, x:temp, in_bed, sleep) |>
  write_parquet("data/processed/model_data/bsl_thigh.parquet")
