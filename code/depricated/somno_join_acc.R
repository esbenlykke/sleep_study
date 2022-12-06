#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(arrow)
library(hms)

acc <-
  read_feather("data/processed/acc_temp_psg_study.feather") |>
  mutate(id = as.numeric(id)) |>
  rename(datetime = time) |>
  mutate(
    time_int = as.integer(as_hms(datetime)),
    time_int_round5 = as.integer(time_int %/% 5 * 5),
    .after = 1
  )

somno <-
  read_tsv("data/processed/somno_sleep_profiles.tsv") |>
  slice(rep(1:n(), each = 30 / 5)) %>%
  mutate(
    time_int = as.integer(as_hms(time)),
    time_int_round5 = as.integer(time_int + rep_len(seq(0, 25, 5), length.out = nrow(.)))
  )

somno |>
  inner_join(acc, by = c("id", "time_int_round5")) |>
  select(id, datetime, time_int_round5, placement,
         x = X, y = Y, z = Z, temp = temperature, status, reliability) |>
  write_parquet("data/processed/somno_acc.parquet")