#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)

yasa <-
  read_tsv("data/processed/yasa_preds.tsv")

start_time <- yasa |>
  pluck(5, 1) |>
  hms::as_hms() |>
  as.integer()

yasa |>
  slice(rep(1:n(), each = 30 / 5)) %>%
  mutate(
    epoch = epoch * 30,
    time_int = epoch + start_time,
    time_int = as.integer(time_int + rep_len(seq(0, 25, 5), length.out = nrow(.))),
    time_int_round5 = as.integer(time_int %/% 5 * 5),
    .after = 1
  ) |>
  select(-c(time_int, start)) |>
  write_parquet("data/processed/yasa_preds.parquet")
