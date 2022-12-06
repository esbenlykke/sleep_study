#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(hms)
library(arrow)

# path <- "data/processed/screens_baseline.parquet"

args <- R.utils::commandArgs(trailingOnly = TRUE)

path <- as.character(args[1])
dest <- as.character(args[2])

acc_all <-
  read_parquet(path) |>
  relocate(datetime = time, .after = 1) |>
  mutate(
    time_int = as.integer(as_hms(datetime)),
    time_int_round5 = as.integer(time_int %/% 5 * 5),
    .after = 1
  ) |>
  select(-c(time_int, sensor_code, datetime)) |>
  arrange(id)

child_ids_acc <- acc_all |>
  distinct(id) |>
  pull()

zm <-
  read_tsv("data/processed/zm_scores.tsv") |>
  filter(id %in% child_ids_acc) |>
  slice(rep(1:n(), each = 30 / 5)) %>%
  mutate(
    time_int = as.integer(as_hms(datetime)),
    time_int = as.integer(time_int + rep_len(seq(0, 25, 5), length.out = nrow(.))),
    time_int_round5 = as.integer(time_int %/% 5 * 5),
    .after = 1
  ) |>
  select(-time_int)

child_ids_zm <-
  zm |>
  distinct(id) |>
  pull()

acc <-
  acc_all |>
  filter(id %in% child_ids_zm)

rm(acc_all)
gc()

# TESTING

dir.create(temp <- tempfile())

acc |>
  group_by(id) |>
  group_walk(~ write_parquet(.x, file.path(temp, paste0(.y$id, ".parquet"))))

list.files(temp, "parquet$", full.names = TRUE)

unlink(temp, recursive = TRUE)

tibble(
  acc = acc |> group_split(id),
  zm = zm |> group_split(id)
) |>
  mutate(combi = map2(acc, zm, ~ inner_join(.x, .y, by = "time_int_round5")))

acc |>
  group_by(id) |>
  group_map(~ inner_join(.x, .y = zm, by = "time_int_round5"))