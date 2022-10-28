#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(hms)
library(arrow)

# TODO test this through

zm <-
  read_tsv("data/processed/zm_scores.tsv")

args <- R.utils::commandArgs(trailingOnly = TRUE)

path <- as.character(args[1])
dest <- as.character(args[2])

zm |>
  slice(rep(1:n(), each = 30 / 5)) %>%
  mutate(
    time_int = as.numeric(as_hms(datetime)),
    time_int = time_int + rep_len(seq(0, 25, 5), length.out = nrow(.)),
    time_int = time_int %/% 5 * 5,
    .after = 1
  ) |>
  left_join(
    read_parquet(path) |>
      rename(datetime = time) |>
      mutate(
        time_int = as.numeric(as_hms(datetime)),
        .after = 1
      ),
    by = c("id", "time_int")
  ) |>
  drop_na() |>
  write_parquet(dest)
