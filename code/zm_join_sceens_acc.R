#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(arrow)

# TODO test this through

zm <-
  read_tsv("data/processed/zm_scores.tsv")

args <- R.utils::commandArgs(trailingOnly = TRUE)

path <- as.character(args[1])
dest <- as.character(args[2])

zm |>
  separate(datetime, c("date", "time"), sep = " ", remove = FALSE) |>
  slice(rep(1:n(), each = 30 / 5)) %>%
  mutate(
    unix_day = as.numeric(hms(time)),
    unix_day = unix_day + rep_len(seq(0, 25, 5), length.out = nrow(.)),
    unix_day = unix_day %/% 5 * 5,
    .after = 1
  ) |> left_join(
read_parquet(path) |>
  rename(datetime = time) |>
  separate(datetime, c("date", "hour"), sep = " ", remove = FALSE) |> 
  mutate(unix_day = as.numeric(hms(hour)),
         .after= 1), by = c("id", "unix_day")
) |> 
  write_parquet(dest)
