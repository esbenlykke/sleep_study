#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(hms)
library(arrow)

# Read in command line arguments
args <- R.utils::commandArgs(trailingOnly = TRUE)
path <- as.character(args[1])
dest <- as.character(args[2])

# Read in accelerometer data
acc_all <-
  read_parquet(path) |>
  relocate(datetime = time, .after = 1) |>
  # Create columns for time intervals rounded to nearest multiple of 5
  mutate(
    time_int = as.integer(as_hms(datetime)),
    time_int_round5 = as.integer(time_int %/% 5 * 5),
    .after = 1
  ) |>
  # Drop unnecessary columns and sort by id
  select(-c(time_int, sensor_code, datetime)) |>
  arrange(id)

# Get list of child ids from accelerometer data
child_ids_acc <- acc_all |>
  distinct(id) |>
  pull()

# Read in Z-scores data and filter by child ids
zm <-
  read_tsv("data/processed/zm_scores.tsv") |>
  filter(id %in% child_ids_acc) |>
  # Repeat rows to get 5-minute intervals
  slice(rep(1:n(), each = 30 / 5)) %>%
  # Create columns for time intervals rounded to nearest multiple of 5
  mutate(
    time_int = as.integer(as_hms(datetime)),
    time_int = as.integer(time_int + rep_len(seq(0, 25, 5), length.out = nrow(.))),
    time_int_round5 = as.integer(time_int %/% 5 * 5),
    .after = 1
  ) |>
  # Drop unnecessary columns
  select(-time_int)

# Get list of child ids from Z-scores data
child_ids_zm <-
  zm |>
  distinct(id) |>
  pull()

# Filter accelerometer data by child ids
acc <-
  acc_all |>
  filter(id %in% child_ids_zm)

# Remove original accelerometer data from memory
rm(acc_all)
gc()

# Create temporary directory
dir.create(temp <- tempfile())

# Write out accelerometer data to parquet files by child id
acc |>
  group_by(id) |>
  group_walk(~ write_parquet(.x, file.path(temp, paste0(.y$id, ".parquet"))))

# Get list of parquet files in temporary directory
list.files(temp, "parquet$", full.names = TRUE)

# Delete temporary directory and its contents
unlink(temp, recursive = TRUE)

# Join accelerometer and Z-scores data by child id and 5-minute intervals
tibble(
  acc = acc |> group_split(id),
  zm = zm |> group_split(id)
) |>
  mutate(combi = map2(acc, zm, ~ inner_join(.x, .y, by = "time_int_round5")))

# Join accelerometer and Z-scores data by child id and 5-minute intervals and return list of data frames
acc |>
  group_by(id) |>
  group_map(~ inner_join(.x, .y = zm, by = "time_int_round5"))
