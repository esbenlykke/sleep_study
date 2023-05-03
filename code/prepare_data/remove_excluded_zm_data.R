#!/usr/bin/env Rscript

library(tidyverse)
library(hms)
library(lubridate)
library(arrow)

cat("Only including valid zm data. Lots of filter joins going on here...")

epoch_length <- 10

data <-
  read_parquet("data/processed/all_thigh_data.parquet") %>%
  rename(datetime = time) %>%
  mutate(
    id = as.numeric(id),
    noon_day = day(datetime - hours(12)),
    month = month(datetime - hours(12)),
    unix_time = as.integer(datetime) %/% epoch_length * epoch_length,
    datetime = as_datetime(unix_time),
    .after = 1
  )

# filter join the days from the zm data
noon_days <-
  data %>%
  distinct(id, noon_day, month) %>%
  arrange(id)

zm_data <-
  read_tsv("data/processed/zm_scores.tsv", progress = FALSE) %>%
  # filter(score != -5) %>%
  slice(rep(1:n(), each = 30 / epoch_length)) %>%
  mutate(
    unix_time = as.integer(datetime + rep_len(seq(0, 25, epoch_length), length.out = nrow(.))) %/% epoch_length * epoch_length,
    datetime = as_datetime(unix_time),
    noon_day = day(datetime - hours(12)),
    month = month(datetime - hours(12)),
    day = day(datetime),
    .after = 1
  ) %>%
  select(id, datetime, noon_day, month, score, -unix_time, -day)


# valid zm days are determined by as a minimum to be including sleep
# (i.e., score %in% c(2, 3, 5))
sensor_problem_days <-
  zm_data %>%
  count(id, noon_day, month, score) %>%
  filter(score == -5) %>%
  distinct(id, noon_day, month)

zm_data %>%
  semi_join(noon_days, by = c("id", "noon_day", "month")) %>%
  anti_join(sensor_problem_days, by = c("id", "noon_day", "month")) 

# Find in-bed periods shorter than 7 hrs and longer than 12 hrs
# see Hirshkowitz, 2014.
# bad_in_bed_periods <-
  zm_data %>%
  group_by(id, noon_day, month) %>%
  summarise(recording_length = n(), .groups = "drop") %>%
  filter(!
      recording_length < (7 * 60 * 60) / epoch_length & 
         recording_length > (12 * 60 * 60) / epoch_length
  )

# filter only for noon_days containing in-bed
only_in_bed_days <-
  temp %>%
  ungroup() %>%
  anti_join(bad_in_bed_periods, by = c("id", "noon_day")) %>%
  count(id, noon_day, group) %>%
  filter(n > 1)

# write data
temp %>%
  ungroup() %>%
  anti_join(bad_in_bed_periods, by = c("id", "noon_day")) %>%
  semi_join(only_in_bed_days, by = c("id", "noon_day")) %>%
  select(-c(score, group, in_bed_cumsum)) %>%
  arrange(id) %>%
  write_parquet("data/processed/all_thigh_no_bad_zm.parquet")
