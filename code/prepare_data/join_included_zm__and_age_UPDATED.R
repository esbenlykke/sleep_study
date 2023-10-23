#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(hms)
library(arrow)
library(slider)

epoch_length <- 30

path <- "/media/esbenlykke/My Passport/all_cwa_children/output"


all_files <- list.files(path, full.names = TRUE)
file_batches <- split(all_files, ceiling(seq_along(all_files) / 102))

# Read in accelerometer data
process_batch <- function(batch_files) {
  acc_all <-
    map_dfr(batch_files, read_parquet) %>%
    relocate(id, .before = 1) %>%
    mutate(id = as.integer(id)) %>%
    arrange(id)

  # Get list of child ids from accelerometer data
  child_ids_acc <- acc_all |>
    distinct(id) |>
    pull()

  # Remove excluded ZM data
  zm <-
    read_tsv("data/processed/zm_scores.tsv", col_select = c(id, datetime, score)) %>%
    filter(id %in% child_ids_acc) %>%
    # Repeat rows to get 5-minute intervals
    mutate(
      id = as.integer(id),
      datetime = floor_date(datetime, "30 seconds")
    ) %>%
    rename(epoch = datetime) %>%
    mutate(
      noon_day = day(epoch - hours(12)),
      month = month(epoch),
      .after = epoch
    ) %>%
    group_by(id, noon_day, month) %>%
    # For each group, perform the following steps
    group_modify(~ {
      # Store the data of the current group
      current_date_data <- .x

      # Check if the first or last recording of the night has a sensor problem (score == -5)
      first_problem <- current_date_data$score[1] == -5
      last_problem <- tail(current_date_data$score, n = 1) == -5

      # If there's no sensor problem at the beginning or end of the night, return the data
      if (!(first_problem | last_problem)) {
        return(current_date_data)
      } else {
        # Otherwise, return an empty tibble, effectively removing this group from the final output
        return(tibble())
      }
    }) %>%
    # For the remaining groups, perform the following steps
    group_modify(~ {
      # Store the data of the current group
      current_group_data <- .x
      # Calculate the recording length as the number of rows in the group
      recording_length <- nrow(current_group_data)

      # Check if the recording_length is within the acceptable range (between 7 and 14 hours)
      if (recording_length >= (7 * 60 * 60) / epoch_length &&
        recording_length <= (14 * 60 * 60) / epoch_length) {
        # If the recording_length is within the acceptable range, return the current group data
        return(current_group_data)
      } else {
        # Otherwise, return an empty tibble, effectively removing this group from the final output
        return(tibble())
      }
    }) %>%
    ungroup()

  # Get list of child ids from ZM-scores data
  distint_zm <-
    zm |>
    distinct(id, noon_day, month)

  # Filter accelerometer data by child ids
  acc_filtered <-
    acc_all |>
    semi_join(distint_zm)

  # join acc and zm data keeping all acc data
  result <- left_join(acc_filtered, zm, by = c("id", "epoch", "noon_day", "month"))
  return(result)
}

# Create a temp directory
temp_dir <- "temp_parquets"
dir.create(temp_dir, showWarnings = FALSE)

# Process each batch and save to a temp parquet file
temp_files <- file_batches %>%
  imap(function(data, i) {
    processed_data <- process_batch(data)
    temp_file_path <- file.path(temp_dir, paste0("temp_", i, ".parquet"))
    write_parquet(processed_data, temp_file_path)
    return(temp_file_path)
  })

# combine participant info
info <- readxl::read_excel("data/participant_info/screens_baseline_info.xlsx") %>% 
  bind_rows(readxl::read_excel("data/participant_info/screens_followup_info.xlsx")) %>% 
  janitor::clean_names() %>% 
  select(id, age) %>% 
  distinct()

# Combine the temporary parquet files
final_data <- map_dfr(temp_files, read_parquet) %>%
  mutate(
    sensor_code = as.integer(sensor_code),
    score = if_else(is.na(score), 1, score),
    score_simple = if_else(score %in% c(2, 3, 5, -5), 2, score), # 0 = in-bed awake, 1 = out-bed awake, 2 = in-bed asleep
    score_simple_median_5 = slide_dbl(score_simple, median, .before = 15, .after = 15), # these are 5 minute windows
    score_simple_median_10 = slide_dbl(score_simple, median, .before = 30, .after = 30),
    in_bed = if_else(score %in% c(0L, 2L, 3L, 5L, -5L), 1L, 0L),
    sleep = if_else(score %in% c(2L, 3L, 5L), 1L, 0L),
    sleep_median5 = slide_dbl(sleep, median, .before = 15, .after = 15),
    sleep_median10 = slide_dbl(sleep, median, .before = 30, .after = 30)
  ) %>% 
  left_join(info, by = "id")

write_parquet(final_data, "data/processed/all_clean_data.parquet")

final_data %>% 
  filter(score != 1) %>% 
  write_parquet("data/processed/in_bed_clean_data.parquet")

# Remove the temporary files and directory
unlink(temp_files)
unlink(temp_dir, recursive = TRUE)
