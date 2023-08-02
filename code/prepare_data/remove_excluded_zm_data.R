#!/usr/bin/env Rscript

library(tidyverse)
library(hms)
library(lubridate)
library(arrow)

# Set the epoch length
epoch_length <- 10

# Read and preprocess the thigh data
data <- read_parquet("data/processed/all_thigh.parquet") %>%
  mutate(
    id = as.numeric(id),
    noon_day = day(datetime - hours(12)),
    month = month(datetime - hours(12)),
    unix_time = as.integer(datetime) %/% epoch_length * epoch_length,
    datetime = as_datetime(unix_time),
    .after = 1
  ) %>% 
  rename(x_mean = macc_x, y_mean = macc_y, z_mean = macc_z,
         x_sd = sdacc_x, y_sd = sdacc_y, z_sd = sdacc_z, sd_max = sdmax) %>% 
  select(-in_bed, -sleep, -location, -sensor_code)

# Calculate distinct accelerometer days
distinct_acc_days <- data %>%
  distinct(id, noon_day, month) %>%
  arrange(id)

# Read and preprocess ZM scores data
zm_data <- read_tsv("data/processed/zm_scores.tsv", progress = FALSE) %>%
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

# Identify all sensor problem days
# sensor_problem_days <- zm_data %>%
#   count(id, noon_day, month, score) %>%
#   filter(score == -5) %>%
#   distinct(id, noon_day, month)

# Find in-bed periods shorter than 7 hrs and longer than 14 hrs
# acceptable_length_recordings <-
#   zm_data %>%
#   group_by(id, noon_day, month) %>%
#   summarise(recording_length = n(), .groups = "drop") %>%
#   filter(!recording_length < (7 * 60 * 60) / epoch_length &
#     !recording_length > (14 * 60 * 60) / epoch_length)

# remove nights with edge SP and remove nights shorter that 7 hours and longer that 14 hours
zm_data_clean <-
  zm_data %>%
  # Keep only the groups that are also present in the distinct_acc_days data frame
  # based on the matching values of id, noon_day, and month columns (the kids)
  semi_join(distinct_acc_days, by = c("id", "noon_day", "month")) %>% 
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

# Get distinct id, noon_day, and month from the cleaned data
valid_zm_days <-
  zm_data_clean %>%
  distinct(id, noon_day, month)

# Join the data with valid_zm_days based on matching id, noon_day, and month columns
# Then, right join with the cleaned zm_data based on matching id, noon_day, month, and datetime columns
# Finally, write the output to a parquet file
data %>%
  semi_join(valid_zm_days, by = c("id", "noon_day", "month")) %>%
  left_join(zm_data_clean, by = c("id", "noon_day", "month", "datetime")) %>% 
  write_parquet("data/processed/zm_acc_no_edge_SP_10_sec_epochs.parquet")

# Plot histogram of sensor problem days
# zm_data %>%
#   semi_join(distinct_acc_days, by = c("id", "noon_day", "month")) %>%
#   count(id, noon_day, month, score) %>%
#   filter(score == -5) %>%
#   mutate(n = n * 10 / 60 / 60) %>%
#   ggplot(aes(n)) +
#   geom_histogram(
#     binwidth = 1 / 2,
#     color = "grey20", fill = "darkorange"
#   ) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_classic()

# zm_acc_data_clean %>%
#   count(id, noon_day, month, score) %>%
#   filter(score == -5) %>%
#   mutate(n = n * 10 / 60)

# Calculate mean and standard deviation of sensor problem lengths
# zm_data %>%
#   semi_join(distinct_acc_days, by = c("id", "noon_day", "month")) %>%
#   count(id, noon_day, month, score) %>%
#   filter(score == -5) %>%
#   mutate(n = n * 10 / 60 / 60) %>%
#   summarise(
#     mean_sp_length = mean(n),
#     sd_sp = sd(n)
#   )
