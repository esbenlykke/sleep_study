#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(arrow)
library(moments)
library(slider)

conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::filter)
cat("Creating sensor-independent features. This will take a while...\n")

# data <-
#   read_parquet("data/processed/data_for_modelling/bsl_thigh.parquet")
data <-
  read_parquet("data/processed/zm_acc_no_edge_SP_10_sec_epochs.parquet")

# The mean_crossing_rate function takes the signal as input and calculates 
# the mean crossing rate. It counts the number of times the signal crosses 
# the mean value and divides it by the length of the signal.
mean_crossing_rate <- function(signal) {
  crossings <- sum(abs(diff(sign(signal))) / 2)
  crossing_rate <- crossings / length(signal)
  return(crossing_rate)
}

# Create static clock proxies --------------------------------------------

# Process the data for 10-second epochs
data_10 <-
  data %>% 
  # filter(id == 3404) %>% 
  # Create new features for sleep, sleep_median5, sleep_median10
  mutate(
    score = if_else(is.na(score), 1, score), # 1 = out-bed awake, otherwise see ZM guide doc
    score_simple = if_else(score %in% c(2, 3, 5, -5), 2, score), # 0 = in-bed awake, 1 = out-bed awake, 2 = in-bed asleep
    score_simple_filtered = slide_dbl(score_simple, median, .before = 15, .after = 15),
    in_bed = if_else(score %in% c(0L, 2L, 3L, 5L, -5L), 1L, 0L),
    in_bed_median5 = slide_dbl(in_bed, median, .before = 15, .after = 15),
    sleep = if_else(score %in% c(2L, 3L, 5L), 1L, 0L),
    sleep_median5 = slide_dbl(sleep, median, .before = 15, .after = 15),
    sleep_median10 = slide_dbl(sleep, median, .before = 30, .after = 30),
    weekday = wday(datetime, label = F, week_start = 1),
    vector_magnitude = sqrt(x^2 + y^2 + z^2),
    across(c(x, y, z), list(crossing_rate = ~ slide_dbl(.x, mean_crossing_rate, .before = 15, .after = 15),
                            skewness = ~ slide_dbl(.x, skewness, .before = 15, .after = 15),
                            kurtosis = ~ slide_dbl(.x, kurtosis, .before = 15, .after = 15))),
    .after = score
  ) %>%
  # Group the data by id and noon_day
  group_by(id, noon_day, month) |>
  # Calculate the standard deviation for columns x, y, and z with a sliding window of 5 minutes
  mutate(
    clock_group = if_else((hms::as_hms(datetime) > lubridate::hms("19:00:00") |
                             hms::as_hms(datetime) < lubridate::hms("10:00:00")), 1, 0),
    across(c(x, y, z), list(sd_long = ~ slider::slide_dbl(.x, sd, .after = 30))),
    across(x_sd_long:z_sd_long, ~ replace_na(.x, mean(.x, na.rm = TRUE))),
    across(c(incl, theta, temp_mean, x_sd, y_sd, z_sd), list(
      lag_1min = ~ lag(.x, 1, default = mean(.x)), # Value from 1 minute ago
      lag_5min = ~ lag(.x, 5, default = mean(.x)), # Value from 5 minutes ago
      lag_30min = ~ lag(.x, 30, default = mean(.x)), # Value from 30 minutes ago
      lead_1min = ~ lead(.x, 1, default = mean(.x)), # Value from 1 minute in the future
      lead_5min = ~ lead(.x, 5, default = mean(.x)), # Value from 5 minutes in the future
      lead_30min = ~ lead(.x, 30, default = mean(.x)) # Value from 30 minutes in the future
    )),
    .after = sd_max
  )  %>% 
  # Group the data by id, noon_day, month, and clock_group
  group_by(id, noon_day, month, clock_group) |>
  # Create new features for clock_proxy_cos and clock_proxy_linear
  mutate(
    clock_proxy_cos = if_else(clock_group == 1,
      cos(seq(-(pi / 2), pi / 2, length.out = n())), 0
    ),
    clock_proxy_linear = if_else(clock_group == 1,
      seq(0, 1, length.out = n()), 0
    ),
    .after = sd_max
  ) |>
  ungroup() 

if (data_10 %>% summarise(across(everything(), ~sum(is.na(.x)))) %>% rowwise() %>% sum() != 0){
  stop("Script stopped. Missing values introduced")
} else cat("10 sec data, all is good, moving on... \n")


# Write the processed data to a new parquet file
write_parquet(data_10, "data/data_for_modelling/no_edge_sp_incl_features_10_sec_epochs.parquet")

# Process the data for 30-second epochs
# data_30 <-
#   data_10 %>%
#   mutate(
# datetime = floor_date(datetime, unit = "30 seconds"),
# across(c(score:sleep_median10, sensor_code, weekday), as_factor)
# ) %>%
#   group_by(id, noon_day, month, datetime) %>%
#   summarise(
#     across(where(is.numeric), mean),
#     across(where(is.factor), ~median(as.numeric(.x) - 1)),
#     .groups = "drop"
#   ) %>% 
#   mutate(
#     across(c(in_bed:in_bed_median5, sleep:sleep_median10), ~ if_else(.x > 0, 1, 0))
#   )


# Define the chunk size
chunk_size <- 1e5  # Adjust this to a suitable value based on your data and available memory

# Create a vector of indices for the chunks
chunk_indices <- split(seq_len(nrow(data_10)), ceiling(seq_len(nrow(data_10)) / chunk_size))

# Function to process a chunk and write to a file
process_and_write_chunk <- function(chunk_indices, index) {
  cat(paste("processing chunk number", index, "\n"))
  
  chunk <- data_10[chunk_indices, ]
  
  data_30 <- chunk %>%
    mutate(
      datetime = floor_date(datetime, unit = "30 seconds"),
      across(c(score:sleep_median10, weekday), as_factor)
    ) %>%
    group_by(id, noon_day, month, datetime) %>%
    summarise(
      across(where(is.numeric), mean),
      across(where(is.factor), ~ceiling(median(as.numeric(.x) - 1))),
      .groups = "drop"
    )
  
  # Define file name
  file_name <- paste0("data/data_for_modelling/data_30_chunk_", index, ".parquet")
  
  # Write to a parquet file
  arrow::write_parquet(data_30, file_name)
}

# Process each chunk
walk2(chunk_indices, seq_along(chunk_indices), ~process_and_write_chunk(.x, .y), .progress = TRUE)

cat("30 sec data done...\n")

# List all parquet files
parquet_files <- list.files("data/data_for_modelling/", pattern = "data_30_chunk_.*\\.parquet$", full.names = TRUE)

# Read all files together
data_30 <- map_df(parquet_files, read_parquet)

# Write the processed data to a new parquet file
write_parquet(data_30, "data/data_for_modelling/no_edge_sp_incl_features_30_sec_epochs.parquet")

file.remove(parquet_files)

beepr::beep(4)
# data_10 %>%
#   ggplot(aes(datetime, in_bed_median5)) +
#   geom_line() +
#   scale_x_datetime(breaks = "2 hours", date_labels = "%H:%M") +
#   facet_wrap(~ noon_day, scales = "free")
#
# data_10 %>% count(in_bed_median5)
