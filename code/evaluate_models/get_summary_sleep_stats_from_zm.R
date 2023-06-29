#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)


count_each_zero_in_consecutive_zeros <- function(x, n = 3) {
  rle_x <- rle(x == 0)
  sum(rle_x$lengths[rle_x$values & rle_x$lengths >= n])
}

count_awakenings_min_duration <- function(x, min_length = 1) {
  # Run-length encoding (RLE) is a simple form of data compression in which 
  # runs of data (sequences in which the same data value occurs in many consecutive 
  # data elements) are stored as a single data value and count.
  rle_x <- rle(x == 0)
  
  # Create a sequence of positions where each 'awake' period starts
  awakening_starts <- cumsum(rle_x$lengths) - rle_x$lengths + 1
  
  # Create a sequence of positions where each 'awake' period ends
  awakening_ends <- cumsum(rle_x$lengths)
  
  # Record the lengths of each 'awake' period
  awakening_lengths <- rle_x$lengths
  
  # Find positions where the 'awake' periods have lengths greater or equal to 
  # the specified minimum length and are actually 'awake' (values are TRUE in rle_x)
  valid_awakening_start_positions <- which(rle_x$values & awakening_lengths >= min_length)
  
  # If there are no valid awakenings, return 0
  if (length(valid_awakening_start_positions) == 0) {
    return(0)
  }
  
  # Get start and end positions of valid awakenings
  valid_awakening_starts <- awakening_starts[valid_awakening_start_positions]
  valid_awakening_ends <- awakening_ends[valid_awakening_start_positions]
  
  # Initiate the count of valid awakenings
  number_of_valid_awakenings <- sum(sapply(1:length(valid_awakening_starts), function(i) {
    start_pos <- valid_awakening_starts[i]
    end_pos <- valid_awakening_ends[i]
    
    # Check if awakening is surrounded by sleep epochs
    # If there are no sleep epochs, then every awakening is considered valid
    if (all(x == 0)) {
      is_valid_awakening <- TRUE
    } else {
      # Awake periods are considered valid if they are both preceded and followed by a sleep period
      is_valid_awakening <- (if (start_pos > 1) {x[start_pos - 1] == 1} else {FALSE}) &&
        (if (end_pos < length(x)) {x[end_pos + 1] == 1} else {FALSE})
    }
    
    # Return 1 if is_valid_awakening is TRUE, 0 otherwise
    return(as.integer(is_valid_awakening))
  }))
  
  # Return the total count of valid awakenings
  return(number_of_valid_awakenings)
}






acc_ids <-
  read_parquet("data/data_for_modelling/no_edge_sp_incl_features_30_sec_epochs.parquet") %>%
  pull(id) %>%
  unique()

age <-
  read_parquet("data/data_for_modelling/no_edge_sp_incl_features_30_sec_epochs.parquet") %>%
  select(id, noon_day, month, age) %>%
  distinct()

############## Look at ZM predictions
zm_score <-
  read_tsv("data/processed/zm_scores.tsv") %>%
  mutate(
    noon_day = day(datetime - hours(12)),
    month = month(datetime - hours(12)),
    in_bed = 1,
    sleep_raw = if_else(score %in% c(2, 3, 5, -5), 1, 0),
    sleep_raw_slide_12 = slide_sum(sleep_raw, after = 24),
    sleep_median_5 = slide_dbl(sleep_raw, median, .before = 5, .after = 5),
    sleep_slide_12_median_5 = slide_sum(sleep_median_5, after = 24),
    sleep_median_10 = slide_dbl(sleep_raw, median, .before = 10, .after = 10),
    sleep_slide_12_median_10 = slide_sum(sleep_median_10, after = 24),
    .after = 1
  )

# Extract no edge SP days
no_edge_SP_zm_data <-
  zm_score %>%
  group_by(id, noon_day, month) %>%
  # For each group, perform the following steps
  group_modify(~ {
    # Store the data of the current group
    current_date_data <- .x
    
    # Check if the first or last recording of the night has a sensor problem (score == -5)
    first_problem <- current_date_data$score[1] != -5
    last_problem <- tail(current_date_data$score, n = 1) != -5
    
    # If there's no sensor problem at the beginning or end of the night, return the data
    if (first_problem & last_problem) {
      return(current_date_data)
    } else {
      # Otherwise, return an empty tibble, effectively removing this group from the final output
      return(tibble())
    }
  }) %>%
  ungroup()

# Calculate precise sleep stats from raw zm predictions
calculated_zm_stats <-
  no_edge_SP_zm_data %>%
  filter(id %in% acc_ids) %>%
  group_by(id, noon_day, month) %>%
  summarise(
    # age_mean = mean(age),
    # age_sd = sd(age),
    spt_hrs_raw = n() * 30 / 60 / 60,
    tst_hrs_raw = sum(sleep_raw, na.rm = TRUE) * 30 / 60 / 60,
    se_percent_raw = ifelse(spt_hrs_raw != 0, 100 * (tst_hrs_raw / spt_hrs_raw), NA),
    lps_min_raw = if (any(sleep_raw_slide_12 == 20)) {
      (min(row_number()[sleep_raw_slide_12 == 20], na.rm = TRUE) + 3) * 30 / 60
    } else {
      NA
    },
    waso_3_min_raw = (count_each_zero_in_consecutive_zeros(sleep_raw) * 30 / 60) - lps_min_raw,
    no_awakenings_raw = count_awakenings_min_duration(sleep_raw),
    spt_hrs_median_5 = n() * 30 / 60 / 60,
    tst_hrs_median_5 = sum(sleep_median_5, na.rm = TRUE) * 30 / 60 / 60,
    se_percent_median_5 = ifelse(spt_hrs_raw != 0, 100 * (tst_hrs_median_5 / spt_hrs_raw), NA),
    lps_min_median_5 = if (any(sleep_slide_12_median_5 == 20)) {
      (min(row_number()[sleep_slide_12_median_5 == 20], na.rm = TRUE) + 3) * 30 / 60
    } else {
      NA
    },
    waso_3_min_median_5 = (count_each_zero_in_consecutive_zeros(sleep_median_5) * 30 / 60) - lps_min_median_5,
    no_awakenings_median_5 = count_awakenings_min_duration(sleep_median_5),
    spt_hrs_median_10 = n() * 30 / 60 / 60,
    tst_hrs_median_10 = sum(sleep_median_10, na.rm = TRUE) * 30 / 60 / 60,
    se_percent_median_10 = ifelse(spt_hrs_raw != 0, 100 * (tst_hrs_median_10 / spt_hrs_raw), NA),
    lps_min_median_10 = if (any(sleep_slide_12_median_10 == 20)) {
      (min(row_number()[sleep_slide_12_median_10 == 20], na.rm = TRUE) + 3) * 30 / 60
    } else {
      NA
    },
    waso_3_min_median_10 = (count_each_zero_in_consecutive_zeros(sleep_median_10) * 30 / 60) - lps_min_median_10,
    no_awakenings_median_10 = count_awakenings_min_duration(sleep_median_10),
    .groups = "drop"
  ) %>%
  rename_with(.cols = -c(id, noon_day, month), ~ paste0("zm_", .))

calculated_zm_stats %>%
  # left_join(age, by = c("id", "noon_day", "month")) %>%
  select(contains("zm")) %>%
  reframe(across(everything(), list(
    mean = ~ mean(.x, na.rm = TRUE),
    sd = ~ sd(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(type = str_replace(type, "zm_", "")) %>%
  separate(type, into = c("row", "column"), "_hrs_|_percent_|_3_min_|_min_|ge_|_awakenings_") %>% 
  pivot_wider(names_from = column, values_from = value) %>%
  rename(
    raw_stat = raw_mean, raw_stat2 = raw_sd, median_5_stat = median_5_mean,
    median_5_stat2 = median_5_sd,
    median_10_stat = median_10_mean, median_10_stat2 = median_10_sd
  ) %>%
  write_csv("data/processed/tbl_overview_sleep_summaries.csv")