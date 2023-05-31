library(tidyverse)
library(furrr)
library(slider)
library(arrow)
library(lubridate)

epoch_length <- 10

# Read in the raw sleep data
zm_data <- read_tsv("data/processed/zm_scores.tsv") %>%
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

# Read in the summary statistics for all participants
zm_stats <- read_csv("data/processed/all_zm_stats.csv")

# Plot the raw and filtered scores over time
zm_data %>%
  filter(id == 8505 ) %>%
  mutate(
    score_filter_5 = slide_dbl(score, median, .after = 10),
    score_filter_10 = slide_dbl(score, median, .after = 20),
    noon_day = day(datetime - hours(12))
  ) %>%
  ggplot() +
  geom_step(aes(datetime, score), alpha = .5) +
  # geom_step(aes(datetime, score_filter_5 - .2), color = "red") +
  # geom_step(aes(datetime, score_filter_10 - .4), color = "darkgreen") +
  scale_x_datetime(breaks = "1 hours", date_labels = "%H:%M") +
  facet_wrap(~noon_day, scales = "free") +
  theme_light()

# Extract only edge SP days
only_edge_SP_zm_data <- 
  zm_data %>%
  group_by(id, noon_day, month) %>%
  # For each group, perform the following steps
  group_modify(~ {
    # Store the data of the current group
    current_date_data <- .x
    
    # Check if the first or last recording of the night has a sensor problem (score == -5)
    first_problem <- current_date_data$score[1] == -5
    last_problem <- tail(current_date_data$score, n = 1) == -5
    
    # If there's no sensor problem at the beginning or end of the night, return the data
    if ((first_problem | last_problem)) {
      return(current_date_data)
    } else {
      # Otherwise, return an empty tibble, effectively removing this group from the final output
      return(tibble())
    }
  }) 

only_edge_SP_zm_data %>% 
  filter(id == 8504) %>%
  mutate(
    score_filter_5 = slide_dbl(score, median, .after = 10),
    score_filter_10 = slide_dbl(score, median, .after = 20),
    noon_day = day(datetime - hours(12))
  ) %>%
  ggplot() +
  geom_step(aes(datetime, score), alpha = .5) +
  # geom_step(aes(datetime, score_filter_5 - .2), color = "red") +
  # geom_step(aes(datetime, score_filter_10 - .4), color = "darkgreen") +
  scale_x_datetime(breaks = "1 hours", date_labels = "%H:%M") +
  facet_wrap(~ noon_day, scales = "free") +
  theme_light()

# Filter the raw sleep data for ID 8505 and create a new dataset with sleep derivatives
zm_sleep <- 
  zm_data %>%
  filter(id == 8504 & !score == -5) %>%
  mutate(
    noon_day = day(datetime - hours(12)),
    month = month(datetime - hours(12)),
    score_filtered = slide_dbl(score, median, .after = 10), # 5 min window
    sleep_raw = if_else(score != 0, 1, 0),
    sleep_filter_5 = if_else(score_filtered != 0, 1, 0),
    sleep_12_cumsum_raw = slide_dbl(sleep_raw, sum, .after = 24),
    sleep_12_cumsum_filter_5 = slide_dbl(sleep_filter_5, sum, .after = 24)
  )

# Define a function to count zeros in runs of at least three consecutive zeros (WASO)
count_each_zero_in_consecutive_zeros <-
  function(x, n = 3) {
    rle_x <- rle(x == 0)
    sum(rle_x$lengths[rle_x$values & rle_x$lengths >= n])
  }

# Calculate sleep metrics using both calculated sleep stats and ZM determined sleep stats
zm_sleep %>%
  group_by(id, noon_day, month) |>
  summarise(
    median_5_spt = n() * 30 / 60 / 60,
    median_5_tst = sum(sleep_filter_5) * 30 / 60 / 60,
    median_5_se = 100 * (median_5_tst / median_5_spt),
    median_5_lps = (min(row_number()[sleep_12_cumsum_filter_5 == 20]) + 4) * 30 / 60,
    median_5_waso = count_each_zero_in_consecutive_zeros(sleep_filter_5, 3) * 30 / 60 - median_5_lps,
    raw_tst = sum(sleep_raw) * 30 / 60 / 60,
    raw_se_percent = 100 * (raw_tst / median_5_spt),
    raw_lps = (min(row_number()[sleep_12_cumsum_raw == 20]) + 4) * 30 / 60,
    raw_waso = count_each_zero_in_consecutive_zeros(sleep_raw, 3) * 30 / 60 - raw_lps
  ) %>%
  bind_cols(zm_stats %>%
    mutate(noon_day = day(as_datetime(start_date)), .before = 1) %>%
    filter(id == 8504) %>%
    select(-id, -noon_day)) %>%
  select(id, noon_day, matches("spt|tst|se|lps|waso")) 

