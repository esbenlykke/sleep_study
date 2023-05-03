library(tidyverse)
library(furrr)
library(slider)
library(arrow)
library(lubridate)


# Read in the raw sleep data
zm_raw <- read_tsv("data/processed/zm_scores.tsv")

# Read in the summary statistics for all participants
zm_stats <- read_csv("data/processed/all_zm_stats.csv")

# Plot the raw and filtered scores over time
zm_raw %>%
  filter(id == 8505 & !score == -5) %>%
  mutate(
    score_filter_5 = slide_dbl(score, median, .after = 10),
    score_filter_10 = slide_dbl(score, median, .after = 20),
    noon_day = day(datetime - hours(12))
  ) %>%
  ggplot() +
  geom_step(aes(datetime, score), alpha = .5) +
  geom_step(aes(datetime, score_filter_5 - .2), color = "red") +
  geom_step(aes(datetime, score_filter_10 - .4), color = "darkgreen") +
  scale_x_datetime(breaks = "1 hours", date_labels = "%H:%M") +
  facet_wrap(~noon_day, scales = "free") +
  theme_light()

# Filter the raw sleep data for ID 8505 and create a new dataset with sleep metrics
zm_sleep <- zm_raw %>%
  filter(id == 8505 & !score == -5) %>%
  mutate(
    noon_day = day(datetime - hours(12)),
    month = month(datetime - hours(12)),
    score_filtered = slide_dbl(score, median, .after = 10), # 5 min window
    sleep_raw = if_else(score != 0, 1, 0),
    sleep_filter_5 = if_else(score_filtered != 0, 1, 0),
    sleep_12_cumsum_raw = slide_dbl(sleep_raw, sum, .after = 24),
    sleep_12_cumsum_filter_5 = slide_dbl(sleep_filter_5, sum, .after = 24)
  )

# Define a function to count zeros in runs of at least three consecutive zeros 
count_each_zero_in_consecutive_zeros <-
  function(x, n = 3) {
    rle_x <- rle(x == 0)
    sum(rle_x$lengths[rle_x$values & rle_x$lengths >= n])
  }

# Calculate sleep metrics using both filtered scores and raw scores
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
  bind_cols(zm_stats %>% filter(id == 8505) %>% select(-id)) %>%
  select(id, noon_day, matches("spt|tst|se_percent|lps|waso")) %>%
  select(id, noon_day, contains(c("waso")))
    