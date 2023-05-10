#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(arrow)

cat("Creating sensor-independent features. This won't take long...")

# data <-
#   read_parquet("data/processed/data_for_modelling/bsl_thigh.parquet")
data <-
  read_parquet("data/processed/zm_acc_no_edge_SP_10_sec_epochs.parquet")


# Subject-level in-bed time -----------------------------------------------

# clock_proxy <-
#   data |>
# group_by(id, noon_day) |>
# mutate(
#   threshold_in_bed = sdacc_y < .1 & incl < 45 &
#     (hms::as_hms(datetime) > hms("17:00:00") & hms::as_hms(datetime) < hms("23:59:00")),
#   threshold_out_bed = sdacc_y > .1 & incl > 75 &
#     (hms::as_hms(datetime) > hms("06:00:00") & hms::as_hms(datetime) < hms("11:00:00")),
#   proxy = if_else(row_number() > max(row_number()[threshold_in_bed == TRUE]) &
#     row_number() < min(row_number()[threshold_out_bed == TRUE]),
#   1L, 0L
#   ),
#   .after = 1
# ) |>
# group_by(id, noon_day, proxy) |>
# mutate(
#   clock_proxy_cos = case_when(
#     (id == 604804 & noon_day == 9) |
#       (id == 757104 & noon_day %in% c(1, 2, 29)) |
#       (id == 2596204 & noon_day == 28) ~ 0,
#     proxy == 1 ~ cos(seq(-(pi / 2), pi / 2, length.out = n())),
#     TRUE ~ 0
#   ),
#   clock_proxy_linear = case_when(
#     (id == 604804 & noon_day == 9) |
#       (id == 757104 & noon_day %in% c(1, 2, 29)) |
#       (id == 2596204 & noon_day == 28) ~ 0,
#     proxy == 1 ~ seq(0, 1, length.out = n()),
#     TRUE ~ 0
#   ),
#   .after = 1
# ) |>
# mutate(
#   clock_proxy_cos = if_else(proxy == 1, cos(seq(-(pi / 2), pi / 2, length.out = n())), 0),
#   clock_proxy_linear = if_else(proxy == 1, seq(0, 1, length.out = n()), 0)
# ) |>
# ungroup()


# Create static clock proxies --------------------------------------------

# Process the data for 10-second epochs
data_10 <-
  data |>
  # Create new features for sleep, sleep_median5, sleep_median10
  mutate(
    sleep = if_else(score %in% c(2L, 3L, 5L), 1L, 0L),
    in_bed = if_else(score %in% c(0L, 2L, 3L, 5L, -5L), 1L, 0L),
    in_bed_median5 = slider::slide_dbl(in_bed, median, .after = 30),
    sleep_median5 = slider::slide_dbl(sleep, median, .after = 30),
    sleep_median10 = slider::slide_dbl(sleep, median, .after = 60),
    weekday = wday(datetime, label = F, week_start = 1),
    .after = score
  ) %>%
  # Calculate the standard deviation for columns x, y, and z with a sliding window of 5 minutes
  mutate(
    across(x:z, list(sd_long = ~ slider::slide_dbl(.x, sd, .after = 30)))
  ) %>%
  # Group the data by id and noon_day
  group_by(id, noon_day, month) |>
  # Create a new feature for clock_group based on the datetime column
  mutate(
    clock_group = if_else((hms::as_hms(datetime) > lubridate::hms("19:00:00") |
      hms::as_hms(datetime) < lubridate::hms("10:00:00")), 1, 0),
    .after = 1
  ) |>
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
    .after = sleep_median10
  ) |>
  ungroup()

# Write the processed data to a new parquet file
write_parquet(data_10, "data/data_for_modelling/no_edge_sp_incl_sensor_independent_features_10_sec_epochs.parquet")

# Process the data for 30-second epochs
data_30 <-
  data_10 %>%
  # Round the datetime column to the nearest 30 seconds
  mutate(
    datetime = floor_date(datetime, unit = "30 seconds")
  ) %>%
  # Convert columns to factors
  mutate(
    across(c(score:sleep_median10, sensor_code, weekday), as_factor)
  ) %>%
  # Group the data by id, noon_day, month, and datetime
  group_by(id, noon_day, month, datetime) %>%
  # Calculate the mean of numeric columns within each group
  reframe(
    across(where(is.numeric), mean),
    across(where(is.factor), ~ slider::slide_dbl(as.numeric(.x) - 1, median))
  ) %>%
  mutate(
      in_bed = if_else(in_bed > 0, 1, 0),
      in_bed_median5 = if_else(in_bed_median5 > 0, 1, 0)
    ) %>% 
  distinct()

# Write the processed data to a new parquet file
write_parquet(data_30, "data/data_for_modelling/no_edge_sp_incl_sensor_independent_features_30_sec_epochs.parquet")

beepr::beep(4)
# data_10 %>%
#   ggplot(aes(datetime, in_bed_median5)) +
#   geom_line() +
#   scale_x_datetime(breaks = "2 hours", date_labels = "%H:%M") +
#   facet_wrap(~ noon_day, scales = "free")
#
# data_10 %>% count(in_bed_median5)
