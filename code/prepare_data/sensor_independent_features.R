#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(arrow)

cat("Creating sensor-independent features. This won't take long...")

# data <-
#   read_parquet("data/processed/data_for_modelling/bsl_thigh.parquet")
data <-
  read_parquet("data/data_for_modelling/all_thigh.parquet")


# Subject-level in-bed time -----------------------------------------------

# clock_proxy <-
#   data |>
  # TODO work out a programmatic solution to the threshold problem...
  # problematic BASELINE time stamps
  # problematic FOLLOWUP time stamps
  # filter((id != 604804 | noon_day != 9) &
  #   (id != 757104 | !noon_day %in% c(1, 2, 29)) &
  #   (id != 2596204 | noon_day != 28) &
  #   (id != 447104 | noon_day != 13) &
  #   (id != 447105 | !noon_day %in% c(11, 13)) &
  #   (id != 1054804 | noon_day != 31) &
  #   (id != 1262105 | !noon_day %in% c(2, 3)) &
  #   (id != 1587606 | noon_day != 27)) |>
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


# Create static clock proxies for problematic time stamps -----------------


data |> 
  mutate(
    month = month(datetime),
    sleep_median5 = slider::slide_dbl(sleep, median, .after = 30),
    sleep_median10 = slider::slide_dbl(sleep, median, .after = 60),
    .after = day
  ) %>% 
  mutate(
    across(x:z, list(sd_long = ~ slider::slide_dbl(.x, sd, .after = 30)))
  ) %>% 
  # problematic BASELINE time stamps
  # # problematic FOLLOWUP time stamps
  # filter(
  #   (id == 604804 & noon_day == 9) |
  #     (id == 757104 & noon_day %in% c(1, 2, 29)) |
  #     (id == 2596204 & noon_day == 28) |
  #     (id == 447104 & noon_day == 13) |
  #     (id == 447105 & noon_day %in% c(11, 13)) |
  #     (id == 1054804 & noon_day == 31) |
  #     (id == 1262105 & noon_day %in% c(2, 3)) |
  #     (id == 1587606 & noon_day == 27)
  # ) |>
  group_by(id, noon_day) |>
  mutate(
    clock_group = if_else((hms::as_hms(datetime) > hms("19:00:00") | hms::as_hms(datetime) < hms("10:00:00")), 1, 0),
    .after = 1
  ) |>
  group_by(id, noon_day, clock_group) |>
  mutate(
    clock_proxy_cos = if_else(clock_group == 1,
      cos(seq(-(pi / 2), pi / 2, length.out = n())), 0
    ),
    clock_proxy_linear = if_else(clock_group == 1,
      seq(0, 1, length.out = n()), 0
    ),
    .after = 1
  ) |>
  ungroup() %>% 
  write_parquet("data/data_for_modelling/all_data_incl_sensor_independent_features.parquet")


# d %>%
#   filter(id == 3404) %>%
#   ggplot(aes(datetime)) +
#   geom_step(aes(y = sleep)) +
#   geom_step(aes(y = sleep_median - 1.2), color = "darkorange") +
#   geom_line(aes(y = scale(incl) - 3.2), color = "grey80") +
#   facet_wrap(~ noon_day, scales = "free") +
#   theme_light()
  
# Merge and write ---------------------------------------------------------


# clock_proxy |>
#   bind_rows(static_proxy) |>
#   replace_na(list(
#     threshold_in_bed = FALSE, threshold_out_bed = FALSE, proxy = 0
#   )) |>
#   select(
#     id, datetime, unix_time, day, noon_day, age, placement,
#     clock_proxy_cos, clock_proxy_linear, incl, theta, temp:time_day
#   ) |>
#   write_parquet("data/data_for_modelling/all_data_incl_sensor_independent_features.parquet")

