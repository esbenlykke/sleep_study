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


data_10 <- 
  data |> 
  mutate(
    sleep = if_else(score %in% c(2, 3, 5), 1, 0),
    sleep_median5 = slider::slide_dbl(sleep, median, .after = 30),
    sleep_median10 = slider::slide_dbl(sleep, median, .after = 60),
    .after = score
  ) %>% 
  mutate(
    across(x:z, list(sd_long = ~ slider::slide_dbl(.x, sd, .after = 30)))
  ) %>% 
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
    .after = sleep_median10
  ) |>
  ungroup() 

write_parquet(data_10, "data/data_for_modelling/no_edge_sp_incl_sensor_independent_features_10_sec_epochs.parquet")

data_30 <-  
  data_10 %>% 
  mutate(
    datetime = floor_date(datetime, unit = "30 seconds")
  ) %>% 
  mutate(
    across(c(score:sleep_median10, sensor_code, weekday), as_factor)
  ) %>% 
  group_by(id, noon_day, month, datetime) %>% 
  summarise(across(where(is.numeric), mean), .groups = "drop")

write_parquet(data_30, "data/data_for_modelling/no_edge_sp_incl_sensor_independent_features_30_sec_epochs.parquet")
  
