#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)
library(lubridate)




zm_stats <-
  read_csv("data/processed/all_zm_stats.csv") |>
  janitor::clean_names() |>
  mutate(
    day = day(start_date),
    month = month(start_date)
  ) |>
  select(id, day, month, spt_hrs, tst_hrs, se_percent, lps_min, waso_min) |>
  rename_with(.cols = -c(id, day, month), ~ paste0("zm_", .))



create_crude_stats <- function(tbl) {
  tbl %>%
    mutate(across(c(pred_in_bed, pred_sleep), as.integer) - 1) |>
    mutate(
      month = month(datetime - hours(12)),
      in_bed_72 = slide_sum(pred_in_bed, before = 36, after = 36),
      sleep_72 = slide_sum(pred_sleep, before = 36, after = 36),
      in_bed_sleep = if_else(in_bed_72 > 60 & sleep_72 > 60, 1, 0),
      in_bed_no_sleep = if_else(in_bed_72 > 60 & sleep_72 < 60, 1, 0)
    ) |>
    group_by(id, noon_day, month) |>
    summarise(
      spt_hrs = ((max(row_number()[in_bed_72 == 60] + 30) - min(row_number()[in_bed_72 == 60] - 30)) * 10) / 60 / 60,
      tst_hrs = (sum(in_bed_sleep) * 10) / 60 / 60,
      se_percent = 100 * (tst_hrs / spt_hrs),
      lps_min = (((min(row_number()[sleep_72 == 60])) - min(row_number()[in_bed_72 == 60] - 30)) * 10) / 60,
      waso_min = ((sum(in_bed_no_sleep) - lps_min) * 10) / 60,
      .groups = "drop"
    ) |>
    inner_join(zm_stats, by = c("id", "noon_day" = "day", "month"))
}

preds_crude %>%
  group_by(model) %>%
  group_modify(~ create_crude_stats(.x), .keep = TRUE) %>%
  write_parquet("data/processed/crude_stats.parquet")
