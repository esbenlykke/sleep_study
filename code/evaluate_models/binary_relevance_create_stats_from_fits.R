#!/usr/bin/env Rscript

library(tidyverse)
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

dfs <-
  map(list.files("/media/esbenlykke/My Passport/binary_relevance_preds",
    full.names = TRUE,
    pattern = "no_ties"
  ), read_parquet)

# create stats ------------------------------------------------------------


create_stats <- function(tbl) {
  tbl |>
    mutate(across(where(is.factor), as.integer) - 1) |>
    mutate(
      month = month(datetime - hours(12)),
      in_bed_asleep_72 = slide_sum(in_bed_asleep_.pred_class, before = 36, after = 36),
      in_bed_awake_72 = slide_sum(in_bed_awake_.pred_class, before = 36, after = 36),
      out_bed_awake_72 = slide_sum(out_bed_awake_.pred_class, before = 36, after = 36),
      in_bed_72 = slide_sum(in_bed_asleep_72 + in_bed_awake_72 > 1, before = 36, after = 36)
    ) %>%
    group_by(id, noon_day, month) |>
    summarise(
      spt_hrs = (max(row_number()[in_bed_72 == 60]) - min(row_number()[in_bed_72 == 60])) * 6 / 60 / 60,
      tst_hrs = sum(in_bed_asleep_72 > 0) * 6 / 60 / 60,
      se_percent = 100 * (tst_hrs / spt_hrs),
      lps_min = (((min(row_number()[in_bed_asleep_72 == 60])) - min(row_number()[in_bed_awake_72 == 60]))) * 6 / 60,
      waso_min = abs(sum(in_bed_awake_72 >= 60) - (lps_min * 60)) / 60,
      .groups = "drop"
    ) %>%
    inner_join(zm_stats, by = c("id", "noon_day" = "day", "month"))
}


dfs_stats <- map(dfs, create_stats)

# names(dfs) <- 

walk2(dfs, names(dfs_stats), ~ write_parquet(.x, .y))
