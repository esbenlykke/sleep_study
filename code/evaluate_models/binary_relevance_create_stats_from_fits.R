#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)
library(lubridate)

test <-
  read_parquet("data/processed/binary_relevance_testing_data.parquet") 
# TODO what causes these outliers?

# Load models -------------------------------------------------------------

# in_bed_fits <-
in_bed_asleep_fit_filenames <-
  list.files("/media/esbenlykke/My Passport/fitted_models", full.names = TRUE) |>
  str_subset("in_bed_asleep") 
  
in_bed_awake_fit_filenames <-
  list.files("data/binary_relevance_models/fitted_models", full.names = TRUE) |>
  str_subset("in_bed_awake") 

out_bed_awake_fit_filenames <- 
  list.files("data/binary_relevance_models/fitted_models", full.names = TRUE) |>
  str_subset("out_bed_awake") 

in_bed_asleep_fits <-
  in_bed_asleep_fit_filenames |>
  map(read_rds)

in_bed_awake_fits <-
  in_bed_awake_fit_filenames |>
  map(read_rds)

out_bed_awake_fits <-
  out_bed_awake_fit_filenames |>
  map(read_rds)


zm_stats <-
  read_csv("data/processed/all_zm_stats.csv") |>
  janitor::clean_names() |>
  mutate(
    day = day(start_date),
    month = month(start_date)
  ) |>
  select(id, day, month, spt_hrs, tst_hrs, se_percent, lps_min, waso_min) |>
  rename_with(.cols = -c(id, day, month), ~ paste0("zm_", .))

# create stats ------------------------------------------------------------

create_stats <-
  function(in_be_asleep_fit, in_bed_awake_fit, out_bed_awake_fit) {
    in_be_asleep_fit |>
      predict(test, type = "prob") 
      bind_cols(
        in_bed_awake_fit |>
          predict(test, type = "prob")
      ) |>
        bind_cols(out_bed_awake_fit |> 
                    predict(test, type = "prob")) # TODO fix colnames
      mutate(across(in_bed_pred:sleep_pred, as.integer) - 1) |>
      mutate(
        month = month(datetime - hours(12)),
        in_bed_72 = slide_sum(in_bed_pred, before = 36, after = 36),
        sleep_72 = slide_sum(sleep_pred, before = 36, after = 36),
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

dfs <- map2(in_bed_fits, sleep_fits, create_stats)

names(dfs) <- c(str_replace_all(sleep_fit_files, "fit.rds", "stats.parquet")) |>
  str_replace_all("data/models/fitted_models/sleep_", "data/processed/stats_predictions/")

walk2(dfs, names(dfs), ~ write_parquet(.x, .y))
