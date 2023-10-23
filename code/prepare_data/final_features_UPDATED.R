#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(furrr))
suppressMessages(library(arrow))
suppressMessages(library(R.utils))
suppressMessages(library(glue))
suppressMessages(library(slider)) 


conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

path <- "/media/esbenlykke/My Passport/all_cwa_children"
temp_path <- file.path(path, "temp")
dest <- file.path(path, "output")

cat("Calculating remaining features\n")

basenames <- list.files(temp_path, ".parquet", full.names = FALSE)
temp_files <- file.path(temp_path, basenames)
output_files <- file.path(dest, basenames)



process_temp_parquet <- function(temp_file, output_file) {
  cat(glue("Processing {str_remove(temp_file, temp_path)}...\n\n"))
  read_parquet(temp_file) %>%
    rowwise() %>%
    mutate(
      sd_max = max(c(x_sd, y_sd, z_sd))
    ) %>%
    group_by(id, noon_day, month) %>%
    mutate(
      across(c(x, y, z), list(sd_long = ~ slider::slide_dbl(.x, sd, .after = 30))),
      across(x_sd_long:z_sd_long, ~ replace_na(.x, mean(.x, na.rm = TRUE))),
      across(c(incl, theta, temp_mean, x_sd, y_sd, z_sd), list(
        lag_1min = ~ lag(.x, 2, default = mean(.x)), # Value from 1 minute ago
        lag_5min = ~ lag(.x, 10, default = mean(.x)), # Value from 5 minutes ago
        lag_30min = ~ lag(.x, 60, default = mean(.x)), # Value from 30 minutes ago
        lead_1min = ~ lead(.x, 2, default = mean(.x)), # Value from 1 minute in the future
        lead_5min = ~ lead(.x, 10, default = mean(.x)), # Value from 5 minutes in the future
        lead_30min = ~ lead(.x, 60, default = mean(.x)) # Value from 30 minutes in the future
      )),
      seconds_since_peak = hour(epoch) * 3600 + minute(epoch) * 60 + second(epoch) - (21 * 3600), # sets c-process peak at 19:00
      clock_proxy_cos = cos(2 * pi * seconds_since_peak / (24 * 3600)),
      weekday = wday(epoch - hours(12), label = FALSE, week_start = 1),
      is_weekend = if_else(weekday %in% c(5, 6), 1, 0), # fri and sat night
      .after = sd_max
    ) %>%
    ungroup() %>% 
    write_parquet(output_file)
}

dir.create(dest, showWarnings = FALSE)

walk2(temp_files, output_files, ~ process_temp_parquet(.x, .y), .progress = TRUE) 