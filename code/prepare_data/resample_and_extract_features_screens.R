#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(furrr))
suppressMessages(library(slider))
suppressMessages(library(arrow))
suppressMessages(library(read.cwa))
suppressMessages(library(glue))
suppressMessages(library(lubridate))
suppressMessages(library(signal))

args <- R.utils::commandArgs(trailingOnly = TRUE)

epoch_length <- as.integer(args[1]) # epoch length in seconds
# dest <- args[2] # destination filename
cwa_path <- args[2] # path to temp cwa files
# cores <- args[4] # number of cores when in parallel

# dir.create("~/sleep_study/data/temp/", recursive = TRUE)

cwa_files <-
  list.files(str_c(cwa_path, "/temp"), full.names = TRUE)

temp_files <-
  paste0("~/sleep_study/data/temp/", str_replace(
    list.files(str_c(cwa_path, "/temp")), "cwa", "feather"
  ))

glue("{length(cwa_files)} split cwa files in this chunk")

stopifnot(
  "Number of temp cwa split files not equal to target feather files." =
    length(cwa_files) == length(temp_files)
)

# define function to extract sampling frequency
get_sf <- function(tbl) {
  tbl |>
    slice_head(n = 1000) |>
    mutate(unix = as.integer(as.numeric(time))) |>
    group_by(unix) |>
    summarise(freq = round(n())) |>
    summarise(freq = round(median(freq), -1)) |>
    pull(freq)
}


downsample_and_write_cwa_to_feather <- function(cwa_file, temp_file) {
  tbl <-
    read_cwa(cwa_file,
      xyz_only = FALSE,
      verbose = FALSE
    ) |>
    pluck("data") |>
    select(time, x = X, y = Y, z = Z, temp = temperature)

  sf <- get_sf(tbl)

  # Create 4th order Butterworth low-pass 5 Hz filter 
  bf <- signal::butter(4, 5 / (sf / 2), type = "low")
  
  tbl |>
    # apply Butterworth filter
    mutate(across(x:z, ~ signal::filtfilt(bf, .x))) |>
    # aggregate in 2 sec overlapping windows
    mutate(
      across(x:temp, list(
        mean = ~ slide_dbl(.x, mean, .after = (sf * 2) * 2, .step = sf * 2),
        sd = ~ slide_dbl(.x, sd, .after = (sf * 2) * 2, .step = sf * 2)
      ))
    ) |>
    drop_na() |>
    # aggregate in 10 sec non-overlapping windows
    mutate(
      across(x:temp, list(
        mean = ~ slide_dbl(.x, mean, .after = 5, .step = 5),
        sd = ~ slide_dbl(.x, sd, .after = 5, .step = 5)
      ))
    ) |>
    drop_na() |> 
    mutate(
      id = str_extract(cwa_file, "\\d{10}"),
      id = str_remove(id, "^0+"),
      sensor_code = str_extract(cwa_file, "\\d{5}"),
      weekday = wday(time),
      incl = 180 / pi * acos(y_mean / sqrt(x_mean^2 + y_mean^2 + z_mean^2)),
      theta = 180 / pi * asin(z_mean / sqrt(x_mean^2 + y_mean^2 + z_mean^2)),
      .before = 1
    ) |>
    rowwise() |>
    mutate(
      sd_max = max(c(x_sd, y_sd, z_sd))
    ) |>
    ungroup() |>
    write_feather(temp_file)
}

cat("Execute the whole thing in parallel. ETA ~ 4-5 hrs")

plan("multisession", workers = 10)

future_walk2(cwa_files, temp_files, ~ downsample_and_write_cwa_to_feather(.x, .y),
  .options = furrr_options(seed = 123), .progress = FALSE
)

plan(sequential)

# TODO work out a sensible progress bar 