#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
library(read.cwa)
library(slider)
library(feather)
suppressMessages(library(furrr))
library(glue)

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

glue("Number of temp cwa files is {length(cwa_files)}")

stopifnot(
  "Number of temp cwa split files not equal to target feather files." =
    length(cwa_files) == length(temp_files)
)

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

  tbl |>
    mutate(
      across(x:temp, ~ slide_dbl(.x,
        mean,
        .after = sf * epoch_length,
        .step = sf * epoch_length
      ))
    ) |>
    drop_na() |>
    mutate(
      id = str_extract(cwa_file, "\\d{10}"),
      id = str_remove(id, "^0+"),
      sensor_code = str_extract(cwa_file, "\\d{5}"),
      .before = 1
    ) |>
    write_feather(temp_file)
}

plan("multisession", workers = 10)

future_walk2(cwa_files, temp_files, ~ downsample_and_write_cwa_to_feather(.x, .y),
  .options = furrr_options(seed = 123, lazy = TRUE), .progress = TRUE
)

cat("\n")

plan(sequential)

# list.files("~/sleep_study/data/temp/", "feather", full.names = TRUE) |>
#   map_dfr(read_feather) |>
#   write_feather(dest)

# if (cores > 1) {
#   plan("multisession", workers = cores)
#
#   future_walk2(cwa_files, temp_files, ~ downsample_and_write_cwa_to_feather(.x, .y),
#     .options = furrr_options(seed = 123, lazy = TRUE), .progress = TRUE
#   )
#
#   plan(sequential)
#
#   list.files("data/temp", "feather", full.names = TRUE) |>
#     map_dfr(read_feather) |>
#     write_feather(dest)
# } else {
#   walk2(cwa_files, temp_files, ~ downsample_and_write_cwa_to_feather(.x, .y))
#
#   list.files("data/temp", "feather", full.names = TRUE) |>
#     map_dfr(read_feather) |>
#     write_feather(dest)
# }

# unlink("~/sleep_study/data/temp/", recursive = TRUE)
