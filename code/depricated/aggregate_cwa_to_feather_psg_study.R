#!/usr/bin/env Rscript

library(tidyverse)
library(read.cwa)
library(slider)
library(feather)
library(furrr)

args <- R.utils::commandArgs(trailingOnly = TRUE)

epoch_length <- as.integer(args[1]) # epoch length in seconds
dest <- args[2] # destination filename
cwa_path <- args[3] # path to cwa files
cores <- as.integer(args[4]) # number of cores when in parallel

dir.create("data/temp", recursive = TRUE)

downsample_and_write_cwa_to_feather <- function(cwa_file, temp_file) {
  read_cwa(cwa_file,
    xyz_only = FALSE,
    verbose = TRUE
  ) |>
    pluck("data") |>
    select(1:6) |>
    mutate(
      across(X:temperature, ~ slide_dbl(.x,
        mean,
        .after = 50 * epoch_length,
        .step = 50 * epoch_length
      ))
    ) |>
    drop_na() |>
    mutate(
      id = str_extract(cwa_file, "\\d+"),
      placement = str_extract(cwa_file, "\\d+_\\w+"),
      placement = str_remove(placement, "\\d+_"),
      .before = 1
    ) |>
    write_feather(temp_file)
}


cwa_files <-
  list.files(cwa_path, "cwa", full.names = TRUE)

temp_files <-
  paste0("data/temp/", str_replace(
    list.files(cwa_path, "cwa"), "cwa", "feather"
  ))


if (cores > 1) {
  plan("multisession", workers = cores)

  future_walk2(cwa_files, temp_files, ~ downsample_and_write_cwa_to_feather(.x, .y),
    .options = furrr_options(seed = 123, lazy = TRUE), .progress = TRUE
  )

  plan(sequential)

  list.files("data/temp", "feather", full.names = TRUE) |>
    map_dfr(read_feather) |>
    write_feather(dest)
} else {
  walk2(cwa_files, temp_files, ~ downsample_and_write_cwa_to_feather(.x, .y))

  list.files("data/temp", "feather", full.names = TRUE) |>
    map_dfr(read_feather) |>
    write_feather(dest)
}


unlink("data/temp", recursive = TRUE)
