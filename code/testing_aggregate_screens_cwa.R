#!/usr/bin/env Rscript

library(tidyverse)
library(read.cwa)
library(slider)
library(feather)
library(furrr)

args <- R.utils::commandArgs(trailingOnly = TRUE)
#
epoch_length <- as.integer(args[1]) # epoch length in seconds
dest <- args[2] # destination filename
cwa_path <- args[3] # path to cwa files
# cores <- args[4] # number of cores when in parallel
#
dir.create("data/temp", recursive = TRUE)

downsample_and_write_cwa_to_feather <- function(cwa_file, temp_file) {
  read_cwa(cwa_file,
           xyz_only = FALSE,
           verbose = TRUE
  ) |>
    pluck("data") |>
    select(time, x = X, y = Y, z = Z, temp = temperature) |>
    mutate(
      across(x:temp, ~ slide_dbl(.x,
                                 mean,
                                 .after = 50 * epoch_length,
                                 .step = 50 * epoch_length
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


cwa_files <-
  list.files("/media/esbenlykke/My\ Passport/screens_cwa_children/baseline/temp", "cwa", full.names = TRUE)


temp_files <-
  paste0("data/temp/", str_replace(
    list.files("/media/esbenlykke/My\ Passport/screens_cwa_children/baseline/temp", "cwa"), "cwa", "feather"
  ))

epoch_length <- 5

plan("multisession", workers = 10)

future_walk2(
  cwa_files, temp_files,
  ~ downsample_and_write_cwa_to_feather(.x, .y),
  .options = furrr_options(seed = 123, lazy = TRUE), .progress = TRUE
)

plan("sequential")

list.files("data/temp", "feather", full.names = TRUE) |>
  map_dfr(read_feather) |>
  write_feather(dest)

unlink("data/temp", recursive = TRUE)
