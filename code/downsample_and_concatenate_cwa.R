#!/usr/bin/env Rscript

library(tidyverse)
library(read.cwa)
library(slider)
library(feather)
library(furrr)

args <- commandArgs(trailingOnly = TRUE)

epoch_length <- as.integer(args[1]) # epoch length in seconds

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
      id = str_extract(cwa_file, "\\d+_\\w+"),
      .before = 1
    ) |>
    write_feather(temp_file)
}


cwa_files <-
  list.files("data/raw/my_study_acc_data/cwa/", "cwa", full.names = TRUE)

temp_files <-
  paste0("data/temp/", str_replace(
    list.files("data/raw/my_study_acc_data/cwa/", "cwa"), "cwa", "feather"
  ))

plan("multisession", workers = 5)

future_walk2(cwa_files, temp_files, ~ downsample_and_write_cwa_to_feather(.x, .y),
             .options = furrr_options(seed = 123, lazy = TRUE), .progress = TRUE)

plan(sequential)

list.files("data/temp", "feather", full.names = TRUE) |> 
  map_dfr(read_feather) |> 
  write_feather("data/processed/acc_temp_psg_study.feather")

unlink("data/temp", recursive = TRUE)