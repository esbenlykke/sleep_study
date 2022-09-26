#!/usr/bin/env Rscript

pacman::p_load(tidyverse,
               feather,
               furrr,
               lubridate)

# commandArgs this!

args <- commandArgs(trailingOnly = TRUE)

feather_path <- as.character(args[1])
core_num <- as.integer(args[2]) 
write_to <- as.character(args[3])

files <- 
  list.files(feather_path,
             full.names = TRUE)

plan("multisession", workers = core_num)

files |> 
  future_map_dfr(read_feather,
                 .options = furrr_options(seed = 123),
                 .progress = TRUE) |> 
  mutate(
    datetime = as_datetime(unix_aligned, tz = "CET"),
    .before = 2
  ) |> 
  drop_na() |> 
  write_feather(write_to)

# read_feather("/media/esben/My Passport/acc_temp_combined/baseline/1026704.feather") |> drop_na()