#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(arrow))

args <- R.utils::commandArgs(trailingOnly = TRUE)

dest <- args[1] # destination filename

list.files("~/projects/sleep_study/data/temp/", full.names = TRUE) |>
  map_dfr(read_feather) |>
  write_parquet(dest)

glue::glue("{length(list.files('~/projects/sleep_study/data/temp/'))} feathers have been merged into '{dest}'")

unlink("~/projects/sleep_study/data/temp/", recursive = TRUE)

beepr::beep(4)

temp <- list.files("~/projects/sleep_study/data/temp_bsl/", full.names = TRUE) |>
  map_dfr(read_feather) |> 
  bind_rows(list.files("~/projects/sleep_study/data/temp_fup/", full.names = TRUE) |>
              map_dfr(read_feather)) |> 
  mutate(across(is.character, as.numeric))

info <- 
  readxl::read_excel("data/participant_info/screens_baseline_info.xlsx") |> 
  bind_rows(readxl::read_excel("data/participant_info/screens_followup_info.xlsx")) |> 
  janitor::clean_names()

info |> 
  pivot_longer(-c(id, age), names_to = "placement", values_to = "sensor_code") |> 
  right_join(temp, by = c("id", "sensor_code"), multiple = "all") |> 
  filter(placement == "thigh") |>
  write_parquet("data/processed/all_thigh_data.parquet")
