#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))

cat("Removing nonwear using decision tree model...")

dir.create("data/data_for_modelling")

data <-
  read_parquet("data/processed/all_thigh_no_bad_zm.parquet")

nw_mod <-
  read_rds("/media/esbenlykke/My Passport/sleep_study/data/models/nonwear_model.rds")

data |>
  group_by(id, noon_day) |>
  mutate(
    time_day = seq(0, 1, length.out = n())) |> 
  ungroup() |> 
  mutate(
    location = if_else(placement == "thigh", 0, 1),
    temp = (temp - 171) / 3.413,
    temp_mean = (temp - 171) / 3.413
  ) |>
  rename(
    macc_x = x_mean, macc_y = y_mean, macc_z = z_mean,
    sdacc_x = x_sd, sdacc_y = y_sd, sdacc_z = z_sd, sdmax = sd_max
  ) |> 
  # augment(x = nw_mod) |>
  # filter(.pred_class == 0) |> 
  # select(-c(contains(".pred"), location, weekday)) |> 
  write_parquet("data/data_for_modelling/all_thigh.parquet")
