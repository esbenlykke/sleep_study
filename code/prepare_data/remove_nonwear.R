#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))

cat("Removing nonwear using decision tree model...")

data <-
  read_parquet("data/processed/fup_thigh_no_bad_zm.parquet")

nw_mod <-
  read_rds("data/models/nonwear_model.rds")

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
  augment(x = nw_mod) |>
  filter(.pred_class == 0) |> 
  select(-c(contains(".pred"), location, weekday)) |> 
  write_parquet("data/processed/data_for_modelling/fup_thigh.parquet")


# test |>
#   filter(id == 8505) |>
#   select(x:temp, incl, datetime, .pred_class) |>
#   pivot_longer(x:incl) |> 
#   ggplot(aes(1:nrow(x = _), value, color = name)) +
#   geom_step() +
#   geom_step(aes(1:nrow(x = _), as.numeric(.pred_class)), inherit.aes = FALSE) +
# facet_wrap(~name, ncol = 1, scales = "free_y")
