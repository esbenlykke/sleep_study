#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)

# dir.create("data/models")

bsl <-
  read_parquet("data/processed/bsl_thigh_no_bad_zm.parquet")

nw_mod <-
  read_rds("data/models/nonwear_model.rds")

nw_mod$fit$fit$preproc[[1]] |> cat(sep = " | ")

# idx | weekday | time_day | location | macc_x | macc_y | macc_z | sdacc_x |
# sdacc_y | sdacc_z | sdmax | incl | temp | wear_ml_pred | wear_heuristic | wear_time_cz

bsl |>
  group_by(id) |>
  mutate(
    idx = 0,
    time_day = seq(0, 1, length.out = n()),
    location = if_else(placement == "thigh", 0, 1),
    temp = (temp - 171) / 3.413,
    temp_mean = (temp - 171) / 3.413
  ) |>
  rename(
    macc_x = x_mean, macc_y = y_mean, macc_z = z_mean,
    sdacc_x = x_sd, sdacc_y = y_sd, sdacc_z = z_sd, sdmax = sd_max
  ) |>
  ungroup() |>
  augment(x = nw_mod) |>
  filter(.pred_class == 0) |> 
  write_parquet("data/processed/model_data/bsl_thigh.parquet")


# test |>
#   filter(id == 8505) |>
#   select(x:temp, incl, datetime, .pred_class) |>
#   pivot_longer(x:incl) |> 
#   ggplot(aes(1:nrow(x = _), value, color = name)) +
#   geom_step() +
#   geom_step(aes(1:nrow(x = _), as.numeric(.pred_class)), inherit.aes = FALSE) +
# facet_wrap(~name, ncol = 1, scales = "free_y")
