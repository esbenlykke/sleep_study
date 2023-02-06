#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)

in_bed_fits_files <- list.files("data/models/fitted_models", full.names = TRUE) |>
  str_subset("mars", negate = TRUE) |>
  str_subset("in_bed")

sleep_fits_files <- list.files("data/models/fitted_models", full.names = TRUE) |>
  str_subset("mars", negate = TRUE) |>
  str_subset("sleep")

in_bed_fits <- map(in_bed_fits_files, read_rds) |>
  set_names(c("logistic_regression", "neural_net", "decision_tree", "xgboost"))

sleep_fits <- map(sleep_fits_files, read_rds) |>
  set_names(c("logistic_regression", "neural_net", "decision_tree", "xgboost"))

test <- read_parquet("data/processed/testing_data.parquet") 



# Get ROC -----------------------------------------------------------------


get_roc <-
  function(fit, truth, estimate) {
    fit |>
      augment(test) |>
      roc_curve(truth = {{ truth }}, estimate = {{ estimate }}, event_level = "second")
  }

map_dfr(in_bed_fits, ~ get_roc(.x, in_bed, .pred_1), .id = "model") |> 
    write_parquet("data/processed/roc_data_in_bed.parquet")

map_dfr(sleep_fits, ~ get_roc(.x, in_bed, .pred_1), .id = "model") |> 
    write_parquet("data/processed/roc_data_sleep.parquet")

