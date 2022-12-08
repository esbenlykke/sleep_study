#!/usr/bin/env RscriptsuppressMessages(library(tidyverse))

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

test <- read_parquet("data/processed/screens_bsl_test_data.parquet")

# Metrics and plots -------------------------------------------------------


my_metrics <-
  metric_set(f_meas, accuracy, sensitivity, specificity)

get_in_bed_metrics <-
  function(in_bed_fit) {
    in_bed_fit |>
      augment(test) |>
      my_metrics(truth = in_bed, estimate = .pred_class)
  }

get_sleep_metrics <-
  function(in_bed_fit) {
    in_bed_fit |>
      augment(test) |>
      my_metrics(truth = sleep, estimate = .pred_class, event_level = "second")
  }

in_bed_metrics <-
  map_dfr(in_bed_fits, get_in_bed_metrics, .id = "model")

sleep_metrics <-
  map_dfr(sleep_fits, get_sleep_metrics, .id = "model")

in_bed_metrics |>
  select(-.estimator) |>
  pivot_wider(names_from = model, values_from = .estimate) |>
  mutate(event = "In-bed Prediction") |>
  bind_rows(
    sleep_metrics |>
      select(-.estimator) |>
      pivot_wider(names_from = model, values_from = .estimate) |>
      mutate(event = "Sleep Prediction")
  ) |> 
  write_csv("data/processed/performance_metrics.csv")
