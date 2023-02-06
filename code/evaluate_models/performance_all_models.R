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

test <- read_parquet("data/processed/testing_data.parquet") 

# Metrics and plots -------------------------------------------------------


my_metrics <-
  metric_set(f_meas, accuracy, sensitivity, precision, specificity)

get_metrics <-
  function(fit, truth, estimate) {
    fit |>
      augment(test) |>
      my_metrics(truth = {{ truth }}, estimate = {{ estimate }}, event_level = "second")
  }

in_bed_metrics <-
  map_dfr(in_bed_fits, ~ get_metrics(.x, in_bed, .pred_class), .id = "model")

sleep_metrics <-
  map_dfr(sleep_fits, ~ get_metrics(.x, sleep, .pred_class), .id = "model")

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



# Calculate performance only on in_bed ------------------------------------


get_scores <-
  function(in_bed_fit, sleep_fit, truth, estimate) {
    in_bed_fit |>
      augment(test) |>
      rename_with(~ paste0(.x, "_in_bed"), starts_with(".")) |>
      bind_cols(
        sleep_fit |>
          predict(test)
      ) |>
      rename_with(~ paste0(.x, "_sleep"), ends_with(".pred_class")) |>
      transmute(
        pred_in_bed_awake = as_factor(if_else(.pred_class_in_bed == 1 & .pred_class_sleep == 0, 1, 0)),
        pred_in_bed_sleep = as_factor(if_else(.pred_class_in_bed == 1 & .pred_class_sleep == 1, 1, 0)),
        zm_in_bed_awake = as_factor(if_else(in_bed == 1 & sleep == 0, 1, 0)),
        zm_in_bed_sleep = as_factor(if_else(in_bed == 1 & sleep == 1, 1, 0))
      ) |>
      my_metrics(truth = {{ truth }}, estimate = {{ estimate }}, event_level = "second")
  }


map2_dfr(in_bed_fits, sleep_fits,
  ~ get_scores(.x, .y, truth = zm_in_bed_awake, estimate = pred_in_bed_awake),
  .id = "model"
) |>
  select(-.estimator) |>
  pivot_wider(names_from = model, values_from = .estimate) |>
  mutate(
    event = "In-Bed Awake Prediction"
  ) |>
  bind_rows(
    map2_dfr(in_bed_fits, sleep_fits,
      ~ get_scores(.x, .y, truth = zm_in_bed_sleep, estimate = pred_in_bed_sleep),
      .id = "model"
    ) |>
      select(-.estimator) |>
      pivot_wider(names_from = model, values_from = .estimate) |>
      mutate(
        event = "In-Bed Sleep Prediction"
      )
  ) |> 
  write_csv("data/processed/combined_preds_performance_metrics.csv")
