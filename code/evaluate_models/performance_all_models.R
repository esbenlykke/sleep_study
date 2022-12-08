#!/usr/bin/env RscriptsuppressMessages(library(tidyverse))

library(tidyverse)
library(tidymodels)
library(arrow)
library(gt)
library(showtext)

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

# Create table ------------------------------------------------------------


font_add_google("Mukta", family = "mukta")
font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()

tab_performance <- 
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
  mutate(
    .metric = factor(.metric, 
                     levels = c("f_meas", "accuracy", "sensitivity", "specificity"),
                     labels = c("F1 Score", "Accuracy", "Sensitivity", "Specificity"))
  ) |> 
  group_by(event) |>
  gt(rowname_col = ".metric") |>
  fmt_percent(
    columns = c(logistic_regression:xgboost),
    decimals = 2
  ) |>
  cols_label(
    logistic_regression = "Logistic Regression",
    neural_net = "Neural Network",
    decision_tree = "Decision Tree",
    xgboost = "XGboost"
  ) |>
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = c(logistic_regression:xgboost)
    )
  ) |>
  tab_header(
    title = md("Performance Metrics"),
    subtitle = "Grouped by event prediction"
  ) |>
  tab_options(table.font.names = "mukta") |> 
  cols_width(everything() ~ px(100)) |> 
  cols_align(align = "center")

gtsave(tab_performance, "visuals/table_permance_metrics.html")