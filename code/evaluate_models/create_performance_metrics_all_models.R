#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)

in_bed_filenames <-
  list.files("/media/esbenlykke/My Passport/crude/fitted_models/axed_models",
    full.names = TRUE
  ) |>
  str_subset("in_bed")

sleep_filenames <-
  list.files("/media/esbenlykke/My Passport/crude/fitted_models/axed_models",
    full.names = TRUE
  ) |>
  str_subset("sleep")

in_bed_fits <- map(in_bed_filenames, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

sleep_fits <- map(sleep_filenames, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

my_metrics <-
  metric_set(f_meas, accuracy, sensitivity, precision, specificity)

get_metrics <-
  function(fit, truth, estimate, estimator = "binary") {
    fit |>
      augment(test) |>
      my_metrics(
        truth = {{ truth }}, estimate = {{ estimate }},
        event_level = "second",
        estimator = estimator
      )
  }

# calculate metrics -------------------------------------------------------

### crude
test <- read_parquet("data/data_for_modelling/crude_testing_data.parquet")

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
  write_csv("data/processed/crude_performance_metrics.csv")

### multiclass
test <-
  read_parquet("data/data_for_modelling/multiclass_testing_data.parquet") %>%
  mutate(
    multiclass = factor(multiclass, levels = c("in_bed_asleep", "in_bed_awake", "out_bed_awake"))
  )

multiclass_filenames <-
  list.files("/media/esbenlykke/My Passport/multiclass/fitted_models/axed_models",
    full.names = TRUE
  )

multiclass_fits <-
  map(multiclass_filenames, read_rds) %>%
  setNames(c("decision_tree", "decision_tree_SMOTE", "logistic_regression", "neural_net", "xgboost"))

multiclass_fits %>%
  map_dfr(
    ~ get_metrics(
      .x, multiclass,
      factor(.pred_class, levels = c("in_bed_asleep", "in_bed_awake", "out_bed_awake")),
      estimator = "macro_weighted" # TODO weighted or not
    ),
    .id = "model"
  ) %>%
  select(-.estimator) |>
  pivot_wider(names_from = model, values_from = .estimate) %>% 
  write_csv("data/processed/multiclass_performance_metrics.csv")


### binary relevance
test <-
  read_parquet("data/data_for_modelling/binary_relevance_testing_data.parquet")

br_filenames <-
  list.files("/media/esbenlykke/My Passport/binary_relevance/fitted_models/axed_models",
    full.names = TRUE
  )

br_in_bed_asleep_fits <-
  br_filenames %>%
  str_subset("in_bed_asleep") %>%
  map(read_rds) %>%
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

br_in_bed_awake_fits <-
  br_filenames %>%
  str_subset("in_bed_awake") %>%
  map(read_rds) %>%
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

br_out_bed_awake_fits <-
  br_filenames %>%
  str_subset("out_bed_awake") %>%
  map(read_rds) %>%
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

in_bed_asleep_metrics <-
  map_dfr(br_in_bed_asleep_fits, ~ get_metrics(.x, in_bed_asleep, .pred_class), .id = "model")

in_bed_awake_metrics <-
  map_dfr(br_in_bed_awake_fits, ~ get_metrics(.x, in_bed_awake, .pred_class), .id = "model")

out_bed_awake_metrics <-
  map_dfr(br_out_bed_awake_fits, ~ get_metrics(.x, out_bed_awake, .pred_class), .id = "model")

in_bed_asleep_metrics |>
  select(-.estimator) |>
  pivot_wider(names_from = model, values_from = .estimate) |>
  mutate(event = "In-bed Asleep Prediction") |>
  bind_rows(
    in_bed_awake_metrics |>
      select(-.estimator) |>
      pivot_wider(names_from = model, values_from = .estimate) |>
      mutate(event = "In-Bed Awake Prediction")
  ) %>%
  bind_rows(
    out_bed_awake_metrics |>
      select(-.estimator) |>
      pivot_wider(names_from = model, values_from = .estimate) |>
      mutate(event = "Out-Bed Awake Prediction")
  ) %>%
  write_csv("data/processed/binary_relevance_performance_metrics.csv")
