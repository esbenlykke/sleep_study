#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)


get_roc <-
  function(fit, truth, estimate) {
    fit |>
      augment(test) |>
      roc_curve(truth = {{ truth }}, estimate = {{ estimate }}, event_level = "second")
  }


# CRUDE -------------------------------------------------------------------


in_bed_fits_files <- list.files("/media/esbenlykke/My Passport/crude/fitted_models/axed_models",
  full.names = TRUE
) %>%
  str_subset("in_bed")


sleep_fits_files <- list.files("/media/esbenlykke/My Passport/crude/fitted_models/axed_models",
  full.names = TRUE
) %>%
  str_subset("sleep")

in_bed_fits <- map(in_bed_fits_files, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

sleep_fits <- map(sleep_fits_files, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

test <- read_parquet("data/data_for_modelling/crude_testing_data.parquet")


map_dfr(in_bed_fits, ~ get_roc(.x, in_bed, .pred_1), .id = "model") |>
  write_parquet("data/processed/crude_roc_data_in_bed.parquet")

map_dfr(sleep_fits, ~ get_roc(.x, in_bed, .pred_1), .id = "model") |>
  write_parquet("data/processed/crude_roc_data_sleep.parquet")


# Binary relevance --------------------------------------------------------


in_bed_asleep_files <- list.files("/media/esbenlykke/My Passport/binary_relevance/fitted_models/axed_models",
  full.names = TRUE
) %>%
  str_subset("asleep")

in_bed_awake_files <- list.files("/media/esbenlykke/My Passport/binary_relevance/fitted_models/axed_models",
  full.names = TRUE
) %>%
  str_subset("in_bed_awake")

out_bed_awake_files <- list.files("/media/esbenlykke/My Passport/binary_relevance/fitted_models/axed_models",
  full.names = TRUE
) %>%
  str_subset("out_bed_awake")

in_bed_asleep_fits <- map(in_bed_asleep_files, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

in_bed_awake_fits <- map(in_bed_awake_files, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

out_bed_awake_fits <- map(out_bed_awake_files, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

test <- read_parquet("data/data_for_modelling/binary_relevance_testing_data.parquet")


map_dfr(in_bed_asleep_fits, ~ get_roc(.x, in_bed_asleep, .pred_1), .id = "model") |>
  write_parquet("data/processed/binary_relevance_roc_data_in_bed_asleep.parquet")

map_dfr(in_bed_awake_fits, ~ get_roc(.x, in_bed_awake, .pred_1), .id = "model") |>
  write_parquet("data/processed/binary_relevance_roc_data_in_bed_awake.parquet")

map_dfr(out_bed_awake_fits, ~ get_roc(.x, out_bed_awake, .pred_1), .id = "model") |>
  write_parquet("data/processed/binary_relevance_roc_data_out_bed_awake.parquet")



# Multiclass --------------------------------------------------------------


filesnames <- list.files("/media/esbenlykke/My Passport/multiclass/fitted_models/axed_models",
  full.names = TRUE
)

fits <- map(filesnames, read_rds) |>
  set_names(c("decision_tree", "decision_tree_SMOTE", "logistic_regression", "neural_net", "xgboost"))

test <- read_parquet("data/data_for_modelling/multiclass_testing_data.parquet") %>%
  mutate(multiclass = factor(multiclass, 
                             levels = c("in_bed_asleep", "in_bed_awake", "out_bed_awake")))


map_dfr(fits,
  ~ get_roc(.x, multiclass, c(.pred_in_bed_asleep, .pred_in_bed_awake, .pred_out_bed_awake)),
  .id = "model"
) |>
  write_parquet("data/processed/multiclass_OvsALL_roc_data.parquet")
