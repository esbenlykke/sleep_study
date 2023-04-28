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


fits_10_paths <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows",
  full.names = TRUE, recursive = TRUE
) %>% 
  str_subset("10_sec")

fits_30_paths <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows",
             full.names = TRUE, recursive = TRUE
  ) %>% 
  str_subset("30_sec")


fits_10 <- map(fits_10_paths, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

fits_30 <- map(fits_30_paths, read_rds) 

test <- read_parquet("data/data_for_modelling/chained_classifiers/testing_30_sec_data.parquet") %>% 
  mutate(across(contains("sleep"), as_factor))


map_dfr(fits_10, ~ get_roc(.x, in_bed, .pred_1), .id = "model") |>
  write_parquet("data/processed/crude_roc_data_in_bed.parquet")

map_dfr(fits_30, ~ get_roc(.x, in_bed, .pred_1), .id = "model") |>
  write_parquet("data/processed/")


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
    levels = c("in_bed_asleep", "in_bed_awake", "out_bed_awake")
  ))


map_dfr(fits,
  ~ get_roc(.x, multiclass, c(.pred_in_bed_asleep, .pred_in_bed_awake, .pred_out_bed_awake)),
  .id = "model"
) |>
  write_parquet("data/processed/multiclass_OvsALL_roc_data.parquet")
