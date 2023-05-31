#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)
library(lubridate)

predict_sleep <- function(model_filename, in_bed_data_filename) {
  # Extract the prediction name from the model filename
  pred_name <- str_extract(model_filename, "sleep_(.*?)_(.*?)_30_sec_epochs")
  
  # Load the model and augment the in-bed data
  read_rds(model_filename) %>%
    augment(read_parquet(in_bed_data_filename)) %>%
    select(id, noon_day, month, datetime, contains("pred_class"))
}

get_filenames <- function(path, pattern) {
  # Get the filenames in a directory that match a specific pattern
  list.files(path, full.names = TRUE) %>% str_subset(pattern)
}

run_predictions <- function(model_filenames, data_filename) {
  # Run predictions for each model on the given data filename
  map(model_filenames, predict_sleep, in_bed_data_filename = data_filename)
}

pair_filenames <- function(model_filenames, in_bed_data_filenames) {
  # Extract model types from filenames
  model_types <- str_extract(model_filenames, "(decision_tree|logistic_regression|neural_network|xgboost)")
  in_bed_data_types <- str_extract(in_bed_data_filenames, "(decision_tree|logistic_regression|neural_network|xgboost)")
  
  # Pair filenames based on their types
  map(in_bed_data_types, ~ model_filenames[model_types == .x])
}

original_30_sec_data <- 
  read_parquet("data/data_for_modelling/no_edge_sp_incl_features_30_sec_epochs.parquet")

original_10_sec_data <- 
  read_parquet("data/data_for_modelling/no_edge_sp_incl_features_10_sec_epochs.parquet")

names_2nd_layer <- c("median_10", "median_5", "raw")

# Set the paths for data and model files
path_data <- "data/data_for_modelling/chained_classifiers/extracted_in_bed_data"
path_models <- "/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep"

# Get the filenames for in-bed data and model files
in_bed_data_30_sec_filenames <- get_filenames(path_data, "30_sec")
in_bed_data_10_sec_filenames <- get_filenames(path_data, "10_sec")
model_30_sec_filenames <- get_filenames(path_models, "30_sec")
model_10_sec_filenames <- get_filenames(path_models, "10_sec")

# Pair 30 sec filenames
paired_30_sec_filenames <- pair_filenames(model_30_sec_filenames, in_bed_data_30_sec_filenames)

# Pair 10 sec filenames
paired_10_sec_filenames <- pair_filenames(model_10_sec_filenames, in_bed_data_10_sec_filenames)

# For each pair of data filename and model filenames, apply `run_predictions`.
preds_30_sec <- map2(
  in_bed_data_30_sec_filenames,
  paired_30_sec_filenames,
  ~ map(.y, run_predictions, data_filename = .x)
) %>%
  # Set names for the second layer of the nested list
  set_names(c("in_bed_by_decision_tree", "in_bed_by_logistic_regression", "in_bed_by_neural_network", "in_bed_by_xgboost")) %>%
  modify_depth(1, set_names, names_2nd_layer)

preds_10_sec <- map2(
  in_bed_data_10_sec_filenames,
  paired_10_sec_filenames,
  ~ map(.y, run_predictions, data_filename = .x)
) %>%
  # Set names for the second layer of the nested list
  set_names(c("in_bed_by_decision_tree", "in_bed_by_logistic_regression", "in_bed_by_neural_network", "in_bed_by_xgboost")) %>%
  modify_depth(1, set_names, names_2nd_layer)

join_predictions_to_original_data <- function(original_data, preds_list) {
  # Iterate over each list of predictions
  preds_joined <- map(preds_list, function(preds_method) {
    map(preds_method, function(preds_type) {
      # The predictions are deeply nested, so you have to get to the actual tibble
      preds_tibble <- preds_type[[1]]
      
      # Join it to the original data
      left_join(original_data, preds_tibble, by = c("id", "noon_day", "month", "datetime"))
    })
  })
  
  preds_joined
}

# Join the 30 sec predictions to the original 30 sec data
joined_preds_30_sec <- join_predictions_to_original_data(original_30_sec_data, preds_30_sec)

# Join the 10 sec predictions to the original 10 sec data
joined_preds_10_sec <- join_predictions_to_original_data(original_10_sec_data, preds_10_sec)
