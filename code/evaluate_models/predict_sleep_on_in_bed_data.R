#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)
library(lubridate)

# Function to predict sleep using a given model and in-bed data
predict_sleep <- function(model_filename, in_bed_data_filename) {
  read_rds(model_filename) %>%
    augment(read_parquet(in_bed_data_filename)) %>%
    select(id, noon_day, month, datetime, contains("pred_class"))
}

# Function to get filenames in a directory matching a specific pattern
get_filenames <- function(path, pattern) {
  list.files(path, full.names = TRUE) %>% str_subset(pattern)
}

# Function to run predictions using multiple models on a given data file
run_predictions <- function(model_filenames, data_filename) {
  map(model_filenames, predict_sleep, in_bed_data_filename = data_filename)
}

# Function to pair model filenames with in-bed data filenames based on their types
pair_filenames <- function(model_filenames, in_bed_data_filenames) {
  model_types <- str_extract(model_filenames, "(decision_tree|logistic_regression|neural_network|xgboost)")
  in_bed_data_types <- str_extract(in_bed_data_filenames, "(decision_tree|logistic_regression|neural_network|xgboost)")
  map(in_bed_data_types, ~ model_filenames[model_types == .x])
}

# Function to get predictions for each in-bed data file using multiple models
get_preds <- function(in_bed_data_filenames, model_filenames) {
  paired_filenames <- pair_filenames(model_filenames, in_bed_data_filenames)
  map2(
    in_bed_data_filenames, paired_filenames,
    ~ map(.y, run_predictions, data_filename = .x)
  ) %>%
    set_names(c("in_bed_by_decision_tree", "in_bed_by_logistic_regression", "in_bed_by_neural_network", "in_bed_by_xgboost")) %>%
    modify_depth(1, set_names, c("median_10", "median_5", "raw"))
}

# Set the paths for data and model files
path_data <- "data/data_for_modelling/chained_classifiers/extracted_in_bed_data"
path_models <- "/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep"

# Get the filenames for in-bed data and model files
in_bed_data_30_sec_filenames <- get_filenames(path_data, "30_sec")
# in_bed_data_10_sec_filenames <- get_filenames(path_data, "10_sec")
model_30_sec_filenames <- get_filenames(path_models, "30_sec")
# model_10_sec_filenames <- get_filenames(path_models, "10_sec")

preds_30_sec <- get_preds(in_bed_data_30_sec_filenames, model_30_sec_filenames)
# preds_10_sec <- get_preds(in_bed_data_10_sec_filenames, model_10_sec_filenames)

# Combine the predictions into a tibble for each model
extract_pred_class_by_model <- function(preds_list) {
  map(preds_list, function(preds_method) {
    # Combine all predictions into a tibble
    preds_combined <- reduce(
      map2(preds_method, names(preds_method), function(preds_type, preds_name) {
        # The predictions are deeply nested, so we have to get to the actual tibble
        preds_tibble <- preds_type[[1]]

        # Select all columns from the predictions tibble
        preds_column <- preds_tibble

        # Rename the column to match the name from the list it came from
        preds_column <- rename(preds_column, !!str_c("sleep_", preds_name, "_pred_class") := .pred_class)

        preds_column
      }),
      inner_join,
      by = c("id", "noon_day", "month", "datetime")
    )

    preds_combined
  }) %>% 
    map(distinct)
}

# Extract .pred_class columns from each tibble in preds_30_sec and combine them into complete data by model
pred_classes_30_sec_by_model <- extract_pred_class_by_model(preds_30_sec)

complete_data_with_in_bed_preds_filenames <-
  list.files("data/data_for_modelling/chained_classifiers", full.names = TRUE) %>%
  str_subset("complete")

walk2(
  pred_classes_30_sec_by_model, complete_data_with_in_bed_preds_filenames,
  ~ {
    preds <- .x
    data <- read_parquet(.y)
    model_name <- str_extract(.y, "(?<=in_bed_median5_)(.*)(?=_30_sec_epoch_fit_predictions_complete)")

    result <- right_join(preds, data,
      by = c("id", "noon_day", "month", "datetime"),
      relationship = "many-to-many"
    ) %>%
      mutate(
        across(sleep_median_10_pred_class:sleep_raw_pred_class, ~ if_else(is.na(.x), as_factor(0), .x))
      ) %>%
      distinct() %>% 
      arrange(id, month, noon_day, datetime)

    output_file <- str_c("data/data_for_modelling/chained_classifiers/sleep_predictions/", model_name, "_sleep_prediction_complete.parquet")

    write_parquet(result, output_file)

    cat("Prediction data for model", model_name, "has been written to", output_file, "\n\n")
  }
)

# test <-
#   read_parquet("data/data_for_modelling/chained_classifiers/sleep_predictions/decision_tree_sleep_prediction_complete.parquet")
# 
# test %>% count(sleep_raw_pred_class)
# 
# test %>%
#   filter(id == 8505) %>% 
#   ggplot(aes(datetime, group = 1)) +
#   geom_line(aes(y = as.numeric(sleep_median5))) +
#   # geom_line(aes(y = as.numeric(sleep_median_10_pred_class) - .1), color = "darkred") +
#   geom_line(aes(y = as.numeric(sleep_median_5_pred_class) - .2), color = "pink") +
#   # geom_line(aes(y = as.numeric(sleep_raw_pred_class) - .3), color = "steelblue") +
#   geom_line(aes(y = as.numeric(in_bed_class) - .5), color = "darkgreen") +
#   geom_line(aes(y = as.numeric(in_bed) - .6), color = "darkblue") +
#   scale_x_datetime(breaks = "2 hours") +
#   facet_wrap(~ noon_day, scales = "free", ncol = 1) +
#   theme_classic()
