#!/usr/bin/env Rscript

# Loading required libraries
library(tidyverse)
library(tidymodels)
library(arrow)
library(gt)
library(showtext)

# Define a set of metrics to be used for evaluation
my_metrics <- metric_set(f_meas, accuracy, sensitivity, precision, specificity)

# Function to compute the specified performance metrics using the provided model and test data
get_metrics <- function(fit, test_data, truth, estimate, estimator = "binary") {
  fit %>%
    augment(test_data) %>%
    my_metrics(truth = {{ truth }}, estimate = {{ estimate }}, event_level = "second", estimator = estimator)
}

# Define file paths for the model
path <- "/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep"

# Get the file names for raw, median5 and median10 models
fits_30_filenames_raw <- list.files(path, full.names = TRUE) %>%
  str_subset("raw") %>%
  str_subset("30_sec")
fits_30_filenames_median5 <- list.files(path, full.names = TRUE) %>%
  str_subset("5_min") %>%
  str_subset("30_sec")
fits_30_filenames_median10 <- list.files(path, full.names = TRUE) %>%
  str_subset("10_min") %>%
  str_subset("30_sec")

# Read the models from the file names and store them in separate lists
fits_30_raw <- map(fits_30_filenames_raw, read_rds)
fits_30_median5 <- map(fits_30_filenames_median5, read_rds)
fits_30_median10 <- map(fits_30_filenames_median10, read_rds)

# Load the test data
test_30 <- read_parquet("data/data_for_modelling/chained_classifiers/30_sec_only_in_bed_testing_data.parquet") %>%
  mutate(across(contains("sleep"), as_factor))

# Define model names
model_names <- c("Decision Tree", "Logistic Regression", "Neural Network", "XGBoost")

# Calculate performance metrics for both models using the test datasets
metrics_raw_30 <- fits_30_raw %>%
  set_names(model_names) %>%
  map_dfr(~ get_metrics(.x, test_30, sleep, .pred_class), .id = "model")
metrics_median5_30 <- fits_30_median5 %>%
  set_names(model_names) %>%
  map_dfr(~ get_metrics(.x, test_30, sleep_median5, .pred_class), .id = "model")
metrics_median10_30 <- fits_30_median10 %>%
  set_names(model_names) %>%
  map_dfr(~ get_metrics(.x, test_30, sleep_median10, .pred_class), .id = "model")

# Combine all the metrics
all_metrics_30 <- bind_rows(raw = metrics_raw_30, median_5 = metrics_median5_30, median_10 = metrics_median10_30, .id = "group")

# Save metrics as CSV
all_metrics_30 %>% write_csv("data/processed/all_metrics_sleep_30.csv")
