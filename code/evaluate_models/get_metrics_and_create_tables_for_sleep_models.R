#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(gt)
library(showtext)

# Define a set of metrics to be used for evaluation
my_metrics <-
  metric_set(f_meas, accuracy, sensitivity, precision, specificity)

# Function to compute the specified performance metrics using the provided model and test data
get_metrics <-
  function(fit, test_data, truth, estimate, estimator = "binary") {
    fit |>
      augment(test_data) |>
      my_metrics(
        truth = {{ truth }}, estimate = {{ estimate }},
        event_level = "second",
        estimator = estimator
      )
  }

# Get the file names of the fitted models for the 10-second epochs, grouped by preprocessing type (raw, median5, and median10)
fits_10_filenames_raw <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("raw") %>% 
  str_subset("30_sec")


fits_10_filenames_median5 <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("5_min") %>% 
  str_subset("30_sec")


fits_10_filenames_median10 <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("10_min") %>% 
  str_subset("30_sec")


# Read the models from the file names and store them in separate lists
fits_10_raw <- map(fits_10_filenames_raw, read_rds)
fits_10_median5 <- map(fits_10_filenames_median5, read_rds)
fits_10_median10 <- map(fits_10_filenames_median10, read_rds)

# Repeat the same process for the 30-second epochs
fits_30_filenames_raw <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("raw") %>% 
  str_subset("30_sec")

fits_30_filenames_median5 <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("5_min") %>% 
  str_subset("30_sec")

fits_30_filenames_median10 <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("10_min") %>% 
  str_subset("30_sec")

# Read the models from the file names and store them in separate lists
fits_30_raw <- map(fits_30_filenames_raw, read_rds)
fits_30_median5 <- map(fits_30_filenames_median5, read_rds)
fits_30_median10 <- map(fits_30_filenames_median10, read_rds)

# Read the test datasets for the 10-second and 30-second epochs
test_10 <-
  read_parquet("data/data_for_modelling/chained_classifiers/only_in_bed_testing_data_10_sec_epochs.parquet") %>% 
  select(-contains(".pred"))

test_30 <-
  read_parquet("data/data_for_modelling/chained_classifiers/only_in_bed_testing_data_30_sec_epochs.parquet") %>% 
  select(-contains(".pred"))

# Calculate performance metrics for both models using the test datasets
model_names <- 
  c("Decision Tree", "Logistic Regression", "Neural Network", "XGBoost")

metrics_raw_10 <- 
  fits_10_raw %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_metrics(.x, test_10, sleep, .pred_class), .id = "model")

metrics_median5_10 <- 
  fits_10_median5 %>%
  set_names(model_names) %>%  
  map_dfr(~ get_metrics(.x, test_10, sleep_median5, .pred_class), .id = "model")

metrics_median10_10 <- 
  fits_10_median10 %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_metrics(.x, test_10, sleep_median10, .pred_class), .id = "model")

all_metrics_10 <- 
  bind_rows(raw = metrics_raw_10, median_5 = metrics_median5_10, median_10 = metrics_median10_10, 
            .id = "group")

# Save metrics as CSV
# all_metrics_10 %>%
#   write_csv("presentation/data/all_metrics_sleep_10.csv")

metrics_raw_30 <- 
  fits_30_raw %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_metrics(.x, test_30, sleep, .pred_class), .id = "model")

metrics_median5_30 <- 
  fits_30_median5 %>%
  set_names(model_names) %>%  
  map_dfr(~ get_metrics(.x, test_30, sleep_median5, .pred_class), .id = "model")

metrics_median10_30 <- 
  fits_30_median10 %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_metrics(.x, test_30, sleep_median10, .pred_class), .id = "model")

all_metrics_30 <- 
  bind_rows(raw = metrics_raw_30, median_5 = metrics_median5_30, median_10 = metrics_median10_30, 
            .id = "group")

# Save metrics as CSV
# all_metrics_30 %>% 
#   write_csv("presentation/data/all_metrics_sleep_30.csv")

# Create a formatted table to display the metrics, grouped by type of filter on zm sleep preds
all_metrics_10 %>% 
  select(-.estimator) %>% 
  mutate(
    .metric = factor(.metric,
                     levels = c("f_meas", "accuracy", "sensitivity", "precision", "specificity"),
                     labels = c("F1 Score", "Accuracy", "Sensitivity", "Precision", "Specificity")
    )
  ) |> 
  pivot_wider(names_from = model, values_from = .estimate) %>% 
  gt(groupname_col = "group") |>
  fmt_percent(
    columns = 3:6,
    decimals = 2
  ) |>
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = 3:6
    )
  ) |>
  tab_header(
    title = md("Performance Metrics"),
    subtitle = "10 second epochs data"
  ) |>
  tab_options(table.font.names = "ibm") |>
  cols_width(everything() ~ px(120)) |>
  cols_align(align = "center") %>%
  cols_label(.metric = "") %>% 
  tab_options(
    # Configure table appearance, such as font, colors, borders, padding, and alignment
  )

all_metrics_30 %>% 
  select(-.estimator) %>% 
  mutate(
    .metric = factor(.metric,
                     levels = c("f_meas", "accuracy", "sensitivity", "precision", "specificity"),
                     labels = c("F1 Score", "Accuracy", "Sensitivity", "Precision", "Specificity")
    )
  ) |> 
  pivot_wider(names_from = model, values_from = .estimate) %>% 
  gt(groupname_col = "group") |>
  fmt_percent(
    columns = 3:6,
    decimals = 2
  ) |>
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = 3:6
    )
  ) |>
  tab_header(
    title = md("Performance Metrics"),
    subtitle = "30 second epochs data"
  ) |>
  tab_options(table.font.names = "ibm") |>
  cols_width(everything() ~ px(120)) |>
  cols_align(align = "center") %>%
  cols_label(.metric = "") %>% 
  tab_options(
    # Configure table appearance, such as font, colors, borders, padding, and alignment
  )

bind_rows(epoch_10 = all_metrics_10, epoch_30 = all_metrics_30, .id = "epoch") %>% 
  select(-.estimator) %>% 
  mutate(
    .metric = factor(.metric,
                     levels = c("f_meas", "accuracy", "sensitivity", "precision", "specificity"),
                     labels = c("F1 Score", "Accuracy", "Sensitivity", "Precision", "Specificity")
    )
  ) |> 
  pivot_wider(names_from = c(model, epoch), values_from = .estimate) %>% 
  gt(groupname_col = "group") |>
  fmt_percent(
    columns = 3:10,
    decimals = 2
  ) |>
  tab_spanner(label = "10 second epochs", columns = 3:6) %>% 
  tab_spanner(label = "30 second epochs", columns = 7:10) %>%
  cols_label("Decision Tree_epoch_10" = "Decision Tree",
             "Decision Tree_epoch_30" = "Decision Tree",
             "Logistic Regression_epoch_10" = "Logistic Regression",
             "Logistic Regression_epoch_30" = "Logistic Regression",
             "Neural Network_epoch_10" = "Neural Network",
             "Neural Network_epoch_30" = "Neural Network",
             "XGBoost_epoch_10" = "XGBoost",
             "XGBoost_epoch_30" = "XGBoost") %>% 
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = 3:10
    )
  ) |>
  tab_header(
    title = md("Performance Metrics")
  ) |>
  tab_options(table.font.names = "ibm") |>
  cols_width(everything() ~ px(120)) |>
  cols_align(align = "center") %>%
  cols_label(.metric = "") %>% 
  tab_options(
    # Configure table appearance, such as font, colors, borders, padding, and alignment
  )