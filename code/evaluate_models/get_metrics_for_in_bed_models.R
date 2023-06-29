#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(gt)
library(showtext)

# # Define a set of metrics to be used for evaluation
my_metrics <-
  metric_set(f_meas, accuracy, sensitivity, precision, specificity)

# Function to compute the specified performance metrics using the provided model and test data
get_metrics <-
  function(fit, test_data, truth, estimate, estimator = "macro") {
    fit %>%
      augment(test_data) %>%
      my_metrics(
        truth = {{ truth }}, estimate = {{ estimate }},
        event_level = "second",
        estimator = estimator
      )
  }

in_bed_filenames <-
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed/", full.names = TRUE)

in_bed_fits <-
  map(in_bed_filenames, read_rds) %>%
  set_names(c("decision_tree", "log_reg", "mlp", "xgboost"))


test_data_30 <-
  read_parquet("data/data_for_modelling/chained_classifiers/30_sec_testing_data.parquet") %>%
  mutate(
    in_bed = as_factor(in_bed)
  )

# Calculate performance metrics for both models using the test datasets
metrics <-
  map_df(in_bed_fits, ~ get_metrics(.x, test_data_30, in_bed, .pred_class), .id = "model")


# Save metrics as CSV
# metrics %>%
#   write_csv("data/processed/in_bed_metrics.csv")

metrics <-
  read_csv("data/processed/in_bed_metrics.csv")

# Create a formatted table to display the metrics, grouped by epoch length
# tab_in_bed <-
#   metrics %>%
#   mutate(
#     .metric = factor(.metric,
#       levels = c("f_meas", "accuracy", "sensitivity", "precision", "specificity"),
#       labels = c("F1 Score", "Accuracy", "Sensitivity", "Precision", "Specificity")
#     )
#   ) %>%
#   select(-.estimator) %>%
#   pivot_wider(names_from = .metric, values_from = .estimate) %>%
#   gt() %>%
#   fmt_percent(
#     columns = 2:6,
#     decimals = 2
#   ) %>%
#   tab_style(
#     style = cell_text(size = px(12)),
#     locations = cells_body(
#       columns = 1:6
#     )
#   )
# %>%
#   tab_header(
#     title = md("Performance Metrics"),
#     subtitle = "Grouped by Epoch Length"
#   ) |>
#   tab_options(table.font.names = "ibm") |>
#   cols_width(everything() ~ px(120)) |>
#   cols_align(align = "center") %>%
#   cols_label(.metric = "") %>%


# gtsave(tab_in_bed, "presentation/visuals/table_in_bed_performance_metrics.png", expand = 10)
