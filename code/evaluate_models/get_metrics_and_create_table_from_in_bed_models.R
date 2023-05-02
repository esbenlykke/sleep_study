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

# Load pre-trained models for 10-second and 30-second epochs
in_bed_fit_10 <-
  read_rds("/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed/in_bed_simple_tree_fit_10_sec_AXED.rds")

in_bed_fit_30 <-
  read_rds("/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed/in_bed_simple_tree_fit_30_sec_AXED.rds")

# Load test data for 10-second and 30-second epochs and convert 'in_bed' column to factors
test_10 <-
  read_parquet("data/data_for_modelling/chained_classifiers/testing_10_sec_data.parquet") %>%
  mutate(
    in_bed = as_factor(in_bed)
  )

test_30 <-
  read_parquet("data/data_for_modelling/chained_classifiers/testing_30_sec_data.parquet") %>%
  mutate(
    in_bed = as_factor(in_bed)
  )

# Calculate performance metrics for both models using the test datasets
metrics <- 
  get_metrics(in_bed_fit_10, test_10, in_bed, .pred_class) %>%
  mutate(epoch_length = "10 sec") %>%
  bind_rows(
    get_metrics(in_bed_fit_30, test_30, in_bed, .pred_class) %>%
      mutate(
        epoch_length = "30 sec"
      )
  ) %>% 
  select(-.estimator)

# Save metrics as CSV
# metrics %>% 
#   write_csv("presentation/data/in_bed_metrics.csv")

# Create a formatted table to display the metrics, grouped by epoch length
tab_in_bed <- 
  metrics %>% 
  mutate(
    .metric = factor(.metric,
                     levels = c("f_meas", "accuracy", "sensitivity", "precision", "specificity"),
                     labels = c("F1 Score", "Accuracy", "Sensitivity", "Precision", "Specificity")
    )
  ) |> 
  pivot_wider(names_from = epoch_length, values_from = .estimate) %>% 
  gt() |>
  fmt_percent(
    columns = 2:3,
    decimals = 2
  ) |>
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = 1:3
    )
  ) |>
  tab_header(
    title = md("Performance Metrics"),
    subtitle = "Grouped by Epoch Length"
  ) |>
  tab_options(table.font.names = "ibm") |>
  cols_width(everything() ~ px(120)) |>
  cols_align(align = "center") %>%
  cols_label(.metric = "") %>% 
  tab_options(
    # Configure table appearance, such as font, colors, borders, padding, and alignment
  )


# gtsave(tab_in_bed, "presentation/visuals/table_in_bed_performance_metrics.png", expand = 10)