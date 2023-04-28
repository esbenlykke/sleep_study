#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(gt)
library(showtext)

my_metrics <-
  metric_set(f_meas, accuracy, sensitivity, precision, specificity)

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



in_bed_fit_10 <-
  read_rds("/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed/in_bed_simple_tree_fit_10_sec_AXED.rds")

in_bed_fit_30 <-
  read_rds("/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed/in_bed_simple_tree_fit_30_sec_AXED.rds")

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

metrics %>% 
  write_csv("presentation/data/in_bed_metrics.csv")

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
    table.font.names = "ibm",
    table.font.color.light = "#EEE8D5",
    table.background.color = "#002B36",
    heading.border.bottom.style = "none",
    heading.align = "center",
    heading.title.font.size = 26,
    table.border.top.width = px(3),
    table.border.top.style = "none", # transparent
    table.border.bottom.style = "none",
    source_notes.border.bottom.color = "#002B36",
    column_labels.font.weight = "normal",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "#2C4E57",
    column_labels.padding = px(1),
    row_group.border.top.style = "none",
    row_group.border.top.color = "#2C4E57",
    row_group.border.bottom.width = px(0),
    row_group.border.bottom.color = "#2C4E57",
    stub.border.color = "#2C4E57",
    stub.border.width = px(0),
    stub.font.size = 14,
    table_body.border.bottom.color = "#002B36",
    table_body.hlines.color = "#2C4E57",
    table_body.vlines.style = "none",
    data_row.padding = px(1)
  )

# gtsave(tab_in_bed, "presentation/visuals/table_in_bed_performance_metrics.png", expand = 10)