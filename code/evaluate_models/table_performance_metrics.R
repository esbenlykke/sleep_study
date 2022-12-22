#!/usr/bin/env RscriptsuppressMessages(library(tidyverse))

library(tidyverse)
library(gt)
library(showtext)


metric_data <- read_csv("data/processed/performance_metrics.csv")
comb_metric_data <- read_csv("data/processed/combined_preds_performance_metrics.csv")

# Create table ------------------------------------------------------------


font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()

tab_performance <-
  comb_metric_data |> 
  mutate(
    .metric = factor(.metric, 
                     levels = c("f_meas", "accuracy", "sensitivity", "specificity"),
                     labels = c("F1 Score", "Accuracy", "Sensitivity", "Specificity"))
  ) |> 
  group_by(event) |>
  gt(rowname_col = ".metric") |>
  fmt_percent(
    columns = c(logistic_regression:xgboost),
    decimals = 2
  ) |>
  cols_label(
    logistic_regression = "Logistic Regression",
    neural_net = "Neural Network",
    decision_tree = "Decision Tree",
    xgboost = "XGboost"
  ) |>
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = c(logistic_regression:xgboost)
    )
  ) |>
  tab_header(
    title = md("Performance Metrics"),
    subtitle = "Grouped by Event Prediction"
  ) |>
  tab_options(table.font.names = "ibm") |>
  cols_width(everything() ~ px(100)) |> 
  cols_align(align = "center") |> 
  data_color(columns = decision_tree, colors = "lightblue", alpha = .6)

gtsave(tab_performance, "visuals/table_combined_preds_performance_metrics.html")