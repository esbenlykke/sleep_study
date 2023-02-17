library(tidyverse)
library(gt)
library(showtext)
library(ggtext)

metric_data <- read_csv("~/sleep_study/data/processed/performance_metrics.csv")


# Create table ------------------------------------------------------------


font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()

tab1 <-
  metric_data |>
  mutate(
    .metric = factor(.metric,
      levels = c("f_meas", "accuracy", "sensitivity", "precision", "specificity"),
      labels = c("F1 Score", "Accuracy", "Sensitivity", "Precision", "Specificity")
    )
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
  tab_style(
    style = cell_fill("#2C4E57"),
    locations = cells_body(rows = c(1, 3, 5, 6, 8, 10))
  ) |>
  tab_header(
    title = "",
    subtitle = md("Performance of the models to predict each class
                             <span style='color:gold'>**seperately**</span>")
  ) |> 
  cols_width(everything() ~ px(100)) |>
  cols_align(align = "center") |>
  # data_color(columns = decision_tree, colors = "#4A666C", alpha = .6) |>
  tab_options(
    table.font.names = "ibm",
    table.font.color.light = "#EEE8D5",
    table.background.color = "#002B36",
    heading.border.bottom.style = "none",
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
    # table_body.hlines.style = "none",
    table_body.vlines.style = "none",
    data_row.padding = px(1),
    heading.align = "center"
  )


comb_metric_data <- 
  read_csv("~/sleep_study/data/processed/combined_preds_performance_metrics.csv")

tab2 <-
  comb_metric_data |>
  mutate(
    .metric = factor(.metric,
      levels = c("f_meas", "accuracy", "sensitivity", "precision", "specificity"),
      labels = c("F1 Score", "Accuracy", "Sensitivity", "Precision", "Specificity")
    )
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
  tab_style(
    style = cell_text(color = "#f88379"),
    locations = cells_body(rows = c(1, 3, 4))
  ) |>
  tab_style(
    style = cell_fill("#2C4E57"),
    locations = cells_body(rows = c(1, 3, 5, 6, 8, 10))
  ) |>
  tab_header(
    title = "",
    subtitle = md("Performance of the models to predict each 
                  <span style='color:gold'>**combined**</span> class")
  ) |>
  cols_width(everything() ~ px(100)) |>
  cols_align(align = "center") |>
  tab_options(
    table.font.names = "ibm",
    table.font.color.light = "#EEE8D5",
    table.background.color = "#002B36",
    heading.border.bottom.style = "none",
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
    # table_body.hlines.style = "none",
    table_body.vlines.style = "none",
    data_row.padding = px(1),
    heading.align = "center"
  )
