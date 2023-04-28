library(tidyverse)
library(gt)
library(showtext)
library(ggtext)

metrics_data <-  
  read_csv(here::here("presentation/data/in_bed_metrics.csv"))

tab_in_bed <- 
  metrics_data %>% 
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