library(tidyverse)
library(arrow)
library(gt)
library(showtext)
library(gtExtras)
library(here)


ba_metrics_crude <-
  read_csv(here::here("data/processed/crude_mixed_effect_ba.csv"))

ba_metrics_multi <- 
  read_csv(here::here("data/processed/multiclass_mixed_effect_ba.csv"))


# create table ------------------------------------------------------------

font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()

create_ba_table <- function(tbl){
    tbl |>
  mutate(
    ba_metric = case_when(
      str_detect(ba_metric, "bias") ~ "Bias (95% CI)",
      str_detect(ba_metric, "lower") ~ "Lower LOA (95% CI)",
      str_detect(ba_metric, "upper") ~ "Upper LOA (95% CI)"
    ),
    model = case_when(
      str_detect(model, "logistic") ~ "Logistic Regression",
      str_detect(model, "decision") ~ "Decision Tree",
      str_detect(model, "neural") ~ "Neural Net",
      TRUE ~ "XGboost"
    ),
    type = case_when(
      str_detect(type, "spt") ~ "Sleep Period Time (hrs)",
      str_detect(type, "tst") ~ "Total Sleep Time (hrs)",
      str_detect(type, "se_percent") ~ "Sleep Efficiency (%)",
      str_detect(type, "lps") ~ "Latency Until Persistent Sleep (min)",
      TRUE ~ "Wake After Sleep onset (min)"
    ),
    estimate = glue::glue("{round(estimate, 2)} ({round(lower_ci, 2)}; {round(upper_ci, 2)})")
  ) |>
  select(-lower_ci:-upper_ci) |>
  pivot_wider(names_from = ba_metric, values_from = estimate) |>
  # Create table
  gt(groupname_col = "type") |>
  # tab_header(title = md("Bland-Altman Analysis"))
  cols_align(align = "right", columns = model) |>
  cols_width(
    2:5 ~ px(200),
    model ~ px(200)
  ) |>
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = 1:5
    )
  ) |>
  tab_style(
    style = cell_fill("#2C4E57"),
    locations = cells_body(rows = seq(1, 19, 2))
  ) |>
  cols_label(model = "") |>
  cols_align(columns = 1:5, align = "right") |>
  tab_footnote(
    footnote = "Bootstrapped mixed effects limits of agreement with multiple
    observations per subject (Parker et al. 2016)"
  ) |>
  tab_options(
    table.font.names = "ibm",
    table.font.color.light = "#EEE8D5",
    table.background.color = "#002B36",
    heading.border.bottom.style = "none",
    heading.padding = px(20),
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
    heading.align = "center",
    footnotes.font.size = 10
  )
}

tab_ba_crude <-
  create_ba_table(ba_metrics_crude)
