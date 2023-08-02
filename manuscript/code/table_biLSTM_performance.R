#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(gt)
library(showtext)

biLSTM_metrics <-
  read_csv(here::here("data/processed/biLSTM_performance_metrics.csv"))


tbl_biLSTM_performance <-
  biLSTM_metrics %>%
  mutate(
    .estimate = .estimate * 100,
    .metric = factor(.metric,
      levels = c("f_meas", "precision", "npv", "sensitivity", "specificity"),
      labels = c("F1 Score", "Precision", "NPV", "Sensitivity", "Specificity")
    ),
    type = case_when(
      type == "raw" ~ "Raw ZM Predictions",
      type == "median_5" ~ "5-Min Median",
      type == "median_10" ~ "10-Min Median"
    )
  ) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  # rename_with(.cols = "F1 Score":"Specificity", ~ paste(.x, "(%)")) %>%
  gt() |>
  fmt_number(everything(), decimals = 1) %>%
  tab_options(table.font.names = "ibm") |>
  cols_width(everything() ~ px(140)) |>
  cols_align(align = "center") %>%
  cols_label(type = "")
