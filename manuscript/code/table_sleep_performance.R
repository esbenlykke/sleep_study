#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(gt)
library(showtext)

all_metrics_30 <-
  read_csv(here::here("data/processed/all_metrics_sleep_30.csv")) %>% 
  mutate(
    group = case_when(group == "raw" ~ "Raw ZM Predictions",
                      group == "median_5" ~ "5-Min Median",
                      group == "median_10" ~ "10-Min Median")
  )

tbl_sleep_performance <-
  all_metrics_30 %>%
  select(-.estimator) %>%
  mutate(
    .estimate = .estimate * 100,
    .metric = factor(.metric,
      levels = c("f_meas", "accuracy", "sensitivity", "precision", "specificity"),
      labels = c("F1 Score", "Accuracy", "Sensitivity", "Precision", "Specificity")
    )
  ) |>
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  rename_with(.cols = "F1 Score":"Specificity", ~ paste(.x, "(%)")) %>% 
  gt(groupname_col = "group") |>
  fmt_number(everything(), decimals = 2) %>% 
  tab_options(table.font.names = "ibm") |>
  cols_width(everything() ~ px(120)) |>
  cols_align(align = "center") %>%
  cols_label(model = "") %>%
  tab_options(
    # Configure table appearance, such as font, colors, borders, padding, and alignment
  )
