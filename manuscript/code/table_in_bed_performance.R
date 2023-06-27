#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(gt)
library(showtext)

metrics <-
  read_csv(here::here("data/processed/in_bed_metrics.csv"))

# Create a formatted table to display the metrics, grouped by epoch length
tbl_in_bed <-
  metrics %>%
  mutate(
    .estimate = .estimate * 100,
    .metric = factor(.metric,
                     levels = c("f_meas", "accuracy", "sensitivity", "precision", "specificity"),
                     labels = c("F1 Score (%)", "Accuracy (%)", "Sensitivity (%)", "Precision (%)", "Specificity (%)")
    ),
    model = case_when(model == "decision_tree" ~ "Decision Tree",
                      model == "log_reg" ~ "Logistic Regression",
                      model == "mlp" ~ "Feed-Forward Neural Network",
                      model == "xgboost" ~ "XGBoost")
  ) %>%
  select(-.estimator) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  gt() %>%
  fmt_percent(
    columns = 2:6,
    decimals = 1
  ) 