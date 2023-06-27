#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)

# File processing function
process_files <- function(suffix){
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE
  ) %>%
    str_subset(suffix) %>%
    str_subset("30_sec") %>%
    map(read_rds) %>%
    set_names(c("Decision Tree", "Logistic Regression", "Neural Network", "XGBoost"))
}

fits_30_raw <- process_files("raw")
fits_30_median5 <- process_files("5_min")
fits_30_median10 <- process_files("10_min")

test_30 <-
  read_parquet("data/data_for_modelling/chained_classifiers/30_sec_testing_data.parquet") %>%
  mutate(
    across(contains("sleep"), as_factor)
  )

# Metric calculation function
calc_metrics <- function(fits, target, file_prefix){
  pr <- fits %>% map_dfr(., ~ augment(.x, test_30) %>% pr_curve(., target, .pred_1), .id = "model")
  pr_auc <- fits %>% map_dfr(., ~ .x %>% augment(test_30) %>% pr_auc(., target, .pred_1), .id = "model")
  roc <- fits %>% map_dfr(., ~ augment(.x, test_30) %>% roc_curve(., target, .pred_1), .id = "model")
  roc_auc <- fits %>% map_dfr(., ~ .x %>% augment(test_30) %>% roc_auc(., target, .pred_1), .id = "model")
  
  # Writing files
  bind_rows(pr, pr_auc, roc, roc_auc, .id = "group") %>%
    write_parquet(paste0("data/processed/all_", file_prefix, "_sleep_30.parquet"))
  bind_rows(pr_auc, roc_auc, .id = "group") %>%
    write_csv(paste0("data/processed/all_", file_prefix, "_auc_sleep_30_sec_epochs.csv"))
}

calc_metrics(fits_30_raw, "sleep", "raw")
calc_metrics(fits_30_median5, "sleep_median5", "median_5")
calc_metrics(fits_30_median10, "sleep_median10", "median_10")
