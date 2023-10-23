#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)

fits_30_filenames_raw <-
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE
  ) %>%
  str_subset("raw") %>%
  str_subset("30_sec")

fits_30_filenames_median5 <-
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE
  ) %>%
  str_subset("5_min") %>%
  str_subset("30_sec")

fits_30_filenames_median10 <-
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE
  ) %>%
  str_subset("10_min") %>%
  str_subset("30_sec")

# Read the models from the file names and store them in separate lists

model_names <-
  c("Decision Tree", "Logistic Regression", "Neural Network", "XGBoost")

fits_30_raw <- map(fits_30_filenames_raw, read_rds) %>%
  set_names(model_names)
fits_30_median5 <- map(fits_30_filenames_median5, read_rds) %>%
  set_names(model_names)
fits_30_median10 <- map(fits_30_filenames_median10, read_rds) %>%
  set_names(model_names)

test_30 <-
  read_parquet("data/data_for_modelling/chained_classifiers/30_sec_testing_data.parquet") %>%
  mutate(
    across(contains("sleep"), as_factor)
  )

pr_raw <- 
  fits_30_raw %>% 
  map_dfr(., ~ augment(.x, test_30) %>% pr_curve(., sleep, .pred_0), .id = "model")

pr_median_5 <- 
  fits_30_median5 %>% 
  map_dfr(., ~ augment(.x, test_30) %>% pr_curve(., sleep_median5, .pred_0), .id = "model")

pr_median_10 <- 
  fits_30_median10 %>% 
  map_dfr(., ~ augment(.x, test_30) %>% pr_curve(., sleep_median10, .pred_0), .id = "model")

all_pr_30 <-
  bind_rows(
    raw = pr_raw, median_5 = pr_median_5, median_10 = pr_median_10,
    .id = "group"
  )

all_pr_30 %>%
  write_parquet("data/processed/all_pr_sleep_30.parquet")

pr_auc_raw <-
  fits_30_raw %>%
  map_dfr(., ~ .x %>% augment(test_30) %>% pr_auc(., sleep, .pred_0), .id = "model")

pr_auc_median_5 <-
  fits_30_raw %>%
  map_dfr(., ~ .x %>%  augment(test_30) %>% pr_auc(., sleep_median5, .pred_0), .id = "model")

pr_auc_median_10 <-
  fits_30_raw %>%
  map_dfr(., ~ .x %>%  augment(test_30) %>% pr_auc(., sleep_median10, .pred_0), .id = "model")

all_pr_auc <-
  bind_rows(
    raw = pr_auc_raw, median_5 = pr_auc_median_5, median_10 = pr_auc_median_10,
    .id = "group"
  )

all_pr_auc %>%
  write_csv("data/processed/all_pr_auc_sleep_30_sec_epochs.csv")


roc_raw <- 
  fits_30_raw %>% 
  map_dfr(., ~ augment(.x, test_30) %>% roc_curve(., sleep, .pred_0), .id = "model")

roc_median_5 <- 
  fits_30_median5 %>% 
  map_dfr(., ~ augment(.x, test_30) %>% roc_curve(., sleep_median5, .pred_0), .id = "model")

roc_median_10 <- 
  fits_30_median10 %>% 
  map_dfr(., ~ augment(.x, test_30) %>% roc_curve(., sleep_median10, .pred_0), .id = "model")

all_roc_30 <-
  bind_rows(
    raw = roc_raw, median_5 = roc_median_5, median_10 = roc_median_10,
    .id = "group"
  )

all_roc_30 %>%
  write_parquet("data/processed/all_roc_sleep_30.parquet")

roc_auc_raw <-
  fits_30_raw %>%
  map_dfr(., ~ .x %>% augment(test_30) %>% roc_auc(., sleep, .pred_0), .id = "model")

roc_auc_median_5 <-
  fits_30_raw %>%
  map_dfr(., ~ .x %>%  augment(test_30) %>% roc_auc(., sleep_median5, .pred_0), .id = "model")

roc_auc_median_10 <-
  fits_30_raw %>%
  map_dfr(., ~ .x %>%  augment(test_30) %>% roc_auc(., sleep_median10, .pred_0), .id = "model")

all_roc_auc <-
  bind_rows(
    raw = roc_auc_raw, median_5 = roc_auc_median_5, median_10 = roc_auc_median_10,
    .id = "group"
  )

all_roc_auc %>%
  write_csv("data/processed/all_roc_auc_sleep_30_sec_epochs.csv")