#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)


get_roc <-
  function(fit, test, truth, estimate) {
    fit |>
      augment(test) |>
      roc_curve(truth = {{ truth }}, estimate = {{ estimate }}, event_level = "second")
  }

get_roc_auc <-
  function(fit, test, truth, estimate) {
    fit |>
      augment(test) |>
      roc_auc(truth = {{ truth }}, estimate = {{ estimate }}, event_level = "second")
  }

## Get the file names of the fitted models for the 10-second epochs, grouped by preprocessing type (raw, median5, and median10)
fits_10_filenames_raw <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("raw") %>% 
  str_subset("30_sec")


fits_10_filenames_median5 <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("5_min") %>% 
  str_subset("30_sec")


fits_10_filenames_median10 <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("10_min") %>% 
  str_subset("30_sec")


# Read the models from the file names and store them in separate lists
fits_10_raw <- map(fits_10_filenames_raw, read_rds)
fits_10_median5 <- map(fits_10_filenames_median5, read_rds)
fits_10_median10 <- map(fits_10_filenames_median10, read_rds)

# Repeat the same process for the 30-second epochs
fits_30_filenames_raw <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("raw") %>% 
  str_subset("30_sec")

fits_30_filenames_median5 <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("5_min") %>% 
  str_subset("30_sec")

fits_30_filenames_median10 <- 
  list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
             full.names = TRUE) %>% 
  str_subset("10_min") %>% 
  str_subset("30_sec")

# Read the models from the file names and store them in separate lists
fits_30_raw <- map(fits_30_filenames_raw, read_rds)
fits_30_median5 <- map(fits_30_filenames_median5, read_rds)
fits_30_median10 <- map(fits_30_filenames_median10, read_rds)

# Read the test datasets for the 10-second and 30-second epochs
test_10 <-
  read_parquet("data/data_for_modelling/chained_classifiers/only_in_bed_testing_data_10_sec_epochs.parquet") %>% 
  select(-contains(".pred"))

test_30 <-
  read_parquet("data/data_for_modelling/chained_classifiers/only_in_bed_testing_data_30_sec_epochs.parquet") %>% 
  select(-contains(".pred"))

model_names <- 
  c("Decision Tree", "Logistic Regression", "Neural Network", "XGBoost")

roc_raw_10 <- 
  fits_10_raw %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_roc(.x, test_10, sleep, .pred_1), .id = "model")

roc_median5_10 <- 
  fits_10_median5 %>%
  set_names(model_names) %>%  
  map_dfr(~ get_roc(.x, test_10, sleep_median5, .pred_1), .id = "model")

roc_median10_10 <- 
  fits_10_median10 %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_roc(.x, test_10, sleep_median10, .pred_1), .id = "model")

all_roc_10 <- 
  bind_rows(raw = roc_raw_10, median_5 = roc_median5_10, median_10 = roc_median10_10, 
            .id = "group")

# Save roc as CSV
# all_roc_10 %>%
#   write_parquet("presentation/data/all_roc_sleep_10.parquet")

roc_raw_30 <- 
  fits_30_raw %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_roc(.x, test_30, sleep, .pred_1), .id = "model")

roc_median5_30 <- 
  fits_30_median5 %>%
  set_names(model_names) %>%  
  map_dfr(~ get_roc(.x, test_30, sleep_median5, .pred_1), .id = "model")

roc_median10_30 <- 
  fits_30_median10 %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_roc(.x, test_30, sleep_median10, .pred_1), .id = "model")

all_roc_30 <- 
  bind_rows(raw = roc_raw_30, median_5 = roc_median5_30, median_10 = roc_median10_30, 
            .id = "group")

# Save roc as CSV
# all_roc_30 %>%
  # write_parquet("presentation/data/all_roc_sleep_30.parquet")

roc_auc_raw_10 <- 
  fits_10_raw %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_roc_auc(.x, test_10, sleep, .pred_1), .id = "model")

roc_auc_median5_10 <- 
  fits_10_median5 %>%
  set_names(model_names) %>%  
  map_dfr(~ get_roc_auc(.x, test_10, sleep_median5, .pred_1), .id = "model")

roc_auc_median10_10 <- 
  fits_10_median10 %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_roc_auc(.x, test_10, sleep_median10, .pred_1), .id = "model")

all_roc_auc_10 <- 
  bind_rows(raw = roc_auc_raw_10, median_5 = roc_auc_median5_10, median_10 = roc_auc_median10_10, 
            .id = "group")

# all_roc_auc_10 %>% 
#   write_csv("presentation/data/all_roc_auc_sleep_10_sec_epochs.csv")

roc_auc_raw_30 <- 
  fits_30_raw %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_roc_auc(.x, test_30, sleep, .pred_1), .id = "model")

roc_auc_median5_30 <- 
  fits_30_median5 %>%
  set_names(model_names) %>%  
  map_dfr(~ get_roc_auc(.x, test_30, sleep_median5, .pred_1), .id = "model")

roc_auc_median10_30 <- 
  fits_30_median10 %>% 
  set_names(model_names) %>% 
  map_dfr(~ get_roc_auc(.x, test_30, sleep_median10, .pred_1), .id = "model")

all_roc_auc_30 <- 
  bind_rows(raw = roc_auc_raw_30, median_5 = roc_auc_median5_30, median_10 = roc_auc_median10_30, 
            .id = "group")

# all_roc_auc_30 %>% 
#     write_csv("presentation/data/all_roc_auc_sleep_30_sec_epochs.csv")