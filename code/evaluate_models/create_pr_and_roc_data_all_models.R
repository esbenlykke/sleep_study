#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)

# Define a function to read the models and calculate the PR and ROC curves
process_models <- function(filenames, test_data, sleep_col) {
  models <- map(filenames, read_rds) %>% set_names(model_names)
  
  pr <- models %>% map_dfr(~ augment(.x, test_data) %>% pr_curve(., {{sleep_col}}, .pred_0), .id = "model")
  pr_auc <- models %>% map_dfr(~ .x %>% augment(test_data) %>% pr_auc(., {{sleep_col}}, .pred_0), .id = "model")
  
  roc <- models %>% map_dfr(~ augment(.x, test_data) %>% roc_curve(., {{sleep_col}}, .pred_0), .id = "model")
  roc_auc <- models %>% map_dfr(~ .x %>% augment(test_data) %>% roc_auc(., {{sleep_col}}, .pred_0), .id = "model")
  
  list(pr = pr, pr_auc = pr_auc, roc = roc, roc_auc = roc_auc)
}

# Define model names
model_names <- c("Decision Tree", "Logistic Regression", "Feed-Forward Neural Network", "XGBoost")

# Read test data
test_30 <- read_parquet("data/data_for_modelling/chained_classifiers/30_sec_testing_data.parquet") %>%
  mutate(across(contains("sleep"), as_factor))

# Define list of filenames
filenames <- list(
  raw = list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
                   full.names = TRUE
  ) %>%
    str_subset("raw") %>%
    str_subset("30_sec"),
  median_5 = list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
                        full.names = TRUE
  ) %>%
    str_subset("5_min") %>%
    str_subset("30_sec"),
  median_10 = list.files("/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep",
                         full.names = TRUE
  ) %>%
    str_subset("10_min") %>%
    str_subset("30_sec")
)

# Define list of sleep column names
sleep_cols <- c("sleep", "sleep_median5", "sleep_median10")

# Apply the function to the different fitted models
results <- map2(filenames, sleep_cols, ~ process_models(.x, test_30, .y))

# Bind rows and write to disk
all_pr_30 <- map(results, "pr") %>% bind_rows(.id = "group")
all_pr_30 %>% write_parquet("data/processed/all_pr_sleep_30.parquet")

all_pr_auc <- map(results, "pr_auc") %>% bind_rows(.id = "group")
all_pr_auc %>% write_csv("data/processed/all_pr_auc_sleep_30_sec_epochs.csv")

all_roc_30 <- map(results, "roc") %>% bind_rows(.id = "group")
all_roc_30 %>% write_parquet("data/processed/all_roc_sleep_30.parquet")

all_roc_auc <- map(results, "roc_auc") %>% bind
