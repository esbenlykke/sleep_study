#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)


tidymodels_prefer()
options(tidymodels.dark = TRUE)

path <- "/media/esbenlykke/My Passport/crude/"

train <- 
  read_parquet("data/data_for_modelling/crude_training_data.parquet")
  
# for testing purposes
# ids <- train %>% distinct(id) %>% slice(1:10)
# 
# train <- train %>%
#   filter(id %in% ids$id) %>%
#   group_by(id, in_bed, sleep) %>%
#   slice_sample(n = 100) %>%
#   ungroup()
###

# IN-BED ------------------------------------------------------------------
cat("Reading in-bed workflowsets\n")

all_in_bed_wf <-
  read_rds(str_c(path, "/grid_results/in_bed_workflowsets_results.rds"))

# Logistic regression
cat("Finalizing and fitting logistic regression model\n")

best_in_bed_logistic_result <-
  all_in_bed_wf %>%
  extract_workflow_set_result("in_bed_logistic_regression") |>
  select_best(metric = "f_meas")

all_in_bed_wf |>
  extract_workflow("in_bed_logistic_regression") |>
  finalize_workflow(best_in_bed_logistic_result) %>%
  fit(train) %>%
  write_rds(str_c(path, "fitted_models/crude_in_bed_log_reg_fit.rds"))


# Neural network
cat("Finalizing and fitting neural network model\n")

best_in_bed_nn_result <-
  all_in_bed_wf %>%
  extract_workflow_set_result("in_bed_neural_network") |>
  select_best(metric = "f_meas")

all_in_bed_wf |>
  extract_workflow("in_bed_neural_network") |>
  finalize_workflow(best_in_bed_nn_result)  %>%
  fit(train) %>%
  write_rds(str_c(path, "fitted_models/crude_in_bed_nnet_fit.rds"))


# XGboost
cat("Finalizing and fitting XGBoost model\n")

best_in_bed_xgboost_result <-
  all_in_bed_wf %>%
  extract_workflow_set_result("in_bed_xgboost") |>
  select_best(metric = "f_meas")

all_in_bed_wf |>
  extract_workflow("in_bed_xgboost") |>
  finalize_workflow(best_in_bed_xgboost_result)  %>%
  fit(train) %>%
  write_rds(str_c(path, "fitted_models/crude_in_bed_xgb_fit.rds"))

rm(all_in_bed_wf)
gc()

cat("All done\n")

# SLEEP -------------------------------------------------------------------

all_sleep_wf <-
  read_rds(str_c(path, "grid_results/sleep_workflowsets_results.rds"))

# Logistic regression

best_sleep_logistic_result <-
  all_sleep_wf %>%
  extract_workflow_set_result("sleep_logistic_regression") |>
  select_best(metric = "f_meas")

all_sleep_wf |>
  extract_workflow("sleep_logistic_regression") |>
  finalize_workflow(best_sleep_logistic_result) %>%
  fit(train) %>%
  write_rds(str_c(path, "fitted_models/crude_sleep_log_reg_fit.rds"))


# Neural network

best_sleep_nn_result <-
  all_sleep_wf %>%
  extract_workflow_set_result("sleep_neural_network") |>
  select_best(metric = "f_meas")

all_sleep_wf |>
  extract_workflow("sleep_neural_network") |>
  finalize_workflow(best_sleep_nn_result)  %>%
  fit(train) %>%
  write_rds(str_c(path, "fitted_models/crude_sleep_nnet_fit.rds"))


# XGboost

best_sleep_xgboost_result <-
  all_sleep_wf %>%
  extract_workflow_set_result("sleep_xgboost") |>
  select_best(metric = "f_meas")

all_sleep_wf |>
  extract_workflow("sleep_xgboost") |>
  finalize_workflow(best_sleep_xgboost_result)  %>%
  fit(train) %>%
  write_rds(str_c(path, "fitted_models/crude_sleep_xgb_fit.rds"))