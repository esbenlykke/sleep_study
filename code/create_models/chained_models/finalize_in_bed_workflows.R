#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)


tidymodels_prefer()
options(tidymodels.dark = TRUE)

path <- "/media/esbenlykke/My Passport/crude/"

train10 <- 
  read_parquet("data/data_for_modelling/crude_training_data.parquet")

train30 <- 
  read_parquet("data/data_for_modelling/crude_training_30_sec_data.parquet")

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

in_bed_grid_results_10_sec_10_sec <-
  read_rds(str_c(path, "/grid_results/in_bed_workflowsets_results_10_sec.rds"))

# Logistic regression
cat("Finalizing and fitting logistic regression model\n")

best_in_bed_logistic_result <-
  in_bed_grid_results_10_sec %>%
  extract_workflow_set_result("in_bed_logistic_regression") |>
  select_best(metric = "f_meas")

in_bed_grid_results_10_sec |>
  extract_workflow("in_bed_logistic_regression") |>
  finalize_workflow(best_in_bed_logistic_result) %>%
  fit(train) %>%
  write_rds(str_c(path, "fitted_models/crude_in_bed_log_reg_fit.rds"))

# Neural network
cat("Finalizing and fitting neural network model\n")

best_in_bed_nn_result <-
  in_bed_grid_results_10_sec %>%
  extract_workflow_set_result("in_bed_neural_network") |>
  select_best(metric = "f_meas")

in_bed_grid_results_10_sec |>
  extract_workflow("in_bed_neural_network") |>
  finalize_workflow(best_in_bed_nn_result)  %>%
  fit(train) %>%
  write_rds(str_c(path, "fitted_models/crude_in_bed_nnet_fit.rds"))

# Decision tree
cat("Finalizing and fitting decision tree model\n")

best_in_bed_CART_result <-
  in_bed_grid_results_10_sec %>%
  extract_workflow_set_result("in_bed_logistic_regression") |>
  select_best(metric = "f_meas")

in_bed_grid_results_10_sec |>
  extract_workflow("in_bed_logistic_regression") |>
  finalize_workflow(best_in_bed_logistic_result) %>%
  fit(train) %>%
  write_rds(str_c(path, "fitted_models/crude_in_bed_simple_tree_fit.rds"))

# XGboost
cat("Finalizing and fitting XGBoost model\n")

best_in_bed_xgboost_result <-
  in_bed_grid_results_10_sec %>%
  extract_workflow_set_result("in_bed_xgboost") |>
  select_best(metric = "f_meas")

in_bed_grid_results_10_sec |>
  extract_workflow("in_bed_xgboost") |>
  finalize_workflow(best_in_bed_xgboost_result)  %>%
  fit(train) %>%
  write_rds(str_c(path, "fitted_models/crude_in_bed_xgb_fit.rds"))

cat("All done\n")
