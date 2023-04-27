#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)


tidymodels_prefer()
options(tidymodels.dark = TRUE)

cat("Reading in-bed workflowsets")

path <- "/media/esbenlykke/My Passport/multiclass/"

train <- 
  read_parquet("data/data_for_modelling/multiclass_training_data.parquet")

# for testing purposes
# ids <- train %>% distinct(id) %>% slice(1:10)
# 
# train <- train %>%
#   filter(id %in% ids$id) %>%
#   group_by(id, multiclass) %>%
#   slice_sample(n = 100) %>%
#   ungroup()
###

# Finalizing models -------------------------------------------------------

grid_results <- 
  read_rds(str_c(path, "/grid_results/multiclass_workflowsets_results.rds"))

# Logistic regression

best_log_reg <- 
  grid_results %>% 
  extract_workflow_set_result("multiclass_rec_logistic_regression") |> 
  select_best(metric = "f_meas") 

grid_results %>% 
  extract_workflow("multiclass_rec_logistic_regression") %>% 
  finalize_workflow(best_log_reg) %>% 
  fit(train) %>% 
  write_rds(str_c(path, "fitted_models/multiclass_log_reg_fit.rds"))


# Neural network

best_nnet <- 
  grid_results %>% 
  extract_workflow_set_result("multiclass_rec_nnet") |> 
  select_best(metric = "f_meas") 

grid_results %>% 
  extract_workflow("multiclass_rec_nnet") %>% 
  finalize_workflow(best_nnet) %>% 
  fit(train) %>% 
  write_rds(str_c(path, "fitted_models/multiclass_nnet_fit.rds"))


# XGboost

best_xgb <- 
  grid_results %>% 
  extract_workflow_set_result("multiclass_rec_xgboost") |> 
  select_best(metric = "f_meas") 

grid_results %>% 
  extract_workflow("multiclass_rec_xgboost") %>% 
  finalize_workflow(best_xgb) %>% 
  fit(train) %>% 
  write_rds(str_c(path, "fitted_models/multiclass_xgb_fit.rds"))