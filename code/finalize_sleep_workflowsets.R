#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))


tidymodels_prefer()
options(tidymodels.dark = TRUE)

cat("Reading sleep workflowsets")

all_sleep_wf <- read_rds("data/models/sleep_workflowsets_results.rds")


# Logistic regression -----------------------------------------------------


best_sleep_logistic_result <-
  all_sleep_wf %>%
  extract_workflow_set_result("sleep_logistic_regression") |> 
  select_best(metric = "f_meas") 

logistic_wf <- 
  all_sleep_wf |> 
  extract_workflow("sleep_logistic_regression") |> 
  finalize_workflow(best_sleep_logistic_result) 

# Neural network ----------------------------------------------------------


best_sleep_nn_result <-
  all_sleep_wf %>%
  extract_workflow_set_result("sleep_neural_network") |> 
  select_best(metric = "f_meas") 

nn_wf <- 
  all_sleep_wf |> 
  extract_workflow("sleep_neural_network") |> 
  finalize_workflow(best_sleep_nn_result) 

# MARS model --------------------------------------------------------------


best_sleep_MARS_result <-
  all_sleep_wf %>%
  extract_workflow_set_result("sleep_MARS") |> 
  select_best(metric = "f_meas") 

MARS_wf <- 
  all_sleep_wf |> 
  extract_workflow("sleep_MARS") |> 
  finalize_workflow(best_sleep_MARS_result) 

# XGboost -----------------------------------------------------------------


best_sleep_xgboost_result <-
  all_sleep_wf %>%
  extract_workflow_set_result("sleep_xgboost") |> 
  select_best(metric = "f_meas") 

xgboost_wf <- 
  all_sleep_wf |> 
  extract_workflow("sleep_xgboost") |> 
  finalize_workflow(best_sleep_xgboost_result) 

list(logistic_wf, nn_wf, MARS_wf, xgboost_wf) |> 
  setNames(nm = c("logistic_wf", "nn_wf", "MARS_wf", "xgboost_wf")) %>%
  walk2(names(.), ~write_rds(.x, file = paste0("data/models/finalized_workflows/sleep_", .y, ".rds")))