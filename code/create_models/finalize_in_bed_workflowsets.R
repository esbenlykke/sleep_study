#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))


tidymodels_prefer()
options(tidymodels.dark = TRUE)

cat("Reading in-bed workflowsets")

all_in_bed_wf <- read_rds("data/models/in_bed_workflowsets_results.rds")


# Logistic regression -----------------------------------------------------


best_in_bed_logistic_result <-
  all_in_bed_wf %>%
  extract_workflow_set_result("in_bed_logistic_regression") |> 
  select_best(metric = "f_meas") 

logistic_wf <- 
  all_in_bed_wf |> 
  extract_workflow("in_bed_logistic_regression") |> 
  finalize_workflow(best_in_bed_logistic_result) 

# Neural network ----------------------------------------------------------


best_in_bed_nn_result <-
  all_in_bed_wf %>%
  extract_workflow_set_result("in_bed_neural_network") |> 
  select_best(metric = "f_meas") 

nn_wf <- 
  all_in_bed_wf |> 
  extract_workflow("in_bed_neural_network") |> 
  finalize_workflow(best_in_bed_nn_result) 

# MARS model --------------------------------------------------------------


# best_in_bed_MARS_result <-
#   all_in_bed_wf %>%
#   extract_workflow_set_result("in_bed_MARS") |> 
#   select_best(metric = "f_meas") 
# 
# MARS_wf <- 
#   all_in_bed_wf |> 
#   extract_workflow("in_bed_MARS") |> 
#   finalize_workflow(best_in_bed_MARS_result) 

# XGboost -----------------------------------------------------------------


best_in_bed_xgboost_result <-
  all_in_bed_wf %>%
  extract_workflow_set_result("in_bed_xgboost") |> 
  select_best(metric = "f_meas") 

xgboost_wf <- 
  all_in_bed_wf |> 
  extract_workflow("in_bed_xgboost") |> 
  finalize_workflow(best_in_bed_xgboost_result) 


# Write 

list(logistic_wf, nn_wf, xgboost_wf) |> 
  setNames(nm = c("logistic_wf", "nn_wf", "xgboost_wf")) %>%
  walk2(names(.), ~write_rds(.x, file = paste0("data/models/finalized_workflows/in_bed_", .y, ".rds")))