#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)


tidymodels_prefer()
options(tidymodels.dark = TRUE)


### IN-BED ASLEEP

cat("Reading in-bed asleep workflowsets")

in_bed_asleep_wfs <-
  read_rds("/media/esbenlykke/My Passport/binary_relevance/grid_results/in_bed_asleep_workflowsets_results.rds")

# Logistic regression -----------------------------------------------------


best_in_bed_asleep_logistic_result <-
  in_bed_asleep_wfs %>%
  extract_workflow_set_result("normalized_in_bed_asleep_logistic_regression") |>
  select_best(metric = "f_meas")

logistic_wf <-
  in_bed_asleep_wfs |>
  extract_workflow("normalized_in_bed_asleep_logistic_regression") |>
  finalize_workflow(best_in_bed_asleep_logistic_result)

# Neural network ----------------------------------------------------------


best_in_bed_asleep_nn_result <-
  in_bed_asleep_wfs %>%
  extract_workflow_set_result("normalized_in_bed_asleep_neural_network") |>
  select_best(metric = "f_meas")

nn_wf <-
  in_bed_asleep_wfs |>
  extract_workflow("normalized_in_bed_asleep_neural_network") |>
  finalize_workflow(best_in_bed_asleep_nn_result)


# XGboost -----------------------------------------------------------------


best_in_bed_asleep_xgboost_result <-
  in_bed_asleep_wfs %>%
  extract_workflow_set_result("basic_in_bed_asleep_xgboost") |>
  select_best(metric = "f_meas")

xgboost_wf <-
  in_bed_asleep_wfs |>
  extract_workflow("basic_in_bed_asleep_xgboost") |>
  finalize_workflow(best_in_bed_asleep_xgboost_result)


# Write

list(logistic_wf, nn_wf, xgboost_wf) |>
  setNames(nm = c("logistic_wf_in_bed_aslep", "nn_wf_in_bed_aslep", "xgboost_wf_in_bed_aslep")) %>%
  walk2(names(.), ~write_rds(.x, file = paste0("/media/esbenlykke/My Passport/best_worflows/", .y, ".rds")))


### IN-BED AWAKE

cat("Reading in-bed awake workflowsets")

in_bed_awake_wfs <-
  read_rds("/media/esbenlykke/My Passport/grid_results/in_bed_awake_workflowsets_results.rds")

# Logistic regression -----------------------------------------------------


best_in_bed_awake_logistic_result <-
  in_bed_awake_wfs %>%
  extract_workflow_set_result("normalized_in_bed_awake_logistic_regression") |>
  select_best(metric = "f_meas")

logistic_wf <-
  in_bed_awake_wfs |>
  extract_workflow("normalized_in_bed_awake_logistic_regression") |>
  finalize_workflow(best_in_bed_awake_logistic_result)

# Neural network ----------------------------------------------------------


best_in_bed_awake_nn_result <-
  in_bed_awake_wfs %>%
  extract_workflow_set_result("normalized_in_bed_awake_neural_network") |>
  select_best(metric = "f_meas")

nn_wf <-
  in_bed_awake_wfs |>
  extract_workflow("normalized_in_bed_awake_neural_network") |>
  finalize_workflow(best_in_bed_awake_nn_result)


# XGboost -----------------------------------------------------------------


best_in_bed_awake_xgboost_result <-
  in_bed_awake_wfs %>%
  extract_workflow_set_result("basic_in_bed_awake_xgboost") |>
  select_best(metric = "f_meas")

xgboost_wf <-
  in_bed_awake_wfs |>
  extract_workflow("basic_in_bed_awake_xgboost") |>
  finalize_workflow(best_in_bed_awake_xgboost_result)


# Write

list(logistic_wf, nn_wf, xgboost_wf) |>
  setNames(nm = c("logistic_wf_in_bed_awake", "nn_wf_in_bed_awake", "xgboost_wf_in_bed_awake")) %>%
  walk2(names(.), ~write_rds(.x, file = paste0("/media/esbenlykke/My Passport/best_worflows/", .y, ".rds")))

rm(list = ls())

### IN-BED AWAKE

cat("Reading out-bed awake workflowsets")

out_bed_awake_wfs <-
  read_rds("/media/esbenlykke/My Passport/grid_results/out_bed_awake_workflowsets_results.rds")

# Logistic regression -----------------------------------------------------


best_out_bed_awake_logistic_result <-
  out_bed_awake_wfs %>%
  extract_workflow_set_result("normalized_out_bed_awake_logistic_regression") |> 
  select_best(metric = "f_meas") 

logistic_wf <-
  out_bed_awake_wfs |>
  extract_workflow("normalized_out_bed_awake_logistic_regression") |> 
  finalize_workflow(best_out_bed_awake_logistic_result)

# Neural network ----------------------------------------------------------


best_out_bed_awake_nn_result <-
  out_bed_awake_wfs %>%
  extract_workflow_set_result("normalized_out_bed_awake_neural_network") |> 
  select_best(metric = "f_meas") 

nn_wf <- 
  out_bed_awake_wfs |> 
  extract_workflow("normalized_out_bed_awake_neural_network") |> 
  finalize_workflow(best_out_bed_awake_nn_result)


# XGboost -----------------------------------------------------------------


best_out_bed_awake_xgboost_result <-
  out_bed_awake_wfs %>%
  extract_workflow_set_result("basic_out_bed_awake_xgboost") |> 
  select_best(metric = "f_meas") 

xgboost_wf <- 
  out_bed_awake_wfs |> 
  extract_workflow("basic_out_bed_awake_xgboost") |> 
  finalize_workflow(best_out_bed_awake_xgboost_result)


# Write 

list(logistic_wf, nn_wf, xgboost_wf) |> 
  setNames(nm = c("logistic_wf_out_bed_awake", "nn_wf_out_bed_awake", "xgboost_wf_out_bed_awake")) %>%
  walk2(names(.), ~write_rds(.x, file = paste0("/media/esbenlykke/My Passport/best_worflows/", .y, ".rds")))
