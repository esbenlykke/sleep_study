#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(butcher)
library(themis)

# Set tidymodels preferences and display options
tidymodels_prefer()
options(tidymodels.dark = TRUE)

# Read training data from parquet files
# train_10 <-
#   read_parquet("data/data_for_modelling/chained_classifiers/10_sec_only_in_bed_training_data.parquet") %>%
#   mutate(across(contains("sleep"), as_factor))

train_30 <-
  read_parquet("data/data_for_modelling/chained_classifiers/30_sec_only_in_bed_training_data.parquet") %>%
  mutate(across(contains("sleep"), as_factor)) %>% 
  drop_na()


# Define a function to finalize and fit the best workflow from grid search results
finalize_and_fit_best_wf <- function(grid_results_fnames, training_data) {
  # Read grid results from the file
  cat(paste("Processing", str_extract(grid_results_fnames, "[^/]+$"), "grid results\n"))
  grid_results <-
    read_rds(grid_results_fnames)

  # Get workflow names and define output paths
  wf_names <-
    unique(grid_results$wflow_id)
  paths <-
    str_c(
      "/media/esbenlykke/My Passport/chained_models/fitted_workflows/sleep/",
      wf_names, "_", parse_number(deparse(substitute(training_data))),
      "_sec_epochs", "_fit.rds"
    )

  # Perform a nested loop operation using walk2
  walk2(wf_names, paths, \(.x, .y) {
    # Display the workflow name being processed
    cat(paste("Finalizing and fitting", .x, "model\n"))
    # Select the best workflow based on f_meas metric
    best_wf <- grid_results %>%
      extract_workflow_set_result(.x) %>%
      select_best(metric = "f_meas")

    # Finalize the workflow, fit it to the training data, and save the result
    grid_results %>%
      extract_workflow(.x) %>%
      finalize_workflow(best_wf) %>%
      fit(training_data) %>%
      butcher() %>%
      write_rds(.y)
  })
}

# List all grid result files in the specified directory
grid_results_fnames_30 <-
  list.files("/media/esbenlykke/My Passport/chained_models/grid_results/sleep", full.names = TRUE) %>%
  str_subset("30.rds")

# Apply the finalize_and_fit_best_wf function to each grid result file and the training data
walk(grid_results_fnames_30, ~ finalize_and_fit_best_wf(.x, train_30),
  .progress = TRUE
)

# TODO this can easily be parallelized

