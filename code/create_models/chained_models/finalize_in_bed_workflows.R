#!/usr/bin/env Rscript

# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(arrow)
library(butcher)

# Set tidymodels to preferred and set theme to dark
tidymodels_prefer()
options(tidymodels.dark = TRUE)

# Define the output path
output_path <- "/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed/"

# Load training data
train30 <- read_parquet("data/data_for_modelling/chained_classifiers/30_sec_training_data.parquet") %>% 
  mutate(
    across(contains("in_bed"), as_factor)
  )

cat("Reading in-bed workflowsets\n\n")

# Load grid results
in_bed_30_sec_grid_res <- read_rds("/media/esbenlykke/My Passport/chained_models/grid_results/in_bed/in_bed_30_sec_epoch_grid_results.rds")

glue::glue("{str_c(unique(in_bed_30_sec_grid_res$wflow_id), collapse = ' + ')} 
           \nWill be fitted! Please be patient...\n\n")

# Function to process models
process_model <- function(model_name){
  cat(paste("Finalizing and fitting", model_name, "model\n"))
  
  # Extract the best result
  best_result <- in_bed_30_sec_grid_res %>%
    extract_workflow_set_result(model_name) |>
    select_best(metric = "f_meas")
  
  # Finalize the workflow, fit it, and save the result
  in_bed_30_sec_grid_res |>
    extract_workflow(model_name) |>
    finalize_workflow(best_result) %>%
    fit(train30) %>%
    butcher() %>% 
    write_rds(str_c(output_path, model_name, "_30_sec_epoch_fit.rds"))
}

# Get unique model names
model_names <- unique(in_bed_30_sec_grid_res$wflow_id)

# Process each model
map(model_names, process_model, .progress = TRUE)

cat("All done\n")

