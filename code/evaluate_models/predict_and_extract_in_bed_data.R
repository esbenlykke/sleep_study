#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)
library(lubridate)

# Function to process data and generate predictions
extract_in_bed_data <- function(model_filename, data_path, epoch_length, output_dir) {
  # Extract model name from filename
  model_name <- str_match(model_filename, ".*/(.*).rds$")[, 2]

  # Read fitted model
  fit <- read_rds(model_filename)

  # Read data, generate predictions, and write data with predictions before mutate step
  fit %>%
    augment(read_parquet(data_path)) %>%
    rename_with(~ str_replace(.x, ".pred", "in_bed")) %>%
    {
      write_parquet(., paste0("data/data_for_modelling/chained_classifiers/", model_name, "_predictions_complete.parquet")) # Write data with predictions before mutate step to parquet file
      cat("Data for model", model_name, "before mutate has been written to", paste0("data/data_for_modelling/chained_classifiers/", model_name, "_predictions_complete.parquet"), "\n\n")
      . # Return the data for further processing
    } %>%
    mutate(
      in_bed_class = as.numeric(in_bed_class) - 1,
      in_bed_slide_sum = slide_sum(in_bed_class, 
                                   before = (6 * 60) / epoch_length,
                                   after = (6 * 60) / epoch_length
      )
    ) %>%
    group_by(id, noon_day, month) %>%
    group_modify(~ .x %>%
      filter(row_number() > min(row_number()[in_bed_slide_sum >= 20]) &
        row_number() < max(row_number()[in_bed_slide_sum >= 20]))) %>%  # Filter rows within the in_bed_slide_sum range
  ungroup() %>%
    {
      write_parquet(., paste0(output_dir, model_name, ".parquet")) # Write the extracted data to a parquet file
      cat("Extracted data for model", model_name, "has been written to", paste0(output_dir, model_name, ".parquet"), "\n\n")
      . # Return the extracted data for further processing
    }
}

# Set common variables
models_dir <- "/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed"
model_filenames <- list.files(models_dir, full.names = TRUE) # Get the filenames of the model files
data_dir <- list.files("data/data_for_modelling", full.names = TRUE) %>%
  str_subset("no_edge") %>%
  str_subset("only", negate = TRUE) # Get the directory for the data files

# Output directory for extracted data
output_dir <- "data/data_for_modelling/chained_classifiers/extracted_in_bed_data/"
dir.create(output_dir, showWarnings = FALSE) # Create the output directory if it doesn't exist

# Process data and write extracted data to disk for epoch_length = 30
preds_30 <- map(model_filenames, ~ extract_in_bed_data(.x,
  data_path = data_dir[2], # Use the second data directory for processing
  epoch_length = 30,
  output_dir = output_dir
))

# Process data and write extracted data to disk for epoch_length = 10
preds_10 <- map(model_filenames, ~ extract_in_bed_data(.x,
  data_path = data_dir[1], # Use the first data directory for processing
  epoch_length = 10,
  output_dir = output_dir
))
