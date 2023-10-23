#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)

# Set seed for reproducibility
set.seed(123)

# Define a function to read data, split it and write the split data to parquet files
process_data <- function(file_path, output_prefix) {
  cat(glue::glue("Processing {str_match(file_path, '(?<=/)[^/]+$')}"), "\n")
  
  # Read data from a parquet file
  data <- read_parquet(file_path)
  
  # Perform an initial 50/50 split of the data by the 'id' column
  split <- group_initial_split(data, group = id, prop = .5)
  
  # Write the training set to a parquet file
  training(split) %>%
    write_parquet(paste0("data/data_for_modelling/chained_classifiers/", output_prefix, "training_data.parquet"))
  
  # Write the testing set to a parquet file
  testing(split) %>%
    write_parquet(paste0("data/data_for_modelling/chained_classifiers/", output_prefix, "testing_data.parquet"))
}

clean_data_path <- "data/processed/all_clean_data.parquet"
clean_in_bed_data_path <- "data/processed/in_bed_clean_data.parquet"

# Apply the function to each dataset
# process_data("data/data_for_modelling/no_edge_sp_incl_features_10_sec_epochs.parquet", "10_sec_")
process_data(clean_data_path, "30_sec_")
# process_data("data/data_for_modelling/only_in_bed_data_no_edge_sp_incl_features_10_sec_epochs.parquet", "10_sec_only_in_bed_")
process_data(clean_in_bed_data_path, "30_sec_only_in_bed_")

beepr::beep(4)
