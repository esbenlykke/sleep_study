#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)
library(lubridate)

# Function to process data
extract_in_bed_data <- function(model_filenames, data_path, epoch_length, output_dir) {
  # Read data
  data <- read_parquet(data_path)

  # Extract model names from filenames
  model_names <- str_match(model_filenames, ".*/(.*).rds$")

  # Read fitted models
  fits <- map(model_filenames, read_rds)

  # Generate predictions
  preds <- map(fits, ~ .x %>%
    augment(data) %>%
    rename_with(~ str_replace(.x, ".pred", "in_bed")) %>%
    mutate(
      in_bed_filtered = slide_dbl(as.numeric(in_bed_class) - 1, median,
        .after = (7.5 * 60) / epoch_length, .before = (7.5 * 60) / epoch_length
      )
    ) %>%
    group_by(id, noon_day, month) %>%
    group_modify(~ .x %>%
      # Filter rows where row_number is within the in_bed_filtered range
      filter(row_number() > min(row_number()[in_bed_filtered == 1]) &
        row_number() < max(row_number()[in_bed_filtered == 1]))) %>%
    ungroup()) %>%
    set_names(model_names[, 2])

  # Write predictions to disk
  walk2(preds, names(preds), \(pred, name) {
    output_file <- paste0(output_dir, name, ".parquet")
    write_parquet(pred, output_file)
    cat("Extracted data for model", name, "has been written to", output_file, "\n\n")
  })


  return(preds)
}

# Set common variables
models_dir <- "/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed"
model_filenames <- list.files(models_dir, full.names = TRUE)
data_dir <- list.files("data/data_for_modelling", full.names = TRUE) %>%
  str_subset("no_edge") %>%
  str_subset("only", negate = TRUE)

# Output directory for extracted data
output_dir <- "data/data_for_modelling/chained_classifiers/extracted_in_bed_data/"
dir.create(output_dir, showWarnings = FALSE)

# Process data and write extracted data to disk
preds_30 <- map(model_filenames, ~ extract_in_bed_data(.x,
  data_path = data_dir[2],
  epoch_length = 30,
  output_dir = output_dir
))

preds_10 <- map(model_filenames, ~ extract_in_bed_data(.x,
  data_path = data_dir[1],
  epoch_length = 10,
  output_dir = output_dir
))
