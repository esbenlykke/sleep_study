#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)
library(torch)

# Define the helper function
prepare_data <- function(data_path, sequence_length, step_size) {
  # Load the data
  data <- read_parquet(data_path) %>%
    select(-c(id, datetime, unix_time, noon_day))
  
  # Define the preprocessing recipe and prep data
  prep_rec <- recipe(score ~ ., data = data) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    prep()
  
  data_prep <- prep_rec %>% juice()
  
  # Split into predictors and score
  data_predictors <- data_prep %>% 
    select(-score) %>% 
    as.matrix() %>% 
    torch::torch_tensor()
  
  data_score <- data_prep %>% 
    select(score) %>% 
    as.matrix() %>% 
    torch::torch_tensor()
  
  # Reshape the data
  reshaped_data <- create_sequences(data_predictors, data_score, sequence_length, step_size)
  
  # Extract the sequences and labels
  data_predictors <- reshaped_data$sequences
  data_labels <- reshaped_data$labels
  
  list(predictors = data_predictors, labels = data_labels)
}

# Define the function to create sequences
create_sequences <- function(data, target, sequence_length, step_size) {
  sequences <- list()
  labels <- list()
  
  for (i in seq(1, nrow(data) - sequence_length, by = step_size)) {
    sequences[[length(sequences) + 1]] <- data[i:(i + sequence_length - 1), ]
    labels[[length(labels) + 1]] <- target[i + sequence_length - 1, ]
  }
  
  list(sequences = torch::torch_stack(sequences), labels = torch::torch_stack(labels))
}

# Sequence length and step size
sequence_length <- 20  # Corresponding to 10 minutes in 30 sec epoch data
step_size <- 10  # Corresponding to 5 minutes in 30 sec epoch data

# Prepare the train and test data
train_data <- prepare_data("data/data_for_modelling/chained_classifiers/30_sec_training_data.parquet", sequence_length, step_size)
test_data <- prepare_data("data/data_for_modelling/chained_classifiers/30_sec_testing_data.parquet", sequence_length, step_size)

# Save the tensors
torch_save(train_data$predictors, "data/data_for_modelling/lstm/train_predictors.pt")
torch_save(train_data$labels, "data/data_for_modelling/lstm/train_labels.pt")
torch_save(test_data$predictors, "data/data_for_modelling/lstm/test_predictors.pt")
torch_save(test_data$labels, "data/data_for_modelling/lstm/test_labels.pt")
