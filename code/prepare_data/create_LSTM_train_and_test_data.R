#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)
library(torch)

# Define the helper function
prepare_data <- function(data_path, sequence_length, step_size) {
  # Load the data
  data <- read_parquet(data_path) %>%
    select(c(age, weekday, incl, theta, x_mean, y_mean, z_mean,
             x_sd, y_sd, z_sd, x_sd_long, y_sd_long, z_sd_long, sd_max,
             temp_mean, temp_sd, clock_proxy_cos, clock_proxy_linear,
             temp_mean_lag_1min, temp_mean_lag_5min, temp_mean_lag_30min,
             temp_mean_lead_1min, temp_mean_lead_5min, temp_mean_lead_30min,
             theta_lag_1min, theta_lag_5min, theta_lag_30min,
             theta_lead_1min, theta_lead_5min, theta_lead_30min,
             incl_lag_1min, incl_lag_5min, incl_lag_30min,
             incl_lead_1min, incl_lead_5min, incl_lead_30min,
             x_sd_lag_1min, x_sd_lag_5min, x_sd_lag_30min,
             y_sd_lag_1min, y_sd_lag_5min, y_sd_lag_30min,
             z_sd_lag_1min, z_sd_lag_5min, z_sd_lag_30min,
             x_sd_lead_1min, x_sd_lead_5min, x_sd_lead_30min,
             y_sd_lead_1min, y_sd_lead_5min, y_sd_lead_30min,
             z_sd_lead_1min, z_sd_lead_5min, z_sd_lead_30min, score))
  
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
torch_save(train_data$predictors, "data/data_for_modelling/lstm/r_train_predictors.pt")
torch_save(train_data$labels, "data/data_for_modelling/lstm/r_train_labels.pt")
torch_save(test_data$predictors, "data/data_for_modelling/lstm/r_test_predictors.pt")
torch_save(test_data$labels, "data/data_for_modelling/lstm/r_test_labels.pt")

