#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(torch)
library(arrow)
library(luz)


data <-
  read_parquet("data/data_for_modelling/no_edge_sp_incl_sensor_independent_features_10_sec_epochs.parquet") %>%
  select(-c(noon_day:unix_time, placement, sensor_code, clock_group, contains(c("in_bed", "sleep")), x:z)) %>%
  mutate(
    target = case_when(
      score == -5 ~ 2,
      # NAs in the score feature corresponds to "out-of-bed", I'll call it "1"
      is.na(score) ~ 1,
      TRUE ~ score
    ),
    hour = hour(datetime),
    minute = minute(datetime),
    # TODO is it only weekday that should be treated as a catagorical feature?
    across(c(weekday), as_factor),
    # across(everything(), ~ replace_na(.x, mean(.x, na.rm = TRUE))), I'll do this in the recipe instead
    .after = 1
  ) %>%
  select(-c(datetime, score))


prepro_rec <-
  recipe(target ~ ., data = data) %>%
  update_role(id, new_role = "id") %>%
  # Apply normalization to the accelerometer columns
  step_normalize(all_numeric_predictors(), -all_outcomes()) %>%
  # One-hot encode catagorical predictors
  step_dummy(all_nominal_predictors()) %>%
  # impute missing values with the mean
  step_impute_mean(all_numeric_predictors()) %>%
  # Prepare the recipe
  prep(data = data)

data_preprocessed <-
  juice(prepro_rec)

# Free up memory
rm(data)

set.seed(123)
spl <-
  group_initial_split(data_preprocessed, group = id, prop = .5)

train <- training(spl) %>%
  slice(1:6000) # slicing here for testing purposes
test <- testing(spl) %>%
  slice(1:6000) # slicing here for testing purposes

# Convert data to tensors
train_data_tensor <- train %>%
  select(-c(id, target)) %>%
  as.matrix() %>%
  torch_tensor()

# Reshape to correct dimensions
train_data_tensor <- train_data_tensor$view(c(-1, 600, 62))

train_labels_tensor <- torch_tensor(train$target)
train_labels_tensor <- train_labels_tensor$view(c(-1, 600, 1))
train_labels_tensor <- train_labels_tensor$to(dtype = torch_long())

# Convert test data to tensors
test_data_tensor <- test %>%
  select(-c(id, target)) %>%
  as.matrix() %>%
  torch_tensor()

test_data_tensor <- test_data_tensor$view(c(-1, 600, 62))

test_labels_tensor <- torch_tensor(train$target)
test_labels_tensor <- train_labels_tensor$view(c(-1, 600, 1))
test_labels_tensor <- train_labels_tensor$to(dtype = torch_long())

model <- nn_module(
  "LSTM-model",
  initialize = function(input_size, hidden_size, num_layers, num_classes) {
    self$lstm <- nn_lstm(input_size, hidden_size, num_layers, batch_first = TRUE)
    self$fc <- nn_linear(hidden_size, num_classes)
  },
  forward = function(x) {
    # Forward propagate the LSTM
    out <- self$lstm(x)$output
    # Decode the hidden state of the last time step
    out <- self$fc(out[, nrow(out), , drop = FALSE])
    out
  }
)


input_size <- 62
hidden_size <- 100
num_layers <- 1
num_classes <- 5

model <- model$new(input_size, hidden_size, num_layers, num_classes)

criterion <- nn_cross_entropy_loss()
optimizer <- optim_adam(model$parameters)

num_epochs <- 100 # Number of epochs to train for

num_epochs <- 100 # Number of epochs to train for

for (epoch in 1:num_epochs) {
  # Forward pass
  outputs <- model$forward(train_data_tensor)
  loss <- criterion(outputs, train_labels_tensor)

  # Backward and optimize
  optimizer$zero_grad()
  loss$backward()
  optimizer$step()

  if (epoch %% 10 == 0) {
    cat(glue::glue("Epoch [{epoch}/{num_epochs}], Loss: {loss$item():.4f}\n"))
  }
}
