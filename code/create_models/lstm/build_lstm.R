#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)
library(torch)
library(luz)

# TODO remember to build tensors in torch R format... 
train_predictors <- torch_load("data/data_for_modelling/lstm/train_predictors.pt")
train_labels <- torch_load("data/data_for_modelling/lstm/train_labels.pt")
test_predictors <- torch_load("data/data_for_modelling/lstm/test_predictors.pt")
test_labels <- torch_load("data/data_for_modelling/lstm/test_labels.pt")

batch_size <- 64  # Choose a batch size that fits your memory and suits your model.

# Create dataloaders
train_dataloader <- dataloader(train_predictors, train_labels, batch_size = batch_size, shuffle = TRUE)
test_dataloader <- dataloader(test_predictors, test_labels, batch_size = batch_size, shuffle = FALSE)

batch <- dataloader_make_iter(train_dataloader) %>% dataloader_next()

net <- nn_module(
  "biLSTM",
  initialize = function(input_dim, hidden_dim, output_dim, num_layers) {
    self$lstm <- nn_lstm(input_dim, hidden_dim, num_layers, bidirectional = TRUE) 
    self$fc <- nn_linear(hidden_dim * 2, output_dim)
  },
  forward = function(self, x) {
    # Feedforward behavior
    output <- self$lstm(x)$output
    output <- self$fc(output)
    output
  }
)

input_dim <- 68  # number of predictors
hidden_dim <- 128  # desired number of hidden units
output_dim <- 5  # number of classes of target variable
num_layers <- 2  # desired number of layers

model <- net(input_dim = input_dim, 
             hidden_dim = hidden_dim, 
             output_dim = output_dim, 
             num_layers = num_layers)


# Create a sample input tensor
sample_input <- torch_randn(c(100, 20, 67))  # Shape: (batch_size=10, sequence_length=20, input_dim=67)

model$forward(sample_input)