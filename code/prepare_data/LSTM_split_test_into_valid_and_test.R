#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(torch)

# Step 1: Read the data from parquet files and combine them into a single data frame
data <- 
  read_parquet("data/data_for_modelling/no_edge_sp_incl_features_30_sec_epochs.parquet") %>% 
  arrange(id)

# Step 2: Set the seed for reproducibility
set.seed(123)

# Step 3: Split the data into train and test sets using group initial split
spl <- group_initial_split(data, group = id, prop = 0.5)

# Step 4: Get the training data
all_train <- training(spl)

# Step 5: Write the testing data to a parquet file
testing(spl) %>% write_parquet("data/data_for_modelling/lstm/30_sec_testing.parquet")

# Step 6: Split the training data into train and validation sets using group initial split
spl_valid <- group_initial_split(all_train, group = id, prop = 0.4)

# Step 7: Write the training and validation data to parquet files
training(spl_valid) %>% write_parquet("data/data_for_modelling/lstm/30_sec_training.parquet")
testing(spl_valid) %>% write_parquet("data/data_for_modelling/lstm/30_sec_validation.parquet")


