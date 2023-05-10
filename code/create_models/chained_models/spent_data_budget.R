#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)

# Read 10-second epochs data from a parquet file
data_10 <- read_parquet("data/data_for_modelling/no_edge_sp_incl_sensor_independent_features_10_sec_epochs.parquet")

# Read 30-second epochs data from a parquet file
data_30 <- read_parquet("data/data_for_modelling/no_edge_sp_incl_sensor_independent_features_30_sec_epochs.parquet")

# Set seed for reproducibility
set.seed(123)

# Perform an initial 50/50 split of the 10-second epochs data by the 'id' column
spl_10 <- group_initial_split(data_10, group = id, prop = .5)

# Write the training set of the 10-second epochs data to a parquet file
training(spl_10) %>% 
  write_parquet("data/data_for_modelling/chained_classifiers/training_10_sec_data.parquet")

# Write the testing set of the 10-second epochs data to a parquet file
testing(spl_10) %>% 
  write_parquet("data/data_for_modelling/chained_classifiers/testing_10_sec_data.parquet")

# Perform an initial 50/50 split of the 30-second epochs data by the 'id' column
spl_30 <- group_initial_split(data_30, group = id, prop = .5)

# Write the training set of the 30-second epochs data to a parquet file
training(spl_30) %>% 
  write_parquet("data/data_for_modelling/chained_classifiers/training_30_sec_data.parquet")

# Write the testing set of the 30-second epochs data to a parquet file
testing(spl_30) %>% 
  write_parquet("data/data_for_modelling/chained_classifiers/testing_30_sec_data.parquet")

beepr::beep(4)