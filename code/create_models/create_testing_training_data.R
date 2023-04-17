library(tidyverse)
library(tidymodels)
library(arrow)


set.seed(132)
data <- 
  read_parquet("data/data_for_modelling/all_data_incl_sensor_independent_features.parquet")

spl <- 
  group_initial_split(data, group = id, prop = .5)

training(spl) %>% 
  write_parquet("data/data_for_modelling/crude_training_data.parquet")

testing(spl) %>% 
  write_parquet("data/data_for_modelling/crude_testing_data.parquet")

# 30 sec data
data30 <- 
  read_parquet("data/data_for_modelling/all_data_incl_sensor_independent_features_30_sec_epochs.parquet")

spl30 <- 
  group_initial_split(data30, group = id, prop = .5)

training(spl30) %>% 
  write_parquet("data/data_for_modelling/crude_training_30_sec_data.parquet")

testing(spl30) %>% 
  write_parquet("data/data_for_modelling/crude_testing_30_sec_data.parquet")

