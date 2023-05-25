#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(torch)

data <- read_parquet("data/data_for_modelling/chained_classifiers/30_sec_testing_data.parquet")

spl <- group_initial_split(data, group = id, prop = .5)

read_parquet("data/data_for_modelling/chained_classifiers/30_sec_training_data.parquet") %>% 
    write_parquet("data/data_for_modelling/lstm/30_sec_training.parquet")
training(spl) %>% write_parquet("data/data_for_modelling/lstm/30_sec_validation.parquet")
testing(spl) %>% write_parquet("data/data_for_modelling/lstm/30_sec_testing.parquet")
