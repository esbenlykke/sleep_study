#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)
library(tidymodels)
library(tensorflow)
library(keras)

data <-
  read_parquet("data/processed/model_data/bsl_thigh_sensor_independent_features.parquet") |>  mutate(
    in_bed = as_factor(in_bed),
    sleep = as_factor(sleep)
  )

spl <- group_initial_split(data, group = id)
train <- training(spl)
test <- testing(spl)

rec <- 
  recipe(in_bed + sleep ~id + datetime + age + incl + temp + macc_x + macc_y + macc_z +
           sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
         data = train) |> 
  update_role(id, new_role = "id") |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())


x <- as_tensor(1:6, dtype = "float32", shape = c(2, 3))

