#!/usr/bin/env RscriptsuppressMessages(library(tidyverse))

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))

in_bed_CART_fit <-  read_rds("data/models/fitted_models/in_bed_simple_CART_fit")
sleep_CART_fit <-  read_rds("data/models/fitted_models/sleep_simple_CART_fit")

# Spend data budget -------------------------------------------------------
cat("Spending data budget...\n")

set.seed(123)
data <-
  read_parquet("data/processed/data_for_modelling/bsl_thigh_sensor_independent_features.parquet") |>
  mutate(
    in_bed = as_factor(in_bed),
    sleep = as_factor(sleep)
  )

set.seed(123)
spl <-
  data |>
  group_initial_split(group = id, prop = .5)

train <- training(spl)
test <- testing(spl)

# Metrics and plots -------------------------------------------------------


my_metrics <-
  metric_set(f_meas, accuracy, sensitivity, specificity)

in_bed_CART_fit |>
  augment(test) |>
  my_metrics(truth = in_bed, estimate = .pred_class)

rpart.plot::rpart.plot(in_bed_CART_fit |> extract_fit_engine(), roundint = FALSE, 
                       type = 0, extra = 2)

sleep_CART_fit |>
  augment(test) |>
  my_metrics(truth = sleep, estimate = .pred_class)

rpart.plot::rpart.plot(sleep_CART_fit |> extract_fit_engine(), roundint = FALSE, 
                       type = 0, extra = 2)

