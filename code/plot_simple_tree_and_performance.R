#!/usr/bin/env RscriptsuppressMessages(library(tidyverse))

library(tidyverse)
library(tidymodels)
library(arrow)

in_bed_CART_fit <-  read_rds("data/models/fitted_models/in_bed_simple_CART_fit")
sleep_CART_fit <-  read_rds("data/models/fitted_models/sleep_simple_CART_fit")


test <- read_parquet("data/processed/screens_test_data.parquet")

# Metrics and plots -------------------------------------------------------


my_metrics <-
  metric_set(f_meas, accuracy, sensitivity, specificity)

in_bed_CART_fit |>
  augment(test) |>
  my_metrics(truth = in_bed, estimate = .pred_class)

rpart.plot::rpart.plot(in_bed_CART_fit |> extract_fit_engine(), roundint = FALSE)

sleep_CART_fit |>
  augment(test) |>
  my_metrics(truth = sleep, estimate = .pred_class)

rpart.plot::rpart.plot(sleep_CART_fit |> extract_fit_engine(), roundint = FALSE)