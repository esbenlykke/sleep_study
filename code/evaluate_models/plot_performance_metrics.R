#!/usr/bin/env RscriptsuppressMessages(library(tidyverse))

library(tidyverse)
library(tidymodels)
library(arrow)


in_bed_CART_fit <-  read_rds("data/models/fitted_models/in_bed_simple_CART_fit.rds")
sleep_CART_fit <-  read_rds("data/models/fitted_models/sleep_simple_CART_fit.rds")


test <- read_parquet("data/processed/screens_bsl_test_data.parquet")

# Metrics and plots -------------------------------------------------------


my_metrics <-
  metric_set(f_meas, accuracy, sensitivity, specificity)

in_bed_CART_fit |>
  augment(test) |>
  my_metrics(truth = in_bed, estimate = .pred_class) |> 
  ggplot(aes(.metric, .estimate)) +
  geom_col()