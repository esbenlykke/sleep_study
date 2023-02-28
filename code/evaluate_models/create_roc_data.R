#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)

in_bed_fits_files <- list.files("data/models/fitted_models", full.names = TRUE) |>
  str_subset("mars", negate = TRUE) |>
  str_subset("in_bed")

sleep_fits_files <- list.files("data/models/fitted_models", full.names = TRUE) |>
  str_subset("mars", negate = TRUE) |>
  str_subset("sleep")

in_bed_fits <- map(in_bed_fits_files, read_rds) |>
  set_names(c("logistic_regression", "neural_net", "decision_tree", "xgboost"))

sleep_fits <- map(sleep_fits_files, read_rds) |>
  set_names(c("logistic_regression", "neural_net", "decision_tree", "xgboost"))

test <- read_parquet("data/processed/testing_data.parquet")



# Get ROC -----------------------------------------------------------------


get_roc <-
  function(fit, truth, estimate) {
    fit |>
      augment(test) |>
      roc_curve(truth = {{ truth }}, estimate = {{ estimate }}, event_level = "second")
  }

map_dfr(in_bed_fits, ~ get_roc(.x, in_bed, .pred_1), .id = "model") |>
  write_parquet("data/processed/roc_data_in_bed.parquet")

map_dfr(sleep_fits, ~ get_roc(.x, in_bed, .pred_1), .id = "model") |>
  write_parquet("data/processed/roc_data_sleep.parquet")



# multiclass ROC ----------------------------------------------------------


# get_combined_roc <- function(fit_sleep, fit_inbed, truth, estimate) {
#   fit_sleep |>
#     predict(test, type = "prob") |>
#     bind_cols(
#       fit_inbed |>
#         predict(test, type = "prob")
#     ) |>
#     rename(in_bed_0 = 1, in_bed_1 = 2, sleep_0 = 3, sleep_1 = 4) |>
#     bind_cols(test |> select(in_bed, sleep)) |>
#     transmute(
#       in_bed_sleep_pred = ((sleep_1 * in_bed_1) - min(sleep_1 * in_bed_1)) / (max(sleep_1 * in_bed_1) - min(sleep_1 * in_bed_1)),
#       in_bed_awake_pred = ((sleep_0 * in_bed_1) - min(sleep_0 * in_bed_1)) / (max(sleep_0 * in_bed_1) - min(sleep_0 * in_bed_1)),
#       in_bed_sleep_zm = as_factor(if_else(in_bed == 1 & sleep == 1, 1, 0)),
#       in_bed_awake_zm = as_factor(if_else(in_bed == 1 & sleep == 0, 1, 0))
#     ) |>
#     roc_curve(truth = {{ truth }}, estimate = {{ estimate }}, event_level = "second")
# }
#
# map2_dfr(sleep_fits, in_bed_fits,
#   ~ get_combined_roc(.x, .y, in_bed_sleep_zm, in_bed_sleep_pred),
#   .id = "model"
# ) |>
#   write_parquet("data/processed/roc_data_in_bed_asleep.parquet")
#
# map2_dfr(sleep_fits, in_bed_fits,
#   ~ get_combined_roc(.x, .y, in_bed_awake_zm, in_bed_awake_pred),
#   .id = "model"
# ) |>
#   write_parquet("data/processed/roc_data_in_bed_awake.parquet")
#
# beepr::beep()


# TESTING -----------------------------------------------------------------

# Weighted Micro-Averaging = (Σ(w_i * TP_i)) / (Σ(w_i * TP_i + (1 - w_i) * FN_i))
#
# where:
#
# n is the number of classes
# TP_i is the number of true positives for class i
# FN_i is the number of false negatives for class i
# w_i is the weight for class i, which is typically the proportion of examples in the dataset
# that belong to that class (i.e., w_i = n_i / N, where n_i is the number of examples in the
# dataset that belong to class i and N is the total number of examples in the dataset).
#
# The formula computes the average true positive rate (TPR) across all classes, with each class
# weighted by its proportion in the dataset. The weight for each class is used to adjust the
# contribution of that class's TPR to the final micro-average, with more weight given to
# classes that are more common in the dataset.

test_outcomes <-
  test |>
  transmute(
    in_bed_asleep = if_else(sleep == 1 & in_bed == 1, 1, 0),
    in_bed_awake = if_else(sleep == 0 & in_bed == 1, 1, 0),
    out_bed_sleep = if_else(sleep == 1 & in_bed == 0, 1, 0),
    out_bed_awake = if_else(sleep == 0 & in_bed == 0, 1, 0),
    combined_outcomes = case_when(
      in_bed_asleep == 1 ~ "in_bed_asleep",
      in_bed_awake == 1 ~ "in_bed_awake",
      out_bed_sleep == 1 ~ "out_bed_sleep",
      out_bed_awake == 1 ~ "out_bed_awake"
    ) |> as_factor()
  )

weights <-
  test_outcomes |>
  count(combined_outcomes) |>
  summarise(
    weight = n / nrow(test_outcomes),
    combined_outcomes
  )

preds <-
  sleep_fits$decision_tree |>
  predict(test) |>
  bind_cols(
    in_bed_fits$decision_tree |>
      predict(test)
  ) |>
  rename(pred_sleep = 1, pred_in_bed = 2) |>
  bind_cols(test_outcomes |> select(combined_outcomes)) |>
  mutate(
    preds_combined_outcome = case_when(
      pred_sleep == 1 & pred_in_bed == 1 ~ "in_bed_asleep",
      pred_sleep == 0 & pred_in_bed == 1 ~ "in_bed_awake",
      TRUE ~ "out_bed_awake"
    ) |> as_factor()
  )

confmat <- preds |>
  conf_mat(truth = combined_outcomes, estimate = preds_combined_outcome)

