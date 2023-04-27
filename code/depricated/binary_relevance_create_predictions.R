#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)

test <-
  read_parquet("data/processed/binary_relevance_testing_data.parquet")

# Load models -------------------------------------------------------------

# remember to mount disk

in_bed_asleep_fit_filenames <-
  list.files("/media/esbenlykke/My Passport/fitted_models/axed_models/", full.names = TRUE) |>
  str_subset("in_bed_asleep")

in_bed_awake_fit_filenames <-
  list.files("/media/esbenlykke/My Passport/fitted_models/axed_models/", full.names = TRUE) |>
  str_subset("in_bed_awake")

out_bed_awake_fit_filenames <-
  list.files("/media/esbenlykke/My Passport/fitted_models/axed_models/", full.names = TRUE) |>
  str_subset("out_bed_awake")


in_bed_asleep_fits <-
  in_bed_asleep_fit_filenames |>
  map(read_rds)

in_bed_awake_fits <-
  in_bed_awake_fit_filenames |>
  map(read_rds)

out_bed_awake_fits <-
  out_bed_awake_fit_filenames |>
  map(read_rds)


create_preds <- function(in_bed_asleep_fits, in_bed_awake_fits, out_bed_awake_fits) {
  in_bed_asleep_fits |>
    augment(test) |>
    rename_with(.cols = contains("pred"), ~ paste0("in_bed_asleep_", .x)) |>
    bind_cols(
      in_bed_awake_fits |>
        predict(test, type = "prob") |>
        rename_with(.cols = contains("pred"), ~ paste0("in_bed_awake_", .x))
    ) |>
    bind_cols(out_bed_awake_fits |>
      predict(test, type = "prob") |>
      rename_with(.cols = contains("pred"), ~ paste0("out_bed_awake_", .x))) |>
    mutate(
      in_bed_awake_.pred_class = as_factor(if_else(in_bed_awake_.pred_1 >= .5, 1, 0)),
      out_bed_awake_.pred_class = as_factor(if_else(out_bed_awake_.pred_1 >= .5, 1, 0)),
      .after = in_bed_asleep_.pred_class
    )
}

dfs <- 
  pmap(list(in_bed_asleep_fits, in_bed_awake_fits, out_bed_awake_fits), create_preds)


names(dfs) <- c("decision_tree", "logistic_regression", "neural_net", "xgboost") %>%
  str_c("/media/esbenlykke/My Passport/binary_relevance_preds/preds", ., ".parquet")

walk2(dfs, names(dfs), ~ write_parquet(.x, .y))
