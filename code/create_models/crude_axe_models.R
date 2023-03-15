#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)
library(furrr)
library(butcher)
library(tidymodels)


axe_a_lot <- function(model_list) {
  model_list |>
    axe_env() |>
    axe_data() |>
    axe_fitted() %>%
    axe_call()
}

name_axed_models <- function(model_list, type) {
  model_list |>
    setNames(c(
      paste0("logistic_regression_", type, "_AXED.rds"),
      paste0("neural_network_", type, "_AXED.rds"),
      paste0("decision_tree_", type, "_AXED.rds"),
      paste0("xgboost_", type, "_AXED.rds")
    ))
}


in_bed_fit_filenames <-
  list.files("/media/esbenlykke/My Passport/crude/fitted_models", full.names = TRUE) |>
  str_subset("in_bed")

sleep_fit_filenames <-
  list.files("/media/esbenlykke/My Passport/crude/fitted_models", full.names = TRUE) |>
  str_subset("sleep")


# plan(multicore, workers = 4)

in_bed_fit_filenames |>
  map(read_rds) %>%
  map(axe_a_lot) %>%
  name_axed_models(type = "in_bed") %>%
  walk2(
    ., names(.),
    ~ write_rds(.x, paste0("/media/esbenlykke/My Passport/crude/fitted_models/axed_models/", .y))
  )

sleep_fit_filenames |>
  map(read_rds) %>%
  map(axe_a_lot) %>%
  name_axed_models(type = "sleep") %>%
  walk2(
    ., names(.),
    ~ write_rds(.x, paste0("/media/esbenlykke/My Passport/crude/fitted_models/axed_models/", .y))
  )

