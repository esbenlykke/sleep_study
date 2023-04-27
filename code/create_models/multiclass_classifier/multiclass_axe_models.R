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
      paste0("decision_tree_SMOTE_", type, "_AXED.rds"),
      paste0("xgboost_", type, "_AXED.rds")
    ))
}


fit_filenames <-
  list.files("/media/esbenlykke/My Passport/multiclass/fitted_models", full.names = TRUE) %>% 
  str_subset("axed", negate = TRUE)




fit_filenames |>
  map(read_rds) %>%
  map(axe_a_lot) %>%
  name_axed_models(type = "multiclass") %>%
  walk2(
    ., names(.),
    ~ write_rds(.x, paste0("/media/esbenlykke/My Passport/multiclass/fitted_models/axed_models/", .y))
  )

