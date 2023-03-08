#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)
library(furrr)
library(butcher)

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

# in_bed_fits <-
in_bed_asleep_fit_filenames <-
  list.files("/media/esbenlykke/My Passport/fitted_models", full.names = TRUE) |>
  str_subset("in_bed_asleep")

in_bed_awake_fit_filenames <-
  list.files("/media/esbenlykke/My Passport/fitted_models", full.names = TRUE) |>
  str_subset("in_bed_awake")

out_bed_awake_fit_filenames <-
  list.files("/media/esbenlykke/My Passport/fitted_models", full.names = TRUE) |>
  str_subset("out_bed_awake")

# plan(multicore, workers = 4)

in_bed_asleep_fit_filenames |>
  map(read_rds) %>%
  map(axe_a_lot) %>%
  name_axed_models(type = "in_bed_asleep") %>%
  walk2(., names(.),
    ~ write_rds(.x, paste0("/media/esbenlykke/My Passport/fitted_models/axed_models/", .y))
  )

in_bed_awake_fit_filenames |>
  map(read_rds) %>%
  map(axe_a_lot) %>%
  name_axed_models(type = "in_bed_awake") %>%
  walk2(., names(.),
        ~ write_rds(.x, paste0("/media/esbenlykke/My Passport/fitted_models/axed_models/", .y))
  )

out_bed_awake_fit_filenames |>
  map(read_rds) %>%
  map(axe_a_lot) %>%
  name_axed_models(type = "out_bed_awake") %>%
  walk2(., names(.),
        ~ write_rds(.x, paste0("/media/esbenlykke/My Passport/fitted_models/axed_models/", .y))
  )
