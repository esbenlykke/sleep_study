#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))

train <-
  read_parquet("data/processed/screens_bsl_train_data.parquet")

# In-bed ------------------------------------------------------------------


in_bed_list <- list.files("data/models/finalized_workflows", full.names = TRUE)

in_bed_wfs <-
  in_bed_list |> 
  map(read_rds)

in_bed_wfs[[1]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/in_bed_logistic_fit.rds")

in_bed_wfs[[2]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/in_bed_logistic_fit.rds")

in_bed_wfs[[3]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/in_bed_logistic_fit.rds")

in_bed_wfs[[4]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/in_bed_logistic_fit.rds")



# Sleep -------------------------------------------------------------------


sleep_list <- list.files("data/models/finalized_workflows", full.names = TRUE)

sleep_wfs <-
  sleep_list |> 
  map(read_rds)

sleep_wfs[[1]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/in_bed_logistic_fit.rds")

sleep_wfs[[2]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/in_bed_logistic_fit.rds")

sleep_wfs[[3]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/in_bed_logistic_fit.rds")

sleep_wfs[[4]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/in_bed_logistic_fit.rds")
