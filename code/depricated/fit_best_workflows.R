#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))

train <-
  read_parquet("data/processed/binary_relevance_training_data.parquet")

# In-bed ------------------------------------------------------------------


in_bed_list <- list.files("data/models/finalized_workflows", full.names = TRUE) |> 
  str_subset("MARS", negate = TRUE)

wfs <-
  in_bed_list |> 
  map(read_rds)

wfs[[1]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/in_bed_logistic_fit.rds")

wfs[[2]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/in_bed_neural_net_fit.rds")

wfs[[3]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/in_bed_xg_boost_fit.rds")



# Sleep -------------------------------------------------------------------


wfs[[4]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/sleep_logistic_fit.rds")

wfs[[5]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/sleep_neural_net_fit.rds")

wfs[[6]] |> 
  fit(train) |> 
  write_rds("data/models/fitted_models/sleep_xg_boost_fit.rds")
