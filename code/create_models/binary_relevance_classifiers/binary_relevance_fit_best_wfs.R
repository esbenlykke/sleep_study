#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))

cat("Read training data\n")

train <-
  read_parquet("data/processed/binary_relevance_training_data.parquet")

### In bed asleep

cat("Fit in bed asleep wfs\n")

in_bed_asleep_list <- list.files("/media/esbenlykke/My Passport/best_worflows/", full.names = TRUE) |> 
  str_subset("in_bed_asleep")

wfs <-
  in_bed_asleep_list |> 
  map(read_rds)

cat("Fitting logistic regression in bed asleep\n")

wfs[[1]] |> 
  fit(train) |> 
  write_rds("/media/esbenlykke/My Passport/fitted_models/logistic_regression_in_bed_asleep_fit.rds")

cat("Fitting neural net in bed asleep\n")

wfs[[2]] |> 
  fit(train) |> 
  write_rds("/media/esbenlykke/My Passport/fitted_models/neural_net_in_bed_asleep_fit.rds")

cat("Fitting xgbost in bed asleep\n")

wfs[[3]] |>
  fit(train) |> 
  write_rds("/media/esbenlykke/My Passport/fitted_models/xg_boost_in_bed_asleep_fit.rds")


### In bed awake

cat("Fit in bed awake wfs\n")

in_bed_awake_list <- list.files("/media/esbenlykke/My Passport/best_worflows/", full.names = TRUE) |> 
  str_subset("in_bed_awake")

wfs <-
  in_bed_awake_list |> 
  map(read_rds)

cat("Fitting logistic regression in bed awake\n")

wfs[[1]] |> 
  fit(train) |> 
  write_rds("/media/esbenlykke/My Passport/fitted_models/logistic_regression_in_bed_awake_fit.rds")

cat("Fitting neural net in bed awake\n")

wfs[[2]] |> 
  fit(train) |> 
  write_rds("/media/esbenlykke/My Passport/fitted_models/neural_net_in_bed_awake_fit.rds")

cat("Fitting xgboost in bed awake\n")

wfs[[3]] |> 
  fit(train) |> 
  write_rds("/media/esbenlykke/My Passport/fitted_models/xg_boost_in_bed_awake_fit.rds")


### out bed awake

cat("Fit out bed asleep wfs\n")

out_bed_awake_list <- list.files("/media/esbenlykke/My Passport/best_worflows/", full.names = TRUE) |> 
  str_subset("out_bed_awake")

wfs <-
  out_bed_awake_list |> 
  map(read_rds)

cat("Fitting logistic regression out bed awake\n")

wfs[[1]] |> 
  fit(train) |> 
  write_rds("/media/esbenlykke/My Passport/fitted_models/logistic_regression_out_bed_awake_fit.rds")

cat("Fitting neural net out bed awake\n")

wfs[[2]] |> 
  fit(train) |> 
  write_rds("/media/esbenlykke/My Passport/fitted_models/neural_net_out_bed_awake_fit.rds")

cat("Fitting xgboost out bed awake\n")

wfs[[3]] |> 
  fit(train) |> 
  write_rds("/media/esbenlykke/My Passport/fitted_models/xg_boost_out_bed_awake_fit.rds")

cat("DONE!\n")
