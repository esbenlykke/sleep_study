#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(rpart.plot)

in_bed_tree <- 
  read_rds("~/sleep_study/data/models/fitted_models/in_bed_simple_CART_fit.rds")

sleep_tree <- 
  read_rds("~/sleep_study/data/models/fitted_models/sleep_simple_CART_fit.rds")

in_bed_tree_plot <- 
  in_bed_tree |>
  extract_fit_engine() |> 
  rpart.plot::rpart.plot(type = 0)

sleep_tree_plot <- 
  sleep_tree |>
  extract_fit_engine() |> 
  rpart.plot::rpart.plot(type = 0)
