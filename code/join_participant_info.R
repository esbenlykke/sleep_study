#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)

args <- R.utils::commandArgs(trailingOnly = TRUE)

feather_path <- as.character(args[1])
info_path <- as.character(args[2])
dest <- as.character(args[3])

feather <- 
  read_feather(feather_path) |> 
  mutate(across(where(is.character), as.numeric))

info <- 
  readxl::read_excel(info_path) |> 
  janitor::clean_names()

info |> 
  pivot_longer(-c(id, age), names_to = "placement", values_to = "sensor_code") |> 
  right_join(feather, by = c("id", "sensor_code")) |> 
  write_parquet(dest)