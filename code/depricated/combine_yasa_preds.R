#!/usr/bin/env Rscript

library(tidyverse)

csv_files <- 
  fs::dir_ls("data/raw/somno_data/somno_edf", glob = "*.csv")

csv_files |>
  set_names() |>
  map_dfr(read_csv, .id = "id") |>
  mutate(id = parse_number(id)) |> 
  write_tsv("data/processed/yasa_preds.tsv")
