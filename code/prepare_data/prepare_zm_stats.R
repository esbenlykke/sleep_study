#!/usr/bin/env Rscript

library(tidyverse)

stats_files <-
  list.files("data/raw/zm_stats", full.names = TRUE)

map_df(stats_files, ~ read_csv(.x, col_types = cols("Start Date" = "c")) |>
  mutate(id = as.integer(parse_number(.x)), .before = 1) |>
  janitor::clean_names()) |>
  arrange(id) |> 
  write_csv("data/processed/all_zm_stats.csv")


