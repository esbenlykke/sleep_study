#!/usr/bin/env Rscript

args <- R.utils::commandArgs(trailingOnly = TRUE)

dest <- args[1] # destination filename

library(tidyverse)
library(feather)

list.files("~/sleep_study/data/temp/", "feather", full.names = TRUE) |>
  map_dfr(read_feather) |>
  write_feather(dest)

unlink("~/sleep_study/data/temp/", recursive = TRUE)