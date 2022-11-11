#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
library(arrow)

args <- R.utils::commandArgs(trailingOnly = TRUE)

dest <- args[1] # destination filename

list.files("~/sleep_study/data/temp/", full.names = TRUE) |>
  map_dfr(read_feather) |>
  write_feather(dest)

glue::glue("{length(list.files('~/sleep_study/data/temp/'))} feathers have been merged into '{dest}'")

unlink("~/sleep_study/data/temp/", recursive = TRUE)

beepr::beep(4)