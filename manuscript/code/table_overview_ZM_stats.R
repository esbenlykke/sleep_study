#!/usr/bin/env Rscript

library(tidyverse)
library(glue)
library(gt)
library(showtext)

stats <-
  read_csv(here::here("data/processed/tbl_overview_sleep_summaries.csv"))

# Create a formatted table to display the metrics, grouped by epoch length
tbl_overview <-
  stats %>% 
  # pivot_wider(names_from = row, values_from = raw_stat:median_10_stat2) %>%
  # mutate(raw_stat2_sleep_count_and_ratio = raw_stat2_sleep_count_and_ratio * 100,
  #        median_5_stat2_sleep_count_and_ratio = median_5_stat2_sleep_count_and_ratio * 100,
  #        median_10_stat2_sleep_count_and_ratio = median_5_stat2_sleep_count_and_ratio * 100)
  transmute(
    row = row,
    "Raw ZM Predictions" = glue("{round(raw_stat, 1)} ({round(raw_stat2, 1)})"),
    "5-Min Median" = glue("{round(median_5_stat, 1)} ({round(median_5_stat2, 1)})"),
    "10-Min Median" = glue("{round(median_10_stat, 1)} ({round(median_10_stat2, 1)})")
  ) %>%
  pivot_longer(-row) %>%
  pivot_wider(names_from = row, values_from = value) %>%
  gt() %>%
  cols_label(
    name = "", spt = "SPT (hrs)", tst = "TST (hrs)", se = "SE (%)", 
    lps = "LPS (min)", waso = "WASO (min)", no = "Awakenings (N)"
  ) %>%
  cols_align(align = "right", columns = -name)