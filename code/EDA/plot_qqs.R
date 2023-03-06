#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)

stats_files <-
  list.files("~/sleep_study/data/processed/stats_predictions", full.names = TRUE)

all_stats <-
  map(stats_files, read_parquet) |>
  set_names(c("logistic_regression", "neural_net", "decision_tree", "xgboost"))


get_diff_stats <-
  function(tbl) {
    tbl |>
      mutate(
        diff_spt_hrs = spt_hrs - zm_spt_hrs,
        diff_tst_hrs = tst_hrs - zm_tst_hrs,
        diff_se_percent = se_percent - zm_se_percent,
        diff_lps_min = lps_min - zm_lps_min,
        diff_waso_min = waso_min - zm_waso_min
      ) |>
      rowwise() |>
      mutate(
        avg_spt_hrs = mean(c(spt_hrs, zm_spt_hrs)),
        avg_tst_hrs = mean(c(tst_hrs, zm_tst_hrs)),
        avg_se_percent = mean(c(se_percent, zm_se_percent)),
        avg_lps_min = mean(c(lps_min, zm_lps_min)),
        avg_waso_min = mean(c(waso_min, zm_waso_min))
      ) |>
      ungroup()
  }


all_diffs <-
  all_stats |>
  map(get_diff_stats)


plot_qqs <- function(df) {
  df |>
    select(id, noon_day, contains("diff")) |>
    pivot_longer(-c(id:noon_day)) |>
    ggplot(aes(sample = value)) +
    geom_qq(shape = 21, fill = "darkorange", alpha = .5) +
    geom_qq_line() +
    facet_wrap(~name, scales = "free") +
    theme_light()
}

qq_plots <-
  map(all_diffs, plot_qqs)

filenames <- 
  paste0("visuals/qqplots_", names(qq_plots), ".png")

walk2(qq_plots, filenames, ~ ggsave(plot = .x, filename = .y))
