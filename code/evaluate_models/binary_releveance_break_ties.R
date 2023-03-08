#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)

dfs <- 
  map(list.files("/media/esbenlykke/My Passport/binary_relevance_preds", full.names = TRUE), 
      read_parquet)

# Break ties --------------------------------------------------------------


break_ties <- function(tbl) {
  preds_no_ties <-
    tbl %>% 
    mutate(across(where(is.factor), as.integer) - 1) |>
    filter(
      !(in_bed_asleep_.pred_class == 1 & in_bed_awake_.pred_class == 1) &
        !(in_bed_asleep_.pred_class == 1 & out_bed_awake_.pred_class == 1) &
        !(in_bed_awake_.pred_class == 1 & out_bed_awake_.pred_class == 1)
    )


  preds_ties_broken <-
    tbl |>
    filter(
      (in_bed_asleep_.pred_class == 1 & in_bed_awake_.pred_class == 1) |
        (in_bed_asleep_.pred_class == 1 & out_bed_awake_.pred_class == 1) |
        (in_bed_awake_.pred_class == 1 & out_bed_awake_.pred_class == 1)
    ) %>%
    mutate(
      max = pmax(in_bed_asleep_.pred_1, in_bed_awake_.pred_1, out_bed_awake_.pred_1),
      win = case_when(
        in_bed_asleep_.pred_1 == max ~ "x",
        in_bed_awake_.pred_1 == max ~ "y",
        out_bed_awake_.pred_1 == max ~ "z"
      ),
      in_bed_asleep_.pred_class = if_else(win == "x", 1, 0),
      in_bed_awake_.pred_class = if_else(win == "y", 1, 0),
      out_bed_awake_.pred_class = if_else(win == "z", 1, 0)
    ) %>%
    mutate(across(where(is.factor), as.integer) - 1) |>
    select(-c(max, win))

  preds_no_ties %>%
    bind_rows(preds_ties_broken)
}

dfs_no_ties <-
  map(dfs, break_ties)

names(dfs_no_ties) <- c("decision_tree", "logistic_regression", "neural_net", "xgboost") %>%
  str_c("/media/esbenlykke/My Passport/binary_relevance_preds/preds_no_ties_", ., ".parquet")

walk2(dfs, names(dfs), ~ write_parquet(.x, .y))
