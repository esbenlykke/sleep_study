#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)

filenames <- list.files("/media/esbenlykke/My Passport/binary_relevance_preds", 
           full.names = TRUE) %>% 
  str_subset("no_ties", negate = TRUE)

dfs <- 
  map(filenames, read_parquet) %>% 
  setNames(str_remove_all(filenames, "/media/esbenlykke/My Passport/binary_relevance_preds/"))

# Break ties --------------------------------------------------------------

### Threshold-based tie breaking
 
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
    full_join(preds_ties_broken)
}

dfs_no_ties <-
  map(dfs, break_ties)

names(dfs_no_ties) <- c("decision_tree", "logistic_regression", "neural_net", "xgboost") %>%
  str_c("/media/esbenlykke/My Passport/binary_relevance_preds/preds_no_ties_", ., ".parquet")

walk2(dfs_no_ties, names(dfs_no_ties), ~ write_parquet(.x, .y))

### Random tie-breaking

### Deterministic tie-breaking based on F1 score

### Cost-based, i.e., give advantage to in-bed awake?