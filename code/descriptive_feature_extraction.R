#!/usr/bin/env Rscript

pacman::p_load(
  tidyverse,
  vroom,
  feather,
  slider,
  furrr
)

bsl <-
  read_feather("data/processed/bsl.feather")

fup <-
  read_feather("data/processed/fup.feather")



# Calculating features in 30-sec overlapping rolling windows.
create_feats <- function(tbl) {
  tbl |>
    mutate(
      across(x_thigh:z_back,
        list(
          trim_mean = ~ slide_dbl(.x, ~ mean(.x, trim = .1), .after = 6),
          MAD = ~ slide_dbl(.x, ~ mad(.x), .after = 6),
          max = ~ slide_dbl(.x, ~ abs(max(.x)), .after = 6),
          IQR = ~ slide_dbl(.x, ~ IQR(.x), .after = 6, .step = 1)
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    mutate(
      across(
        temp_thigh:temp_back,
        list(
          trim_mean = ~ slide_dbl(.x, ~ mean(.x, trim = .1), .after = 6),
          min = ~ slide_dbl(.x, ~ min(.x), .after = 6),
          max = ~ slide_dbl(.x, ~ abs(max(.x)), .after = 6),
          sd = ~ slide_dbl(.x, ~ sd(.x), .after = 6, .step = 1)
        )
      )
    )
}

# furrr multisession with 12 CPU cores
plan(multisession, workers = 12)

bsl |>
  group_split(id) |>
  future_map_dfr(~ create_feats(.x),
                 .progress = TRUE) |>
  write_feather("data/processed/bsl_features.feather")

fup |>
  group_split(id) |>
  future_map_dfr(~ create_feats(.x),
                 .progress = TRUE) |>
  write_feather("data/processed/fup_features.feather")

# TODO next step is to create the sensor-independent features

plan(sequential)