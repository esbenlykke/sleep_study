#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(finetune))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))


tidymodels_prefer()
options(tidymodels.dark = TRUE)

cat("Packages loaded...\n")

# Spend data budget -------------------------------------------------------
cat("Spending data budget...\n")

data <-
  read_parquet("data/processed/model_data/bsl_thigh_sensor_independent_features.parquet") |>
  mutate(
    in_bed = as_factor(in_bed),
    sleep = as_factor(sleep)
  )

set.seed(123)
spl <-
  data |>
  group_initial_split(group = id, prop = .5)

train <- training(spl)
test <- testing(spl)

folds <-
  group_vfold_cv(train, group = id, v = 5, balance = "groups")


# Create preprocessors ----------------------------------------------------


in_bed_rec <-
  recipe(
    in_bed ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
    data = train
  ) |>
  step_zv(all_predictors())

sleep_rec <-
  recipe(
    sleep ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
    data = train
  ) |>
  step_zv(all_predictors())


# Create CART model spec --------------------------------------------------

cart_spec <-
  decision_tree(cost_complexity = tune(), min_n = tune(), tree_depth = tune()) |>
  set_engine("rpart") |>
  set_mode("classification")

tree_grid <- 
  grid_latin_hypercube(cost_complexity(),
                       min_n(),
                       tree_depth(range = c(5, 10)),
                       size = 10)

# Setup parallel back-end -------------------------------------------------


doParallel::registerDoParallel(cores = 6)


# Workflows ---------------------------------------------------------------

in_bed_wf <- 
  workflow(in_bed_rec, cart_spec)

sleep_wf <- 
  workflow(sleep_rec, cart_spec)


# Tune grids --------------------------------------------------------------

# In bed
tictoc::tic()
grid_ctrl <-
  control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    event_level = "second",
    parallel_over = "everything",
    save_pred = FALSE,
    save_workflow = TRUE
  )

in_bed_CART_results <-
  in_bed_wf |>
  tune_grid(
    resamples = folds,
    grid = tree_grid,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc)
  )
tictoc::toc()

write_rds(in_bed_CART_results, "data/models/in_bed_CART_results.rds")

# Sleep
tictoc::tic()
grid_ctrl <-
  control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    event_level = "second",
    parallel_over = "everything",
    save_pred = FALSE,
    save_workflow = TRUE
  )

sleep_CART_results <-
  sleep_wf |>
  tune_grid(
    resamples = folds,
    grid = tree_grid,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc)
  )
tictoc::toc()

write_rds(sleep_CART_results, "data/models/sleep_CART_results.rds")
