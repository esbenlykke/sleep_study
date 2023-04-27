#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)

tidymodels_prefer()
options(tidymodels.dark = TRUE)

# TODO import in-bed data predicted by best in-bed model. Both 10-sec and 30-sec epoch data.
# Also consider a SMOTE variant for waso

data_10 <-
  read_parquet("data/data_for_modelling/only_in_bed_data_10.parquet") %>%
  mutate(across(contains("sleep"), as_factor)) 

data_30 <-
  read_parquet("data/data_for_modelling/only_in_bed_data_30.parquet") %>%
  mutate(across(contains("sleep"), as_factor)) 

set.seed(123)
spl_10 <-
  group_initial_split(data_10, group = id, prop = .5)

train_10 <-
  training(spl_10)

test_10 <-
  testing(spl_10)

spl_30 <-
  group_initial_split(data_30, group = id, prop = .5)

train_30 <-
  training(spl_30)

test_30 <-
  testing(spl_30)

folds_10 <-
  group_mc_cv(train_10, group = id, times = 5, prop = .5)

folds_30 <-
  group_mc_cv(train_30, group = id, times = 5, prop = .5)


# Recipes -----------------------------------------------------------------
cat("Creating preprocessing recipes...\n")

### 10 sec epochs ###
sleep_raw_rec_10 <-
  recipe(
    sleep ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train_10
  ) |>
  step_zv(all_predictors())

sleep_raw_norm_rec_10 <-
  sleep_raw_rec_10 |>
  step_normalize(all_numeric_predictors())

sleep_median5_rec_10 <-
  recipe(
    sleep_median5 ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train_10
  ) |>
  step_zv(all_predictors())

sleep_median5_norm_rec_10 <-
  sleep_median5_rec_10 |>
  step_normalize(all_numeric_predictors())

sleep_median10_rec_10 <-
  recipe(
    sleep_median5 ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train_10
  ) |>
  step_zv(all_predictors())

sleep_median10_norm_rec_10 <-
  sleep_median10_rec_10 |>
  step_normalize(all_numeric_predictors())

### 30 sec epochs ###

sleep_raw_rec_30 <-
  recipe(
    sleep ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train_30
  ) |>
  step_zv(all_predictors())

sleep_raw_norm_rec_30 <-
  sleep_raw_rec_30 |>
  step_normalize(all_numeric_predictors())

sleep_median5_rec_30 <-
  recipe(
    sleep_median5 ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train_30
  ) |>
  step_zv(all_predictors())

sleep_median5_norm_rec_30 <-
  sleep_median5_rec_30 |>
  step_normalize(all_numeric_predictors())

sleep_median10_rec_30 <-
  recipe(
    sleep_median5 ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train_30
  ) |>
  step_zv(all_predictors())

sleep_median10_norm_rec_30 <-
  sleep_median10_rec_30 |>
  step_normalize(all_numeric_predictors())

# Model specifications ----------------------------------------------------

glmnet_spec <-
  logistic_reg(penalty = tune(), mixture = tune()) |>
  set_mode("classification") |>
  set_engine("glmnet", verbose = TRUE)

nnet_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) |>
  set_engine("nnet", MaxNWts = 500) |> # MaxNWts is a regularization term based on number of predictors
  set_mode("classification")

CART_spec <-
  decision_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("rpart")

xgb_spec <-
  boost_tree(
    tree_depth = tune(), learn_rate = tune(),
    loss_reduction = tune(), min_n = tune(),
    sample_size = tune(), trees = seq(100, 1000, 200)
  ) |>
  set_engine("xgboost", verbose = TRUE) |>
  set_mode("classification")


# update nnet params
nnet_param <-
  nnet_spec |>
  extract_parameter_set_dials() |>
  update(hidden_units = hidden_units(c(1, 27)))

rpart_param <-
  CART_spec %>%
  extract_parameter_set_dials() %>%
  update(tree_depth = tree_depth(c(3, 7)))


# Create workflow sets ----------------------------------------------------
cat("Creating workflow sets...\n")

### 10 sec epochs ###
sleep_norm_wfs_10 <-
  workflow_set(
    preproc = list(
      sleep_raw = sleep_raw_norm_rec_10,
      sleep_5_min_median = sleep_median5_norm_rec_10,
      sleep_10_min_median = sleep_median10_norm_rec_10
    ),
    models = list(
      logistic_regression = glmnet_spec, # works in parallel
      neural_network = nnet_spec # works in parallel
    ),
    cross = TRUE
  ) %>%
  option_add(param_info = nnet_param, id = c(
    "sleep_raw_neural_network",
    "sleep_5_min_median_neural_network",
    "sleep_10_min_median_neural_network"
  ))

sleep_no_norms_wfs_10 <-
  workflow_set(
    preproc = list(
      sleep_raw = sleep_raw_rec_10,
      sleep_5_min_median = sleep_median5_rec_10,
      sleep_10_min_median = sleep_median10_rec_10
    ),
    models = list(
      decision_tree = CART_spec,
      # MARS = mars_spec, # works in parallel
      # random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = TRUE
  ) %>%
  option_add(param_info = rpart_param, id = c(
    "sleep_raw_decision_tree",
    "sleep_5_min_median_decision_tree",
    "sleep_10_min_median_decision_tree"
  ))


### all 10 sec epoch workdflows ###
all_wfs_10 <- list(
  decision_tree_wfs_10 =
    bind_rows(sleep_norm_wfs_10, sleep_no_norms_wfs_10) %>%
      filter(str_detect(wflow_id, "decision")),
  log_reg_wfs_10 =
    bind_rows(sleep_norm_wfs_10, sleep_no_norms_wfs_10) %>%
      filter(str_detect(wflow_id, "logistic")),
  nnet_wfs_10 =
    bind_rows(sleep_norm_wfs_10, sleep_no_norms_wfs_10) %>%
      filter(str_detect(wflow_id, "neural")),
  xgb_wfs_10 =
    bind_rows(sleep_norm_wfs_10, sleep_no_norms_wfs_10) %>%
      filter(str_detect(wflow_id, "xgboost"))
)

### 30 sec epochs ###
sleep_norm_wfs_30 <-
  workflow_set(
    preproc = list(
      sleep_raw = sleep_raw_norm_rec_30,
      sleep_5_min_median = sleep_median5_norm_rec_30,
      sleep_10_min_median = sleep_median10_norm_rec_30
    ),
    models = list(
      logistic_regression = glmnet_spec, # works in parallel
      neural_network = nnet_spec # works in parallel
    ),
    cross = TRUE
  ) %>%
  option_add(param_info = nnet_param, id = c(
    "sleep_raw_neural_network",
    "sleep_5_min_median_neural_network",
    "sleep_10_min_median_neural_network"
  ))

sleep_no_norms_wfs_30 <-
  workflow_set(
    preproc = list(
      sleep_raw = sleep_raw_rec_30,
      sleep_5_min_median = sleep_median5_rec_30,
      sleep_10_min_median = sleep_median10_rec_30
    ),
    models = list(
      decision_tree = CART_spec,
      # MARS = mars_spec, # works in parallel
      # random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = TRUE
  ) %>%
  option_add(param_info = rpart_param, id = c(
    "sleep_raw_decision_tree",
    "sleep_5_min_median_decision_tree",
    "sleep_10_min_median_decision_tree"
  ))


### all 30 sec epoch workdflows ###
all_wfs_30 <-
  list(
    decision_tree_wfs_30 =
      bind_rows(sleep_norm_wfs_30, sleep_no_norms_wfs_30) %>%
        filter(str_detect(wflow_id, "decision")),
    log_reg_wfs_30 =
      bind_rows(sleep_norm_wfs_30, sleep_no_norms_wfs_30) %>%
        filter(str_detect(wflow_id, "logistic")),
    nnet_wfs_30 =
      bind_rows(sleep_norm_wfs_30, sleep_no_norms_wfs_30) %>%
        filter(str_detect(wflow_id, "neural")),
    xgb_wfs_30 =
      bind_rows(sleep_norm_wfs_30, sleep_no_norms_wfs_30) %>%
        filter(str_detect(wflow_id, "xgboost"))
  )


# Setup parallel cluster --------------------------------------------------


doParallel::registerDoParallel(cores = 6)


# Tuning models with regular grid search ----------------------------------

grid_ctrl <-
  control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    parallel_over = "everything",
    save_pred = FALSE,
    save_workflow = FALSE
  )

tune_wf_and_write_10 <- function(wfs, fname) {
  res <-
    wfs |>
    workflow_map(
      seed = 123,
      "tune_grid",
      resamples = folds_10,
      grid = 5,
      control = grid_ctrl,
      metrics = metric_set(f_meas, accuracy),
      verbose = TRUE
    ) %>% 
    write_rds(fname)
}

tune_wf_and_write_30 <- function(wfs, fname) {
  res <-
    wfs |>
    workflow_map(
      seed = 123,
      "tune_grid",
      resamples = folds_30,
      grid = 5,
      control = grid_ctrl,
      metrics = metric_set(f_meas, accuracy),
      verbose = TRUE
    ) %>% 
    write_rds(fname)
}

fnames_10 <- 
  str_c("/media/esbenlykke/My Passport/chained_models/grid_results/", names(all_wfs_10), ".rds")


fnames_30 <- 
  str_c("/media/esbenlykke/My Passport/chained_models/grid_results/", names(all_wfs_30), ".rds")


walk2(all_wfs_10, fnames_10, tune_wf_and_write_10)
walk2(all_wfs_30, fnames_30, tune_wf_and_write_30)
