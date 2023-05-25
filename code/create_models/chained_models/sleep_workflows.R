#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)

tidymodels_prefer()
options(tidymodels.dark = TRUE)

# Function to read data from parquet files
load_data <- function(input_path) {
  read_parquet(input_path) %>%
    mutate(across(contains("sleep"), as_factor))
}


# Load data
cat("Spending data budget...\n")

train_10 <- load_data("data/data_for_modelling/chained_classifiers/10_sec_only_in_bed_training_data.parquet")
test_10 <- load_data("data/data_for_modelling/chained_classifiers/10_sec_only_in_bed_testing_data.parquet")

train_30 <- load_data("data/data_for_modelling/chained_classifiers/30_sec_only_in_bed_training_data.parquet")
test_30 <- load_data("data/data_for_modelling/chained_classifiers/30_sec_only_in_bed_testing_data.parquet")

# Create cross-validation objects
folds_10 <- group_mc_cv(train_10, group = id, times = 5, prop = .5)
folds_30 <- group_mc_cv(train_30, group = id, times = 5, prop = .5)


# Function to create preprocessing recipes
create_recipe <- function(target_var, data, normalize = FALSE) {
  # Create a formula using paste and as.formula
  formula_str <- paste(target_var, "~ age + weekday + incl + theta + x_mean + y_mean + z_mean +
                       x_sd + y_sd + z_sd + x_sd_long + y_sd_long + z_sd_long + sd_max +
                       temp_mean + temp_sd + clock_proxy_cos + clock_proxy_linear +
                       temp_mean_lag_1min + temp_mean_lag_5min + temp_mean_lag_30min +
                       temp_mean_lead_1min + temp_mean_lead_5min + temp_mean_lead_30min +
                       theta_lag_1min + theta_lag_5min + theta_lag_30min +
                       theta_lead_1min + theta_lead_5min + theta_lead_30min +
                       incl_lag_1min + incl_lag_5min + incl_lag_30min +
                       incl_lead_1min + incl_lead_5min + incl_lead_30min +
                       x_sd_lag_1min + x_sd_lag_5min + x_sd_lag_30min +
                       y_sd_lag_1min + y_sd_lag_5min + y_sd_lag_30min +
                       z_sd_lag_1min + z_sd_lag_5min + z_sd_lag_30min +
                       x_sd_lead_1min + x_sd_lead_5min + x_sd_lead_30min +
                       y_sd_lead_1min + y_sd_lead_5min + y_sd_lead_30min +
                       z_sd_lead_1min + z_sd_lead_5min + z_sd_lead_30min +
                       vector_magnitude + 
                       x_crossing_rate + y_crossing_rate + z_crossing_rate + 
                       x_skewness + y_skewness + z_skewness +
                       x_kurtosis + y_kurtosis + z_kurtosis")

  formula_obj <- as.formula(formula_str)

  rec <- recipe(formula_obj, data = data)

  if (normalize) {
    rec <- rec %>%
      step_normalize(all_numeric_predictors())
  }

  return(rec)
}

# Define preprocessing recipes
cat("Creating preprocessing recipes...\n")

# 10 second epochs data
sleep_raw_10_sec_rec <- create_recipe("sleep", train_10)
sleep_raw_10_sec_norm_rec <- create_recipe("sleep", train_10, normalize = TRUE)
sleep_median5_10_sec_rec <- create_recipe("sleep_median5", train_10)
sleep_median5_10_sec_norm_rec <- create_recipe("sleep_median5", train_10, normalize = TRUE)
sleep_median10_10_sec_rec <- create_recipe("sleep_median10", train_10)
sleep_median10_10_sec_norm_rec <- create_recipe("sleep_median10", train_10, normalize = TRUE)

# 30 second epochs data
sleep_raw_30_sec_rec <- create_recipe("sleep", train_30)
sleep_raw_30_sec_norm_rec <- create_recipe("sleep", train_30, normalize = TRUE)
sleep_median5_30_sec_rec <- create_recipe("sleep_median5", train_30)
sleep_median5_30_sec_norm_rec <- create_recipe("sleep_median5", train_30, normalize = TRUE)
sleep_median10_30_sec_rec <- create_recipe("sleep_median10", train_30)
sleep_median10_30_sec_norm_rec <- create_recipe("sleep_median10", train_30, normalize = TRUE)

# Model specifications ----------------------------------------------------

CART_spec <-
  decision_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("rpart")

glmnet_spec <-
  logistic_reg(penalty = tune(), mixture = tune()) |>
  set_mode("classification") |>
  set_engine("glmnet", verbose = TRUE)

nnet_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) |>
  set_engine("nnet", MaxNWts = 7000) |> # MaxNWts is a regularization term based on number of predictors
  set_mode("classification")

# mars_spec <-
#   mars(prod_degree = tune(), num_terms = tune()) |> #<- use GCV to choose terms
#   set_engine("earth") |>
#   set_mode("classification")

# rf_spec <-
# rand_forest(mtry = tune(), min_n = tune(), trees = tune()) |>
# set_engine("ranger") |>
# set_mode("classification")

xgb_spec <-
  boost_tree(
    tree_depth = tune(), learn_rate = tune(),
    loss_reduction = tune(), min_n = tune(),
    sample_size = tune(), trees = tune()
  ) |>
  set_engine("xgboost", verbose = TRUE) |>
  set_mode("classification")

nnet_param <-
  nnet_spec |>
  extract_parameter_set_dials() |>
  recipes::update(hidden_units = hidden_units(c(3, 27)))

rpart_param <-
  CART_spec %>%
  extract_parameter_set_dials() %>%
  recipes::update(tree_depth = tree_depth(c(3, 7)))

xgb_param <-
  xgb_spec %>%
  extract_parameter_set_dials() %>%
  recipes::update(trees = trees(c(200, 800)))



# Create workflow sets ----------------------------------------------------
cat("Creating workflow sets...\n")

### 10 sec epochs ###
sleep_norm_wfs_10 <-
  workflow_set(
    preproc = list(
      sleep_raw = sleep_raw_10_sec_norm_rec,
      sleep_5_min_median = sleep_median5_10_sec_norm_rec,
      sleep_10_min_median = sleep_median10_10_sec_norm_rec
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
      sleep_raw = sleep_raw_10_sec_rec,
      sleep_5_min_median = sleep_median5_10_sec_rec,
      sleep_10_min_median = sleep_median10_10_sec_rec
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
  )) %>%
  option_add(param_info = xgb_param, id = c(
    "sleep_raw_xgboost",
    "sleep_5_min_median_xgboost",
    "sleep_10_min_median_xgboost"
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
      sleep_raw = sleep_raw_30_sec_norm_rec,
      sleep_5_min_median = sleep_median5_30_sec_norm_rec,
      sleep_10_min_median = sleep_median10_30_sec_norm_rec
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
      sleep_raw = sleep_raw_30_sec_rec,
      sleep_5_min_median = sleep_median5_30_sec_rec,
      sleep_10_min_median = sleep_median10_30_sec_rec
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
  )) %>%
  option_add(param_info = xgb_param, id = c(
    "sleep_raw_xgboost",
    "sleep_5_min_median_xgboost",
    "sleep_10_min_median_xgboost"
  ))


### all 30 sec epoch workdflows ###
all_wfs_30 <- list(
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

tune_wf_and_write <- function(wfs, fname, folds) {
  tictoc::tic()
  wfs |>
    workflow_map(
      seed = 123,
      "tune_grid",
      resamples = folds,
      grid = 5,
      control = grid_ctrl,
      metrics = metric_set(f_meas),
      verbose = TRUE
    ) %>%
    write_rds(fname)
  tictoc::toc()
}


fnames_10 <-
  str_c("/media/esbenlykke/My Passport/chained_models/grid_results/sleep/", names(all_wfs_10), ".rds")
fnames_30 <-
  str_c("/media/esbenlykke/My Passport/chained_models/grid_results/sleep/", names(all_wfs_30), ".rds")


walk2(all_wfs_10, fnames_10, ~ tune_wf_and_write(.x, .y, folds_10))
walk2(all_wfs_30, fnames_30, ~ tune_wf_and_write(.x, .y, folds_30))
