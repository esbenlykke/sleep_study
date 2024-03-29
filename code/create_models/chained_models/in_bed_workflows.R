#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(themis)

tidymodels_prefer()
options(tidymodels.dark = TRUE)

load_data <- function(input_path) {
  read_parquet(input_path) %>%
    drop_na() %>% 
    mutate(across(contains("in_bed"), forcats::as_factor))
}

train_30 <- load_data("data/data_for_modelling/chained_classifiers/30_sec_training_data.parquet")
  
# test_30 <- load_data("data/data_for_modelling/chained_classifiers/30_sec_testing_data.parquet")

# Create cross-validation objects
# folds_10 <- group_mc_cv(train_10, group = id, times = 5, prop = .25)
folds <- group_mc_cv(train_30, group = id, times = 5, prop = .5)


rec <- recipe(in_bed ~ age + weekday + is_weekend + incl + theta + x_mean + y_mean + z_mean +
                       x_sd + y_sd + z_sd + x_sd_long + y_sd_long + z_sd_long + sd_max +
                       temp_mean + temp_sd + clock_proxy_cos +
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
                       x_kurtosis + y_kurtosis + z_kurtosis,
              data = train_30)

rec_norm <- rec %>% 
  step_normalize(all_numeric_predictors())

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
  recipes::update(hidden_units = hidden_units(c(1, 27)))

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

# 30 second epochs data
# in-bed raw
normalized_in_bed_30_sec_wf <-
  workflow_set(
    preproc = list(
      in_bed_raw = rec_norm
    ),
    models = list(
      logistic_regression = glmnet_spec, # works in parallel
      neural_network = nnet_spec # works in parallel
    ),
    cross = FALSE
  )

no_preproc_in_bed_30_sec_wf <-
  workflow_set(
    preproc = list(
      in_bed_raw = rec
    ),
    models = list(
      decision_tree = CART_spec,
      # MARS = mars_spec, # works in parallel
      # random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = FALSE
  )

all_in_bed_30_sec_workflows <-
  bind_rows(normalized_in_bed_30_sec_wf, no_preproc_in_bed_30_sec_wf) %>%
  option_add(param_info = nnet_param, id = "in_bed_raw_neural_network") %>%
  option_add(param_info = rpart_param, id = "in_bed_raw_decision_tree") %>%
  option_add(param_info = xgb_param, id = "in_bed_raw_xgboost")

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

tictoc::tic()
all_in_bed_30_sec_workflows[4,] |>
    workflow_map(
      seed = 123,
      "tune_grid",
      resamples = folds,
      grid = 10,
      control = grid_ctrl,
      metrics = metric_set(f_meas),
      verbose = TRUE
    ) %>%
    write_rds("/media/esbenlykke/My Passport/chained_models/grid_results/in_bed/in_bed_30_sec_epoch_grid_results.rds")
  tictoc::toc()
