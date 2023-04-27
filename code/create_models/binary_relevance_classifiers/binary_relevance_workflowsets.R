#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)


tidymodels_prefer()
options(tidymodels.dark = TRUE)

cat("Packages loaded...\n")

# Spend data budget -------------------------------------------------------
cat("Spending data budget...\n")
set.seed(123)
data <-
  read_parquet("data/data_for_modelling/all_data_incl_sensor_independent_features.parquet") |>
  mutate(
    in_bed_asleep = as_factor(if_else(in_bed == 1 & sleep == 1, 1, 0)),
    in_bed_awake = as_factor(if_else(in_bed == 1 & sleep == 0, 1, 0)),
    out_bed_awake = as_factor(if_else(in_bed == 0 & sleep == 0, 1, 0))
  )

set.seed(123)
spl <-
  data |>
  group_initial_split(group = id, prop = .5)

train <- training(spl)
test <- testing(spl)

# write out train data for later fitting of optimized workflows
# write_parquet(train, "data/processed/binary_relevance_training_data.parquet")

# write out test data for later evaluation
# write_parquet(test, "data/processed/binary_relevance_testing_data.parquet")

folds <-
  group_vfold_cv(train, group = id, v = 5, balance = "groups")


# Recipes -----------------------------------------------------------------
cat("Creating preprocessing recipes...\n")

in_bed_asleep_rec <-
  recipe(
    in_bed_asleep ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
    data = train
  ) |>
  step_zv(all_predictors())

in_bed_asleep_norm_rec <-
  in_bed_asleep_rec |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

in_bed_awake_rec <-
  recipe(
    in_bed_awake ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
    data = train
  ) |>
  step_zv(all_predictors())

in_bed_awake_norm_rec <-
  in_bed_awake_rec |>
  step_normalize(all_numeric_predictors())

out_bed_awake_rec <-
  recipe(
    out_bed_awake ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
    data = train
  ) |>
  step_zv(all_predictors())

out_bed_awake_norm_rec <-
  out_bed_awake_rec |>
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



# update nnet params
nnet_param <-
  nnet_spec |>
  extract_parameter_set_dials() |>
  update(hidden_units = hidden_units(c(1, 27)))


# Create workflow sets ----------------------------------------------------
cat("Creating workflow sets...\n")

basic_in_bed_asleep_wf <-
  workflow_set(
    preproc = list(
      basic_in_bed_asleep = in_bed_asleep_rec
    ),
    models = list(
      # MARS = mars_spec, # works in parallel
      # random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = FALSE
  )

basic_in_bed_awake_wf <-
  workflow_set(
    preproc = list(
      basic_in_bed_awake = in_bed_awake_rec
    ),
    models = list(
      # MARS = mars_spec, # works in parallel
      # random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = FALSE
  )

basic_out_bed_awake_wf <-
  workflow_set(
    preproc = list(
      basic_out_bed_awake = out_bed_awake_rec
    ),
    models = list(
      # MARS = mars_spec, # works in parallel
      # random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = FALSE
  )

normalized_in_bed_asleep_wf <-
  workflow_set(
    preproc = list(
      normalized_in_bed_asleep = in_bed_asleep_norm_rec
    ),
    models = list(
      logistic_regression = glmnet_spec, # works in parallel
      neural_network = nnet_spec # works in parallel
    ),
    cross = FALSE
  ) |>
  option_add(param_info = nnet_param, id = "normalized_in_bed_asleep_neural_network")

normalized_in_bed_awake_wf <-
  workflow_set(
    preproc = list(normalized_in_bed_awake = in_bed_awake_norm_rec),
    models = list(
      logistic_regression = glmnet_spec, # works in parallel
      neural_network = nnet_spec # works in parallel)
    ),
    cross = FALSE
  )

normalized_out_bed_awake_wf <-
  workflow_set(
    preproc = list(normalized_out_bed_awake = out_bed_awake_norm_rec),
    models = list(
      logistic_regression = glmnet_spec, # works in parallel
      neural_network = nnet_spec # works in parallel)
    ),
    cross = FALSE
  )



all_in_bed_asleep_workflows <-
  bind_rows(normalized_in_bed_asleep_wf, basic_in_bed_asleep_wf)

all_in_bed_awake_workflows <-
  bind_rows(normalized_in_bed_awake_wf, basic_in_bed_awake_wf)

all_out_bed_awake_workflows <-
  bind_rows(normalized_out_bed_awake_wf, basic_out_bed_awake_wf)


# Setup parallel cluster --------------------------------------------------


doParallel::registerDoParallel(cores = 6)


# Tuning models with regular grid search ----------------------------------
# In bed

grid_ctrl <-
  control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    parallel_over = "everything",
    save_pred = FALSE,
    save_workflow = FALSE,
    event_level = "second"
  )

cat("Tuning the following workflows:\n")
all_in_bed_asleep_workflows

tictoc::tic()
in_bed_asleep_grid_results <-
  all_in_bed_asleep_workflows |>
  workflow_map(
    seed = 123,
    "tune_grid",
    resamples = folds,
    grid = 5,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc),
    verbose = TRUE
  )
tictoc::toc()

write_rds(in_bed_asleep_grid_results, "/media/esbenlykke/My Passport/grid_results/in_bed_asleep_workflowsets_results.rds")

# Sleep

cat("Tuning the following workflows:\n")
all_in_bed_awake_workflows

in_bed_awake_grid_results <-
  all_in_bed_awake_workflows |>
  workflow_map(
    seed = 123,
    "tune_grid",
    resamples = folds,
    grid = 5,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc),
    verbose = TRUE
  )
tictoc::toc()

write_rds(in_bed_awake_grid_results, "/media/esbenlykke/My Passport/grid_results/in_bed_awake_workflowsets_results.rds")

cat("Tuning the following workflows:\n")
all_out_bed_awake_workflows

out_bed_awake_grid_results <-
  all_out_bed_awake_workflows |>
  workflow_map(
    seed = 123,
    "tune_grid",
    resamples = folds,
    grid = 5,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc),
    verbose = TRUE
  )
tictoc::toc()

write_rds(out_bed_awake_grid_results, "/media/esbenlykke/My Passport/grid_results/out_bed_awake_workflowsets_results.rds")

