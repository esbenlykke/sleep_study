#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
# suppressMessages(library(finetune))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))
library(here)


tidymodels_prefer()
options(tidymodels.dark = TRUE)

cat("Packages loaded...\n")

# Spend data budget -------------------------------------------------------
cat("Spending data budget...\n")
set.seed(123)
data <-
  read_csv("presentation/data/sample_data.csv") |> 
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

# write out train data for later fitting of optimized workflows
# write_parquet(train, "data/processed/training_data.parquet")

# write out test data for later evaluation
# write_parquet(test, "data/processed/testing_data.parquet")

folds <-
  vfold_cv(train, v = 10)


# Recipes -----------------------------------------------------------------
cat("Creating preprocessing recipes...\n")

in_bed_rec <-
  recipe(
    in_bed ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
    data = train
  ) |>
  step_zv(all_predictors())

in_bed_norm_rec <-
  in_bed_rec |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

sleep_rec <-
  recipe(
    sleep ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
    data = train
  ) |>
  step_zv(all_predictors())

sleep_norm_rec <-
  sleep_rec |>
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

mars_spec <-
  mars(prod_degree = tune(), num_terms = tune()) |> #<- use GCV to choose terms
  set_engine("earth") |>
  set_mode("classification")

rf_spec <-
rand_forest(mtry = tune(), min_n = tune(), trees = tune()) |>
set_engine("ranger") |>
set_mode("classification")

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

normalized_in_bed_wf <-
  workflow_set(
    preproc = list(
      in_bed = in_bed_norm_rec
    ),
    models = list(
      logistic_regression = glmnet_spec, # works in parallel
      neural_network = nnet_spec # works in parallel
    ),
    cross = FALSE
  ) |>
  option_add(param_info = nnet_param, id = "in_bed_neural_network")

normalized_sleep_wf <-
  workflow_set(
    preproc = list(sleep = sleep_norm_rec),
    models = list(
      logistic_regression = glmnet_spec, # works in parallel
      neural_network = nnet_spec # works in parallel)
    ),
    cross = FALSE
  )

no_preproc_in_bed_wf <-
  workflow_set(
    preproc = list(
      in_bed = in_bed_rec
    ),
    models = list(
      MARS = mars_spec, # works in parallel
      random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = FALSE
  )

no_preproc_sleep_wf <-
  workflow_set(
    preproc = list(
      sleep = sleep_rec
    ),
    models = list(
      MARS = mars_spec, # works in parallel
      random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = FALSE
  )

all_in_bed_workflows <-
  bind_rows(normalized_in_bed_wf, no_preproc_in_bed_wf)

all_sleep_workflows <-
  bind_rows(normalized_sleep_wf, no_preproc_sleep_wf)


# Tuning models with regular grid search ----------------------------------
# In bed

doParallel::registerDoParallel()

cat("Tuning the following workflows:\n")
all_in_bed_workflows

tictoc::tic()
grid_ctrl <-
  control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    parallel_over = "everything",
    save_pred = FALSE,
    save_workflow = FALSE
  )

in_bed_grid_results <-
  all_in_bed_workflows |>
  workflow_map(
    seed = 123,
    "tune_grid",
    resamples = folds,
    grid = 10,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc),
    verbose = TRUE
  )
tictoc::toc()

write_rds(in_bed_grid_results, "presentation/data/in_bed_workflowsets_results.rds")

# Sleep

cat("Tuning the following workflows:\n")
all_sleep_workflows

sleep_grid_results <-
  all_sleep_workflows |>
  workflow_map(
    seed = 123,
    "tune_grid",
    resamples = folds,
    grid = 10,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc),
    verbose = TRUE
  )
tictoc::toc()

write_rds(sleep_grid_results, "presentation/data/sleep_workflowsets_results.rds")
