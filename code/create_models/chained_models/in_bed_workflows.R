#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)

tidymodels_prefer()
options(tidymodels.dark = TRUE)

train <- 
  read_parquet("data/data_for_modelling/chained_classifiers/training_10_sec_data.parquet") %>% 
  filter(id %in% c(3404, 8505, 54704, 55704, 132804))

# write out train data for later fitting of optimized workflows
# write_parquet(train, "data/processed/training_data.parquet")

# write out test data for later evaluation
# write_parquet(test, "data/processed/testing_data.parquet")

folds <-
  group_mc_cv(train, group = id, times = 5, prop = .5)


# Recipes -----------------------------------------------------------------
cat("Creating preprocessing recipes...\n")

in_bed_rec <-
  recipe(
    in_bed ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + 
      clock_proxy_linear + x_sd_long + y_sd_long + z_sd_long,
    data = train
  ) |>
  step_zv(all_predictors())

in_bed_norm_rec <-
  in_bed_rec |>
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
    sample_size = tune(), trees = seq(200, 1000, 200)
  ) |>
  set_engine("xgboost", verbose = TRUE) |>
  set_mode("classification")



# update nnet params
nnet_param <-
  nnet_spec |>
  extract_parameter_set_dials() |>
  update(hidden_units = hidden_units(c(5, 27)))

rpart_param <-
  CART_spec %>%
  extract_parameter_set_dials() %>%
  update(tree_depth = tree_depth(c(3, 7)))


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


no_preproc_in_bed_wf <-
  workflow_set(
    preproc = list(
      in_bed = in_bed_rec
    ),
    models = list(
      decision_tree = CART_spec,
      # MARS = mars_spec, # works in parallel
      # random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = FALSE
  )

all_in_bed_workflows <-
  bind_rows(normalized_in_bed_wf, no_preproc_in_bed_wf)


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

# In bed

cat("Tuning the following workflows:\n")
all_in_bed_workflows

tictoc::tic()
in_bed_grid_results <-
  all_in_bed_workflows |>
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

write_rds(
  in_bed_grid_results,
  "/media/esbenlykke/My Passport/chained_models/grid_results/in_bed/in_bed_workflowsets_results_10_sec.rds"
)


### 30 second epohs ###

train_30_sec <-
  read_parquet("data/data_for_modelling/chained_classifiers/training_30_sec_data.parquet") %>% 
  filter(id %in% c(8504, 8505, 37304, 132804, 182604))

folds_30_sec <-
  group_mc_cv(train_30_sec, group = id, times = 5, prop = .5)

in_bed_rec_30_sec <-
  recipe(
    in_bed ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train_30_sec
  ) |>
  step_zv(all_predictors())

in_bed_norm_rec_30_sec <-
  in_bed_rec_30_sec |>
  step_normalize(all_numeric_predictors())

normalized_in_bed_30_sec_wf <-
  workflow_set(
    preproc = list(
      in_bed = in_bed_norm_rec_30_sec
    ),
    models = list(
      logistic_regression = glmnet_spec, # works in parallel
      neural_network = nnet_spec # works in parallel
    ),
    cross = FALSE
  ) |>
  option_add(param_info = nnet_param, id = "in_bed_neural_network")

no_preproc_in_bed_30_sec_wf <-
  workflow_set(
    preproc = list(
      in_bed = in_bed_rec_30_sec
    ),
    models = list(
      decision_tree = CART_spec,
      # MARS = mars_spec, # works in parallel
      # random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = FALSE
  )

all_in_bed_workflows_30_sec <-
  bind_rows(normalized_in_bed_30_sec_wf, no_preproc_in_bed_30_sec_wf)


# In bed

cat("Tuning the following workflows:\n")
all_in_bed_workflows_30_sec

tictoc::tic()
in_bed_grid_results_30_sec <-
  all_in_bed_workflows_30_sec |>
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

write_rds(
  in_bed_grid_results_30_sec,
  "/media/esbenlykke/My Passport/chained_models/grid_results/in_bed/in_bed_workflowsets_results_30_sec.rds"
)
