#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)

tidymodels_prefer()
options(tidymodels.dark = TRUE)

# TODO import in-bed data predicted by best in-bed model. Both 10-sec and 30-sec epoch data.
# Also consider a SMOTE variant for waso

folds <-
  group_vfold_cv(train, group = id, v = 5, balance = "groups")


# Recipes -----------------------------------------------------------------
cat("Creating preprocessing recipes...\n")

sleep_raw_rec <-
  recipe(
    sleep ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train
  ) |>
  step_zv(all_predictors())

sleep_raw_norm_rec <-
  sleep_raw_rec |>
  step_normalize(all_numeric_predictors())

sleep_median5_rec <-
  recipe(
    sleep_median5 ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train
  ) |>
  step_zv(all_predictors())

sleep_median5_norm_rec <-
  sleep_median5_rec |>
  step_normalize(all_numeric_predictors())

sleep_median10_rec <-
  recipe(
    sleep_median5 ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train
  ) |>
  step_zv(all_predictors())

sleep_median10_norm_rec <-
  sleep_median10_rec |>
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

rpart_param <-
  CART_spec %>%
  extract_parameter_set_dials() %>%
  update(tree_depth = tree_depth(c(3, 7)))


# Create workflow sets ----------------------------------------------------
cat("Creating workflow sets...\n")

sleep_norm_wfs <-
  workflow_set(
    preproc = list(
      sleep_raw = sleep_raw_norm_rec,
      sleep_5_sec = sleep_median5_norm_rec,
      sleep_10_sec = sleep_median10_norm_rec
    ),
    models = list(
      logistic_regression = glmnet_spec, # works in parallel
      neural_network = nnet_spec # works in parallel
    ),
    cross = TRUE
  ) %>% 
option_add(param_info = nnet_param, id = c("sleep_raw_neural_network", 
                                           "sleep_5_sec_neural_network",
                                           "sleep_10_sec_neural_network"))

sleep_no_norms_wfs <-
  workflow_set(
    preproc = list(
      sleep_raw = sleep_raw_rec,
      sleep_5_sec = sleep_median5_rec,
      sleep_10_sec = sleep_median10_rec
    ),
    models = list(
      decision_tree = CART_spec,
      # MARS = mars_spec, # works in parallel
      # random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = TRUE
  ) %>%
  option_add(param_info = rpart_param, id = c("sleep_raw_decision_tree",
                                              "sleep_5_sec_decision_tree",
                                              "sleep_10_sec_decision_tree"))



decision_tree_wfs <-
  bind_rows(sleep_norm_wfs, sleep_no_norms_wfs) %>% 
  filter(str_detect(wflow_id, "decision"))

log_reg_wfs <-
  bind_rows(sleep_norm_wfs, sleep_no_norms_wfs) %>% 
  filter(str_detect(wflow_id, "logistic"))

nnet_wfs <-
  bind_rows(sleep_norm_wfs, sleep_no_norms_wfs) %>% 
  filter(str_detect(wflow_id, "neural"))

xgb_wfs <-
  bind_rows(sleep_norm_wfs, sleep_no_norms_wfs) %>% 
  filter(str_detect(wflow_id, "xgboost"))



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
  "/media/esbenlykke/My Passport/crude/grid_results/in_bed_workflowsets_results.rds"
)
