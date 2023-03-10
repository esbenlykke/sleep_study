#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)


tidymodels_prefer()
options(tidymodels.dark = TRUE)

cat("Packages loaded...\n")

path <- "/media/esbenlykke/My Passport/multiclass/"

# Spend data budget -------------------------------------------------------
cat("Spending data budget...\n")
set.seed(123)

data <-
  read_parquet("data/data_for_modelling/all_data_incl_sensor_independent_features.parquet") |>
  mutate(
    in_bed = as_factor(in_bed),
    sleep = as_factor(sleep),
    multiclass = case_when(
      in_bed == 1 & sleep == 1 ~ "in_bed_asleep",
      in_bed == 1 & sleep == 0 ~ "in_bed_awake",
      in_bed == 0 & sleep == 0 ~ "out_bed_awake"
    )
  )

# ids <- data %>% distinct(id) %>% slice(1:10)
#   
# data <- data %>% 
#   filter(id %in% ids$id) %>% 
#   group_by(id, multiclass) %>% 
#   slice_sample(n = 1000) %>% 
#   ungroup()

set.seed(123)
spl <-
  data |>
  group_initial_split(group = id, prop = .5)

train <- training(spl)
test <- testing(spl)

# write out train data for later fitting of optimized workflows
# write_parquet(train, "data/data_for_modelling/multiclass_training_data.parquet")

# write out test data for later evaluation
# write_parquet(test, "data/data_for_modelling/multiclass_testing_data.parquet")

folds <-
  group_vfold_cv(train, group = id, v = 5, balance = "groups")


# Recipes -----------------------------------------------------------------
cat("Creating preprocessing recipe...\n")

multiclass_rec <-
  recipe(
    multiclass ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
    data = train
  ) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())



# Model specifications ----------------------------------------------------

glmnet_spec <-
  multinom_reg(penalty = tune(), mixture = tune()) |>
  set_mode("classification") |>
  set_engine("glmnet", verbose = TRUE)

nnet_spec <-
  mlp(penalty = tune(), hidden_units = tune(), epochs = tune()) |>
  set_engine("nnet") |>
  set_mode("classification")

# svm_spec <- 
#   svm_rbf(cost = tune(), rbf_sigma = tune()
# ) %>% 
#   set_mode("classification") %>% 
#   set_engine("kernlab")

xgb_spec <-
  boost_tree(
    tree_depth = tune(), learn_rate = tune(),
    loss_reduction = tune(), min_n = tune(),
    sample_size = tune(), trees = seq(100, 700, 200)
  ) |>
  set_engine("xgboost", verbose = TRUE) |>
  set_mode("classification")

# Create workflow sets ----------------------------------------------------
cat("Creating workflow sets...\n")

multiclass_wf <-
  workflow_set(
    preproc = list(
      multiclass_rec = multiclass_rec
    ),
    models = list(
      logistic_regression = glmnet_spec, # works in parallel
      nnet_regression = nnet_spec, # works in parallel
      # svm_rbf = svm_spec,
      xgboost = xgb_spec
    ),
    cross = FALSE
  ) 


# Setup parallel cluster --------------------------------------------------


doParallel::registerDoParallel(cores = 6)


# Tuning models with regular grid search ----------------------------------

cat("Tuning multiclass workflows:\n")
tibble(workflow = multiclass_wf$wflow_id)

tictoc::tic()
grid_ctrl <-
  control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    parallel_over = "everything",
    save_pred = FALSE,
    save_workflow = FALSE
  )

grid_results <-
  multiclass_wf |>
  workflow_map(
    seed = 123,
    "tune_grid",
    resamples = folds,
    grid = 5,
    control = grid_ctrl,
    metrics = metric_set(f_meas),
    verbose = TRUE
  )
tictoc::toc()

write_rds(grid_results, str_c(path, "grid_results/multiclass_workflowsets_results.rds"))

