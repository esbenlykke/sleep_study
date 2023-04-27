#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(themis)


tidymodels_prefer()
options(tidymodels.dark = TRUE)

cat("Packages loaded...\n")

# Spend data budget -------------------------------------------------------
cat("Spending data budget...\n")

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

# write test data to file for use in later stages
# write_parquet(train, "data/data_for_modelling/binary_relevance_training_data.parquet")

folds <-
  group_vfold_cv(train, group = id, v = 5, balance = "groups")


# Create preprocessors ----------------------------------------------------

in_bed_asleep_rec <-
  recipe(
    in_bed_asleep ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
    data = train
  ) |>
  step_zv(all_predictors())

in_bed_awake_rec <-
  recipe(
    in_bed_awake ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
    data = train
  ) |>
  step_zv(all_predictors()) |> 
  step_smote(in_bed_awake)

out_bed_awake_rec <-
  recipe(
    out_bed_awake ~ age + incl + temp + macc_x + macc_y + macc_z +
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
                       tree_depth(range = c(3, 7)),
                       size = 15
  )


# Workflows ---------------------------------------------------------------

in_bed_asleep_wf <-
  workflow(in_bed_asleep_rec, cart_spec)

in_bed_awake_wf <-
  workflow(in_bed_awake_rec, cart_spec)

out_bed_awake_wf <- 
  workflow(out_bed_awake_rec, cart_spec)


grid_ctrl <-
  control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    event_level = "second",
    parallel_over = "everything",
    save_pred = FALSE,
    save_workflow = TRUE
  )

# Setup parallel back-end -------------------------------------------------


doParallel::registerDoParallel(cores = 6)


# Tune grids --------------------------------------------------------------

cat("Tune grid for in-bed asleep models\n")
# In-bed asleep
tictoc::tic()
in_bed_asleep_CART_results <-
  in_bed_asleep_wf |>
  tune_grid(
    resamples = folds,
    grid = tree_grid,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc)
  )
tictoc::toc()

write_rds(in_bed_asleep_CART_results, "/media/esbenlykke/My Passport/grid_results/in_bed_asleep_simple_CART_results.rds")


# cat("Tune grid for in-bed awake models\n")

# in-bed awake
tictoc::tic()
in_bed_awake_CART_results <-
  in_bed_awake_wf |>
  tune_grid(
    resamples = folds,
    grid = tree_grid,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc)
  )
tictoc::toc()

write_rds(in_bed_awake_CART_results, "/media/esbenlykke/My Passport/grid_results/in_bed_awake_simple_CART_results.rds")

rm(in_bed_awake_CART_results)
gc()


# out-bed awake
# 
cat("Tune grid for out-bed awake models\n")

tictoc::tic()
out_bed_awake_CART_results <-
  out_bed_awake_wf |>
  tune_grid(
    resamples = folds,
    grid = tree_grid,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc)
  )
tictoc::toc()

write_rds(out_bed_awake_CART_results, "/media/esbenlykke/My Passport/grid_results/out_bed_awake_simple_CART_results.rds")

rm(out_bed_awake_CART_results)
gc()

# Finalizing model --------------------------------------------------------
# In-bed asleep
ctrl_fit <- control_parsnip(verbosity = 2L)

in_bed_asleep_CART_results <- 
  read_rds("/media/esbenlykke/My Passport/grid_results/in_bed_asleep_simple_CART_results.rds")
 
best_in_bed_asleep_results <-
  in_bed_asleep_CART_results %>%
  select_best(metric = "f_meas")

in_bed_asleep_CART_fit <-
  in_bed_asleep_wf %>%
  finalize_workflow(best_in_bed_asleep_results) %>%
  fit(train)

write_rds(in_bed_asleep_CART_fit, "/media/esbenlykke/My Passport/fitted_models/in_bed_asleep_simple_CART_fit.rds")

# in-bed awake
in_bed_awake_CART_results <- 
  read_rds("/media/esbenlykke/My Passport/grid_results/in_bed_awake_simple_CART_results.rds")

best_in_bed_awake_results <-
  in_bed_awake_CART_results %>%
  select_best(metric = "f_meas")

in_bed_awake_CART_fit <-
  in_bed_awake_wf %>%
  finalize_workflow(best_in_bed_awake_results) %>%
  fit(train)

write_rds(in_bed_awake_CART_fit, "/media/esbenlykke/My Passport/fitted_models/in_bed_awake_simple_CART_fit.rds")

# out bed awake
out_bed_awake_CART_results <- 
  read_rds("/media/esbenlykke/My Passport/grid_results/out_bed_awake_simple_CART_results.rds")

best_out_bed_awake_results <-
  out_bed_awake_CART_results %>%
  select_best(metric = "f_meas")

out_bed_awake_CART_fit <-
  out_bed_awake_wf %>%
  finalize_workflow(best_out_bed_awake_results) %>%
  fit(train)

write_rds(out_bed_awake_CART_fit, "/media/esbenlykke/My Passport/fitted_models/out_bed_awake_simple_CART_fit.rds")
