#!/usr/bin/env Rscript

library(tidyverse)
library(arrow)
library(furrr)
library(butcher)
library(tidymodels)


tidymodels_prefer()
options(tidymodels.dark = TRUE)

cat("Packages loaded...\n")

path <- "/media/esbenlykke/My Passport/crude/"

# Spend data budget -------------------------------------------------------
cat("Spending data budget...\n")

train_10 <- read_parquet("data/data_for_modelling/crude_training_data.parquet")
test_10 <- read_parquet("data/data_for_modelling/crude_testing_data.parquet")

train_30 <- read_parquet("data/data_for_modelling/crude_training_30_sec_data.parquet")
test_30 <- read_parquet("data/data_for_modelling/crude_testing_30_sec_data.parquet")

folds_10 <-
  group_mc_cv(train_10, group = id, times = 5, prop = .5)

folds_30 <-
  group_mc_cv(train_30, group = id, times = 5, prop = .5)


# Create preprocessor -----------------------------------------------------

in_bed_rec_10 <-
  recipe(
    in_bed ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train_10
  ) |>
  step_zv(all_predictors())

in_bed_rec_30 <-
  recipe(
    in_bed ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
    data = train_30
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

# Setup parallel back-end -------------------------------------------------


doParallel::registerDoParallel(cores = 6)


# Workflows ---------------------------------------------------------------

in_bed_wf_10 <-
  workflow(in_bed_rec_10, cart_spec)

in_bed_wf_30 <-
  workflow(in_bed_rec_30, cart_spec)

# Tune grids --------------------------------------------------------------

grid_ctrl <-
  control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    event_level = "second",
    parallel_over = "everything",
    save_pred = FALSE,
    save_workflow = TRUE
  )

cat("Tune grid for in-bed models\n")

tictoc::tic()
in_bed_CART_results_10 <-
  in_bed_wf_10 |>
  tune_grid(
    resamples = folds_10,
    grid = tree_grid,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc)
  )
tictoc::toc()

write_rds(in_bed_CART_results_10, str_c(path, "grid_results/in_bed_simple_tree_results_10.rds"))

tictoc::tic()
in_bed_CART_results_30 <-
  in_bed_wf_30 |>
  tune_grid(
    resamples = folds_30,
    grid = tree_grid,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc)
  )
tictoc::toc()

write_rds(in_bed_CART_results_30, str_c(path, "grid_results/in_bed_simple_tree_results_30.rds"))


# Finalize in bed model ---------------------------------------------------


best_in_bed_results_10 <-
  in_bed_CART_results_10 %>%
  select_best(metric = "f_meas")

ctrl_fit <- control_parsnip(verbosity = 2L)

in_bed_CART_fit_10 <-
  in_bed_wf_10 %>%
  finalize_workflow(best_in_bed_results_10) %>%
  fit(train_10)

write_rds(in_bed_CART_fit_10, str_c(path, "fitted_models/in_bed_simple_tree_fit_10.rds"))

best_in_bed_results_30 <-
  in_bed_CART_results_30 %>%
  select_best(metric = "f_meas")

in_bed_CART_fit_30 <-
  in_bed_wf_30 %>%
  finalize_workflow(best_in_bed_results_30) %>%
  fit(train_30)

write_rds(in_bed_CART_fit_30, str_c(path, "fitted_models/in_bed_simple_tree_fit_30.rds"))


axe_a_lot <- function(model_list) {
  model_list |>
    axe_env() |>
    axe_data() |>
    axe_fitted() %>%
    axe_call()
}

axe_a_lot(in_bed_CART_fit_10) %>%
  write_rds(str_c(path, "fitted_models/axed_models/in_bed_simple_tree_fit_10_AXED.rds"))

axe_a_lot(in_bed_CART_fit_30) %>%
  write_rds(str_c(path, "fitted_models/axed_models/in_bed_simple_tree_fit_30_AXED.rds"))
