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

train <- read_parquet("data/data_for_modelling/crude_training_data.parquet")
test <- read_parquet("data/data_for_modelling/crude_testing_data.parquet")

folds <-
  group_vfold_cv(train, group = id, v = 5, balance = "groups")


# Create preprocessor -----------------------------------------------------

in_bed_rec <-
  recipe(
    in_bed ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear +
      x_sd_long + y_sd_long + z_sd_long,
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

# Setup parallel back-end -------------------------------------------------


doParallel::registerDoParallel(cores = 6)


# Workflows ---------------------------------------------------------------

in_bed_wf <-
  workflow(in_bed_rec, cart_spec)


# Tune grid ---------------------------------------------------------------

cat("Tune grid for in-bed models\n")
# In bed
tictoc::tic()
grid_ctrl <-
  control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    event_level = "second",
    parallel_over = "everything",
    save_pred = FALSE,
    save_workflow = TRUE
  )

in_bed_CART_results <-
  in_bed_wf |>
  tune_grid(
    resamples = folds,
    grid = tree_grid,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc)
  )
tictoc::toc()

write_rds(in_bed_CART_results, str_c(path, "grid_results/in_bed_simple_tree_results.rds"))



# Finalize in bed model ---------------------------------------------------


best_in_bed_results <-
  in_bed_CART_results %>%
  select_best(metric = "f_meas")

ctrl_fit <- control_parsnip(verbosity = 2L)

in_bed_CART_fit <-
  in_bed_wf %>%
  finalize_workflow(best_in_bed_results) %>%
  fit(train)

write_rds(in_bed_CART_fit, str_c(path, "fitted_models/in_bed_simple_tree_fit.rds"))


axe_a_lot <- function(model_list) {
  model_list |>
    axe_env() |>
    axe_data() |>
    axe_fitted() %>%
    axe_call()
}

axe_a_lot(in_bed_CART_fit) %>% 
  write_rds(str_c(path, "fitted_models/axed_models/in_bed_simple_tree_fit_AXED.rds"))
