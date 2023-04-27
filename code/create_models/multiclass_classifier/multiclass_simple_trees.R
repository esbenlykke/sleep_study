#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(themis)


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
    multiclass = as_factor(case_when(
      in_bed == 1 & sleep == 1 ~ "in_bed_asleep",
      in_bed == 1 & sleep == 0 ~ "in_bed_awake",
      in_bed == 0 & sleep == 0 ~ "out_bed_awake"
    ))
  )

### for testing purposes
# ids <- data %>% distinct(id) %>% slice(1:10)
# 
# data <- data %>%
#   filter(id %in% ids$id) %>%
#   group_by(id, multiclass) %>%
#   slice_sample(n = 100) %>%
#   ungroup()
###


set.seed(123)
spl <-
  data |>
  group_initial_split(group = id, prop = .5)

train <- training(spl)
test <- testing(spl)

# write test data to file for use in later stages
# write_parquet(test, "data/processed/screens_test_data.parquet")

folds <-
  group_vfold_cv(train, group = id, v = 5, balance = "groups")


# Create preprocessors ----------------------------------------------------


cat("Creating preprocessing recipe...\n")

multiclass_rec <-
  recipe(
    multiclass ~ age + incl + temp + macc_x + macc_y + macc_z +
      sdacc_x + sdacc_y + sdacc_z + sdmax + temp_sd + clock_proxy_cos + clock_proxy_linear,
    data = train
  ) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

smote_rec <- 
  multiclass_rec %>% 
  step_smote(multiclass)


# Create CART model spec --------------------------------------------------

cart_spec <-
  decision_tree(cost_complexity = tune(), min_n = tune(), tree_depth = tune()) |>
  set_engine("rpart") |>
  set_mode("classification")

tree_grid <-
  grid_latin_hypercube(cost_complexity(),
                       min_n(),
                       tree_depth(range = c(3, 7)),
                       size = 5
  )

# Setup parallel back-end -------------------------------------------------


# doParallel::registerDoParallel(cores = 6)


# Workflows ---------------------------------------------------------------

multiclass_wf <- 
  workflow_set(preproc = list(base = multiclass_rec, smote = smote_rec),
               models = list(cart_spec))

# Tune grids --------------------------------------------------------------

cat("Tune grid for multiclass simple trees\n")

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

grid_results <-
  multiclass_wf |>
  workflow_map(
    seed = 123,
    "tune_grid",
    resamples = folds,
    grid = tree_grid,
    control = grid_ctrl,
    metrics = metric_set(f_meas),
    verbose = TRUE
  )
tictoc::toc()

write_rds(grid_results, str_c(path, "grid_results/multiclass_simple_tree_results.rds"))

# Finalizing model --------------------------------------------------------

best_no_SMOTE <- 
  grid_results %>% 
  extract_workflow_set_result("base_decision_tree") |> 
  select_best(metric = "f_meas") 

best_no_SMOTE_fit <- 
  grid_results %>% 
  extract_workflow("base_decision_tree") %>% 
  finalize_workflow(best_no_SMOTE) %>% 
  fit(train)

write_rds(best_no_SMOTE_fit, str_c(path, "fitted_models/multiclass_simple_tree_fit.rds"))


best_SMOTE <- 
  grid_results %>%
  extract_workflow_set_result("smote_decision_tree") |> 
  select_best(metric = "f_meas")

best_SMOTE_fit <- 
  grid_results %>% 
  extract_workflow("smote_decision_tree") %>% 
  finalize_workflow(best_SMOTE) %>% 
  fit(train)

write_rds(best_SMOTE_fit, str_c(path, "fitted_models/multiclass_simple_tree_SMOTE_fit.rds"))
