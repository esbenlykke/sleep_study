#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(finetune))
suppressMessages(library(tidymodels))
suppressMessages(library(arrow))
suppressMessages(library(stacks))
# suppressMessages(library(baguette))
# suppressMessages(library(tabnet))
# suppressMessages(library(torch))

tidymodels_prefer()
options(tidymodels.dark = TRUE)

cat("Packages loaded...\n")

# Spend data budget -------------------------------------------------------
cat("Spending data budget...\n")

data <-
  read_parquet("data/processed/model_data/bsl_thigh_sensor_independent_features.parquet") |>
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

folds <-
  group_vfold_cv(train, group = id, v = 5, balance = "groups")


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

# glmnet_spec <-
#   logistic_reg(penalty = tune(), mixture = tune()) |>
#   set_mode("classification") |>
#   set_engine("glmnet", verbose = TRUE)

nnet_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) |>
  set_engine("nnet", MaxNWts = 500) |> # MaxNWts is a regularization term based on number of predictors
  set_mode("classification")

mars_spec <-
  mars(prod_degree = tune(), num_terms = tune()) |> #<- use GCV to choose terms
  set_engine("earth") |>
  set_mode("classification")

# svm_linear_spec <-
#   svm_linear(cost = tune()) |>
#   set_engine("kernlab") |>
#   set_mode("classification")

# svm_radial_spec <-
#   svm_rbf(cost = tune(), rbf_sigma = tune()) |>
#   set_engine("kernlab") |>
#   set_mode("classification")

# svm_poly_spec <-
#   svm_poly(cost = tune(), degree = tune()) |>
#   set_engine("kernlab") |>
#   set_mode("classification")

# knn_spec <-
#   nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) |>
#   set_engine("kknn") |>
#   set_mode("classification")

cart_spec <-
  decision_tree(cost_complexity = tune(), min_n = tune()) |>
  set_engine("rpart") |>
  set_mode("classification")

# bag_cart_spec <-
#   bag_tree(min_n = tune(), class_cost = tune()) |>
#   set_engine("C5.0", times = 50L) |>
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

# tabnet_spec <- tabnet(
#   epochs = 5,
#   batch_size = 10000,
#   decision_width = tune(),
#   attention_width = tune(),
#   num_steps = tune()
# ) %>%
#   set_engine("torch", verbose = TRUE) %>%
#   set_mode("classification")


# update nnet params
# nnet_param <-
#   nnet_spec |>
#   extract_parameter_set_dials() |>
#   update(hidden_units = hidden_units(c(1, 27)))


# Create workflow sets ----------------------------------------------------
cat("Creating workflow sets...\n")

normalized_preproc_wf <-
  workflow_set(
    preproc = list(
      in_bed = in_bed_norm_rec,
      sleep = sleep_norm_rec
    ),
    models = list(
      # logistic_regression = glmnet_spec, # works in parallel
      # svm_linear = svm_linear_spec, # works in parallel
      # svm_radial = svm_radial_spec,
      # svm_poly = svm_poly_spec,
      # KNN = knn_spec, # works in parallel
      neural_network = nnet_spec # works in parallel
      # tabnet = tabnet_spec
    ),
    cross = TRUE
  )
# option_add(param_info = nnet_param, id = c("in_bed_norm_zv_neural_network",
#                                            "sleep_norm_zv_neural_network"))

no_preproc_wf <-
  workflow_set(
    preproc = list(
      in_bed = in_bed_rec,
      sleep = sleep_rec
    ),
    models = list(
      # MARS = mars_spec, # works in parallel
      CART = cart_spec, # works in parallel
      # CART_bagged = bag_cart_spec, # works in parallel
      # random_forest = rf_spec, # works in parallel
      xgboost = xgb_spec # works in parallel
    ),
    cross = TRUE
  )

all_workflows <-
  bind_rows(normalized_preproc_wf, no_preproc_wf) |>
  mutate(wflow_id = str_remove_all(wflow_id, "simple_|normalized_"))


# Setup parallel cluster --------------------------------------------------


doParallel::registerDoParallel(cores = 8)


# Tuning models with regular grid search ----------------------------------

tictoc::tic()
grid_ctrl <-
  control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    parallel_over = "everything",
    save_pred = TRUE,
    save_workflow = TRUE
  )

grid_results <-
  all_workflows |>
  workflow_map(
    seed = 123,
    "tune_grid",
    resamples = folds,
    grid = 5,
    control = grid_ctrl,
    metrics = metric_set(f_meas, roc_auc),
    verbose = TRUE,
  )
tictoc::toc()

write_rds(grid_results, "data/models/grid_results.rds", compress = "xz")

# Tuning models with race method ------------------------------------------

# tictoc::tic()
# race_ctrl <-
#   control_race(
#     save_pred = TRUE,
#     verbose = TRUE,
#     allow_par = TRUE,
#     parallel_over = "resamples",
#     save_workflow = TRUE
#   )
#
#
# race_results <-
#   all_workflows |>
#   workflow_map(
#     "tune_race_anova",
#     seed = 123,
#     resamples = folds,
#     grid = 5,
#     control = race_ctrl,
#     metrics = metric_set(f_meas, roc_auc),
#     verbose = TRUE
#   )
#
# write_rds(race_results, "data/models/race_results.rds")
# tictoc::toc()

# stack_ctrl <-
#   control_stack_resamples()
#
# resamples_results <-
#   all_workflows |>
#   workflow_map(
#     "fit_resamples",
#     seed = 123,
#     resamples = folds,
#     control = stack_ctrl,
#     metrics = metric_set(f_meas, roc_auc),
#     verbose = TRUE
#   )
#
# write_rds(resamples_results, "data/models/resamples_results.rds")

# Evaluate tuning results -------------------------------------------------


# autoplot(
#   grid_results,
#   rank_metric = "f_meas",
#   metric = "f_meas",
#   select_best = TRUE
# ) +
#   geom_text(aes(y = mean - .01, label = wflow_id), angle = 90, hjust = 1) +
#   lims(y = c(.86, 1)) +
#   labs(
#     title = "Best performing models from hyperparameter tuning on 5-fold CV",
#     subtitle = "RF, KNN, and tabnet were very slow...",
#     y = "F1 Metric",
#     x = "Model Rank"
#   ) +
#   scale_x_continuous(
#     breaks = seq(1, 10, 1)
#   ) +
#   theme_classic() +
#   theme(
#     legend.position = "none",
#     plot.title.position = "plot"
#   )
#
# ggsave("visuals/many_models_test.png", height = 4, width = 8)


# Finalizing model --------------------------------------------------------

# best_results <-
#   grid_results %>%
#   extract_workflow_set_result("random_forest") %>%
#   select_best(metric = "f_meas")
#
# ctrl_fit <- control_parsnip(verbosity = 2L)
#
# rf_fit <-
#   grid_results %>%
#   extract_workflow("random_forest") %>%
#   finalize_workflow(best_results) %>%
#   fit(train)
#
# rf_fit |>
#   augment(test) |>
#   select(datetime, in_bed, .pred_class) |>
#   pivot_longer(-datetime) |>
#   ggplot(aes(group = 1, color = name)) +
#   geom_step(aes(datetime, value)) +
#   facet_wrap(~ name, ncol = 1) +
#   theme_light()


# Stack in_bed models -----------------------------------------------------
cat("Creating model stack for improved performance...\n")
in_bed_data_stack <-
  stacks() |>
  # add_candidates(extract_workflow_set_result(grid_results, "in_bed_random_forest"), "in_bed_random_forest") |>
  add_candidates(extract_workflow_set_result(grid_results, "in_bed_xgboost"), "in_bed_xgboost") |>
  add_candidates(extract_workflow_set_result(grid_results, "in_bed_neural_network"), "in_bed_neural_network") |>
  add_candidates(extract_workflow_set_result(grid_results, "in_bed_CART"), "in_bed_CART")
# add_candidates(extract_workflow_set_result(grid_results, "in_bed_MARS"), "in_bed_MARS")

in_bed_model_stack <-
  in_bed_data_stack |>
  blend_predictions() |>
  fit_members()

write_rds("data/models/in_bed_model_stack.rds", compress = "xz")

sleep_data_stack <-
  stacks() |>
  # add_candidates(extract_workflow_set_result(grid_results, "sleep_random_forest"), "sleep_random_forest") |>
  add_candidates(extract_workflow_set_result(grid_results, "sleep_xgboost"), "sleep_xgboost") |>
  add_candidates(extract_workflow_set_result(grid_results, "sleep_neural_network"), "sleep_neural_network") |>
  add_candidates(extract_workflow_set_result(grid_results, "sleep_CART"), "sleep_CART")
# add_candidates(extract_workflow_set_result(grid_results, "sleep_MARS"), "sleep_MARS")

sleep_model_stack <-
  sleep_data_stack |>
  blend_predictions() |>
  fit_members()

write_rds("data/models/sleep_model_stack.rds", compress = "xz")

beepr::beep(4)
