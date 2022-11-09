#!/usr/bin/env Rscript

library(tidyverse)
library(finetune)
library(tidymodels)
library(arrow)
library(parallel)
library(doParallel)

tidymodels_prefer()
options(tidymodels.dark = TRUE)


# initial settings --------------------------------------------------------


ctrl <-
  control_race(
    save_workflow = TRUE,
    verbose = TRUE,
    verbose_elim = TRUE,
    allow_par = TRUE,
    parallel_over = "everything"
  )

my_metrics <-
  metric_set(f_meas, accuracy, precision, sensitivity)


# spend data budget -------------------------------------------------------


data <- 
  read_parquet("data/processed/model_data/bsl_thigh.parquet") 

set.seed(123)
# spl <- 
#   group_initial_split(data, group = id)

spl <- 
  data |> 
  filter(id %in% c(3404, 338304)) |> 
  group_initial_split(group = id, prop = 1/2)

train <- training(spl)
test <- testing(spl)

folds <-
  vfold_cv(train, strata = in_bed, v = 5)


# build XGboost model -----------------------------------------------------


xgboost_recipe <- 
  recipe(in_bed ~ ., data = train) |> 
  update_role(sleep, new_role = "second_target") |> 
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) 

xgboost_spec <- 
  boost_tree(trees = tune()) |> 
  set_mode("classification") |> 
  set_engine("xgboost") 

xgboost_workflow <- 
  workflow() |> 
  add_recipe(xgboost_recipe) |> 
  add_model(xgboost_spec) 

xgboost_grid <- 
  crossing(trees = c(100, 500, 1000))

all_cores <- detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

set.seed(32868)
xgboost_tune <-
  tune_race_anova(xgboost_workflow, 
                  resamples = folds, 
                  grid = xgboost_grid,
                  control = ctrl,
                  metrics = my_metrics)

collect_metrics(xgboost_tune)

xg_fit <- 
  xgboost_tune %>%
  extract_workflow() %>%
  finalize_workflow(select_best(xgboost_tune)) %>%
  fit(train)

xg_fit %>%
  augment(test) %>%
  select(datetime, in_bed, .pred_class) %>%
  pivot_longer(-datetime) %>%
  ggplot(aes(datetime, value, color = name, group = 1)) +
  geom_line(alpha = 0.8, size = 1.2) +
  theme_minimal()

xg_fit |> 
  augment(test) |> 
  f_meas(truth = in_bed, estimate = .pred_class)

xg_fit |> 
  augment(test) |>
  ggplot(aes(group = 1)) +
  geom_step(aes(datetime, in_bed), color = "blue") +
  geom_step(aes(datetime, .pred_class), color = "black")

train |> 
  ggplot(aes(lubridate::as_datetime(datetime), in_bed, group = 1, color = in_bed)) +
  geom_path()