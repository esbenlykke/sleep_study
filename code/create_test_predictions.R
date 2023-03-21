#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)


### Crude

test_crude <-
  read_parquet("data/data_for_modelling/crude_testing_data.parquet")
# filter((id != 1377205) &
#          (id != 1377204 | !noon_day %in% c(9, 25)) &
#          (id != 1377204 | noon_day != 22))
# TODO what causes these outliers?


in_bed_filenames <-
  list.files("/media/esbenlykke/My Passport/crude/fitted_models/axed_models",
    full.names = TRUE
  ) |>
  str_subset("in_bed")

sleep_filenames <-
  list.files("/media/esbenlykke/My Passport/crude/fitted_models/axed_models",
    full.names = TRUE
  ) |>
  str_subset("sleep")

in_bed_fits <- map(in_bed_filenames, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

sleep_fits <- map(sleep_filenames, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

create_crude_preds <- function(fit1, fit2) {
  fit1 %>%
    augment(test_crude) %>%
    rename(pred_in_bed = .pred_class, pred_in_bed_0 = .pred_0, pred_in_bed_1 = .pred_1) %>%
    bind_cols(predict(fit2, test_crude, type = "prob")) %>%
    bind_cols(predict(fit2, test_crude)) %>%
    rename(pred_sleep = .pred_class, pred_sleep_0 = .pred_0, pred_sleep_1 = .pred_1)
}

preds_crude <-
  map2_dfr(in_bed_fits, sleep_fits, create_crude_preds, 
           .progress = TRUE,
           .id = "model")

write_parquet(preds_crude, "data/processed/crude_test_predictions.parquet")


### Binary relevance
test_br <-
  read_parquet("data/data_for_modelling/binary_relevance_testing_data.parquet")

in_bed_asleep_filenames <-
  list.files("/media/esbenlykke/My Passport/binary_relevance/fitted_models/axed_models",
    full.names = TRUE
  ) |>
  str_subset("in_bed_asleep")

in_bed_awake_filenames <-
  list.files("/media/esbenlykke/My Passport/binary_relevance/fitted_models/axed_models",
    full.names = TRUE
  ) |>
  str_subset("in_bed_awake")

out_bed_awake_filenames <-
  list.files("/media/esbenlykke/My Passport/binary_relevance/fitted_models/axed_models",
    full.names = TRUE
  ) |>
  str_subset("out_bed_awake")


in_bed_asleep_fits <- map(in_bed_asleep_filenames, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))


in_bed_awake_fits <- map(in_bed_awake_filenames, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

out_bed_awake_fits <- map(out_bed_awake_filenames, read_rds) |>
  set_names(c("decision_tree", "logistic_regression", "neural_net", "xgboost"))

create_br_preds <- function(fit1, fit2, fit3) {
  fit1 %>%
    augment(test_br) %>%
    rename(pred_in_bed_asleep = .pred_class, pred_in_bed_asleep_0 = .pred_0, pred_in_bed_asleep_1 = .pred_1) %>%
    bind_cols(predict(fit2, test_br, type = "prob")) %>%
    bind_cols(predict(fit2, test_br)) %>%
    rename(pred_in_bed_awake = .pred_class, pred_in_bed_awake_0 = .pred_0, pred_in_bed_awake_1 = .pred_1) %>%
    bind_cols(predict(fit3, test_br, type = "prob")) %>%
    bind_cols(predict(fit3, test_br)) %>%
    rename(pred_out_bed_awake = .pred_class, pred_out_bed_awake_0 = .pred_0, pred_out_bed_awake_1 = .pred_1)
}

preds_br <-
  pmap_dfr(list(in_bed_asleep_fits, in_bed_awake_fits, out_bed_awake_fits), create_br_preds, 
           .id = "model",
           .progress = TRUE)

write_parquet(preds_br, "data/processed/binary_relevance_test_predictions.parquet")



### Multiclass
test_multi <-
  read_parquet("data/data_for_modelling/multiclass_testing_data.parquet") %>%
  mutate(
    multiclass = factor(multiclass, levels = c("in_bed_asleep", "in_bed_awake", "out_bed_awake"))
  )

multiclass_filenames <-
  list.files("/media/esbenlykke/My Passport/multiclass/fitted_models/axed_models",
             full.names = TRUE
  )

multiclass_fits <-
  map(multiclass_filenames, read_rds) %>%
  setNames(c("decision_tree", "decision_tree_SMOTE", "logistic_regression", "neural_net", "xgboost"))

preds_multi <- 
  map_dfr(multiclass_fits, ~ augment(.x, test_multi),
          .progress = TRUE,
          .id = "model")

write_parquet(preds_multi, "data/processed/multiclass_test_predictions.parquet")