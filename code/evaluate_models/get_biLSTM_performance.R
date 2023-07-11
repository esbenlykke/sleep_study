library(tidyverse)
library(tidymodels)
library(arrow)

# Define the renaming function
rename_cols <- function(df, suffix) {
  df %>%
    rename_with(.fn = ~ paste0(.x, "_", suffix))
}

preds_filenames <-
  list.files("data/processed/predictions/", full.names = TRUE)

preds <-
  map(preds_filenames, ~ read_csv(.x) %>%
    janitor::clean_names()) %>%
  set_names(c("raw", "median_5", "median_10")) %>%
  imap(~ rename_cols(.x, .y)) %>%
  reduce(bind_cols)

test <-
  read_parquet("data/data_for_modelling/lstm/30_sec_testing.parquet")

test_preds <-
  tibble(
    predicted_class_raw = rep(1, 20),
    probability_class_0_raw = rep(0, 20),
    probability_class_1_raw = rep(0, 20),
    predicted_class_median_5 = rep(1, 20),
    probability_class_0_median_5 = rep(0, 20),
    probability_class_1_median_5 = rep(0, 20),
    predicted_class_median_10 = rep(1, 20),
    probability_class_0_median_10 = rep(0, 20),
    probability_class_1_median_10 = rep(0, 20)
  ) %>%
  bind_rows(preds) %>%
  bind_cols(test) %>%
  mutate(
    across(c(
      score_simple, score_simple_median_5, score_simple_median_10,
      matches("predicted|probability")
    ), as_factor),
    in_bed_raw = if_else(score_simple %in% c(0, 2), 1, 0),
    pred_in_bed_raw = if_else(predicted_class_raw %in% c(0, 2), 1, 0),
    sleep_raw = if_else(score_simple == 2 & in_bed_raw == 1, 1, 0),
    pred_sleep_raw = if_else(predicted_class_raw == 2 & pred_in_bed_raw == 1, 1, 0),
    
    in_bed_median_5 = if_else(score_simple %in% c(0, 2), 1, 0),
    pred_in_bed_median_5 = if_else(predicted_class_median_5 %in% c(0, 2), 1, 0),
    sleep_median_5 = if_else(score_simple == 2 & in_bed_median_5 == 1, 1, 0),
    pred_sleep_median_5 = if_else(predicted_class_median_5 == 2 & pred_in_bed_median_5 == 1, 1, 0),
    
    in_bed_median_10 = if_else(score_simple %in% c(0, 2), 1, 0),
    pred_in_bed_median_10 = if_else(predicted_class_median_10 %in% c(0, 2), 1, 0),
    sleep_median_10 = if_else(score_simple == 2 & in_bed_median_10 == 1, 1, 0),
    pred_sleep_median_10 = if_else(predicted_class_median_10 == 2 & pred_in_bed_median_10 == 1, 1, 0),
    across(in_bed_raw:pred_sleep_median_10, as_factor)
  )

# write_parquet(test_preds, "data/data_for_modelling/lstm/predictions/lstm_multiclass_preds.parquet")


my_metrics_in_bed <- metric_set(f_meas, precision, accuracy, sensitivity, specificity)
my_metrics_sleep <- metric_set(f_meas, precision, npv, sensitivity, specificity)

get_metrics <- function(data, truth, estimate, filter = FALSE, filter_var) {
  if (filter) {
    data %>%
      filter(!!sym(filter_var) == 1) %>%
      my_metrics_sleep(
        truth = {{ truth }}, estimate = {{ estimate }},
        event_level = "second", estimator = "macro"
      )
  } else {
    data %>%
      my_metrics_in_bed(
        truth = {{ truth }}, estimate = {{ estimate }},
        event_level = "second", estimator = "macro"
      )
  }
}

# truths <- c("score_simple", "score_simple_median_5", "score_simple_median_10")
# estimates <- c("predicted_class_raw", "predicted_class_median_5", "predicted_class_median_10")
in_bed_truths <- c("in_bed_raw", "in_bed_median_5", "in_bed_median_10")
sleep_truths <- c("sleep_raw", "sleep_median_5", "sleep_median_10")
in_bed_estimates <- c("pred_in_bed_raw", "pred_in_bed_median_5", "pred_in_bed_median_10")
sleep_estimates <- c("pred_sleep_raw", "pred_sleep_median_5", "pred_sleep_median_10")

in_bed_results <-
  map2_dfr(in_bed_truths, in_bed_estimates, ~ get_metrics(test_preds, .x, .y), .id = "type") %>%
  mutate(type = case_when(
    type == 1 ~ "raw",
    type == 2 ~ "median_5",
    type == 3 ~ "median_10"
  ),
  model = "biLSTM") %>%
  select(type, model, .metric, .estimate)

write_csv(in_bed_results, "data/processed/biLSTM_in_bed_performance_metrics.csv")

sleep_results <-
  pmap_dfr(list(sleep_truths, sleep_estimates, in_bed_estimates), ~ 
             get_metrics(test_preds, ..1, ..2, filter = TRUE, filter_var = ..3), .id = "type") %>%
  mutate(type = case_when(
    type == 1 ~ "raw",
    type == 2 ~ "median_5",
    type == 3 ~ "median_10"
  ),
  model = "biLSTM") %>%
  select(type, model, .metric, .estimate, .estimator)

write_csv(sleep_results, "data/processed/biLSTM_sleep_performance_metrics.csv")

