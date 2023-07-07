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
    ), as_factor)
  )


# write_parquet(test_preds, "data/data_for_modelling/lstm/predictions/lstm_multiclass_preds.parquet")


my_metrics <- metric_set(f_meas, precision, npv, sensitivity, specificity)

get_metrics <- function(data, truth, estimate) {
  data %>%
    my_metrics(truth = {{ truth }}, estimate = {{ estimate }}, 
               event_level = "second", estimator = "macro")
}

truths <- c("score_simple", "score_simple_median_5", "score_simple_median_10")
estimates <- c("predicted_class_raw", "predicted_class_median_5", "predicted_class_median_10")

results <- 
  map2_dfr(truths, estimates, ~ get_metrics(test_preds, .x, .y), .id = "type") %>% 
  mutate(type = case_when(type == 1 ~ "raw",
                          type == 2 ~ "median_5", 
                          type == 3 ~ "median_10")) %>% 
  select(type, .metric, .estimate) 


write_csv(results, "data/processed/biLSTM_performance_metrics.csv")
