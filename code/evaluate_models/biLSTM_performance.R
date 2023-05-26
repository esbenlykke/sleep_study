library(tidyverse)
library(tidymodels)
library(arrow)

preds <-
  read_csv("data/processed/predictions/biLSTM_simple_score_predictions.csv") %>%
  janitor::clean_names()

test <-
  read_parquet("data/data_for_modelling/lstm/30_sec_testing.parquet")

test_preds <-
  tibble(
    predicted_class = rep(1, 20),
    probability_class_0 = rep(0, 20),
    probability_class_1 = rep(1, 20)
  ) %>%
  bind_rows(preds) %>%
  mutate(probability_class_2 = abs(probability_class_0 + probability_class_1 - 1)) %>%
  bind_cols(test) %>%
  mutate(
    across(c(score_simple, predicted_class), as_factor)
  )


my_metrics <- metric_set(accuracy, sensitivity, specificity, precision, f_meas)

metrics <- test_preds %>%
  my_metrics(truth = score_simple, estimate = predicted_class, estimator = "macro_weighted")

test_preds %>% count(id)

p <- test_preds %>%
  filter(id == 54704) %>%
  ggplot(aes(datetime)) +
  geom_line(aes(y = score_simple, group = 1), color = "grey50") +
  geom_line(aes(y = as.numeric(predicted_class) - 1.2, group = 1), color = "darkorange") +
  facet_wrap(~noon_day, scales = "free", ncol = 1) +
  theme_classic()

plotly::ggplotly(p)

valid_metrics <- 
  read_csv("models/lstm_model_metrics.csv") %>% 
  janitor::clean_names()

valid_metrics
