library(tidyverse)
library(glue)
library(gt)
library(showtext)


stats <-
  read_csv(here::here("data/processed/all_boostrap_ba_cis.csv")) %>%
  mutate(
    variable = str_remove(variable, "diff_")
  ) %>%
  left_join(read_csv(here::here("data/processed/all_cors.csv")),
    by = c("type", "model", "variable" = "correlation")
  )

tbl_ba_cor <-
  stats %>%
  mutate(
    model = factor(model, levels = c(
      "decision_tree", "logistic_regression",
      "neural_network", "xgboost", "biLSTM"
    ))
  ) %>%
  arrange(model, desc(type)) %>%
  mutate(
    across(bias:loa_lower_ci_upper, ~ round(.x, 1)),
    across(estimate:upper.ci, ~ round(.x, 2))
  ) %>%
  transmute(
    type = type, model = model, variable = variable,
    bias = glue("{bias} ({bias_ci_lower};{bias_ci_upper})"),
    lower_loa = glue("{loa_lower} ({loa_lower_ci_lower};{loa_lower_ci_upper})"),
    upper_loa = glue("{loa_upper} ({loa_upper_ci_lower};{loa_upper_ci_upper})"),
    pearson = glue("{estimate} ({lower.ci};{upper.ci})")
  ) %>%
  mutate(
    type = case_when(
      type == "raw" ~ "Raw ZM Predictions",
      type == "median5" ~ "5-Min Median",
      type == "median10" ~ "10-Min Median"
    ),
    model = case_when(
      model == "decision_tree" ~ "Decision Tree",
      model == "logistic_regression" ~ "Logistic Regression",
      model == "neural_network" ~ "Feed-Forward Neural Net",
      model == "xgboost" ~ "XGboost",
      TRUE ~ model
    ),
    variable = str_to_upper(variable),
    variable = case_when(
      variable == "SPT" ~ "SPT (min)",
      variable == "TST" ~ "TST (min)",
      variable == "SE" ~ "SE (%)",
      variable == "LPS" ~ "LPS (min)",
      variable == "WASO" ~ "WASO (min)"
    )
  ) %>%
  gt(groupname_col = c("type", "model")) %>%
  cols_label(
    bias = "Bias (95% CI)", lower_loa = "lower LOA (95% CI)",
    upper_loa = "upper LOA (95% CI)", pearson = md("Pearson, _r_ (95% CI)"),
    variable = ""
  ) %>%
  cols_align(align = "right", columns = bias:pearson) %>%
  cols_width(variable ~ px(200)) %>%
  fmt_markdown(pearson)


### short table ###
short_tbl_ba_cor <-
  stats %>%
  mutate(
    model = factor(model, levels = c(
      "decision_tree", "logistic_regression",
      "neural_network", "xgboost", "biLSTM"
    ))
  ) %>%
  arrange(model, desc(type)) %>%
  mutate(
    across(bias:loa_lower_ci_upper, ~ round(.x, 1)),
    across(estimate:upper.ci, ~ round(.x, 2))
  ) %>%
  transmute(
    type = type, model = model, variable = variable,
    bias = glue("{bias} ({bias_ci_lower};{bias_ci_upper})"),
    lower_loa = glue("{loa_lower} ({loa_lower_ci_lower};{loa_lower_ci_upper})"),
    upper_loa = glue("{loa_upper} ({loa_upper_ci_lower};{loa_upper_ci_upper})"),
    pearson = glue("{estimate} ({lower.ci};{upper.ci})")
  ) %>%
  mutate(
    type = case_when(
      type == "raw" ~ "Raw ZM Predictions",
      type == "median5" ~ "5-Min Median",
      type == "median10" ~ "10-Min Median"
    ),
    model = case_when(
      model == "decision_tree" ~ "Decision Tree",
      model == "logistic_regression" ~ "Logistic Regression",
      model == "neural_network" ~ "Feed-Forward Neural Net",
      model == "xgboost" ~ "XGboost",
      TRUE ~ model
    ),
    variable = str_to_upper(variable),
    variable = case_when(
      variable == "SPT" ~ "SPT (min)",
      variable == "TST" ~ "TST (min)",
      variable == "SE" ~ "SE (%)",
      variable == "LPS" ~ "LPS (min)",
      variable == "WASO" ~ "WASO (min)"
    )
  ) %>%
  filter(type == "5-Min Median") %>%
  gt(groupname_col = c("type", "model")) %>%
  cols_label(
    bias = "Bias (95% CI)", lower_loa = "Lower LOA (95% CI)",
    upper_loa = "Upper LOA (95% CI)", pearson = md("Pearson, _r_ (95% CI)"),
    variable = ""
  ) %>%
  cols_align(align = "right", columns = bias:pearson) %>%
  cols_width(variable ~ px(200)) %>%
  fmt_markdown(pearson)
