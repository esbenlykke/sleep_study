library(lme4)
library(tidyverse)
library(arrow)
library(gt)
library(showtext)
library(gtExtras)

crude_stats <-
  read_parquet(here::here("data/processed/crude_stats.parquet"))


get_diff_stats <-
  function(tbl) {
    tbl |>
      mutate(
        diff_spt_hrs = spt_hrs - zm_spt_hrs,
        diff_tst_hrs = tst_hrs - zm_tst_hrs,
        diff_se_percent = se_percent - zm_se_percent,
        diff_lps_min = lps_min - zm_lps_min,
        diff_waso_min = waso_min - zm_waso_min
      ) |>
      ungroup()
  }

diffs <- get_diff_stats(crude_stats)


### Means and SDs ###

spt <-
  diffs %>%
  filter(abs(diff_spt_hrs) < 5) %>%
  select(zm_spt_hrs) %>%
  summarise(spt_hrs_sd = sd(zm_spt_hrs), spt_hrs_mean = mean(zm_spt_hrs)) %>%
  mutate(model = "zm", .before = 1) %>%
  bind_rows(
    diffs %>%
      filter(abs(diff_spt_hrs) < 5) %>%
      select(model:noon_day, spt_hrs, zm_spt_hrs) %>%
      group_by(model) %>%
      summarise(across(spt_hrs, list(sd = sd, mean = mean)),
        .groups = "drop"
      )
  ) %>%
  rename(method = model) %>%
  transmute(
    method = method,
    SPT = glue::glue("{round(spt_hrs_mean, 2)} ({round(spt_hrs_sd, 2)})")
  )


tst <-
  diffs %>%
  filter(abs(diff_tst_hrs) < 5) %>%
  select(zm_tst_hrs) %>%
  summarise(tst_hrs_sd = sd(zm_tst_hrs), tst_hrs_mean = mean(zm_tst_hrs)) %>%
  mutate(model = "zm", .before = 1) %>%
  bind_rows(
    diffs %>%
      filter(abs(diff_tst_hrs) < 5) %>%
      select(model:noon_day, tst_hrs, zm_tst_hrs) %>%
      group_by(model) %>%
      summarise(across(tst_hrs, list(sd = sd, mean = mean)),
        .groups = "drop"
      )
  ) %>%
  rename(method = model) %>%
  transmute(
    method = method,
    TST = glue::glue("{round(tst_hrs_mean, 2)} ({round(tst_hrs_sd, 2)})")
  )

se <-
  diffs %>%
  filter(abs(diff_se_percent) < 30) %>%
  select(zm_se_percent) %>%
  summarise(se_percent_sd = sd(zm_se_percent), se_percent_mean = mean(zm_se_percent)) %>%
  mutate(model = "zm", .before = 1) %>%
  bind_rows(
    diffs %>%
      filter(abs(diff_se_percent) < 30) %>%
      select(model:noon_day, se_percent, zm_se_percent) %>%
      group_by(model) %>%
      summarise(across(se_percent, list(sd = sd, mean = mean)),
        .groups = "drop"
      )
  ) %>%
  rename(method = model) %>%
  transmute(
    method = method,
    SE = glue::glue("{round(se_percent_mean, 2)} ({round(se_percent_sd, 2)})")
  )

lps <-
  diffs %>%
  filter(abs(diff_lps_min) < 100) %>%
  select(zm_lps_min) %>%
  summarise(lps_min_sd = sd(zm_lps_min), lps_min_mean = mean(zm_lps_min)) %>%
  mutate(model = "zm", .before = 1) %>%
  bind_rows(
    diffs %>%
      filter(abs(diff_lps_min) < 100) %>%
      select(model:noon_day, lps_min, zm_lps_min) %>%
      group_by(model) %>%
      summarise(across(lps_min, list(sd = sd, mean = mean)),
        .groups = "drop"
      )
  ) %>%
  rename(method = model) %>%
  transmute(
    method = method,
    LPS = glue::glue("{round(lps_min_mean, 2)} ({round(lps_min_sd, 2)})")
  )


waso <-
  diffs %>%
  filter(abs(diff_waso_min) < 100) %>%
  select(zm_waso_min) %>%
  summarise(waso_min_sd = sd(zm_waso_min), waso_min_mean = mean(zm_waso_min)) %>%
  mutate(model = "zm", .before = 1) %>%
  bind_rows(
    diffs %>%
      filter(abs(diff_waso_min) < 100) %>%
      select(model:noon_day, waso_min, zm_waso_min) %>%
      group_by(model) %>%
      summarise(across(waso_min, list(sd = sd, mean = mean)),
        .groups = "drop"
      )
  ) %>%
  rename(method = model) %>%
  transmute(
    method = method,
    WASO = glue::glue("{round(waso_min_mean, 2)} ({round(waso_min_sd, 2)})")
  )



### Covariance between repeated measures ###
# model_outcomes <- c(
#   "spt_hrs", "tst_hrs", "se_percent", "lps_min", "waso_min"
# )
#
# zm_outcomes <-
#   c("zm_spt_hrs", "zm_tst_hrs", "zm_se_percent", "zm_lps_min", "zm_waso_min")

# covariance values between the repeated measures for each outcome, grouped by model.
# covariance <-
# diffs %>%
# select(model, id, noon_day, all_of(model_outcomes)) %>%
# drop_na() %>%
# pivot_longer(cols = all_of(model_outcomes), names_to = "outcome", values_to = "value") %>%
# group_by(id, outcome, model) %>%
# arrange(model, id, outcome, noon_day) %>%
# mutate(value_diff = value - lag(value)) %>%
# filter(!value_diff == "NaN" & !value == "Inf" & !value == "-Inf") %>%
#   group_by(model, outcome) %>%
#   filter((outcome == "lps_min" & value_diff < 100) |
#     (outcome == "waso_min" & value_diff < 100) |
#     (outcome == "spt_hrs" & value_diff < 5) |
#     (outcome == "tst_hrs" & value_diff < 5) |
#     (outcome == "se_percent" & value_diff < 30)) %>%
#   summarize(
#     mean_cov = cov(noon_day, value_diff, use = "pairwise.complete.obs"),
#     .groups = "drop"
#   ) %>%
#   pivot_wider(names_from = outcome, values_from = mean_cov) %>%
#   rename(method = model) %>%
#   bind_rows(
#     diffs %>%
#       filter(model == "decision_tree") %>%
#       select(id, noon_day, contains("zm")) %>%
#       mutate(
#         method = "zm",
#         .before = 1
#       ) %>%
#       drop_na() %>%
#       pivot_longer(cols = all_of(zm_outcomes), names_to = "outcome", values_to = "value") %>%
#       group_by(id, outcome, method) %>%
#       arrange(method, id, outcome, noon_day) %>%
#       mutate(value_diff = value - lag(value)) %>%
#       filter(!value_diff == "NaN" & !value == "Inf" & !value == "-Inf") %>%
#       group_by(method, outcome) %>%
#       summarize(
#         mean_cov = cov(noon_day, value_diff, use = "pairwise.complete.obs"),
#         .groups = "drop"
#       ) %>%
#       pivot_wider(names_from = outcome, values_from = mean_cov) %>%
#       rename_with(.fn = ~ str_remove(.x, "zm_"))
#   ) %>%
#   rename_with(-method, .fn = ~ str_c(.x, "_cov"))

### Correlation with ZM ###

library(rmcorr)

cor_df <-
  diffs %>%
  drop_na() %>%
  filter(!if_any(spt_hrs:zm_waso_min, is.infinite)) %>%
  mutate(id = as_factor(id))

dc_spt_rmcor <- rmcorr(id, spt_hrs, zm_spt_hrs, dataset = cor_df %>%
  filter(model == "decision_tree", abs(diff_spt_hrs) < 5))
dc_tst_rmcor <- rmcorr(id, tst_hrs, zm_tst_hrs, dataset = cor_df %>%
  filter(model == "decision_tree", abs(diff_tst_hrs) < 5))
dc_se_rmcor <- rmcorr(id, se_percent, zm_se_percent, dataset = cor_df %>%
  filter(model == "decision_tree", abs(diff_se_percent) < 30))
dc_lps_rmcor <- rmcorr(id, lps_min, zm_lps_min, dataset = cor_df %>%
  filter(model == "decision_tree", abs(diff_lps_min) < 100))
dc_waso_rmcor <- rmcorr(id, waso_min, zm_waso_min, dataset = cor_df %>%
  filter(model == "decision_tree", abs(diff_waso_min) < 100))
lr_spt_rmcor <- rmcorr(id, spt_hrs, zm_spt_hrs, dataset = cor_df %>%
  filter(model == "logistic_regression", abs(diff_spt_hrs) < 5))
lr_tst_rmcor <- rmcorr(id, tst_hrs, zm_tst_hrs, dataset = cor_df %>%
  filter(model == "logistic_regression", abs(diff_tst_hrs) < 5))
lr_se_rmcor <- rmcorr(id, se_percent, zm_se_percent, dataset = cor_df %>%
  filter(model == "logistic_regression", abs(diff_se_percent) < 30))
lr_lps_rmcor <- rmcorr(id, lps_min, zm_lps_min, dataset = cor_df %>%
  filter(model == "logistic_regression", abs(diff_lps_min) < 100))
lr_waso_rmcor <- rmcorr(id, waso_min, zm_waso_min, dataset = cor_df %>%
  filter(model == "logistic_regression", abs(diff_waso_min) < 100))
nn_spt_rmcor <- rmcorr(id, spt_hrs, zm_spt_hrs, dataset = cor_df %>%
  filter(model == "neural_net", abs(diff_spt_hrs) < 5))
nn_tst_rmcor <- rmcorr(id, tst_hrs, zm_tst_hrs, dataset = cor_df %>%
  filter(model == "neural_net", abs(diff_tst_hrs) < 5))
nn_se_rmcor <- rmcorr(id, se_percent, zm_se_percent, dataset = cor_df %>%
  filter(model == "neural_net", abs(diff_se_percent) < 30))
nn_lps_rmcor <- rmcorr(id, lps_min, zm_lps_min, dataset = cor_df %>%
  filter(model == "neural_net", abs(diff_lps_min) < 100))
nn_waso_rmcor <- rmcorr(id, waso_min, zm_waso_min, dataset = cor_df %>%
  filter(model == "neural_net", abs(diff_waso_min) < 100))
xgb_spt_rmcor <- rmcorr(id, spt_hrs, zm_spt_hrs, dataset = cor_df %>%
  filter(model == "xgboost", abs(diff_spt_hrs) < 5))
xgb_tst_rmcor <- rmcorr(id, tst_hrs, zm_tst_hrs, dataset = cor_df %>%
  filter(model == "xgboost", abs(diff_tst_hrs) < 5))
xgb_se_rmcor <- rmcorr(id, se_percent, zm_se_percent, dataset = cor_df %>%
  filter(model == "xgboost", abs(diff_se_percent) < 30))
xgb_lps_rmcor <- rmcorr(id, lps_min, zm_lps_min, dataset = cor_df %>%
  filter(model == "xgboost", abs(diff_lps_min) < 100))
xgb_waso_rmcor <- rmcorr(id, waso_min, zm_waso_min, dataset = cor_df %>%
  filter(model == "xgboost", abs(diff_waso_min) < 100))

stick <- function(rmcorr) {
  glue::glue("{round(rmcorr$r, 2)} ({round(rmcorr$CI[1], 2)} - {round(rmcorr$CI[2], 2)})")
}

rmcorrs <-
  tibble(
    method = c(rep("zm", 5), rep("decision_tree", 5), rep("logistic_regression", 5), rep("neural_net", 5), rep("xgboost", 5)),
    rm_corr = c(
      rep("-", 5),
      stick(dc_spt_rmcor), stick(dc_tst_rmcor), stick(dc_se_rmcor), stick(dc_lps_rmcor), stick(dc_waso_rmcor),
      stick(lr_spt_rmcor), stick(lr_tst_rmcor), stick(lr_se_rmcor), stick(lr_lps_rmcor), stick(lr_waso_rmcor),
      stick(nn_spt_rmcor), stick(nn_tst_rmcor), stick(nn_se_rmcor), stick(nn_lps_rmcor), stick(nn_waso_rmcor),
      stick(xgb_spt_rmcor), stick(xgb_tst_rmcor), stick(xgb_se_rmcor), stick(xgb_lps_rmcor), stick(xgb_waso_rmcor)
    )
  )

spt_all <-
  spt %>% bind_cols(
    rm_corr_spt =
      c(
        "-",
        stick(dc_spt_rmcor),
        stick(lr_spt_rmcor),
        stick(nn_spt_rmcor),
        stick(xgb_spt_rmcor)
      )
  )

tst_all <-
  tst %>% bind_cols(
    rm_corr_tst = c(
      "-",
      stick(dc_tst_rmcor),
      stick(lr_tst_rmcor),
      stick(nn_tst_rmcor),
      stick(xgb_tst_rmcor)
    )
  )

se_all <-
  se %>% bind_cols(
    rm_corr_se = c(
      "-",
      stick(dc_se_rmcor),
      stick(lr_se_rmcor),
      stick(nn_se_rmcor),
      stick(xgb_se_rmcor)
    )
  )


lps_all <-
  lps %>% bind_cols(
    rm_corr_lps = c(
      "-",
      stick(dc_lps_rmcor),
      stick(lr_lps_rmcor),
      stick(nn_lps_rmcor),
      stick(xgb_lps_rmcor)
    )
  )

waso_all <-
  waso %>% bind_cols(
    rm_corr_waso = c(
      "-",
      stick(dc_waso_rmcor),
      stick(lr_waso_rmcor),
      stick(nn_waso_rmcor),
      stick(xgb_waso_rmcor)
    )
  )

### table ###
font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()

tab_descriptives <-
  reduce(list(spt_all, tst_all, se_all, lps_all, waso_all), left_join, by = "method") %>%
  mutate(
    method = case_when(
      method == "zm" ~ "ZMachine Insight+",
      method == "decision_tree" ~ "Decision Tree",
      method == "logistic_regression" ~ "Logistic Regression",
      method == "neural_net" ~ "Neural Network",
      method == "xgboost" ~ "XGBoost"
    )
  ) %>%
  gt() %>%
  cols_label(
    method = "",
    SPT = "mean (SD)",
    TST = "mean (SD)",
    SE = "mean (SD)",
    LPS = "mean (SD)",
    WASO = "mean (SD)",
    rm_corr_spt = "r (CI95%)",
    rm_corr_tst = "r (CI95%)",
    rm_corr_se = "r (CI95%)",
    rm_corr_lps = "r (CI95%)",
    rm_corr_waso = "r (CI95%)",
  ) %>%
  # tab_header(title = md("Descriptives of Sleep Quality Statistics Across Methods")) |>
  tab_style(
    style = cell_text(size = px(12), color = "#EEE8D5"),
    locations = cells_body(
      columns = c(1:11)
    )
  ) %>% 
    tab_style(
      style = cell_text(size = px(12), color = "#93A1A1"),
      locations = cells_column_labels(
        columns = 1:11
      )
    ) %>% 
  cols_align(align = "right", columns = 2:11) %>%
  tab_spanner(label = "Sleep Period Time", columns = 2:3) %>%
  tab_spanner(label = "Total Sleep Time", columns = 4:5) %>%
  tab_spanner(label = "Sleep Efficiency", columns = 6:7) %>%
  tab_spanner(label = "Latency Until Persistent Sleep", columns = 8:9) %>%
  tab_spanner(label = "Wake After Sleep Onset", columns = 10:11) %>%
    tab_style(
      style = cell_text(size = px(16)),
      locations = cells_column_spanners(
        spanners = 1:5
      )
    ) %>% 
  tab_footnote(
    footnote = "Covariance between repeated measures",
    locations = cells_column_labels(
      columns = contains("cov")
    )
  ) %>%
  tab_footnote(
    footnote = "Repeated measures correlation coefficient between outcomes and ZMachine Insight+ and corresponding 95% confidence intervals.",
    locations = cells_column_labels(
      columns = c(3, 5, 7, 9, 11))
  ) %>% 
  tab_footnote(
    footnote = "Sleep outcome means and standard deviations.",
    locations = cells_column_labels(
      columns = c(2, 4, 6, 8, 10))
  ) %>% 
    tab_style(
      style = cell_fill("#2C4E57"),
      locations = cells_body(rows = c(1, 3, 5))
    ) |>
  tab_options(
    table.font.names = "ibm",
    table.font.color.light = "#EEE8D5",
    table.background.color = "#002B36",
    heading.border.bottom.style = "none",
    heading.padding = px(20),
    table.border.top.width = px(3),
    table.border.top.style = "none", # transparent
    table.border.bottom.style = "none",
    source_notes.border.bottom.color = "#002B36",
    column_labels.font.weight = "normal",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "#2C4E57",
    column_labels.padding = px(1),
    row_group.border.top.style = "none",
    row_group.border.top.color = "#2C4E57",
    row_group.border.bottom.width = px(0),
    row_group.border.bottom.color = "#2C4E57",
    stub.border.color = "#2C4E57",
    stub.border.width = px(0),
    stub.font.size = 14,
    table_body.border.bottom.color = "#002B36",
    table_body.hlines.color = "#2C4E57",
    # table_body.hlines.style = "none",
    table_body.vlines.style = "none",
    data_row.padding = px(1),
    heading.align = "center",
    footnotes.font.size = 10
  )

# An intraclass correlation coefficient (ICC) of 0.346 indicates that 34.6% of
# the total variation in the outcome variable can be attributed to the between-group
# (or between-subject) differences, while the remaining 65.4% of the variation is due
# to within-group (or within-subject) differences.


### MDES ###

# You have calculated a minimum detectable effect size (MDES) of 47 minutes with
# a 95% confidence interval ranging from 39.1  to 56.4 minutes. This result is based
# on a study with 75 subjects, each having 4 repeated measures, and with a desired
# statistical power of 80% and a significance level (alpha) of 0.05.
#
# The MDES represents the smallest effect size that your study is capable of
# detecting with the specified power and significance level. In this case, an
# effect size of 45 minutes means that if the true difference in sleep time
# (or the effect of the intervention or predictor) is equal to or greater than 45
# minutes, you have an 80% chance of detecting this effect as statistically
# significant at a 0.05 significance level.
#
# The confidence interval (39.1 - 56.4) provides a range of plausible values for
# the MDES. It indicates that you can be 95% confident that the true MDES lies
# within this interval. In other words, the smallest effect size that your study
# can detect with 80% power and a 0.05 significance level is likely to be between
# 29.1 and 47.2 minutes.
