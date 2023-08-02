library(tidyverse)
library(arrow)
library(gt)
library(showtext)
library(gtExtras)
library(here)


# TODO consider calculating 95% CI for bias


stats_files <-
  list.files("data/processed/stats_predictions", full.names = TRUE)

all_stats <-
  map(stats_files, read_parquet) |>
  set_names(c("logistic_regression", "neural_net", "decision_tree", "xgboost"))
# |>
#   map(
#     ~ filter(.x, (id != 1742705 | noon_day != 24) &
#         (id != 1044005 | noon_day != 16) &
#         (id != 1750904 | noon_day != 21) &
#         (id != 1718904 | noon_day != 8)
#     )
#   ) # TODO look into these lps_min outliers

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
      rowwise() |>
      mutate(
        avg_spt_hrs = mean(c(spt_hrs, zm_spt_hrs)),
        avg_tst_hrs = mean(c(tst_hrs, zm_tst_hrs)),
        avg_se_percent = mean(c(se_percent, zm_se_percent)),
        avg_lps_min = mean(c(lps_min, zm_lps_min)),
        avg_waso_min = mean(c(waso_min, zm_waso_min))
      ) |>
      ungroup()
  }


get_summary_stats <-
  function(tbl) {
    tbl |>
      # filter(se_percent > 0) |>
      summarise(
        across(contains("diff"), list(
          upper = ~ mean(.x) + 1.96 * sd(.x),
          lower = ~ mean(.x) - 1.96 * sd(.x),
          mean = ~ mean(.x)
        ))
      )
  }

all_diffs <-
  all_stats |>
  map(get_diff_stats)

all_summaries <-
  all_stats |>
  map(get_diff_stats) |>
  map(get_summary_stats) |>
  bind_rows(.id = "model")


# create table ------------------------------------------------------------

font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()

all_summaries |>
  pivot_longer(-model) |>
  mutate(
    group = case_when(
      str_detect(name, "mean") ~ "Bias",
      str_detect(name, "upper") ~ "LOA Upper",
      str_detect(name, "lower") ~ "LOA Lower"
    ),
    name = str_remove_all(name, "diff_|_upper|_lower|_mean"),
    name = case_when(
      name == "spt_hrs" ~ "Sleep Period Time",
      name == "tst_hrs" ~ "Total Sleep Time",
      name == "se_percent" ~ "Sleep Efficiency",
      name == "lps_min" ~ "Latency Until Persistent Sleep",
      name == "waso_min" ~ "Wake After Sleep Onset"
    )
  ) |>
  group_by(name, model) |>
  arrange(group, .by_group = TRUE) |>
  pivot_wider(names_from = model) |>
  ungroup() |>
  # Create table
  gt(rowname_col = "group", groupname_col = "name") |>
  tab_header(title = md("Bland-Altman Analysis")) |>
  cols_align(align = "center", columns = decision_tree:xgboost) |>
  cols_align(align = "right", columns = group) |>
  cols_width(
    decision_tree:xgboost ~ px(100),
    group ~ px(250)
  ) |>
  cols_label(
    logistic_regression = md("Logistic Regression"),
    neural_net = "Neural Network",
    decision_tree = md("Decision<br>Tree"),
    xgboost = "XGboost"
  ) |>
  fmt_number(columns = decision_tree:xgboost) |>
  tab_style(
    style = cell_text(size = px(14)),
    locations = cells_body(
      columns = c(decision_tree:xgboost)
    )
  ) |>
  tab_options(
    table.font.names = "ibm",
    table.font.color.light = "#EEE8D5",
    table.background.color = "#002B36"
  )

# TODO fix color theme