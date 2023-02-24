library(tidyverse)
library(arrow)
library(patchwork)
library(showtext)


ba_metrics <-
  read_csv(here::here("data/processed/mixed_effect_ba.csv"))

stats_files <-
  list.files("~/sleep_study/data/processed/stats_predictions", full.names = TRUE)

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

diff_avg <-
  all_stats |>
  map(get_diff_stats) |>
  map(~ .x |> select(id, contains(c("diff", "avg")))) |>
  map(~ .x |> pivot_longer(contains("diff"), names_to = "diffs", values_to = "diff_values")) |>
  map(~ .x |> pivot_longer(contains("avg"), names_to = "avgs", values_to = "avg_values")) |>
  map(~ .x |> filter(
    (str_detect(diffs, "spt") & str_detect(avgs, "spt")) |
      (str_detect(diffs, "tst") & str_detect(avgs, "tst")) |
      (str_detect(diffs, "se") & str_detect(avgs, "se")) |
      (str_detect(diffs, "lps") & str_detect(avgs, "lps")) |
      (str_detect(diffs, "waso") & str_detect(avgs, "waso"))
  )) |>
  map(~ .x |> group_split(diffs, avgs))


h_lines <-
  ba_metrics |>
  group_split(model, .keep = FALSE) |>
  setNames(c("decision_tree", "logistic_regression", "neural_net", "xgboost")) |>
  map(~ .x |>
    select(ba_metric, estimate, type) |>
    pivot_wider(names_from = ba_metric, values_from = estimate) |>
    group_split(type))

ci_lines <-
  ba_metrics |>
  group_split(model, .keep = FALSE) |>
  setNames(c("decision_tree", "logistic_regression", "neural_net", "xgboost")) |>
  map(~
    .x |>
      select(ba_metric, type, lower_ci:upper_ci) |>
      pivot_wider(names_from = ba_metric, values_from = lower_ci:upper_ci) |>
      group_split(type))


# BA plots for all models and sleep quality metrics -----------------------



font_add_google("Mukta", family = "mukta")
font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()


plot_ba <- function(...) {
  ggplot(..1, aes(avg_values, diff_values)) +
    annotate(
      geom = "rect",
      xmin = -Inf, xmax = Inf,
      ymin = ..3$lower_ci_mean_bias, ymax = ..3$upper_ci_mean_bias,
      fill = "lightblue", alpha = .2, lty = 2
    ) +
    annotate(
      geom = "rect",
      xmin = -Inf, xmax = Inf,
      ymin = ..3$lower_ci_lower_loa, ymax = ..3$upper_ci_lower_loa,
      fill = "#f88379", alpha = .2, lty = 2
    ) +
    annotate(
      geom = "rect",
      xmin = -Inf, xmax = Inf,
      ymin = ..3$lower_ci_upper_loa, ymax = ..3$upper_ci_upper_loa,
      fill = "#f88379", alpha = .2, lty = 2
    ) +
    geom_point(
      shape = 21, size = 3,
      fill = "#4D8C57",
      color = "#EEE8D5",
      alpha = .7
    ) +
    geom_smooth(
      se = FALSE, color = "#EEE8D5", method = "lm",
      linewidth = 1, lineend = "round", fullrange = TRUE
    ) +
    geom_smooth(
      se = FALSE, color = "#127A62", method = "lm",
      lineend = "round", lty = 2, fullrange = TRUE,
      linewidth = .6
    ) +
    geom_hline(yintercept = ..2$mean_bias, color = "lightblue") +
    annotate(
      geom = "label",
      x = min(..1$avg_values),
      y = ..2$mean_bias,
      label = format(..2$mean_bias, digits = 2, size = 18),
      color = "lightblue", fill = "#002B36"
    ) +
    geom_hline(yintercept = ..2$lower_loa, color = "#f88379") +
    annotate(
      geom = "label",
      x = min(..1$avg_values),
      y = ..2$lower_loa,
      label = format(..2$lower_loa, digits = 2, size = 18),
      color = "#f88379", fill = "#002B36"
    ) +
    geom_hline(yintercept = ..2$upper_loa, color = "#f88379") +
    annotate(
      geom = "label",
      x = min(..1$avg_values),
      y = ..2$upper_loa,
      label = format(..2$upper_loa, digits = 2, size = 18),
      color = "#f88379", fill = "#002B36"
    ) +
    labs(
      x = "Average of Two Measurements",
      y = "Difference Between Methods"
    ) +
    theme(
      text = element_text(color = "#EEE8D5"),
      axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
      axis.text = element_text(size = 20, color = "#EEE8D5"),
      plot.title = element_text(family = "ibm", size = 30, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid = element_line(color = "#293D42"),
      plot.background = element_rect(fill = "#002B36", color = "#EEE8D5"),
      panel.background = element_rect(color = "#002B36", fill = "#002B36")
    ) 
}

titles <-
  c(
    "Sleep Period Time (hrs)", "Total Sleep Time (hrs)",
    "Sleep Efficiency (%)", "Latency Until Persisten Sleep (min)",
    "Wake After Sleep Onset (min)"
  )


create_titles <- function(x) {
  x %>%
    setNames(c(
      "lps", "se", "spt", "tst", "waso"
    )) %>%
    .[c(3, 4, 2, 1, 5)] |>
    map2(titles, ~ .x + labs(title = .y))
}


# dc_plots <-
dc_plots <-
  pmap(
    list(diff_avg$decision_tree, h_lines$decision_tree, ci_lines$decision_tree), plot_ba
  ) |> 
  create_titles() 

lr_plots <-
  pmap(
    list(diff_avg$logistic_regression, h_lines$logistic_regression, ci_lines$logistic_regression), plot_ba) |>
  create_titles()

nn_plots <-
  pmap(
    list(diff_avg$neural_net, h_lines$neural_net, ci_lines$neural_net), plot_ba) |>
  create_titles()

xg_plots <-
  pmap(
    list(diff_avg$xgboost, h_lines$xgboost, ci_lines$xgboost), plot_ba) |>
  create_titles()
