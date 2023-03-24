library(tidyverse)
library(arrow)
library(patchwork)
library(showtext)


crude_me_ba <-
  read_csv(here::here("data/processed/crude_mixed_effect_ba.csv"))

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

x <- get_diff_stats(crude_stats) %>%
  pivot_longer(contains("diff"), names_to = "type_diff", values_to = "all_diffs") %>%
  select(model, type_diff, all_diffs) %>%
  split(.$type_diff)

y <- get_diff_stats(crude_stats) %>%
  pivot_longer(contains("avg"), names_to = "type_avg", values_to = "all_avgs") %>%
  select(type_avg, all_avgs) %>%
  split(.$type_avg)

crude_all_diffs <-
  map2(x, y, ~ bind_cols(.x, .y)) %>%
  map(~ group_split(.x, model)) %>%
  set_names(c("lps", "se", "spt", "tst", "waso"))



h_lines <-
  crude_me_ba %>%
  split(.$type) %>%
  map(~ .x %>%
    select(ba_metric, estimate, model) |>
    pivot_wider(names_from = ba_metric, values_from = estimate) |>
    group_split(model))

ci_lines <-
  crude_me_ba %>%
  split(.$type) %>%
  map(~ .x |>
    select(ba_metric, model, lower_ci:upper_ci) |>
    pivot_wider(names_from = ba_metric, values_from = lower_ci:upper_ci) |>
    group_split(model))


# BA plots for all models and sleep quality metrics -----------------------



font_add_google("Mukta", family = "mukta")
font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()


plot_ba <- function(...) {
  ggplot(..1, aes(all_avgs, all_diffs)) +
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
      x = min(..1$all_avgs),
      y = ..2$mean_bias,
      label = format(..2$mean_bias, digits = 2, size = 18),
      color = "lightblue", fill = "#002B36"
    ) +
    geom_hline(yintercept = ..2$lower_loa, color = "#f88379") +
    annotate(
      geom = "label",
      x = min(..1$all_avgs),
      y = ..2$lower_loa,
      label = format(..2$lower_loa, digits = 2, size = 18),
      color = "#f88379", fill = "#002B36"
    ) +
    geom_hline(yintercept = ..2$upper_loa, color = "#f88379") +
    annotate(
      geom = "label",
      x = min(..1$all_avgs),
      y = ..2$upper_loa,
      label = format(..2$upper_loa, digits = 2, size = 18),
      color = "#f88379", fill = "#002B36"
    ) +
    # labs(
    #   x = "Average of Two Measurements",
    #   y = "Difference Between Methods"
    # ) +
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
    "Decision Tree", "Logistic Regression",
    "Neural Network", "XGboost"
  )


create_titles <- function(x) {
  x %>%
    set_names(c(
      "dc", "lr", "nn", "xgb"
    )) %>%
    map2(titles, ~ .x + labs(title = .y))
}



# dc_plots
spt_plots <-
  pmap(
    list(
      crude_all_diffs$spt %>% map(~ filter(.x, !abs(all_diffs) > 5)),
      h_lines$spt_hrs,
      ci_lines$spt_hrs
    ), plot_ba
  ) %>%
  create_titles() %>%
  map(~ .x + labs(
    x = "Average of Two Measurements (hrs)",
    y = "Difference Between Methods (hrs)"
  ))

tst_plots <-
  pmap(
    list(
      crude_all_diffs$tst %>% map(~ filter(.x, !abs(all_diffs) > 5)),
      h_lines$tst_hrs,
      ci_lines$tst_hrs
    ), plot_ba
  ) %>%
  create_titles() %>%
  map(~ .x + labs(
    x = "Average of Two Measurements (hrs)",
    y = "Difference Between Methods (hrs)"
  ))

se_plots <-
  pmap(
    list(
      crude_all_diffs$se %>% map(~ filter(.x, !abs(all_diffs) > 30)),
      h_lines$se_percent,
      ci_lines$se_percent
    ), plot_ba
  ) %>%
  create_titles() %>%
  map(~ .x + labs(x = "Average of Two Measurements (%)",
                  y = "Difference Between Methods (%)"))

lps_plots <-
  pmap(
    list(
      crude_all_diffs$lps %>% map(~ filter(.x, !abs(all_diffs) > 100)),
      h_lines$lps_min,
      ci_lines$lps_min
    ), plot_ba
  ) %>%
  create_titles() %>%
  map(~ .x + labs(x = "Average of Two Measurements (min)",
                  y = "Difference Between Methods (min)"))

waso_plots <-
  pmap(
    list(
      crude_all_diffs$waso %>% map(~ filter(.x, !abs(all_diffs) > 100)),
      h_lines$waso_min,
      ci_lines$waso_min
    ), plot_ba
  ) %>%
  create_titles() %>%
  map(~ .x + labs(x = "Average of Two Measurements (min)",
                  y = "Difference Between Methods (min)"))
