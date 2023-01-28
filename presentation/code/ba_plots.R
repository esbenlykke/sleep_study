library(tidyverse)
library(arrow)
library(patchwork)
library(showtext)

stats_files <-
  list.files("~/sleep_study/data/processed/stats_predictions", full.names = TRUE)

all_stats <-
  map(stats_files, read_parquet) |>
  set_names(c("logistic_regression", "neural_net", "decision_tree", "xgboost"))

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
    # filter(se_percent > 0 & lps_min > 0) # TODO why are these numbers fucked?
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
  map(get_summary_stats)

# BA plots for simple trees -----------------------------------------------


font_add_google("Mukta", family = "mukta")
font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()

all_plots <-
  function(diffs, summaries) {
    ba_plot <- function(diffs, avg, diff, mean_diff, lower, upper, title, x_axis = "", y_axis = "") {
      diffs |>
        ggplot(aes({{ avg }}, {{ diff }})) +
        geom_hline(yintercept = mean_diff, color = "lightblue", lty = 1) +
        geom_hline(
          yintercept = lower,
          lty = 2, color = "#f88379", linewidth = .5, alpha = .8
        ) +
        geom_hline(
          yintercept = upper,
          lty = 2, color = "#f88379", linewidth = .5, alpha = .8
        ) +
        geom_point(
          shape = 21, size = 3,
          fill = "#4D8C57",
          color = "#EEE8D5",
          alpha = .7
        ) +
        labs(
          title = title,
          x = x_axis,
          y = y_axis
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

    spt <-
      ba_plot(
        diffs, avg_spt_hrs, diff_spt_hrs, summaries$diff_spt_hrs_mean,
        summaries$diff_spt_hrs_lower, summaries$diff_spt_hrs_upper,
        "Sleep Period Time (hrs)",
        x_axis = "Average of Two Measurements",
        y_axis = "Difference Between Methods"
      ) +
      scale_y_continuous(
        breaks = seq(-10, 10, 2),
        limits = c(-6, 6)
      )

    tst <-
      ba_plot(
        diffs, avg_tst_hrs, diff_tst_hrs, summaries$diff_tst_hrs_mean,
        summaries$diff_tst_hrs_lower, summaries$diff_tst_hrs_upper,
        "Total Sleep Time (hrs)",
        x_axis = "Average of Two Measurements",
        y_axis = "Difference Between Methods"
      ) +
      scale_y_continuous(
        breaks = seq(-10, 10, 2),
        limits = c(-7, 7)
      )

    se_percent <-
      ba_plot(diffs, avg_se_percent, diff_se_percent, summaries$diff_se_percent_mean,
        summaries$diff_se_percent_lower, summaries$diff_se_percent_upper,
        "Sleep Efficiency (%)",
        x_axis = "Average of Two Measurements",
        y_axis = "Difference Between Methods"
      ) +
      scale_y_continuous(breaks = seq(-30, 30, 10))

    lps <-
      ba_plot(
        diffs, avg_lps_min, diff_lps_min, summaries$diff_lps_min_mean,
        summaries$diff_lps_min_lower, summaries$diff_lps_min_upper,
        "Latency until Persistent Sleep (min)",
        x_axis = "Average of Two Measurements",
        y_axis = "Difference Between Methods"
      ) +
      scale_y_continuous(limits = c(-100, 100)) +
      scale_x_continuous(limits = c(0, 100))

    waso <-
      ba_plot(
        diffs, avg_waso_min, diff_waso_min, summaries$diff_waso_min_mean,
        summaries$diff_waso_min_lower, summaries$diff_waso_min_upper,
        "Wake After Sleep Onset (min)",
        x_axis = "Average of Two Measurements",
        y_axis = "Difference Between Methods"
      ) +
      scale_y_continuous(
        breaks = seq(-200, 200, 50),
        limits = c(-150, 150)
      )


    all <- list(spt, tst, se_percent, lps, waso)
  }

plots <- map2(all_diffs, all_summaries, all_plots)
