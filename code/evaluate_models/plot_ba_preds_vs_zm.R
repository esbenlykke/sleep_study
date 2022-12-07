#!/usr/bin/env RscriptsuppressMessages(library(tidyverse))

library(tidyverse)
library(arrow)
library(patchwork)
library(showtext)

stats_files <-
  list.files("data/processed/stats_predictions", full.names = TRUE)

all_stats <-
  map(stats_files, read_parquet)

simple_stats <-
  read_parquet("data/processed/stats_predictions/simple_CART_stats.parquet")

get_diff_stats <- 
  function(tbl){
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
  ungroup() |>
  filter(se_percent > 0 & lps_min > 0) # TODO why are these numbers fucked?
}

get_summary_stats <- 
  function(tbl){
    tbl |>
  filter(se_percent > 0) |>
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
        geom_hline(yintercept = mean_diff) +
        geom_hline(
          yintercept = lower,
          lty = 2, color = "darkred", linewidth = .5, alpha = .5
        ) +
        geom_hline(
          yintercept = upper,
          lty = 2, color = "darkred", linewidth = .5, alpha = .5
        ) +
        geom_point(shape = 21, size = 2, fill = "#556b2f", alpha = .7) +
        labs(
          title = title,
          x = x_axis,
          y = y_axis
        ) +
        theme_minimal() +
        theme(
          axis.title = element_text(family = "mukta", size = 16),
          plot.title = element_text(family = "ibm", size = 22, face = "bold")
        )
    }

    spt <-
        ba_plot(diffs, avg_spt_hrs, diff_spt_hrs, summaries$diff_spt_hrs_mean,
                summaries$diff_spt_hrs_lower, summaries$diff_spt_hrs_upper,
        "Sleep Period Time (hrs)"
      )

    tst <-
      ba_plot(
        diffs, avg_tst_hrs, diff_tst_hrs, summaries$diff_tst_hrs_mean,
        summaries$diff_tst_hrs_lower, summaries$diff_tst_hrs_upper,
        "Total Sleep Time (hrs)"
      )

    se_percent <-
      ba_plot(diffs, avg_se_percent, diff_se_percent, summaries$diff_se_percent_mean,
              summaries$diff_se_percent_lower, summaries$diff_se_percent_upper,
        "Sleep Efficiency (%)",
        y_axis = "Difference Between Methods"
      )

    lps <-
      ba_plot(
        diffs, avg_lps_min, diff_lps_min, summaries$diff_lps_min_mean,
        summaries$diff_lps_min_lower, summaries$diff_lps_min_upper,
        "Latency until Persistent Sleep (min)"
      )

    waso <-
      ba_plot(
        diffs, avg_waso_min, diff_waso_min, summaries$diff_waso_min_mean,
        summaries$diff_waso_min_lower, summaries$diff_waso_min_upper,
        "Wake After Sleep Onset (min)", "Average of Two Measurements"
      )


    list(spt, tst, se_percent, lps, waso) |>
      wrap_plots(ncol = 1)
  }

plots <- map2(all_diffs, all_summaries, all_plots) 

names(plots) <- c(str_replace_all(stats_files, "data/processed/stats_predictions/", 
                                  "visuals/ba_plots_")) |> 
  str_replace_all(".parquet", ".png")

walk2(plots, names(plots), ~ ggsave(plot = .x, filename = .y, height = 12, width = 6))

