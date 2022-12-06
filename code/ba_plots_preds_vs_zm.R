#!/usr/bin/env RscriptsuppressMessages(library(tidyverse))

library(tidyverse)
library(arrow)
library(patchwork)

stats <-
  read_parquet("data/processed/pred_stats_zm_stats.parquet")

diff_stats <-
  stats |>
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

summary_stats <-
  diff_stats |>
  filter(se_percent > 0) |>
  summarise(
    across(contains("diff"), list(
      upper = ~ mean(.x) + 1.96 * sd(.x),
      lower = ~ mean(.x) - 1.96 * sd(.x),
      mean = ~ mean(.x)
    ))
  )


# BA plots ----------------------------------------------------------------


ba_plot <- function(diff_stats, avg, diff, mean_diff, lower, upper, title, x_axis = "", y_axis = "") {
  diff_stats |>
    ggplot(aes({{avg}}, {{diff}})) +
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
    theme_light()
}

spt <- 
  ba_plot(diff_stats, avg_spt_hrs, diff_spt_hrs, summary_stats$diff_spt_hrs_mean, 
          summary_stats$diff_spt_hrs_lower, summary_stats$diff_spt_hrs_upper, 
          "Sleep Period Time (hrs)")

tst <- 
  ba_plot(diff_stats, avg_tst_hrs, diff_tst_hrs, summary_stats$diff_tst_hrs_mean, 
        summary_stats$diff_tst_hrs_lower, summary_stats$diff_tst_hrs_upper,
        "Total Sleep Time (hrs)")

se_percent <- 
  ba_plot(diff_stats, avg_se_percent, diff_se_percent, summary_stats$diff_se_percent_mean, 
          summary_stats$diff_se_percent_lower, summary_stats$diff_se_percent_upper,
          "Sleep Efficiency (%)", y_axis = "Difference Between Methods")

lps <- 
  ba_plot(diff_stats, avg_lps_min, diff_lps_min, summary_stats$diff_lps_min_mean, 
          summary_stats$diff_lps_min_lower, summary_stats$diff_lps_min_upper,
          "Latency until Persistent Sleep (min)")

waso <- 
  ba_plot(diff_stats, avg_waso_min, diff_waso_min, summary_stats$diff_waso_min_mean, 
          summary_stats$diff_waso_min_lower, summary_stats$diff_waso_min_upper,
          "Wake After Sleep Onset (min)")


list(spt, tst, se_percent, lps, waso) |> 
  wrap_plots(ncol = 2) + 
  patchwork::

ggsave("visuals/ba_plots_from_simple_trees_predictions.png", 
       height = 8, width = 8, dpi = 400)
