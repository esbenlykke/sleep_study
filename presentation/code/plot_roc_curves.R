#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)
library(ggtext)
library(here)
library(patchwork)

in_bed_roc <-
  read_parquet(here("data/processed/crude_roc_data_in_bed.parquet")) |>
  group_by(model) |>
  slice_sample(n = 1000, replace = TRUE)

sleep_roc <-
  read_parquet(here("data/processed/crude_roc_data_sleep.parquet")) |>
  group_by(model) |>
  slice_sample(n = 1000, replace = TRUE)


# ROC curve plots ---------------------------------------------------------

roc_in_bed <-
  in_bed_roc |>
  ggplot(
    aes(1 - specificity, sensitivity, color = model)
  ) + # plot with 2 ROC curves for each model
  geom_line(linewidth = .8, alpha = .7) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.4, lty = 2, color = "lightblue") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Prediction of In-Bed",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  theme(
    plot.margin = margin(r = 50, unit = "pt")
  )


roc_sleep <-
  sleep_roc |>
  ggplot(
    aes(1 - specificity, sensitivity, color = model)
  ) + # plot with 2 ROC curves for each model
  geom_line(linewidth = .8, alpha = .7) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.4, lty = 2, color = "lightblue") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Prediction of Sleep",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  theme(
    plot.margin = margin(l = 50, unit = "pt")
  )


roc_plots <-
  roc_in_bed + roc_sleep + plot_layout(guides = "collect") &
  theme(
    text = element_text(color = "#EEE8D5", family = "ibm"),
    axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
    axis.text = element_text(size = 20, color = "#EEE8D5"),
    plot.title = element_text(family = "ibm", size = 30, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 20, hjust = .5),
    plot.caption = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = "#293D42"),
    plot.background = element_rect(fill = "#002B36", color = "#002B36"),
    panel.background = element_rect(color = "#002B36", fill = "#002B36"),
    legend.background = element_rect(fill = "#002B36"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "bold"),
    legend.key = element_rect(fill = "#002B36"),
    legend.position = "bottom"
  )
