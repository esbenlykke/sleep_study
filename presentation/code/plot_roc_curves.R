#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)
library(ggtext)
library(here)
library(patchwork)

multi_roc <-
  read_parquet(here("data/processed/multiclass_OvsALL_roc_data.parquet")) %>%
  group_by(model) %>%
  slice_sample(n = 1000, replace = FALSE)

in_bed_awake <-
  read_parquet(here("data/processed/binary_relevance_roc_data_in_bed_awake.parquet")) %>%
  group_by(model) %>%
  slice_sample(n = 1000, replace = TRUE)

in_bed_asleep <-
  read_parquet(here("data/processed/binary_relevance_roc_data_in_bed_asleep.parquet")) %>%
  group_by(model) %>%
  slice_sample(n = 1000, replace = TRUE)

out_bed_awake <-
  read_parquet(here("data/processed/binary_relevance_roc_data_out_bed_awake.parquet")) %>%
  group_by(model) %>%
  slice_sample(n = 1000, replace = TRUE)

in_bed_roc <-
  read_parquet(here("data/processed/crude_roc_data_in_bed.parquet")) %>%
  group_by(model) %>%
  slice_sample(n = 1000, replace = TRUE)

sleep_roc <-
  read_parquet(here("data/processed/crude_roc_data_sleep.parquet")) %>%
  group_by(model) %>%
  slice_sample(n = 1000, replace = TRUE)


plot_roc <- function(roc_data, title) {
  roc_data %>%
    ggplot(
      aes(1 - specificity, sensitivity, color = model)
    ) + # plot with 2 ROC curves for each model
    geom_line(linewidth = .8, alpha = .7) +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.4, lty = 2, color = "lightblue") +
    scale_color_brewer(palette = "Dark2") +
    labs(
      subtitle = title,
      x = "1 - Specificity",
      y = "Sensitivity"
    ) 
}

roc_theme <- 
  theme(
    text = element_text(color = "#EEE8D5", family = "ibm"),
    axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
    axis.text = element_text(size = 20, color = "#EEE8D5"),
    plot.title = element_text(family = "ibm", size = 30, face = "bold", hjust = .5, 
                              margin = margin(b = 40)),
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

# ROC curve plots ---------------------------------------------------------

p1 <- plot_roc(in_bed_asleep, "Prediction of In-Bed-Asleep")
p2 <- plot_roc(in_bed_awake, "Prediction of In-Bed-Awake")
p3 <- plot_roc(out_bed_awake, "Prediction of Out-Bed-Awake")

three_classifiers_roc_plot <-
  wrap_plots(p1, p2, p3) +
  plot_annotation(
    title = "Three classifiers") +
    plot_layout(guides = "collect") &
  roc_theme
    

p4 <- plot_roc(in_bed_roc, "Prediction of In-Bed")
p5 <- plot_roc(sleep_roc, "Prediction of Sleep")

two_classifiers_roc_plot <-
  p4 + p5 + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Two classifiers") &
    roc_theme

multi_roc_plot <- 
  multi_roc %>% 
  ggplot(
    aes(1 - specificity, sensitivity, color = model)
  ) + # plot with 2 ROC curves for each model
  geom_line(linewidth = .8, alpha = .7) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.4, lty = 2, color = "lightblue") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Multiclass Classifier",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  facet_wrap(~ .level, labeller = 
               labeller(.level = c("in_bed_asleep" = "In-Bed Asleep", 
                                   "in_bed_awake" = "In-Bed Awake", 
                                   "out_bed_awake" = "Out-Bed Awake"))) +
  roc_theme +
  theme(
    strip.background = element_rect(fill = "#002B36"),
    strip.text = element_text(color = "#EEE8D5", size = 20)
  )