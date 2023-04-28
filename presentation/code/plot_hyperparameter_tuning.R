#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(patchwork)


in_bed_10 <- 
  read_rds("/media/esbenlykke/My Passport/chained_models/grid_results/in_bed/in_bed_workflowsets_results_10_sec.rds")

in_bed_30 <- 
  read_rds("/media/esbenlykke/My Passport/chained_models/grid_results/in_bed/in_bed_workflowsets_results_30_sec.rds")


hyper_plot_10 <- 
  in_bed_10 |> 
  autoplot(
    rank_metric = "f_meas",  # <- how to order models
    metric = "f_meas",       # <- which metric to visualize
    select_best = FALSE     # <- one point per workflow
  ) +
  # geom_text(aes(y = mean - .003, label = wflow_id), angle = 90, hjust = 1) +
  # lims(y = c(.96, 1)) +
  labs(
    title = "Hyperparameter Tuning",
    subtitle = "5-fold cross validation using 5-level grid search",
    color = "Model Type",
    shape = NULL,
    y = "F1 Metric",
    caption = "Note: the decision tree model was chosen to extract in-bed time."
  ) +
  scale_color_brewer(palette = "Dark2",
                     label = c("XGBoost", "Neural Net", "Decision Tree", "Logistic Regression")) +
  theme(
    text = element_text(color = "#EEE8D5", family = "ibm"),
    axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
    axis.text = element_text(size = 20, color = "#EEE8D5"),
    plot.title = element_text(family = "ibm", size = 30, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 20, hjust = .5),
    plot.caption = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = "#293D42"),
    plot.background = element_rect(fill = "#002B36", color = "#EEE8D5"),
    panel.background = element_rect(color = "#002B36", fill = "#002B36"),
    legend.background = element_rect(fill = "#002B36"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "bold"),
    legend.key = element_rect(fill = "#002B36"),
    legend.position = "bottom"
  ) +
  guides(shape = "none")

ggsave("presentation/visuals/plot_hyper_10.png", width = 14, height = 6)


hyper_plot_30 <- 
  in_bed_30 |> 
  autoplot(
    rank_metric = "f_meas",  # <- how to order models
    metric = "f_meas",       # <- which metric to visualize
    select_best = FALSE     # <- one point per workflow
  ) +
  # geom_text(aes(y = mean - .003, label = wflow_id), angle = 90, hjust = 1) +
  # lims(y = c(.96, 1)) +
  labs(
    title = "Hyperparameter Tuning",
    subtitle = "5-fold cross validation using 5-level grid search",
    color = "Model Type",
    shape = NULL,
    y = "F1 Metric",
    caption = "Note: this is based on simulated data and is not representative of the actual training data from the study"
  ) +
  scale_color_brewer(palette = "Dark2",
                     label = c("XGBoost", "Neural Net", "Decision Tree", "Logistic Regression")) +
  theme(
    text = element_text(color = "#EEE8D5", family = "ibm"),
    axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
    axis.text = element_text(size = 20, color = "#EEE8D5"),
    plot.title = element_text(family = "ibm", size = 30, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 20, hjust = .5),
    plot.caption = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = "#293D42"),
    plot.background = element_rect(fill = "#002B36", color = "#EEE8D5"),
    panel.background = element_rect(color = "#002B36", fill = "#002B36"),
    legend.background = element_rect(fill = "#002B36"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "bold"),
    legend.key = element_rect(fill = "#002B36"),
    legend.position = "bottom"
  ) +
  guides(shape = "none")

hyper_plot_10 / hyper_plot_30