#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)
library(patchwork)


get_roc <-
  function(fit, truth, estimate, test_data) {
    fit |>
      augment(test_data) |>
      roc_curve(truth = {{ truth }}, estimate = {{ estimate }}, event_level = "second")
  }

in_bed_10 <- 
  read_rds("/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed/in_bed_simple_tree_fit_10_sec_AXED.rds")

in_bed_30 <- 
  read_rds("/media/esbenlykke/My Passport/chained_models/fitted_workflows/in_bed/in_bed_simple_tree_fit_30_sec_AXED.rds")

test_10 <- 
  read_parquet("data/data_for_modelling/chained_classifiers/testing_10_sec_data.parquet") %>% 
  mutate(
    in_bed = as_factor(in_bed)
  )

test_30 <- 
  read_parquet("data/data_for_modelling/chained_classifiers/testing_30_sec_data.parquet") %>% 
  mutate(
    in_bed = as_factor(in_bed)
  )

roc_data_10 <- get_roc(in_bed_30, in_bed, .pred_1, test_30) 

roc_data_30 <- get_roc(in_bed_10, in_bed, .pred_1, test_10) 

# Create plots
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

plot_roc <- function(roc_data, title) {
  roc_data %>%
    ggplot(
      aes(1 - specificity, sensitivity)
    ) + # plot with 2 ROC curves for each model
    geom_line(linewidth = .8, alpha = .7, color = "#EEE8D5") +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.4, lty = 2, color = "lightblue") +
    scale_color_brewer(palette = "Dark2") +
    labs(
      subtitle = title,
      x = "1 - Specificity",
      y = "Sensitivity"
    ) 
}

p1 <- plot_roc(roc_data_10, "10 Sec Epoch Data")
p2 <- plot_roc(roc_data_30, "30 Sec Epoch Data")

plot_in_bed_roc <- p1 + p2 & roc_theme

ggsave("presentation/visuals/plot_in_bed_roc.png", width = 14, height = 6)
