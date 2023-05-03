#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)
library(patchwork)


sleep_roc_data_10 <- 
  read_parquet("presentation/data/all_roc_sleep_10.parquet") %>% 
  group_by(model, group) %>% 
  slice_sample(n = 1000)

sleep_roc_data_30 <- 
  read_parquet("presentation/data/all_roc_sleep_30.parquet") %>% 
  group_by(model, group) %>% 
  slice_sample(n = 1000)

roc_theme <- 
  theme(
    text = element_text(color = "#EEE8D5", family = "ibm"),
    axis.title = element_text(family = "ibm", size = 16, color = "#EEE8D5"),
    axis.text = element_text(size = 14, color = "#EEE8D5"),
    plot.title = element_text(family = "ibm", size = 30, face = "bold", hjust = .5, 
                              margin = margin(b = 40)),
    plot.subtitle = element_text(size = 20, hjust = .5),
    plot.caption = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = "#293D42"),
    plot.background = element_rect(fill = "#002B36", color = "#002B36"),
    panel.background = element_rect(color = "#002B36", fill = "#002B36"),
    legend.background = element_rect(fill = "#002B36"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 14, face = "bold"),
    legend.key = element_rect(fill = "#002B36"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "#002B36"),
    strip.text = element_text(color = "#EEE8D5", face = "bold")
  )

plot_roc <- function(roc_data, title) {
  roc_data %>%
    ggplot(
      aes(1 - specificity, sensitivity, color = model)
    ) +
    geom_line(linewidth = .8, alpha = .7) +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.4, lty = 2, color = "lightblue") +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = c(0, .5, 1)) +
    scale_y_continuous(breaks = c(0, .5, 1)) +
    facet_wrap(~ group) +
    labs(
      subtitle = title,
      x = "1 - Specificity",
      y = "Sensitivity",
      color = NULL
    ) 
}

p1 <- 
  plot_roc(sleep_roc_data_10, "10 Sec Epoch Data")

p2 <- 
  plot_roc(sleep_roc_data_30, "30 Sec Epoch Data") 

plot_sleep_roc <- 
  p1 / p2 + 
  plot_layout(guides = "collect") &
  roc_theme

ggsave("presentation/visuals/plot_sleep_roc.png", width = 14, height = 12)