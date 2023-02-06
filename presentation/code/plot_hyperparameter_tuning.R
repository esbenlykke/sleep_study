library(tidyverse)
library(tidymodels)

data <- 
  read_rds(here::here("presentation/data/sleep_workflowsets_results.rds"))

hyper_plot <- 
  data |> 
  autoplot(
    rank_metric = "f_meas",  # <- how to order models
    metric = "f_meas",       # <- which metric to visualize
    select_best = FALSE     # <- one point per workflow
  ) +
  # geom_text(aes(y = mean - .003, label = wflow_id), angle = 90, hjust = 1) +
  lims(y = c(.96, 1)) +
  labs(
    title = "Hyperparameter Tuning",
    subtitle = "10-fold cross validation using 10-level grid search",
    color = "Model Type",
    shape = NULL,
    y = "F1 Metric",
    caption = "This figure contains only data from 1 person and, hence, is only for illustrative purposes."
  ) +
  scale_color_brewer(palette = "Dark2",
                     label = c("XGboost", "Logistic Regression", "MARS", "Neural Net", "Random Forest")) +
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