library(tidyverse)
library(arrow)

in_bed_roc <- read_parquet("data/processed/roc_data_in_bed.parquet")
sleep_roc <- read_parquet("data/processed/roc_data_sleep.parquet")
# ROC curve plots ---------------------------------------------------------


in_bed_roc |>
  ggplot(
    aes(1 - specificity, sensitivity, color = model)
  ) + # plot with 2 ROC curves for each model
  geom_line(linewidth = .8, alpha = .7) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.4, lty = 2) +
  scale_color_brewer(palette = "Dark2") +
  coord_fixed() +
  theme_light()

sleep_roc |>
  ggplot(
    aes(1 - specificity, sensitivity, color = model)
  ) + # plot with 2 ROC curves for each model
  geom_line(linewidth = .8, alpha = .7) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.4, lty = 2) +
  scale_color_brewer(palette = "Dark2") +
  coord_fixed() +
  theme_light()
