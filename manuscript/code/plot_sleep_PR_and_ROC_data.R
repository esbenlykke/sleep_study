#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)
library(patchwork)
library(ggsci)


sleep_pr_data <-
  read_parquet("data/processed/all_pr_sleep_30.parquet") %>%
  group_by(model, group) %>%
  slice_sample(n = 1000) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = c("raw", "median_5", "median_10")))

sleep_pr_auc <-
  read_csv("data/processed/all_pr_auc_sleep_30_sec_epochs.csv") 

pr_auc_labels <-
  sleep_pr_auc %>% 
  select(-.metric, -.estimator) %>% 
    mutate(recall = .2, precision = rep(c(.25, .23, .21, .19), 3),
           label = paste(model, round(.estimate, digits = 2))) %>% 
  mutate(
    group = case_when(group == "raw" ~ "Raw ZM Predictions",
                      group == "median_5" ~ "5-Min Median",
                      group == "median_10" ~ "10-Min Median")
  ) 

levels(sleep_pr_data$group)

sleep_pr_data %>%
  mutate(
    group = case_when(group == "raw" ~ "Raw ZM Predictions",
                      group == "median_5" ~ "5-Min Median",
                      group == "median_10" ~ "10-Min Median")
  ) %>% 
  ggplot(aes(recall, precision, color = model)) +
  geom_line(linewidth = .3) +
  geom_hline(yintercept = .5, linewidth = 0.3, lty = 2, color = "grey50") +
  scale_x_continuous(breaks = c(0, .5, 1)) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  scale_color_jama() +
  facet_wrap(~ group) +
  geom_text(data = pr_auc_labels, aes(label = label), size = 2, hjust = 0, vjust = 0) +
  labs(
    x = "Recall",
    y = "Precision",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 8),
    strip.background = element_rect(color = NA)
  )

ggsave("manuscript/visuals/plot_sleep_pr.pdf", width = 7, height = 3)

sleep_roc_data <-
  read_parquet("data/processed/all_roc_sleep_30.parquet") %>%
  group_by(model, group) %>%
  slice_sample(n = 1000) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = c("raw", "median_5", "median_10")))


sleep_roc_auc <-
  read_csv("data/processed/all_roc_auc_sleep_30_sec_epochs.csv")

roc_auc_labels <-
  sleep_pr_auc %>% 
  select(-.metric, -.estimator) %>% 
  mutate(specificity = .75, sensitivity = rep(c(.25, .23, .21, .19), 3),
         label = paste(model, round(.estimate, digits = 2))) %>% 
  mutate(
    group = case_when(group == "raw" ~ "Raw ZM Predictions",
                      group == "median_5" ~ "5-Min Median",
                      group == "median_10" ~ "10-Min Median")
  ) 

sleep_roc_data %>%
  mutate(
    group = case_when(group == "raw" ~ "Raw ZM Predictions",
                      group == "median_5" ~ "5-Min Median",
                      group == "median_10" ~ "10-Min Median")
  ) %>% 
  ggplot(aes(1 - specificity, sensitivity, color = model)) +
  geom_line(linewidth = .3) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.3, lty = 2, color = "grey50") +
  scale_x_continuous(breaks = c(0, .5, 1)) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  scale_color_jama() +
  facet_wrap(~ group) +
  geom_text(data = roc_auc_labels, aes(label = label), size = 2, hjust = 0, vjust = 0) +
  labs(
    x = "1 - Specificity",
    y = "Sensitivity",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 8),
    strip.background = element_rect(color = NA)
  )

ggsave("manuscript/visuals/plot_sleep_roc.pdf", width = 7, height = 3)
