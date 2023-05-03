#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)
library(gt)

roc_auc_10 <-
  read_csv(here::here("presentation/data/all_roc_auc_sleep_10_sec_epochs.csv"))

roc_auc_30 <-
  read_csv(here::here("presentation/data/all_roc_auc_sleep_30_sec_epochs.csv"))

table_sleep_roc_auc <-
  bind_rows(epoch_10 = roc_auc_10, epoch_30 = roc_auc_30, .id = "epoch_length") %>%
  select(-.metric, -.estimator) %>%
  pivot_wider(names_from = epoch_length, values_from = .estimate) %>%
  gt(groupname_col = "group") %>%
  fmt_number(3:4, decimals = 2) %>%
  tab_spanner(label = "ROC AUC", columns = 3:4) %>% 
  cols_label(epoch_10 = "10 sec epochs",
             epoch_30 = "30 sec epochs",
             model = "") %>% 
  tab_options(
    table.font.names = "ibm",
    table.font.color.light = "#EEE8D5",
    table.background.color = "#002B36",
    heading.border.bottom.style = "none",
    heading.align = "center",
    heading.title.font.size = 26,
    table.border.top.width = px(3),
    table.border.top.style = "none", # transparent
    table.border.bottom.style = "none",
    source_notes.border.bottom.color = "#002B36",
    column_labels.font.weight = "normal",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "#2C4E57",
    column_labels.padding = px(1),
    row_group.border.top.style = "none",
    row_group.border.top.color = "#2C4E57",
    row_group.border.bottom.width = px(0),
    row_group.border.bottom.color = "#2C4E57",
    stub.border.color = "#2C4E57",
    stub.border.width = px(0),
    stub.font.size = 14,
    table_body.border.bottom.color = "#002B36",
    table_body.hlines.color = "#2C4E57",
    table_body.vlines.style = "none",
    data_row.padding = px(1)
  )
