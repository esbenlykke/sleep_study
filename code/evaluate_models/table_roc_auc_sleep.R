#!/usr/bin/env

library(tidyverse)
library(tidymodels)
library(arrow)
library(gt)

roc_auc_10 <- 
  read_csv("presentation/data/all_roc_auc_sleep_10_sec_epochs.csv")

roc_auc_30 <- 
  read_csv("presentation/data/all_roc_auc_sleep_30_sec_epochs.csv")

bind_rows(epoch_10 = roc_auc_10, epoch_30 = roc_auc_30, .id = "epoch_length") %>% 
  select(-.metric, -.estimator) %>% 
  pivot_wider(names_from = epoch_length, values_from = .estimate) %>% 
  gt(groupname_col = "group") %>% 
  fmt_number(3:4, decimals = 2)