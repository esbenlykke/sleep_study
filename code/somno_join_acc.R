#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(arrow)

acc <- 
  read_feather("data/processed/acc_temp_psg_study.feather") |> 
  mutate(id = as.numeric(id)) |> 
  rename(datetime = time) |> 
  separate(datetime, c("date", "hour"), sep = " ", remove = FALSE) |> 
  mutate(unix_day = as.numeric(hms(hour)),
         unix_day = unix_day %/% 5 * 5)

somno <- 
  read_tsv("data/processed/somno_sleep_profiles.tsv") |> 
  slice(rep(1:n(), each = 30 / 5)) %>%
  mutate(unix_day = as.numeric(hms(time)),
         unix_day = unix_day + rep_len(seq(0, 25, 5), length.out = nrow(.))) 

somno |> 
  left_join(acc, by = c("id", "unix_day")) |> 
  select(id, datetime, placement, x = X, y = Y, z = Z, temp = temperature, status, reliability) |> 
  write_parquet("data/processed/somno_acc.parquet")
