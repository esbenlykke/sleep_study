#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)

# Path to zm zmachine files

files <-
  list.files("/media/esbenlykke/My Passport/sleep_study/data/raw/screens_zmachine", ".csv", full.names = TRUE)

read_csv(files, id = "id", col_types = "??iiiii") %>%
  janitor::clean_names() %>%
  unite("datetime", date:time, sep = " ") |> 
  mutate(
    datetime = as_datetime(datetime),
    id = parse_number(id)
  ) %>%
  arrange(id) %>%
  write_tsv("data/processed/zm_scores.tsv")
