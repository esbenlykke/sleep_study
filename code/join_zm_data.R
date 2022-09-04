pacman::p_load(tidyverse,
               vroom)

# Path to zm zmachine files
path <- "data/raw/screens_zmachine"

files <-
  list.files(path, ".csv", full.names = TRUE)

vroom(files, id = "id", col_types = "??iiiii") %>%
  janitor::clean_names() %>%
  mutate(
    date = anytime::anydate(date),
    time = hms::parse_hms(time),
    id = parse_number(id),
    datetime = lubridate::ymd_hms(paste0(date, time))
  ) %>%
  relocate(datetime, .after = time) %>%
  arrange(id) %>%
  vroom_write("data/processed/zm_scores.tsv")
