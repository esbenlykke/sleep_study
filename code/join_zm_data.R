pacman::p_load(tidyverse,
               vroom,
               here)

files <-
  list.files(here("data/raw/zmachine"), ".csv", full.names = TRUE)

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
  vroom_write(here("data/processed/zm_scores.tsv"))