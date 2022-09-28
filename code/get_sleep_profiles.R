pacman::p_load(
  tidyverse,
  vroom,
  here
)

reliability_files <-
  list.files(here("data/raw/somno_analyses_data"),
    "Sleep Profile Reliability.txt",
    full.names = TRUE,
    recursive = TRUE
  )

sleep_status_files <-
  list.files(here("data/raw/somno_analyses_data"),
    "Sleep Profile.txt",
    full.names = TRUE,
    recursive = TRUE
  )

reliability_df <-
  vroom(reliability_files,
    id = "id", delim = ";",
    skip = 7, col_names = c("time", "reliability")
  ) %>%
  mutate(
    time = str_remove(time, ",000"),
    reliability = factor(str_remove(reliability, " Reliability")),
    id = parse_number(id),
    time = hms::as_hms(time)
  )

status_df <-
  vroom(sleep_status_files,
    id = "id", delim = ";",
    skip = 8, col_names = c("time", "status")
  ) %>%
  mutate(
    time = str_remove(time, ",000"),
    id = parse_number(id),
    time = hms::as_hms(time),
    status = factor(status)
  )

sleep_profiles <-
  inner_join(reliability_df, status_df, by = c("id", "time")) %>% 
  write_tsv(here("data/processed/somno_sleep_profiles.tsv"))
