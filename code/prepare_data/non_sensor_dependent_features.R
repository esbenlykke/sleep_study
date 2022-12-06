pacman::p_load(
  tidyverse,
  lubridate,
  hms,
  feather
)

bsl <-
  read_feather("data/processed/bsl.feather")

fup <-
  read_feather("data/processed/fup.feather")

## feature of time since recording onset (for each night)
# By defining the lights-off and lights-on instants as time
# t = 0 and t = 1 respectively, and by linearly interpolating
# the values for all epochs, a comparable (and normalized) time
# feature can be used for each recording.
bsl |> 
  filter(id == 8504) |>
  mutate(
    day = day(datetime_aligned),
    tod = as_hms(datetime_aligned),
    is_new_recording = if_else(unix_aligned - lag(unix_aligned) == 5, FALSE, TRUE),
    .after = datetime_aligned
  ) 


group_by(id, day) |>
  summarise(
    first = as_hms(min(tod)),
    last = as_hms(max(tod)),
    .groups = "drop"
  )


# cosine feature since recording onset (for each night)
