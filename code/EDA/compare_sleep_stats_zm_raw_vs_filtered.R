library(tidyverse)
library(furrr)
library(slider)
library(arrow)
library(lubridate)

zm_raw <-
  read_tsv("data/processed/zm_scores.tsv")

zm_stats <-
  read_csv("data/processed/all_zm_stats.csv")


zm_raw %>%
  # filter(score != -5) %>%
  mutate(
    noon_day = day(datetime - hours(12)),
    sleep = if_else(score != 0, 1, 0),
    sleep_24 = slide_dbl(sleep, sum, .after = 24),
    waso = slide_lgl(sleep, ~ sum(.x) == 0, .before = 3) # NB 90 consecutive seconds
  ) |> # filter(id == 8504 & noon_day == 14) %>% view()
  group_by(id, noon_day) |>
  summarise(
    spt_hrs = n() * 30 / 60 / 60,
    tst_hrs = sum(sleep) * 30 / 60 / 60,
    se_percent = 100 * (tst_hrs / spt_hrs),
    row = min(row_number()[sleep_24 == 20]) + 4,
    lps_min = (min(row_number()[sleep_24 == 20]) + 4) * 30 / 60,
    waso_min = (sum(waso) * 30) / 60,
    .groups = "drop"
  )

zm_filtered <-
  zm_raw %>%
  filter(id == 37304) %>%
  mutate(
    score_filtered = slide_dbl(score, median, .after = 10)
  )

# write_parquet(zm_filtered, "data/processed/all_zm_stats_median_filtered_10_min.parquet")

# plot raw score vs filtered score
zm_filtered %>%
  filter(id == 37304) %>%
  ggplot() +
  geom_step(aes(datetime, score), alpha = .5) +
  geom_step(aes(datetime, score_filtered - 1.2), color = "red") +
  scale_x_datetime(breaks = "1 hours", date_labels = "%H:%M") +
  facet_wrap(~day, scales = "free") +
  theme_light()

zm_filtered %>%
  # filter(score != -5) %>%
  mutate(
    noon_day = day(datetime - hours(12)),
    score_filtered = slide_dbl(score, median, .after = 10),
    sleep = if_else(score_filtered != 0, 1, 0),
    sleep_24 = slide_dbl(sleep, sum, .after = 24),
    waso = slide_lgl(sleep, ~ sum(.x) == 0, .before = 3)
  ) |> # filter(id == 8504 & noon_day == 14) %>% view()
  group_by(id, noon_day) |>
  summarise(
    spt_hrs = n() * 30 / 60 / 60,
    tst_hrs = sum(sleep) * 30 / 60 / 60,
    se_percent = 100 * (tst_hrs / spt_hrs),
    row = min(row_number()[sleep_24 == 20]) + 4,
    lps_min = (min(row_number()[sleep_24 == 20]) + 4) * 30 / 60,
    waso_min = (sum(waso) * 30) / 60 - lps_min,
    .groups = "drop"
  ) %>%
  filter(id == 37304)


zm_stats %>% filter(id == 37304)
