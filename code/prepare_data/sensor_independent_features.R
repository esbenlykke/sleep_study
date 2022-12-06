#!/usr/bin/env Rscript

suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))
suppressMessages(library(arrow))

cat("Creating sensor-independent features. This won't take long...")

data <-
  read_parquet("data/processed/model_data/bsl_thigh.parquet")


# Subject-level in-bed time -----------------------------------------------

suppressWarnings({
  clock_proxy <-
    data |>
    # filter((id != 604804 | noon_day != 9) &
    #   (id != 757104 | !noon_day %in% c(1, 2, 29)) &
    #   (id != 2596204 | noon_day != 28)) |>
    rowid_to_column() |>
    group_by(id, noon_day) |>
    mutate(
      threshold_in_bed = sdacc_y < .1 & incl < 45 &
        (hms::as_hms(datetime) > hms("17:00:00") & hms::as_hms(datetime) < hms("23:59:00")),
      threshold_out_bed = sdacc_y > .1 & incl > 75 & # TODO snak med Jan om denne threshold!
        (hms::as_hms(datetime) > hms("06:00:00") & hms::as_hms(datetime) < hms("11:00:00")),
      proxy = if_else(row_number() > max(row_number()[threshold_in_bed == TRUE]) &
        row_number() < min(row_number()[threshold_out_bed == TRUE]),
      1L, 0L
      ),
      .after = 1
    ) |>
    group_by(id, noon_day, proxy) |>
    mutate(
      clock_proxy_cos = case_when(
        (id == 604804 & noon_day == 9) |
          (id == 757104 & noon_day %in% c(1, 2, 29)) |
          (id == 2596204 & noon_day == 28) ~ 0,
        proxy == 1 ~ cos(seq(-(pi / 2), pi / 2, length.out = n())),
        TRUE ~ 0
      ),
      clock_proxy_linear = case_when(
        (id == 604804 & noon_day == 9) |
          (id == 757104 & noon_day %in% c(1, 2, 29)) |
          (id == 2596204 & noon_day == 28) ~ 0,
        proxy == 1 ~ seq(0, 1, length.out = n()),
        TRUE ~ 0
      ),
      .after = 1
    ) |>
    ungroup() # |>count(id, noon_day) |> print(n = Inf)
})

# x <- warnings()

# Create static clock proxies for outlier IDs -----------------------------

# glue::glue("Handling {length(x)} problematic timestamps.\n")

static_proxy <-
  data |>
  filter((id == 604804 & noon_day == 9) |
    (id == 757104 & noon_day %in% c(1, 2, 29)) |
    (id == 2596204 & noon_day == 28)) |>
  group_by(id, noon_day) |>
  mutate(
    clock_group = if_else((hms::as_hms(datetime) > hms("18:00:00") | hms::as_hms(datetime) < hms("08:00:00")), 1, 0),
    .after = 1
  ) |>
  group_by(id, noon_day, clock_group) |>
  mutate(
    static_proxy_cos = if_else(hms::as_hms(datetime) > hms("18:00:00") | hms::as_hms(datetime) < hms("08:00:00"),
      cos(seq(-(pi / 2), pi / 2, length.out = n())), 0
    ),
    static_proxy_linear = if_else(hms::as_hms(datetime) > hms("18:00:00") | hms::as_hms(datetime) < hms("08:00:00"),
      seq(0, 1, length.out = n()), 0
    ),
    .after = 1
  ) |>
  ungroup() |>
  select(id, datetime, starts_with("static"))


# Merge and write ---------------------------------------------------------


clock_proxy |>
  full_join(static_proxy, by = c("id", "datetime")) |>
  replace_na(list(
    static_proxy_cos = 0, static_proxy_linear = 0,
    clock_proxy_cos = 0, clock_proxy_linear = 0
  )) |>
  # select(id, datetime, starts_with(c("static", "clock"))) |>
  mutate(
    clock_proxy_cos = static_proxy_cos + clock_proxy_cos,
    clock_proxy_linear = static_proxy_linear + clock_proxy_linear
  ) |>
  select(
    id, datetime, unix_time, day, noon_day, age, placement,
    clock_proxy_cos, clock_proxy_linear, incl, theta, temp:time_day
  ) |>
  write_parquet("data/processed/model_data/bsl_thigh_sensor_independent_features.parquet")


# Exploratory stuff on weekdays and time in bed ---------------------------


# in_bed <-
#   data |>
#   rowid_to_column() |>
#   group_by(id, noon_day) |>
#   mutate(
#     threshold = sdacc_y > .1 & incl < 45 &
#       (hms::as_hms(datetime) < hms("23:59:00") & hms::as_hms(datetime) > hms("17:00:00")),
#     .after = 1
#   ) |>
#   filter(threshold == TRUE) |>
#   slice_max(n = 1, order_by = rowid) |>
#   ungroup() |>
#   arrange(hms::as_hms(datetime))
#
#
# in_bed |>
#   ggplot(aes(hms::as_hms(datetime))) +
#   geom_density(fill = "brown", alpha = .7) +
#   theme_light()
#
# out_bed <-
#   data |>
#   rowid_to_column() |>
#   group_by(id, noon_day) |>
#   mutate(
#     threshold = sdacc_y > .1 & incl < 45 &
#       (hms::as_hms(datetime) > hms("06:00:00") & hms::as_hms(datetime) < hms("12:00:00")),
#     .after = 1
#   ) |>
#   filter(threshold == TRUE) |>
#   slice_min(n = 1, order_by = rowid) |>
#   ungroup() |>
#   arrange(-hms::as_hms(datetime))
#
# out_bed |>
#   ggplot(aes(hms::as_hms(datetime))) +
#   geom_density(fill = "brown", alpha = .7) +
#   theme_light()
#
#
# data |>
#   mutate(
#     weekday = wday(datetime, label = TRUE),
#     .after = 1
#   ) |>
#   count(weekday) |>
#   ggplot(aes(weekday, n)) +
#   geom_col(fill = "darkorange", color = "black") +
#   labs(
#     x = NULL,
#     y = NULL
#   ) +
#   theme_light()

# test |>
#   filter(id == 604804) |>
#   ggplot(aes(datetime, clock_proxy_cos)) +
#   geom_line() +
#   scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M") +
#   theme_light()
