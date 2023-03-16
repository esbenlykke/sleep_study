library(tidyverse)
library(lubridate)
library(ggtext)

font_add_google("Mukta", family = "mukta")
font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()

my_theme <- 
  theme(
    text = element_text(color = "#EEE8D5"),
    axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
    axis.text = element_text(size = 20, color = "#EEE8D5"),
    plot.title = element_text(family = "ibm", size = 30, face = "bold"),
    plot.subtitle = element_text(family = "ibm", size = 20),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = "#293D42"),
    plot.background = element_rect(fill = "#002B36", color = "#EEE8D5"),
    panel.background = element_rect(color = "#002B36", fill = "#002B36"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#002B36"),
    legend.key = element_rect(fill = "#002B36"),
    legend.text = element_text(size = 16),
    strip.text = element_blank()
  )


multiclass_ex <-
  tibble(
  datetime = seq(
    as_datetime("2020-01-15 12:00:00"),
    as_datetime("2020-01-16 12:00:00"), seconds(10)
  ),
  in_bed_awake = if_else(
    (datetime > as_datetime("2020-01-15 21:00:00") &
      datetime < as_datetime("2020-01-15 21:30:00")) |
      (datetime > as_datetime("2020-01-16 06:00:00") &
        datetime < as_datetime("2020-01-16 06:50:00")), 1, 0
  ),
  in_bed_asleep = if_else(
    datetime > as_datetime("2020-01-15 21:30:00") &
      datetime < as_datetime("2020-01-16 06:00:00"), 1, 0
  ),
  out_bed_awake = if_else(
    datetime > as_datetime("2020-01-15 12:00:00") &
      datetime < as_datetime("2020-01-15 21:00:00") |
      datetime > as_datetime("2020-01-16 06:50:00") &
        datetime < as_datetime("2020-01-16 12:00:00"), 1, 0
  )
) %>% 
  # pivot_longer(-datetime) %>% 
    ggplot() +
  # ggplot(aes(datetime, value, color = name)) +
  # geom_step() +
  # facet_wrap(~ name, ncol = 1) +
  geom_step(aes(datetime, in_bed_awake,
                color = "1")
  ) +
  geom_step(aes(datetime, in_bed_asleep + 1.1,
                color = "2"), alpha = .7
  ) +
  geom_step(aes(datetime, out_bed_awake - 1.1,
                color = "3"), alpha = .7
  ) +
  scale_y_continuous(
    breaks = c(0, 1),
    labels = c("No", "Yes")
    # limits = c(-.2, 1.5)
  ) +
  scale_x_datetime(
    breaks = c(
      as_datetime("2020-01-15 21:00:00"),
      as_datetime("2020-01-16 00:00:00"),
      as_datetime("2020-01-16 07:00:00")
    ),
    labels = c("Evening", "Midnight", "Morning"),
    limits = c(as_datetime("2020-01-15 16:00:00"), as_datetime("2020-01-16 09:00:00"))
  ) +
  scale_color_manual(
    values = c("palegreen", "darkorange", "lightblue"),
    labels = c("In-Bed-Awake", "In-Bed-Asleep", "Out-Bed-Awake")
  ) +
  labs(
    title = "Example from Single-Night Predictions",
    subtitle = "Multiclass and binary relevance approach",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Note: simulated data"
  ) +
  my_theme

multiclass_lps <- 
tibble(
  datetime = seq(
    as_datetime("2020-01-15 12:00:00"),
    as_datetime("2020-01-16 12:00:00"), seconds(10)
  ),
  in_bed_awake = if_else(
    (datetime > as_datetime("2020-01-15 21:00:00") &
       datetime < as_datetime("2020-01-15 21:30:00")) |
      (datetime > as_datetime("2020-01-16 06:00:00") &
         datetime < as_datetime("2020-01-16 06:50:00")), 1, 0
  ),
  in_bed_asleep = if_else(
    datetime > as_datetime("2020-01-15 21:30:00") &
      datetime < as_datetime("2020-01-16 06:00:00"), 1, 0
  ),
  out_bed_awake = if_else(
    datetime > as_datetime("2020-01-15 12:00:00") &
      datetime < as_datetime("2020-01-15 21:00:00") |
      datetime > as_datetime("2020-01-16 06:50:00") &
      datetime < as_datetime("2020-01-16 12:00:00"), 1, 0
  )
) %>% 
  # pivot_longer(-datetime) %>% 
  ggplot() +
  annotate(
    geom = "rect",
    xmin = as_datetime("2020-01-15 21:00:00"),
    xmax = as_datetime("2020-01-15 21:30:00"),
    ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = .5
  ) +
  annotate(
    geom = "text", label = "Latency Until Persistent Sleep",
    x = as_datetime("2020-01-15 21:00:00") + minutes(150),
    y = .7,
    color = "#EEE8D5", size = 6
  ) +
  # ggplot(aes(datetime, value, color = name)) +
  # geom_step() +
  # facet_wrap(~ name, ncol = 1) +
  geom_step(aes(datetime, in_bed_awake,
                color = "1")
  ) +
  geom_step(aes(datetime, in_bed_asleep + 1.1,
                color = "2"), alpha = .7
  ) +
  geom_step(aes(datetime, out_bed_awake - 1.1,
                color = "3"), alpha = .7
  ) +
  scale_y_continuous(
    breaks = c(0, 1),
    labels = c("No", "Yes")
    # limits = c(-.2, 1.5)
  ) +
  scale_x_datetime(
    breaks = c(
      as_datetime("2020-01-15 21:00:00"),
      as_datetime("2020-01-16 00:00:00"),
      as_datetime("2020-01-16 07:00:00")
    ),
    labels = c("Evening", "Midnight", "Morning"),
    limits = c(as_datetime("2020-01-15 16:00:00"), as_datetime("2020-01-16 09:00:00"))
  ) +
  scale_color_manual(
    values = c("palegreen", "darkorange", "lightblue"),
    labels = c("In-Bed-Awake", "In-Bed-Asleep", "Out-Bed-Awake")
  ) +
  labs(
    title = "Example from Single-Night Predictions",
    subtitle = "Multiclass and binary relevance approach",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Note: simulated data"
  ) +
  my_theme

multiclass_tst <- 
  tibble(
    datetime = seq(
      as_datetime("2020-01-15 12:00:00"),
      as_datetime("2020-01-16 12:00:00"), seconds(10)
    ),
    in_bed_awake = if_else(
      (datetime > as_datetime("2020-01-15 21:00:00") &
         datetime < as_datetime("2020-01-15 21:30:00")) |
        (datetime > as_datetime("2020-01-16 06:00:00") &
           datetime < as_datetime("2020-01-16 06:50:00")), 1, 0
    ),
    in_bed_asleep = if_else(
      datetime > as_datetime("2020-01-15 21:30:00") &
        datetime < as_datetime("2020-01-16 06:00:00"), 1, 0
    ),
    out_bed_awake = if_else(
      datetime > as_datetime("2020-01-15 12:00:00") &
        datetime < as_datetime("2020-01-15 21:00:00") |
        datetime > as_datetime("2020-01-16 06:50:00") &
        datetime < as_datetime("2020-01-16 12:00:00"), 1, 0
    )
  ) %>% 
  # pivot_longer(-datetime) %>% 
  ggplot() +
  annotate(
    geom = "rect",
    xmin = as_datetime("2020-01-15 21:30:00"),
    xmax = as_datetime("2020-01-16 06:50:00"),
    ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = .5
  ) +
  annotate(
    geom = "text", x = as_datetime("2020-01-16 02:00:00") - minutes(30),
    y = .5, label = "Total Sleep Time",
    color = "#EEE8D5", size = 6
  ) +
  # ggplot(aes(datetime, value, color = name)) +
  # geom_step() +
  # facet_wrap(~ name, ncol = 1) +
  geom_step(aes(datetime, in_bed_awake,
                color = "1")
  ) +
  geom_step(aes(datetime, in_bed_asleep + 1.1,
                color = "2"), alpha = .7
  ) +
  geom_step(aes(datetime, out_bed_awake - 1.1,
                color = "3"), alpha = .7
  ) +
  scale_y_continuous(
    breaks = c(0, 1),
    labels = c("No", "Yes")
    # limits = c(-.2, 1.5)
  ) +
  scale_x_datetime(
    breaks = c(
      as_datetime("2020-01-15 21:00:00"),
      as_datetime("2020-01-16 00:00:00"),
      as_datetime("2020-01-16 07:00:00")
    ),
    labels = c("Evening", "Midnight", "Morning"),
    limits = c(as_datetime("2020-01-15 16:00:00"), as_datetime("2020-01-16 09:00:00"))
  ) +
  scale_color_manual(
    values = c("palegreen", "darkorange", "lightblue"),
    labels = c("In-Bed-Awake", "In-Bed-Asleep", "Out-Bed-Awake")
  ) +
  labs(
    title = "Example from Single-Night Predictions",
    subtitle = "Multiclass and binary relevance approach",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Note: simulated data"
  ) +
  my_theme


plot_ex <-
  tibble(
    datetime = seq(
      as_datetime("2020-01-15 12:00:00"),
      as_datetime("2020-01-16 12:00:00"), seconds(10)
    ),
    in_bed = if_else(
      datetime > as_datetime("2020-01-15 21:00:00") &
        datetime < as_datetime("2020-01-16 07:00:00"), 1, 0
    ),
    sleep = if_else(
      datetime > as_datetime("2020-01-15 21:30:00") &
        datetime < as_datetime("2020-01-16 06:50:00"), 1, 0
    )
  ) |>
  ggplot() +
  geom_step(aes(datetime, in_bed, color = "1")) +
  geom_step(aes(datetime, sleep - .05, color = "2")) +
  scale_y_continuous(
    breaks = c(0, 1),
    labels = c("No", "Yes"),
    limits = c(-.2, 1.5)
  ) +
  scale_x_datetime(
    breaks = c(
      as_datetime("2020-01-15 21:00:00"),
      as_datetime("2020-01-16 00:00:00"),
      as_datetime("2020-01-16 07:00:00")
    ),
    labels = c("Evening", "Midnight", "Morning"),
    limits = c(as_datetime("2020-01-15 16:00:00"), as_datetime("2020-01-16 09:00:00"))
  ) +
  scale_color_manual(
    values = c("palegreen", "darkorange"),
    labels = c("In-Bed", "Asleep")
  ) +
  labs(
    title = "Example from Single-Night Predictions",
    subtitle = "Two binary classifiers approach",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Note: simulated data"
  ) +
  my_theme

plot_lps <-
  tibble(
    datetime = seq(
      as_datetime("2020-01-15 12:00:00"),
      as_datetime("2020-01-16 12:00:00"), seconds(10)
    ),
    in_bed = if_else(
      datetime > as_datetime("2020-01-15 21:00:00") &
        datetime < as_datetime("2020-01-16 07:00:00"), 1, 0
    ),
    sleep = if_else(
      datetime > as_datetime("2020-01-15 21:30:00") &
        datetime < as_datetime("2020-01-16 06:50:00"), 1, 0
    )
  ) |>
  ggplot() +
  annotate(
    geom = "rect",
    xmin = as_datetime("2020-01-15 21:00:00"),
    xmax = as_datetime("2020-01-15 21:30:00"),
    ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = .5
  ) +
  annotate(
    geom = "text", label = "Latency Until Persistent Sleep",
    x = as_datetime("2020-01-15 21:00:00") + minutes(150),
    y = 1.2,
    color = "#EEE8D5", size = 6
  ) +
  geom_step(aes(datetime, in_bed, color = "1")) +
  geom_step(aes(datetime, sleep - .05, color = "2")) +
  scale_y_continuous(
    breaks = c(0, 1),
    labels = c("No", "Yes"),
    limits = c(-.2, 1.5)
  ) +
  scale_x_datetime(
    breaks = c(
      as_datetime("2020-01-15 21:00:00"),
      as_datetime("2020-01-16 00:00:00"),
      as_datetime("2020-01-16 07:00:00")
    ),
    labels = c("Evening", "Midnight", "Morning"),
    limits = c(as_datetime("2020-01-15 16:00:00"), as_datetime("2020-01-16 09:00:00"))
  ) +
  scale_color_manual(
    values = c("palegreen", "darkorange"),
    labels = c("In-Bed", "Asleep")
  ) +
  labs(
    title = "Example from Single-Night Predictions",
    subtitle = "Two binary classifiers approach",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Note: simulated data"
  ) +
  my_theme

plot_tst <-
  tibble(
    datetime = seq(
      as_datetime("2020-01-15 12:00:00"),
      as_datetime("2020-01-16 12:00:00"), seconds(10)
    ),
    in_bed = if_else(
      datetime > as_datetime("2020-01-15 21:00:00") &
        datetime < as_datetime("2020-01-16 07:00:00"), 1, 0
    ),
    sleep = if_else(
      datetime > as_datetime("2020-01-15 21:30:00") &
        datetime < as_datetime("2020-01-16 06:50:00"), 1, 0
    )
  ) |>
  ggplot() +
  annotate(
    geom = "rect",
    xmin = as_datetime("2020-01-15 21:30:00"),
    xmax = as_datetime("2020-01-16 06:50:00"),
    ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = .5
  ) +
  annotate(
    geom = "text", x = as_datetime("2020-01-16 02:00:00") - minutes(30),
    y = .5, label = "Total Sleep Time",
    color = "#EEE8D5", size = 6
  ) +
  geom_step(aes(datetime, in_bed, color = "1")) +
  geom_step(aes(datetime, sleep - .05, color = "2")) +
  scale_y_continuous(
    breaks = c(0, 1),
    labels = c("No", "Yes"),
    limits = c(-.2, 1.5)
  ) +
  scale_x_datetime(
    breaks = c(
      as_datetime("2020-01-15 21:00:00"),
      as_datetime("2020-01-16 00:00:00"),
      as_datetime("2020-01-16 07:00:00")
    ),
    labels = c("Evening", "Midnight", "Morning"),
    limits = c(as_datetime("2020-01-15 16:00:00"), as_datetime("2020-01-16 09:00:00"))
  ) +
  scale_color_manual(
    values = c("palegreen", "darkorange"),
    labels = c("In-Bed", "Asleep")
  ) +
  labs(
    title = "Example from Single-Night Predictions",
    subtitle = "Two binary classifiers approach",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Note: simulated data"
  ) +
  my_theme
