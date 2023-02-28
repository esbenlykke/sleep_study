library(tidyverse)
library(lubridate)
library(ggtext)

font_add_google("Mukta", family = "mukta")
font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()

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
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Note: simulated data"
  ) +
  theme(
    text = element_text(color = "#EEE8D5"),
    axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
    axis.text = element_text(size = 20, color = "#EEE8D5"),
    plot.title = element_text(family = "ibm", size = 30, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = "#293D42"),
    plot.background = element_rect(fill = "#002B36", color = "#EEE8D5"),
    panel.background = element_rect(color = "#002B36", fill = "#002B36"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#002B36"),
    legend.key = element_rect(fill = "#002B36"),
    legend.text = element_text(size = 16)
  )

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
    x = as_datetime("2020-01-15 21:00:00") - minutes(30),
    y = .5,  
    color = "#EEE8D5", size = 6, angle = 90
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
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Note: simulated data"
  ) +
  theme(
    text = element_text(color = "#EEE8D5"),
    axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
    axis.text = element_text(size = 20, color = "#EEE8D5"),
    plot.title = element_text(family = "ibm", size = 30, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = "#293D42"),
    plot.background = element_rect(fill = "#002B36", color = "#EEE8D5"),
    panel.background = element_rect(color = "#002B36", fill = "#002B36"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#002B36"),
    legend.key = element_rect(fill = "#002B36"),
    legend.text = element_text(size = 16)
  )

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
    limits = c(as_datetime("2020-01-15 16:00:00"), as_datetime("2020-01-16 09:00:00"))) +
  scale_color_manual(
    values = c("palegreen", "darkorange"),
    labels = c("In-Bed", "Asleep")
  ) +
  labs(
    title = "Example from Single-Night Predictions",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Note: simulated data"
  ) +
  theme(
    text = element_text(color = "#EEE8D5"),
    axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
    axis.text = element_text(size = 20, color = "#EEE8D5"),
    plot.title = element_text(family = "ibm", size = 30, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = "#293D42"),
    plot.background = element_rect(fill = "#002B36", color = "#EEE8D5"),
    panel.background = element_rect(color = "#002B36", fill = "#002B36"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#002B36"),
    legend.key = element_rect(fill = "#002B36"),
    legend.text = element_text(size = 16)
  )
