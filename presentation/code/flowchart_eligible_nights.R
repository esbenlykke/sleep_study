library(tidyverse)
library(ggtext)

data <-
  tibble(x = 1:100, y = 1:100)

flow <-
  data |>
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  # main boxes
  geom_rect(
    xmin = 10, xmax = 50, ymin = 92, ymax = 102, color = "#93A1A1",
    fill = "grey15", linewidth = 0.25
  ) +
  annotate("text",
    x = 30, y = 97,
    label = "2278 nights were recorded with\nZmachine during the SCREENS trial",
    size = 4, color = "#EEE8D5"
  ) +
  geom_rect(
    xmin = 10, xmax = 50, ymin = 67, ymax = 77, color = "#93A1A1",
    fill = "grey15", linewidth = 0.25
  ) +
  annotate("text",
    x = 30, y = 72,
    label = "1174 night from children\nwith accelerometer recordings",
    size = 4, color = "#EEE8D5"
  ) +
  geom_rect(
    xmin = 10, xmax = 50, ymin = 42, ymax = 52, color = "#93A1A1",
    fill = "grey15", linewidth = 0.25
  ) +
  annotate("text",
    x = 30, y = 47, label = "1032 night with measured sleep",
    size = 4, color = "#EEE8D5"
  ) +
  geom_rect(
    xmin = 10, xmax = 50, ymin = 17, ymax = 27, color = "#93A1A1",
    fill = "grey15", linewidth = 0.25
  ) +
  annotate("text",
    x = 30, y = 22, label = "779 nights with a duration\nbetween 7 and 12 hours",
    size = 4, color = "#EEE8D5"
  ) +
  # vertical arrows
  geom_segment(
    x = 30, xend = 30, y = 92, yend = 77,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 30, xend = 30, y = 67, yend = 52,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 30, xend = 30, y = 42, yend = 27,
    size = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  # horizontal arrows
  geom_segment(
    x = 30, xend = 45, y = 84.5, yend = 84.5,
    size = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 30, xend = 45, y = 59.5, yend = 59.5,
    size = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 30, xend = 45, y = 34.5, yend = 34.5,
    size = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  # boxes to the side
  geom_rect(
    xmin = 45, xmax = 85, ymin = 79.5, ymax = 89.5, color = "#93A1A1",
    fill = "grey15", linewidth = 0.25
  ) +
  annotate("text",
    x = 65, y = 84.5,
    label = "1104 nights\nfrom adults excluded",
    size = 4, color = "#EEE8D5"
  ) +
  geom_rect(
    xmin = 45, xmax = 85, ymin = 54.5, ymax = 64.5, color = "#93A1A1",
    fill = "grey15", linewidth = 0.25
  ) +
  annotate("text",
    x = 65, y = 59.5,
    label = "42 nights with no\nmeasured sleep excluded",
    size = 4, color = "#EEE8D5"
  ) +
  geom_rect(
    xmin = 45, xmax = 85, ymin = 29.5, ymax = 39.5, color = "#93A1A1",
    fill = "grey15", linewidth = 0.25
  ) +
  annotate("text",
    x = 65, y = 34.5,
    label = "253 nights not between\n7 hrs and 12 hrs excluded¹",
    size = 4, color = "#EEE8D5"
  ) +
  # train and test boxes
  geom_rect(
    xmin = 20, xmax = 40, ymin = 0, ymax = 10, color = "#93A1A1",
    fill = "grey15", linewidth = 0.25
  ) +
  annotate("text",
    x = 30, y = 5, label = "391 nights, 80 subjects\nfor training",
    size = 4, color = "#EEE8D5"
  ) +
  geom_rect(
    xmin = 55, xmax = 75, ymin = 0, ymax = 10, color = "#93A1A1",
    fill = "grey15", linewidth = 0.25
  ) +
  annotate("text",
    x = 65, y = 5,
    label = "388 nights, 77 subjects\nfor testing",
    size = 4, color = "#EEE8D5"
  ) +
  # final arrows
  geom_segment(
    x = 30, xend = 30, y = 17, yend = 10,
    size = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 50, xend = 65, y = 22, yend = 22,
    size = .15, lineend = "butt",
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 65, xend = 65, y = 22, yend = 10,
    size = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  theme_void() +
  theme(
    text = element_text(color = "#EEE8D5", family = "ibm"),
    plot.title = element_text(family = "ibm", size = 30, face = "bold", hjust = .5),
    plot.subtitle = element_markdown(size = 20, hjust = .5),
    plot.background = element_rect(fill = "#002B36", color = "#EEE8D5"),
    panel.background = element_rect(color = "#002B36", fill = "#002B36")
  )
