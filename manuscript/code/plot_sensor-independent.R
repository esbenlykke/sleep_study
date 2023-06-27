library(tidyverse)
library(lubridate)
library(showtext)
library(ggtext)
library(patchwork)


font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()

text_size <- 2
linewidth <- .3

# Define the start and end date-time
start_datetime <- as.POSIXct("2023-06-01 12:00:00", tz = "UTC")
end_datetime <- as.POSIXct("2023-06-03 12:00:00", tz = "UTC")
datetime_seq <- seq(from = start_datetime, to = end_datetime, by = "3 min")

start_group1 <- as.POSIXct("2023-06-01 19:00:00", tz = "UTC")
end_group1 <- as.POSIXct("2023-06-02 10:00:00", tz = "UTC")
start_group2 <- as.POSIXct("2023-06-02 19:00:00", tz = "UTC")
end_group2 <- as.POSIXct("2023-06-03 10:00:00", tz = "UTC")

plot_proxy_cos <- tibble(
  datetime = datetime_seq,
  group = case_when(
    datetime >= start_group1 & datetime <= end_group1 ~ 1,
    datetime >= start_group2 & datetime <= end_group2 ~ 2,
    TRUE ~ 0
  )
) %>%
  group_by(group) %>%
  mutate(
    clock_proxy_cos = if_else(group != 0, cos(seq(-(pi / 2), pi / 2, length.out = n())), 0),
    clock_proxy_linear = if_else(group != 0, seq(0, 1, length.out = n()), 0)
  ) %>%
  ggplot(aes(datetime)) +
  geom_vline(xintercept = start_group1, color = "grey50", lty = 2, linewidth = linewidth) +
  geom_vline(xintercept = end_group1, color = "grey50", lty = 2, linewidth = linewidth) +
  geom_vline(xintercept = start_group2, color = "grey50", lty = 2, linewidth = linewidth) +
  geom_vline(xintercept = end_group2, color = "grey50", lty = 2, linewidth = linewidth) +
  geom_line(aes(y = clock_proxy_cos), linewidth = linewidth) +
  annotate(
    geom = "text",
    x = start_group1 - minutes(40),
    y = .5,
    label = "10:00",
    color = "grey50",
    angle = 90,
    size = text_size
  ) +
  annotate(
    geom = "text",
    x = end_group1 + minutes(40),
    y = .5,
    label = "19:00",
    color = "grey50",
    angle = 90,
    size = text_size
  ) +
  annotate(
    geom = "text",
    x = start_group2 - minutes(40),
    y = .5,
    label = "10:00",
    color = "grey50",
    angle = 90,
    size = text_size
  ) +
  annotate(
    geom = "text",
    x = end_group2 + minutes(40),
    y = .5,
    label = "19:00",
    color = "grey50",
    angle = 90,
    size = text_size
  ) +
  scale_x_continuous(
    breaks = seq(start_datetime, end_datetime, "3 hours"),
    labels = format(seq(from = start_datetime, to = end_datetime, by = "3 hours"), format = "%H:%M")
  ) +
  # scale_y_continuous(expand = c(.001, 0)) +
  labs(
    # title = "Clock Proxy Cosinus",
    x = NULL,
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    axis.text = element_text(size = 5)
  )

plot_proxy_lin <-
  tibble(
    datetime = datetime_seq,
    group = case_when(
      datetime >= start_group1 & datetime <= end_group1 ~ 1,
      datetime >= start_group2 & datetime <= end_group2 ~ 2,
      TRUE ~ 0
    )
  ) %>%
  group_by(group) %>%
  mutate(
    clock_proxy_cos = if_else(group != 0, cos(seq(-(pi / 2), pi / 2, length.out = n())), 0),
    clock_proxy_linear = if_else(group != 0, seq(0, 1, length.out = n()), 0)
  ) %>%
  ggplot(aes(datetime)) +
  geom_vline(xintercept = start_group1, color = "grey50", lty = 2, linewidth = linewidth) +
  geom_vline(xintercept = end_group1, color = "grey50", lty = 2, linewidth = linewidth) +
  geom_vline(xintercept = start_group2, color = "grey50", lty = 2, linewidth = linewidth) +
  geom_vline(xintercept = end_group2, color = "grey50", lty = 2, linewidth = linewidth) +
  geom_line(aes(y = clock_proxy_linear), linewidth = linewidth) +
  annotate(
    geom = "text",
    x = start_group1 - minutes(40),
    y = .5,
    label = "10:00",
    color = "grey50",
    angle = 90,
    size = text_size
  ) +
  annotate(
    geom = "text",
    x = end_group1 + minutes(40),
    y = .5,
    label = "19:00",
    color = "grey50",
    angle = 90,
    size = text_size
  ) +
  annotate(
    geom = "text",
    x = start_group2 - minutes(40),
    y = .5,
    label = "10:00",
    color = "grey50",
    angle = 90,
    size = text_size
  ) +
  annotate(
    geom = "text",
    x = end_group2 + minutes(40),
    y = .5,
    label = "19:00",
    color = "grey50",
    angle = 90,
    size = text_size
  ) +
  scale_x_continuous(
    breaks = seq(start_datetime, end_datetime, "3 hours"),
    labels = format(seq(from = start_datetime, to = end_datetime, by = "3 hours"), format = "%H:%M")
  ) +
  # scale_y_continuous(expand = c(.001, 0)) +
  labs(
    # title = "Clock Proxy Cosinus",
    x = NULL,
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    axis.text = element_text(size = 5)
  )

plot_proxy_cos / plot_proxy_lin &
  plot_annotation(tag_levels = "A")

ggsave("manuscript/visuals/sensor_independent.pdf", height = 3.5, width = 7)
