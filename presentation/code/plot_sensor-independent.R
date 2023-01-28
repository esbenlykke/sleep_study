library(tidyverse)
library(lubridate)
library(showtext)
library(ggtext)

proxy <-
  read_csv(here::here("presentation/data/__clock_proxy_example.csv"))

font_add_google("IBM Plex Serif", family = "ibm")
showtext_auto()

static_start <-
  proxy |>
  slice(min(row_number()[clock_proxy_cos > 0 & noon_day == 27])) |>
  pull(datetime)

static_end <-
  proxy |>
  slice(max(row_number()[clock_proxy_cos > 0 & noon_day == 27])) |>
  pull(datetime)

clock_proxy_start27 <-
  proxy |>
  slice(min(row_number()[clock_proxy_cos > 0 & noon_day == 25])) |>
  pull(datetime)

clock_proxy_end27 <-
  proxy |>
  slice(max(row_number()[clock_proxy_cos > 0 & noon_day == 25])) |>
  pull(datetime)

# clock_proxy_start26 <-
#   proxy |>
#   slice(min(row_number()[clock_proxy_cos > 0 & noon_day == 26])) |>
#   pull(datetime)
#
# clock_proxy_end26 <-
#   proxy |>
#   slice(max(row_number()[clock_proxy_cos > 0 & noon_day == 26])) |>
#   pull(datetime)

plot_proxy_cos <-
  proxy |>
  ggplot(aes(datetime, clock_proxy_cos)) +
  geom_step(color = "#EEE8D5", linewidth = .8, alpha = .8) +
  geom_step(color = "#4D8C57", linewidth = .3, alpha = .8) +
  geom_vline(xintercept = as_datetime(static_start), color = "darkorange", lty = 2) +
  geom_vline(xintercept = as_datetime(static_end), color = "darkorange", lty = 2) +
  geom_vline(xintercept = as_datetime(clock_proxy_start27), color = "lightblue", lty = 2) +
  geom_vline(xintercept = as_datetime(clock_proxy_end27), color = "lightblue", lty = 2) +
  # geom_vline(xintercept = as_datetime(clock_proxy_start26), color = "lightblue", lty = 2) +
  # geom_vline(xintercept = as_datetime(clock_proxy_end26), color = "lightblue", lty = 2) +
  annotate(
    geom = "text",
    x = as_datetime(static_start) - hours(1),
    y = .5,
    label = hms::as_hms(static_start - seconds(10)),
    color = "#EEE8D5",
    angle = 90,
    size = 8
  ) +
  annotate(
    geom = "text",
    x = as_datetime(static_end) + hours(1),
    y = .5,
    label = hms::as_hms(static_end + seconds(10)),
    color = "#EEE8D5",
    angle = 90,
    size = 8
  ) +
  annotate(
    geom = "text",
    x = as_datetime(clock_proxy_start27) - hours(1),
    y = .5,
    label = hms::as_hms(clock_proxy_start27),
    color = "#EEE8D5",
    angle = 90,
    size = 8
  ) +
  annotate(
    geom = "text",
    x = as_datetime(clock_proxy_end27) + hours(1),
    y = .5,
    label = hms::as_hms(clock_proxy_end27),
    color = "#EEE8D5",
    angle = 90,
    size = 8
  ) +
  # annotate(
  #   geom = "text",
  #   x = as_datetime(clock_proxy_start26) - hours(1),
  #   y = .5,
  #   label = hms::as_hms(clock_proxy_start26),
  #   color = "#EEE8D5",
  #   angle = 90,
  #   size = 8
  # ) +
  # annotate(
  #   geom = "text",
  #   x = as_datetime(clock_proxy_end26) + hours(1),
  #   y = .5,
  #   label = hms::as_hms(clock_proxy_end26),
  #   color = "#EEE8D5",
  #   angle = 90,
  #   size = 8
  # ) +
  scale_x_datetime(
    date_labels = "%b %d",
    date_breaks = "1 day",
  ) +
  labs(
    title = "Clock Proxy Cosinus",
    subtitle = "Determined by <span style='color:lightblue'>Standard Deviation Threshold</span> *(n<sub>nights</sub> = 760)* and <span style = 'color:darkorange'>Static Time Stamps</span> *(n<sub>nights</sub> = 19)*",
    x = NULL,
    y = NULL
  ) +
  theme(
    text = element_text(color = "#EEE8D5", family = "ibm"),
    axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
    axis.text = element_text(size = 20, color = "#EEE8D5"),
    axis.text.y = element_blank(),
    plot.title = element_text(family = "ibm", size = 30, face = "bold", hjust = .5),
    plot.subtitle = element_markdown(size = 20, hjust = .5),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = "#293D42"),
    plot.background = element_rect(fill = "#002B36", color = "#EEE8D5"),
    panel.background = element_rect(color = "#002B36", fill = "#002B36")
  )

plot_proxy_lin <-
  proxy |>
  ggplot(aes(datetime, clock_proxy_linear)) +
  geom_step(color = "#EEE8D5", linewidth = .8, alpha = .8) +
  geom_step(color = "#4D8C57", linewidth = .3, alpha = .8) +
  geom_vline(xintercept = as_datetime(static_start), color = "darkorange", lty = 2) +
  geom_vline(xintercept = as_datetime(static_end), color = "darkorange", lty = 2) +
  geom_vline(xintercept = as_datetime(clock_proxy_start27), color = "lightblue", lty = 2) +
  geom_vline(xintercept = as_datetime(clock_proxy_end27), color = "lightblue", lty = 2) +
  # geom_vline(xintercept = as_datetime(clock_proxy_start26), color = "lightblue", lty = 2) +
  # geom_vline(xintercept = as_datetime(clock_proxy_end26), color = "lightblue", lty = 2) +
  annotate(
    geom = "text",
    x = as_datetime(static_start) - hours(1),
    y = .5,
    label = hms::as_hms(static_start - seconds(10)),
    color = "#EEE8D5",
    angle = 90,
    size = 8
  ) +
  annotate(
    geom = "text",
    x = as_datetime(static_end) + hours(1),
    y = .5,
    label = hms::as_hms(static_start - seconds(10)),
    color = "#EEE8D5",
    angle = 90,
    size = 8
  ) +
  annotate(
    geom = "text",
    x = as_datetime(clock_proxy_start27) - hours(1),
    y = .5,
    label = hms::as_hms(clock_proxy_start27),
    color = "#EEE8D5",
    angle = 90,
    size = 8
  ) +
  annotate(
    geom = "text",
    x = as_datetime(clock_proxy_end27) + hours(1),
    y = .5,
    label = hms::as_hms(clock_proxy_end27),
    color = "#EEE8D5",
    angle = 90,
    size = 8
  ) +
  # annotate(
  #   geom = "text",
  #   x = as_datetime(clock_proxy_start26) - hours(1),
  #   y = .5,
  #   label = hms::as_hms(clock_proxy_start26),
  #   color = "#EEE8D5",
  #   angle = 90,
  #   size = 8
  # ) +
  # annotate(
  #   geom = "text",
#   x = as_datetime(clock_proxy_end26) + hours(1),
#   y = .5,
#   label = hms::as_hms(clock_proxy_end26),
#   color = "#EEE8D5",
#   angle = 90,
#   size = 8
# ) +
scale_x_datetime(
  date_labels = "%b %d",
  date_breaks = "1 day",
) +
  labs(
    title = "Clock Proxy Linear",
    subtitle = "Determined by <span style='color:lightblue'>Standard Deviation Threshold</span> *(n<sub>nights</sub> = 760)* and <span style = 'color:darkorange'>Static Time Stamps</span> *(n<sub>nights</sub> = 19)*",
    x = NULL,
    y = NULL
  ) +
  theme(
    text = element_text(color = "#EEE8D5", family = "ibm"),
    axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
    axis.text = element_text(size = 20, color = "#EEE8D5"),
    axis.text.y = element_blank(),
    plot.title = element_text(family = "ibm", size = 30, face = "bold", hjust = .5),
    plot.subtitle = element_markdown(size = 20, hjust = .5),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(color = "#293D42"),
    plot.background = element_rect(fill = "#002B36", color = "#EEE8D5"),
    panel.background = element_rect(color = "#002B36", fill = "#002B36")
  )

# plot_static <-
#   static |>
#   ggplot(aes(datetime, clock_proxy_cos)) +
#   geom_step(color = "#EEE8D5", linewidth = .8, alpha = .8) +
#   geom_step(color = "#4D8C57", linewidth = .3, alpha = .8) +
#   geom_vline(xintercept = as_datetime("2020-02-29 21:00:00"), color = "lightblue", lty = 2) +
#   geom_vline(xintercept = as_datetime("2020-03-01 07:00:00"), color = "lightblue", lty = 2) +
#   annotate(
#     geom = "text",
#     x = as_datetime("2020-02-29 21:00:00") - hours(1),
#     y = .5,
#     label = "21:00:00",
#     color = "#EEE8D5",
#     angle = 90,
#     size = 8
#   ) +
#   annotate(
#     geom = "text",
#     x = as_datetime("2020-03-01 07:00:00") + hours(1),
#     y = .5,
#     label = "07:00:00",
#     color = "#EEE8D5",
#     angle = 90,
#     size = 8
#   ) +
#   scale_x_datetime(
#     date_labels = "%b %d",
#     date_breaks = "1 day",
#   ) +
#   labs(
#     title = "Clock Proxy Cosinus",
#     subtitle = "From Static Time Stamps",
#     x = NULL,
#     y = NULL
#   ) +
#   theme(
#     text = element_text(color = "#EEE8D5", family = "ibm"),
#     axis.title = element_text(family = "ibm", size = 20, color = "#EEE8D5"),
#     axis.text = element_text(size = 20, color = "#EEE8D5"),
#     axis.text.y = element_blank(),
#     plot.title = element_text(family = "ibm", size = 30, face = "bold"),
#     plot.subtitle = element_text(size = 20),
#     panel.grid.minor = element_blank(),
#     panel.grid = element_line(color = "#293D42"),
#     plot.background = element_rect(fill = "#002B36", color = "#EEE8D5"),
#     panel.background = element_rect(color = "#002B36", fill = "#002B36")
#   )
