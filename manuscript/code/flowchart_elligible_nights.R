library(tidyverse)
library(ggtext)
library(showtext)


showtext_auto()
font_add_google("Montserrat", family = "Montserrat")

data <-
  tibble(x = 1:100, y = 1:100)

annotate_text_size <- 2
annotate_lineheight <- 1
box_color <- "grey80"
font_family <- "Montserrat"

flow <-
  data |>
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 130, 10)) +
  # main boxes
  geom_rect(
    xmin = 10, xmax = 50, ymin = 117, ymax = 127, color = "#93A1A1",
    fill = "grey80", linewidth = 0.25
  ) +
  annotate("text",
           x = 30, y = 122,
           label = "2278 nights recorded with\nZmachine during SCREENS trial",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 10, xmax = 50, ymin = 92, ymax = 102, color = "#93A1A1",
    fill = "grey80", linewidth = 0.25
  ) +
  annotate("text",
           x = 30, y = 97,
           label = "1174 nights from children\nwith accelerometer recordings",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 10, xmax = 50, ymin = 67, ymax = 77, color = "#93A1A1",
    fill = "grey80", linewidth = 0.25
  ) +
  annotate("text",
           x = 30, y = 72, label = "1032 nights with measured sleep",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 10, xmax = 50, ymin = 42, ymax = 52, color = "#93A1A1",
    fill = "grey80", linewidth = 0.25
  ) +
  annotate("text",
           x = 30, y = 47, label = "779 nights with a duration\nbetween 7 and 14 hours",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 10, xmax = 50, ymin = 17, ymax = 27, color = "#93A1A1",
    fill = "grey80", linewidth = 0.25
  ) +
  annotate("text",
           x = 30, y = 22, label = "585 nights were included in this study",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  # vertical arrows
  geom_segment(
    x = 30, xend = 30, y = 117, yend = 102,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 30, xend = 30, y = 92, yend = 77,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 30, xend = 30, y = 67, yend = 52,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 30, xend = 30, y = 42, yend = 27,
    linewidth = 0.15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  # horizontal arrows
  geom_segment(
    x = 30, xend = 45, y = 109.5, yend = 109.5,
    linewidth = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 30, xend = 45, y = 84.5, yend = 84.5,
    linewidth = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 30, xend = 45, y = 84.5, yend = 84.5,
    linewidth = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 30, xend = 45, y = 59.5, yend = 59.5,
    linewidth = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  geom_segment(
    x = 30, xend = 45, y = 34.5, yend = 34.5,
    linewidth = .15, lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    color = "#93A1A1"
  ) +
  # boxes to the side
  geom_rect(
    xmin = 45, xmax = 85, ymin = 104.5, ymax = 114.5, color = "#93A1A1",
    fill = "grey80", linewidth = 0.25
  ) +
  annotate("text",
           x = 65, y = 109.5,
           label = "1104 nights\nfrom adults excluded",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 45, xmax = 85, ymin = 79.5, ymax = 89.5, color = "#93A1A1",
    fill = "grey80", linewidth = 0.25
  ) +
  annotate("text",
           x = 65, y = 84.5,
           label = "42 nights with no\nmeasured sleep excluded",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 45, xmax = 85, ymin = 54.5, ymax = 64.5, color = "#93A1A1",
    fill = "grey80", linewidth = 0.25
  ) +
  annotate("text",
           x = 65, y = 59.5,
           label = "Excluded 253 nights not\n7 hrs to 14 hrs in duration",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  geom_rect(
    xmin = 45, xmax = 85, ymin = 29.5, ymax = 39.5, color = "#93A1A1",
    fill = "grey80", linewidth = 0.25
  ) +
  annotate("text",
           x = 65, y = 34.5,
           label = "Excluded 194 nights\nwith sensor problems",
           size = annotate_text_size, color = "grey15", lineheight = annotate_lineheight,
           family = font_family
  ) +
  ylim(20, 125) +
  theme_void() 

ggsave("manuscript/visuals/flowchart_of_elligible_nights.pdf", width = 12.5, height = 8, units = "cm", dpi = 600)
