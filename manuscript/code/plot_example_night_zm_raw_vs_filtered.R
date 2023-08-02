#!/usr/bin/env Rscript
library(tidyverse)
library(patchwork)

zm_score <-
  read_tsv("data/processed/zm_scores.tsv") %>% # count(id) %>% print(n = Inf)
  filter(id == 8505) %>% 
  mutate(
    noon_day = day(datetime - hours(12)),
    month = month(datetime - hours(12)),
    in_bed = 1,
    sleep = if_else(score %in% c(2, 3, 5, -5), 1, 0),
    sleep_slide_12 = slide_sum(sleep, after = 24),
    sleep_median_5 = slide_dbl(sleep, median, .before = 5, .after = 5),
    sleep_median_10 = slide_dbl(sleep, median, .before = 10, .after = 10),
    sleep_slide_12_median_10 = slide_sum(sleep_median_10, after = 24),
    .after = 1
  )

p1 <- zm_score %>% 
  filter(noon_day == 15) %>%
  ggplot(aes(datetime, group = 1)) +
  geom_line(aes(y = sleep + .05), color = "grey75", linewidth = .3, lineend = "round") +
  geom_line(aes(y = sleep_median_5 - .05), lineend = "round") +
  # geom_line(aes(y = sleep_median_10 - .1), lineend = "round") +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  scale_x_datetime(breaks = "hour", date_labels = "%H:%M") +
  labs(x = NULL,
       y = "Asleep") +
  # facet_wrap(~noon_day, scales = "free", ncol = 1) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 8)
  )

p2 <- zm_score %>% 
  filter(noon_day == 15) %>%
  ggplot(aes(datetime, group = 1)) +
  geom_line(aes(y = sleep + .05), color = "grey75", linewidth = .3, lineend = "round") +
  # geom_line(aes(y = sleep_median_5 - .05), lineend = "round") +
  geom_line(aes(y = sleep_median_10 - .05), lineend = "round") +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  scale_x_datetime(breaks = "hour", date_labels = "%H:%M") +
  labs(x = NULL,
       y = "Asleep") +
  # facet_wrap(~noon_day, scales = "free", ncol = 1) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 8)
  )

p1 / p2 & plot_annotation(tag_levels = "A")

ggsave("manuscript/visuals/zm_raw_vs_filtered.pdf", width = 7, height = 3.5)
