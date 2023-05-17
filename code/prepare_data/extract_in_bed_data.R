#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)


process_data <- function(input_file, output_file) {
  cat(glue::glue("Extracting in-bed time from {str_match(input_file, '(?<=/)[^/]+$')}\n"))
  read_parquet(input_file) %>%
    group_by(id, noon_day, month) %>%
    group_modify(~ .x %>%
                   # Filter rows where row_number is within the in_bed_filtered range
                   filter(row_number() >= min(row_number()[in_bed_median5 == 1]) &
                            row_number() <= max(row_number()[in_bed_median5 == 1]))) %>%
    ungroup() %>%
    write_parquet(output_file)
}

# Define input and output file paths
files <- list(
  list(input = "data/data_for_modelling/no_edge_sp_incl_features_10_sec_epochs.parquet",
       output = "data/data_for_modelling/only_in_bed_data_no_edge_sp_incl_features_10_sec_epochs.parquet"),
  list(input = "data/data_for_modelling/no_edge_sp_incl_features_30_sec_epochs.parquet",
       output = "data/data_for_modelling/only_in_bed_data_no_edge_sp_incl_features_30_sec_epochs.parquet")
)

# Process each file
walk(files, ~process_data(.x$input, .x$output))


# EDA
# data_30 <-
#   read_parquet("data/data_for_modelling/no_edge_sp_incl_features_30_sec_epochs.parquet")
# 
# data_30 %>%
#   filter(id == 3404) %>%
#   mutate(
#     in_bed_median5 = slider::slide_dbl(in_bed, median, .before = 15, .after = 15)
#   ) %>%
#   ggplot(aes(datetime)) +
#   geom_line(aes(y = sleep_median5), color = "pink") +
#   geom_line(aes(y = sleep_median10 + .2), color = "lightblue") +
#   geom_line(aes(y = in_bed_median5 + 1.2), color = "grey60") +
#   geom_line(aes(y = y_sd), color = "brown") +
#   geom_line(aes(y = scale(incl)), color = "darkgreen") +
#   scale_x_datetime(date_labels = "%H", breaks = "1 hour") +
#   facet_wrap(~noon_day, scales = "free", ncol = 1) +
#   theme_classic() +
#   theme(
#     strip.background = element_rect(color = NA),
#     strip.text = element_text(size = 16, face = "bold")
#   )
# 
# data_30_no_in_bed <- 
#   read_parquet("data/data_for_modelling/only_in_bed_data_no_edge_sp_incl_features_30_sec_epochs.parquet")
# 
# data_30_no_in_bed %>%
#   filter(id == 3404) %>%
#   group_by(id, noon_day, month) %>%
#   group_modify(~ .x %>%
#     # Filter rows where row_number is within the in_bed_filtered range
#     filter(row_number() >= min(row_number()[in_bed_median5 == 1]) &
#       row_number() <= max(row_number()[in_bed_median5 == 1]))) %>%
#   ungroup() %>%
#   ggplot(aes(datetime)) +
#   geom_line(aes(y = sleep_median5), color = "pink") +
#   geom_line(aes(y = sleep_median10 + .2), color = "lightblue") +
#   geom_line(aes(y = in_bed_median5 + 1.2), color = "grey60") +
#   geom_line(aes(y = y_sd), color = "brown") +
#   geom_line(aes(y = scale(incl)), color = "darkgreen") +
#   scale_x_datetime(date_labels = "%H", breaks = "1 hour") +
#   facet_wrap(~noon_day, scales = "free", ncol = 1) +
#   theme_classic() +
#   theme(
#     strip.background = element_rect(color = NA),
#     strip.text = element_text(size = 16, face = "bold")
#   )

beepr::beep(4)