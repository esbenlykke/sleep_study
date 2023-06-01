library(tidyverse)
library(slider)
library(arrow)

# Define a function to count zeros in runs of at least three consecutive zeros (WASO)
count_each_zero_in_consecutive_zeros <- function(x, in_bed_window, n = 3) {
  x_in_bed <- x[in_bed_window == 1]
  rle_x <- rle(x_in_bed == 0)
  sum(rle_x$lengths[rle_x$values & rle_x$lengths >= n])
}


all_preds_filenames <-
  list.files("data/data_for_modelling/chained_classifiers/sleep_predictions", full.names = TRUE)

test_dc <-
  read_parquet(all_preds_filenames[[1]]) %>%
  select(id, noon_day, month, datetime, matches("in_bed|sleep"))

testslide <-
  test_dc %>%
  filter(id == 3404) %>%
  group_by(id, noon_day, month) %>%
  mutate(
    across(where(is.factor), as.numeric) - 1,
    across(matches("sleep|in_bed"), ~ slide_sum(.x, after = 24), .names = "{.col}_slide_sum_12"), # 24 epochs = 12 minutes
  )

testslide %>% # select(id, noon_day, datetime, in_bed, in_bed_class, in_bed_class_slide_sum_12) %>% view()
  mutate(
    in_bed_window = if_else(between(
      row_number(),
      min(which(in_bed_class_slide_sum_12 == 20)),
      max(which(in_bed_class_slide_sum_12 == 20))
    ), 1, 0)
  ) %>%
  summarise(
    spt_hrs = sum(in_bed_window) * 30 / 60 / 60,
    tst_hrs = (sum(sleep_median_10_pred_class[in_bed_window == 1]) * 30) / 60 / 60,
    se_percent = 100 * (tst_hrs / spt_hrs),
    lps_min = (((min(row_number()[sleep_median_10_pred_class_slide_sum_12 == 20])) - min(row_number()[in_bed_window == 1] - 30)) * 10) / 60,
    waso_min = ((sum(sleep_median_10_pred_class == 0 & in_bed_window == 1) - lps_min) * 30) / 60,
    waso_3 = count_each_zero_in_consecutive_zeros(sleep_median_10_pred_class, in_bed_window) * 30 / 60,
    .groups = "drop"
  )

############## Look at ZM raw predictions
zm_score <-
  read_tsv("data/processed/zm_scores.tsv") %>%
  mutate(
    noon_day = day(datetime - hours(12)),
    month = month(datetime - hours(12)),
    in_bed = 1,
    sleep = if_else(score %in% c(2, 3, 5, -5), 1, 0),
    sleep_slide_12 = slide_sum(sleep, after = 24),
    .after = 1
  )

# Extract no edge SP days
no_edge_SP_zm_data <- 
  zm_score %>%
  group_by(id, noon_day, month) %>%
  # For each group, perform the following steps
  group_modify(~ {
    # Store the data of the current group
    current_date_data <- .x
    
    # Check if the first or last recording of the night has a sensor problem (score == -5)
    first_problem <- current_date_data$score[1] != -5
    last_problem <- tail(current_date_data$score, n = 1) != -5
    
    # If there's no sensor problem at the beginning or end of the night, return the data
    if (first_problem & last_problem) {
      return(current_date_data)
    } else {
      # Otherwise, return an empty tibble, effectively removing this group from the final output
      return(tibble())
    }
  }) 


 no_edge_SP_zm_data %>% 
  filter(id == 8504) %>%
  group_by(id, noon_day, month) %>% 
  summarise(
    spt_hrs = n() * 30 / 60 / 60,
    tst_hrs = sum(sleep) * 30 / 60 / 60,
    se_percent = 100 * (tst_hrs / spt_hrs), ### TODO fix the last stats!!!
    lps_min = (((min(row_number()[sleep_median_10_pred_class_slide_sum_12 == 20])) - min(row_number()[in_bed_window == 1] - 30)) * 10) / 60,
    waso_min = ((sum(sleep_median_10_pred_class == 0 & in_bed_window == 1) - lps_min) * 30) / 60,
    waso_3 = count_each_zero_in_consecutive_zeros(sleep_median_10_pred_class, in_bed_window) * 30 / 60,
    .groups = "drop"
  )

zm_stats <-
  read_csv("data/processed/all_zm_stats.csv") |>
  janitor::clean_names() |>
  mutate(
    day = day(start_date),
    month = month(start_date)
  ) |>
  select(id, day, month, spt_hrs, tst_hrs, se_percent, lps_min, waso_min) |>
  rename_with(.cols = -c(id, day, month), ~ paste0("zm_", .))

zm_stats %>% 
  filter(id == 8504)
################






