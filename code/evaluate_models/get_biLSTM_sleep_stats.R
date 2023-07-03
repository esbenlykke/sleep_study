library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)

lstm_preds <-
  read_parquet("data/data_for_modelling/lstm/predictions/lstm_multiclass_preds.parquet") %>%
  select(id, datetime, noon_day, month, matches("score|predicted"))


# Function to count zeros in runs of at least three consecutive zeros (WASO)
count_each_zero_in_consecutive_zeros <- function(x, n = 3) {
  rle_x <- rle(x == 0)
  sum(rle_x$lengths[rle_x$values & rle_x$lengths >= n])
}

# 0 = in-bed awake, 1 = out-bed awake, 2 = in-bed asleep

lstm_stats <-
  lstm_preds %>%
  mutate(
    across(where(is.factor), as.numeric) - 1,
    slide_sum_in_bed_awake_raw = slide_sum(predicted_class_raw == 0, before = 12, after = 12),
    slide_sum_out_bed_awake_raw = slide_sum(predicted_class_raw == 1, before = 12, after = 12),
    slide_sum_in_bed_asleep_raw = slide_sum(predicted_class_raw == 2, before = 12, after = 12),
    slide_sum_in_bed_awake_median5 = slide_sum(predicted_class_median_5 == 0, before = 12, after = 12),
    slide_sum_out_bed_awake_median5 = slide_sum(predicted_class_median_5 == 1, before = 12, after = 12),
    slide_sum_in_bed_asleep_median5 = slide_sum(predicted_class_median_5 == 2, before = 12, after = 12),
    slide_sum_in_bed_awake_median10 = slide_sum(predicted_class_median_10 == 0, before = 12, after = 12),
    slide_sum_out_bed_awake_median10 = slide_sum(predicted_class_median_10 == 1, before = 12, after = 12),
    slide_sum_in_bed_asleep_median10 = slide_sum(predicted_class_median_10 == 2, before = 12, after = 12)
  ) %>%
  group_by(id, noon_day, month) %>%
  summarise(
    spt_raw = (sum(slide_sum_in_bed_awake_raw >= 20, slide_sum_in_bed_asleep_raw >= 20)) * 30 / 60 / 60,
    tst_raw = (sum(slide_sum_in_bed_asleep_raw >= 20)) * 30 / 60 / 60,
    se_raw = (tst_raw / spt_raw) * 100,
    lps_raw = if (any(slide_sum_in_bed_awake_raw > 20) &
      min(row_number()[slide_sum_in_bed_asleep_raw >= 20]) >= min(row_number()[slide_sum_in_bed_awake_raw >= 20])) {
      (min(row_number()[slide_sum_in_bed_asleep_raw >= 20]) -
        min(row_number()[slide_sum_in_bed_awake_raw > 20])) * 30 / 60
    } else {
      0
    },
    waso_raw = if ((count_each_zero_in_consecutive_zeros(predicted_class_raw) * 30 / 60) - lps_raw > 0) {
      (count_each_zero_in_consecutive_zeros(predicted_class_raw) * 30 / 60) - lps_raw
    } else {
      0
    },
    spt_median5 = (sum(slide_sum_in_bed_awake_median5 >= 20, slide_sum_in_bed_asleep_median5 >= 20)) * 30 / 60 / 60,
    tst_median5 = (sum(slide_sum_in_bed_asleep_median5 >= 20)) * 30 / 60 / 60,
    se_median5 = (tst_median5 / spt_median5) * 100,
    lps_median5 = if (any(slide_sum_in_bed_awake_median5 > 20) &
      min(row_number()[slide_sum_in_bed_asleep_median5 >= 20]) >= min(row_number()[slide_sum_in_bed_awake_median5 >= 20])) {
      (min(row_number()[slide_sum_in_bed_asleep_median5 >= 20]) -
        min(row_number()[slide_sum_in_bed_awake_median5 > 20])) * 30 / 60
    } else {
      0
    },
    waso_median5 = if ((count_each_zero_in_consecutive_zeros(predicted_class_median_5) * 30 / 60) - lps_median5 > 0) {
      (count_each_zero_in_consecutive_zeros(predicted_class_median_5) * 30 / 60) - lps_median5
    } else {
      0
    },
    spt_median10 = (sum(slide_sum_in_bed_awake_median10 >= 20, slide_sum_in_bed_asleep_median10 >= 20)) * 30 / 60 / 60,
    tst_median10 = (sum(slide_sum_in_bed_asleep_median10 >= 20)) * 30 / 60 / 60,
    se_median10 = (tst_median10 / spt_median10) * 100,
    lps_median10 = if (any(slide_sum_in_bed_awake_median10 > 20) &
      min(row_number()[slide_sum_in_bed_asleep_median10 >= 20]) >= min(row_number()[slide_sum_in_bed_awake_median10 >= 20])) {
      (min(row_number()[slide_sum_in_bed_asleep_median10 >= 20]) -
        min(row_number()[slide_sum_in_bed_awake_median10 > 20])) * 30 / 60
    } else {
      0
    },
    waso_median10 = if ((count_each_zero_in_consecutive_zeros(predicted_class_median_10) * 30 / 60) - lps_median10 > 0) {
      (count_each_zero_in_consecutive_zeros(predicted_class_median_10) * 30 / 60) - lps_median10
    } else {
      0
    },
    .groups = "drop"
  ) %>%
  pivot_longer(-c(id, noon_day, month)) %>%
  separate(name, c("metric", "type"), sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  arrange(desc(type))


############## Look at ZM raw predictions
zm_score <-
  read_tsv("data/processed/zm_scores.tsv") %>%
  mutate(
    noon_day = day(datetime - hours(12)),
    month = month(datetime - hours(12)),
    in_bed = 1,
    sleep_raw = if_else(score %in% c(2, 3, 5, -5), 1, 0),
    sleep_raw_slide_12 = slide_sum(sleep_raw, after = 24),
    sleep_median_5 = slide_dbl(sleep_raw, median, .before = 5, .after = 5),
    sleep_slide_12_median_5 = slide_sum(sleep_median_5, after = 24),
    sleep_median_10 = slide_dbl(sleep_raw, median, .before = 10, .after = 10),
    sleep_slide_12_median_10 = slide_sum(sleep_median_10, after = 24),
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

# Calculate precise sleep stats from raw zm predictions
calculated_zm_stats <-
  no_edge_SP_zm_data %>%
  group_by(id, noon_day, month) %>%
  summarise(
    spt_hrs_raw = n() * 30 / 60 / 60,
    tst_hrs_raw = sum(sleep_raw, na.rm = TRUE) * 30 / 60 / 60,
    se_percent_raw = ifelse(spt_hrs_raw != 0, 100 * (tst_hrs_raw / spt_hrs_raw), NA),
    lps_min_raw = if (any(sleep_raw_slide_12 == 20)) {
      (min(row_number()[sleep_raw_slide_12 == 20], na.rm = TRUE) + 3) * 30 / 60
    } else {
      NA
    },
    waso_3_min_raw = (count_each_zero_in_consecutive_zeros(sleep_raw) * 30 / 60) - lps_min_raw,
    spt_hrs_median_5 = n() * 30 / 60 / 60,
    tst_hrs_median_5 = sum(sleep_median_5, na.rm = TRUE) * 30 / 60 / 60,
    se_percent_median_5 = ifelse(spt_hrs_raw != 0, 100 * (tst_hrs_median_5 / spt_hrs_raw), NA),
    lps_min_median_5 = if (any(sleep_slide_12_median_5 == 20)) {
      (min(row_number()[sleep_slide_12_median_5 == 20], na.rm = TRUE) + 3) * 30 / 60
    } else {
      NA
    },
    waso_3_min_median_5 = (count_each_zero_in_consecutive_zeros(sleep_median_5) * 30 / 60) - lps_min_median_5,
    spt_hrs_median_10 = n() * 30 / 60 / 60,
    tst_hrs_median_10 = sum(sleep_median_10, na.rm = TRUE) * 30 / 60 / 60,
    se_percent_median_10 = ifelse(spt_hrs_raw != 0, 100 * (tst_hrs_median_10 / spt_hrs_raw), NA),
    lps_min_median_10 = if (any(sleep_slide_12_median_10 == 20)) {
      (min(row_number()[sleep_slide_12_median_10 == 20], na.rm = TRUE) + 3) * 30 / 60
    } else {
      NA
    },
    waso_3_min_median_10 = (count_each_zero_in_consecutive_zeros(sleep_median_10) * 30 / 60) - lps_min_median_10,
    .groups = "drop"
  ) %>%
  rename_with(.cols = -c(id, noon_day, month), ~ paste0("zm_", .)) %>%
  pivot_longer(-c(id, noon_day, month)) %>%
  mutate(name = case_when(
    str_detect(name, "(?=.*spt)(?=.*raw)") ~ "spt_raw",
    str_detect(name, "(?=.*tst)(?=.*raw)") ~ "tst_raw",
    str_detect(name, "(?=.*se)(?=.*raw)") ~ "se_raw",
    str_detect(name, "(?=.*lps)(?=.*raw)") ~ "lps_raw",
    str_detect(name, "(?=.*waso)(?=.*raw)") ~ "waso_raw",
    str_detect(name, "(?=.*spt)(?=.*median_5)") ~ "spt_median5",
    str_detect(name, "(?=.*tst)(?=.*median_5)") ~ "tst_median5",
    str_detect(name, "(?=.*se)(?=.*median_5)") ~ "se_median5",
    str_detect(name, "(?=.*lps)(?=.*median_5)") ~ "lps_median5",
    str_detect(name, "(?=.*waso)(?=.*median_5)") ~ "waso_median5",
    str_detect(name, "(?=.*spt)(?=.*median_10)") ~ "spt_median10",
    str_detect(name, "(?=.*tst)(?=.*median_10)") ~ "tst_median10",
    str_detect(name, "(?=.*se)(?=.*median_10)") ~ "se_median10",
    str_detect(name, "(?=.*lps)(?=.*median_10)") ~ "lps_median10",
    str_detect(name, "(?=.*waso)(?=.*median_10)") ~ "waso_median10"
  )) %>%
  separate(name, c("metric", "type"), sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename_with(.cols = spt:waso, ~ paste0("zm_", .)) %>%
  arrange(desc(type)) %>%
  drop_na()

lstm_stats %>% 
  left_join(calculated_zm_stats, by = c("id", "noon_day", "month", "type")) %>% 
  mutate(model = "biLSTM") %>% 
  write_csv("data/data_for_modelling/lstm/stats/biLSTM_stats.csv")