library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)

lstm_preds <-
  read_parquet("data/data_for_modelling/lstm/predictions/lstm_multiclass_preds.parquet")


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
    slide_sum_in_bed_awake = slide_sum(predicted_class == 0, before = 12, after = 12),
    slide_sum_out_bed_awake = slide_sum(predicted_class == 1, before = 12, after = 12),
    slide_sum_in_bed_asleep = slide_sum(predicted_class == 2, before = 12, after = 12)
  ) %>% # select(score_simple_filtered, contains("sum"))
  select(id, noon_day, month, datetime, matches("score|predicted|sum")) %>%
  group_by(id, noon_day, month) %>%
  summarise(
    spt_hrs = if(any(slide_sum_in_bed_awake >= 20 | slide_sum_in_bed_asleep >= 20)) {
      (max(row_number()[slide_sum_in_bed_awake >= 20 | slide_sum_in_bed_asleep >= 20])
       - min(row_number()[slide_sum_in_bed_awake >= 20 | slide_sum_in_bed_asleep >= 20])) * 30 / 60 / 60
    } else {
      NA
    },
    tst_hrs = sum(predicted_class == 2 & slide_sum_in_bed_asleep >= 20) * 30 / 60 / 60,
    se_percent = ifelse(spt_hrs != 0, 100 * (tst_hrs / spt_hrs), NA),
    lps_min = if (any(slide_sum_in_bed_asleep == 20) & any(slide_sum_in_bed_awake > 20)) {
      (min(row_number()[slide_sum_in_bed_asleep >= 20]) -
         min(row_number()[slide_sum_in_bed_awake > 20])) * 30 / 60
    } else {
      NA
    },
    waso_min = sum(predicted_class == 1) * 30 / 60 / 60,
    waso_3 = count_each_zero_in_consecutive_zeros(predicted_class) * 30 / 60,
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
    sleep_median_10 = slide_dbl(sleep, median, .after = 20),
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
    spt_hrs = n() * 30 / 60 / 60,
    tst_hrs = sum(sleep_median_10, na.rm = TRUE) * 30 / 60 / 60,
    se_percent = ifelse(spt_hrs != 0, 100 * (tst_hrs / spt_hrs), NA),
    lps_min = if (any(sleep_slide_12_median_10 == 20)) {
      (min(row_number()[sleep_slide_12_median_10 == 20], na.rm = TRUE) + 3) * 30 / 60
    } else {
      NA
    },
    waso_3_min = (count_each_zero_in_consecutive_zeros(sleep) * 30 / 60) - lps_min,
    .groups = "drop"
  ) %>%
  rename_with(.cols = -c(id, noon_day, month), ~ paste0("zm_", .)) 


lstm_zm_stats <-
  lstm_stats %>%
  inner_join(calculated_zm_stats, by = c("id", "noon_day", "month")) %>% 
  drop_na() %>% 
  filter(
    between(spt_hrs, 5, 15) &
      between(tst_hrs, 5, 15) &
      between(se_percent, 50, 100) &
      between(lps_min, 10, 60) &
      between(waso_min, 5, 60) &
      between(waso_3, 5, 60) &
      reduce(.x = map(.x = ., .f = is.finite), .f = `&`)
  )

calculated_raw_zm_stats <-
  no_edge_SP_zm_data %>%
  # filter(id == 8504) %>%
  group_by(id, noon_day, month) %>%
  summarise(
    spt_hrs = n() * 30 / 60 / 60,
    tst_hrs = sum(sleep) * 30 / 60 / 60,
    se_percent = 100 * (tst_hrs / spt_hrs),
    lps_min = (min(row_number()[sleep_slide_12 == 20]) + 3) * 30 / 60,
    waso_3_min = (count_each_zero_in_consecutive_zeros(sleep) * 30 / 60) - lps_min,
    .groups = "drop"
  ) %>%
  rename_with(.cols = -c(id, noon_day, month), ~ paste0("zm_", .))

zm_stats <-
  read_csv("data/processed/all_zm_stats.csv") %>%
  janitor::clean_names() %>%
  rename_with(.cols = spt_hrs:waso_min, ~ paste0("zm_", .)) %>%
  mutate(
    noon_day = day(as_datetime(start_date)),
    .after = id
  ) %>%
  select(id, noon_day, zm_spt_hrs:zm_waso_min)

zm_stats %>%
  filter(id == 37305) %>%
  inner_join(
    calculated_raw_zm_stats %>% filter(id == 37305)
  ) %>%
  select(contains("lps"))



lstm_zm_stats %>%
  mutate(spt_hrs = if_else(is.infinite(spt_hrs) | spt_hrs < 0,
    mean(spt_hrs, na.rm = TRUE, trim = .1),
    spt_hrs
  )) %>%
  summarise(
    across(everything(), ~ sum(is.infinite(.x) | .x < 0))
  ) %>%
  t()

x <- lstm_zm_stats %>%
  mutate(spt_hrs = if_else(is.infinite(spt_hrs) | spt_hrs < 0,
    mean(spt_hrs, na.rm = TRUE, trim = .1),
    spt_hrs
  )) %>%
  filter(!tst_hrs < 5 & !tst_hrs > 12 & !zm_tst_hrs < 5 & !zm_tst_hrs > 12)

rmcorr(id, tst_hrs, zm_tst_hrs, x)
cor(x$spt_hrs, x$zm_spt_hrs)

x %>%
  ggplot(aes(tst_hrs, zm_tst_hrs)) +
  geom_point() +
  coord_fixed() +
  theme_light()

lstm_preds %>%
  filter(id == 298505) %>%
  ggplot(aes(datetime, group = 1)) +
  geom_line(aes(y = score_simple_filtered), color = "grey75") +
  geom_line(aes(y = as.numeric(predicted_class) - .2), color = "steelblue") +
  facet_wrap(~noon_day, scales = "free", ncol = 1) +
  theme_classic()
