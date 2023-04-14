library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)

test <-
  read_parquet("data/data_for_modelling/crude_testing_data.parquet") 

in_bed_fit <-
  read_rds("/media/esbenlykke/My Passport/crude/fitted_models/axed_models/decision_tree_in_bed_AXED.rds")

sleep_fit <-
  read_rds("/media/esbenlykke/My Passport/crude/fitted_models/axed_models/decision_tree_sleep_median_AXED.rds")

get_in_bed_data <- function(in_bed_fit, sleep_fit) {
  in_bed_data <-
    fit %>%
    augment(test) %>%
    mutate(
      in_bed_filtered = slider::slide_dbl(as.numeric(.pred_class) - 1, median, .after = 50, .before = 50),
    ) %>% 
    group_by(id, noon_day) %>%
    group_modify(~ .x %>%
      filter(row_number() > min(row_number()[in_bed_filtered == 1]) &
        row_number() < max(row_number()[in_bed_filtered == 1])))

  sleep_data <-
    sleep_fit %>%
    augment(in_bed_data)
}


in_bed_fit %>%
  augment(test) %>%
  mutate(
    in_bed_filtered = slider::slide_dbl(as.numeric(.pred_class) - 1, median, .after = 50, .before = 50),
  ) %>%
  group_by(id, noon_day) %>%
  group_modify(~ .x %>%
    filter(row_number() > min(row_number()[in_bed_filtered == 1]) &
      row_number() < max(row_number()[in_bed_filtered == 1])))
