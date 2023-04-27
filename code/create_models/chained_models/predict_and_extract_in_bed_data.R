library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)


extract_in_bed_data <- function(fit, test) {
  fit %>%
    augment(test) %>%
    mutate(
      in_bed_filtered = slider::slide_dbl(as.numeric(.pred_class) - 1, median,
        .after = 50, .before = 50
      ),
    ) %>%
    group_by(id, noon_day) %>%
    group_modify(~ .x %>%
      filter(row_number() > min(row_number()[in_bed_filtered == 1]) &
        row_number() < max(row_number()[in_bed_filtered == 1]))) %>%
    ungroup()
}


data_10 <-
  read_parquet("data/data_for_modelling/all_data_incl_sensor_independent_features_10_sec_epochs.parquet") 

data_30 <-
  read_parquet("data/data_for_modelling/all_data_incl_sensor_independent_features_30_sec_epochs.parquet") 

in_bed_CART_fit_10 <-
  read_rds("/media/esbenlykke/My Passport/crude/fitted_models/axed_models/in_bed_simple_tree_fit_10_AXED.rds")

in_bed_CART_fit_30 <-
  read_rds("/media/esbenlykke/My Passport/crude/fitted_models/axed_models/in_bed_simple_tree_fit_30_AXED.rds")

### 10 sec data ###
extract_in_bed_data(in_bed_CART_fit_10, data_10) %>% 
  write_parquet("data/data_for_modelling/only_in_bed_data_10.parquet")

### 30 sec data ###
extract_in_bed_data(in_bed_CART_fit_30, data_30) %>% 
  write_parquet("data/data_for_modelling/only_in_bed_data_30.parquet")
