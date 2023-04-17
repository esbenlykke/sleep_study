library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)


extract_in_bed_data <- function(fit) {
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

### 10 second epochs ###

test <-
  read_parquet("data/data_for_modelling/crude_testing_data.parquet") %>%
  filter(id == 8504)

in_bed_CART_fit <-
  read_rds("/media/esbenlykke/My Passport/crude/fitted_models/axed_models/decision_tree_in_bed_AXED.rds")

in_bed_fits <-
  list.files("/media/esbenlykke/My Passport/crude/fitted_models/axed_models/", full.names = TRUE) %>%
  str_subset("in_bed")

in_bed_fits %>%
  map(read_rds) %>%
  map(extract_in_bed_data) %>%
  write_rds("data/data_for_modelling/in_bed_data_10_sec.rds")


### 30 second epochs ###

# test <-
#   read_parquet("data/data_for_modelling/crude_testing_30_sec_data.parquet")
# 
# in_bed_fits <-
#   list.files("/media/esbenlykke/My Passport/crude/fitted_models/axed_models/", full.names = TRUE) %>%
#   str_subset("in_bed")
# 
# in_bed_fits %>%
#   map(read_rds) %>%
#   map(extract_in_bed_data) %>%
#   write_rds("data/data_for_modelling/in_bed_data_10_sec.rds")
