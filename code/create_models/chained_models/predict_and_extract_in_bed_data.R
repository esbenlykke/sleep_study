library(tidyverse)
library(tidymodels)
library(arrow)
library(slider)

# Define a function to filter and extract in-bed data based on the fitted model and test data
extract_in_bed_data <- function(fit, test, epoch_length) {
  fit %>%
    augment(test) %>%
    mutate(
      # Compute median-filtered in_bed_filtered values for the specified epoch_length
      in_bed_filtered = slider::slide_dbl(as.numeric(.pred_class) - 1, median,
                                          .after = (7.5 * 60) / epoch_length, .before = (7.5 * 60) / epoch_length
      ),
    ) %>%
    group_by(id, noon_day, month) %>%
    group_modify(~ .x %>%
                   # Filter rows where row_number is within the in_bed_filtered range
                   filter(row_number() > min(row_number()[in_bed_filtered == 1]) &
                            row_number() < max(row_number()[in_bed_filtered == 1]))) %>%
    ungroup()
}

# Read the 10-second epoch dataset
data_10 <-
  read_parquet("data/data_for_modelling/all_data_incl_sensor_independent_features_10_sec_epochs.parquet") 

# Read the 30-second epoch dataset
data_30 <-
  read_parquet("data/data_for_modelling/all_data_incl_sensor_independent_features_30_sec_epochs.parquet") 

# Read the fitted decision tree model for the 10-second epoch data
in_bed_CART_fit_10 <-
  read_rds("/media/esbenlykke/My Passport/crude/fitted_models/axed_models/in_bed_simple_tree_fit_10_AXED.rds")

# Read the fitted decision tree model for the 30-second epoch data
in_bed_CART_fit_30 <-
  read_rds("/media/esbenlykke/My Passport/crude/fitted_models/axed_models/in_bed_simple_tree_fit_30_AXED.rds")

### Process 10-second epoch data ###

# Use the extract_in_bed_data function to filter the 10-second epoch data based on the fitted model
filtered_data_10 <- extract_in_bed_data(in_bed_CART_fit_10, data_10, 10)

# Write the filtered 10-second epoch data to a parquet file
write_parquet(filtered_data_10, "data/data_for_modelling/only_in_bed_data_10.parquet")

### Process 30-second epoch data ###

# Use the extract_in_bed_data function to filter the 30-second epoch data based on the fitted model
filtered_data_30 <- extract_in_bed_data(in_bed_CART_fit_30, data_30, 30)

# Write the filtered 30-second epoch data to a parquet file
write_parquet(filtered_data_30, "data/data_for_modelling/only_in_bed_data_30.parquet")

