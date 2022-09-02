library(tidyverse)
library(hms)
library(lubridate)
library(slider)
library(progress)

# resample data and join acc with temp ------------------------------------
# acc and temp data aggregated in (sec)...


resample_and_align <- function(rds_file_name, zm_file_name, epoch_length = 5) {
  rds_file <-
    read_rds(rds_file_name)

  acc_temp <-
    rds_file$raw %>%
    transmute(
      across(x_thigh:z_back, ~ slide_dbl(.x,
        mean,
        .after = epoch_length * 30,
        .step = epoch_length * 30
      ))
    ) %>%
    drop_na() %>%
    bind_cols(
      rds_file$temp %>%
        mutate(across(temp_thigh:temp_back, ~ slide_dbl(.x,
          mean,
          .after = epoch_length,
          .step = epoch_length
        ))) %>%
        drop_na()
    ) %>%
    mutate(
      unix = seq(1, nrow(.) * epoch_length, 5) + as.numeric(rds_file$start_time) - 1,
      datetime = as_datetime(unix, tz = "CET"),
      .before = x_thigh
    )

  print(glue::glue("{rds_file_name} has been resampled"))

  # resample zm data to "epoch_length" --------------------------------------


  zm_file <-
    vroom::vroom(zm_file_name, show_col_types = FALSE)

  zm_data <-
    zm_file %>%
    janitor::clean_names() %>%
    slice(rep(1:n(), each = 30 / epoch_length)) %>%
    mutate(
      time = str_replace(time, ":\\d$", ":01") %>%
        as_hms(),
      date = as_date(date)
    ) %>%
    unite("datetime", c(date, time), sep = " ") %>%
    # convert datetime to UNIX and align time to acc/temp data
    mutate(
      datetime = ymd_hms(datetime) - 1,
      unix = round(as.numeric(as.POSIXct(datetime)), digits = -1),
      unix_aligned = unix + rep_len(seq(0, 25, 5), length.out = nrow(zm_file)),
      datetime_aligned = as_datetime(unix_aligned, tz = "CET"),
      .before = score
    ) %>%
    select(-c(datetime, unix))

  print(glue::glue("{zm_file_name} has been resampled and aligned"))


  # join --------------------------------------------------------------------

  filename <-
    str_replace(zm_file_name, "_Score.csv", "_combined.rds")

  acc_temp %>%
    inner_join(zm_data, by = c("datetime" = "datetime_aligned")) %>%
    write_rds(filename)

  print(glue::glue("Combined file has been combined and written to .rds"))
}



# test
# resample_and_align(rds_files[1], zm_files[1])


# loop through all files --------------------------------------------------



rds_files <-
  list.files("data/test/baseline", "*.rds", full.names = TRUE)

zm_files <-
  list.files("data/test/baseline", "*.csv", full.names = TRUE)

for (f in seq_along(rds_files)) {
  
  print(glue::glue("Processing .rds-files {f}/{nrow(base_children_wide)}"))
  
  resample_and_align(rds_files[f], zm_files[f])
}

# TESTING
# test <- read_rds("data/test/baseline/3401_combined.rds")
# 
# test 