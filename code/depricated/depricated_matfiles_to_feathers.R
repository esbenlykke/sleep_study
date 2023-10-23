#!/usr/bin/env Rscript

pacman::p_load(
  tidyverse,
  hms,
  lubridate,
  slider,
  feather
)

# TODO create commandArgs() for use as command tool!


# Specify paths -----------------------------------------------------------

# Path to mat files:
mat_files_path <-
  "/media/esben/My Passport/screens_children_mat_files/followup/"


# Path to z machine files
zm_files_path <-
  "data/raw/screens_zmachine"

# choose "baseline" or "followup"
participant_info_path <-
  "followup"

destination_path <-
  "/media/esben/My Passport/acc_temp_combined/followup"


# Build tbl with file directories -----------------------------------------


mat_files <-
  tibble(
    filenames = list.files(
      mat_files_path,
      ".mat",
      full.names = TRUE
    )
  ) %>%
  mutate(
    sensor_id = parse_number(filenames),
    id = str_extract(filenames, "\\d{10}"),
    id = str_remove(id, "^0*") %>%
      as.numeric()
  ) %>%
  select(id, sensor_id, filenames) %>%
  arrange(id)

zm_files <-
  tibble(
    zm_filenames = list.files(
      zm_files_path,
      "_Score.csv",
      full.names = TRUE
    )
  ) %>%
  mutate(
    id = parse_number(zm_filenames),
    .before = zm_filenames
  ) %>%
  arrange(id)


join_participant_info <- function(x, y) {
  x %>%
    filter(age < 18) %>%
    pivot_longer(thigh:back,
      names_to = "sensor_location",
      values_to = "sensor_id"
    ) %>%
    left_join(y, by = c("id", "sensor_id")) %>%
    drop_na()
}

participant_info <-
  readxl::read_excel(
    glue::glue("data/participant_info/screens_{participant_info_path}_info.xlsx")
  ) %>%
  janitor::clean_names()

mat_zm_files <-
  participant_info %>%
  join_participant_info(mat_files) %>%
  arrange(id) %>%
  pivot_wider(id:age, names_from = sensor_location, values_from = filenames) %>%
  mutate(
    destination_filename = str_c(destination_path, "/", id, ".feather")
    # destination_filename = str_replace(thigh, "\\d*_(\\d*).mat", "\\1.gz"),
    # destination_filename = str_replace(destination_filename, "\\/0+", "/")
  ) %>%
  left_join(
    zm_files,
    by = "id"
  ) %>%
  drop_na()



# Build functions ---------------------------------------------------------

make_index <-
  function(tbl) {
    tbl %>%
      mutate(
        index = seq(1, nrow(.), 1),
        .before = 1
      )
  }

merge_resample <-
  function(tbl, epoch_length = 5) { # epoch length is in seconds
    thigh <- rmatio::read.mat(tbl$thigh[i])

    thigh_raw <-
      thigh$data$raw %>%
      as.data.frame() %>%
      as_tibble(.name_repair = ~ c("x_thigh", "y_thigh", "z_thigh")) %>%
      make_index()

    thigh_temp <-
      thigh$data$temp %>%
      as.data.frame() %>%
      as_tibble(.name_repair = ~"temp_thigh") %>%
      make_index()

    back <- rmatio::read.mat(tbl$back[i])

    back_raw <-
      back$data$raw %>%
      as.data.frame() %>%
      as_tibble(.name_repair = ~ c("x_back", "y_back", "z_back")) %>%
      make_index()

    back_temp <-
      back$data$temp %>%
      as.data.frame() %>%
      as_tibble(.name_repair = ~"temp_back") %>%
      make_index()

    combined_raw <-
      inner_join(thigh_raw, back_raw, by = "index")

    combined_temp <-
      inner_join(thigh_temp, back_temp, by = "index")

    combined <-
      list(
        raw = combined_raw,
        temp = combined_temp,
        start_time = as.numeric(thigh$data$starttime)
      )

    print(glue::glue("Raw and temp data extracted and combined"))

    # resample data and join acc with temp ------------------------------------


    acc_temp <-
      combined$raw %>%
      mutate(
        across(x_thigh:z_back, ~ slide_dbl(.x,
          mean,
          .after = epoch_length * 30,
          .step = epoch_length * 30
        ))
      ) %>%
      drop_na() %>%
      select(-index) %>%
      bind_cols(
        combined$temp %>%
          mutate(across(temp_thigh:temp_back, ~ slide_dbl(.x,
            mean,
            .after = epoch_length,
            .step = epoch_length
          ))) %>%
          drop_na() %>%
          select(-index)
      ) %>%
      mutate(
        unix_aligned = seq(combined$start_time,
          nrow(combined$temp) + combined$start_time - 1,
          by = epoch_length
        ),
        # datetime_aligned = as_datetime(unix_aligned, tz = "CET"),
        .before = 1
      )

    print(glue::glue("Raw and temp data resampled to {epoch_length} second epochs"))

    # resample zm data to "epoch_length" --------------------------------------

    zm_data <-
      vroom::vroom(tbl$zm_filenames[i], show_col_types = FALSE) %>%
      janitor::clean_names() %>%
      slice(rep(1:n(), each = 30 / epoch_length)) %>%
      unite("datetime", c(date, time), sep = " ") %>%
      # convert datetime to UNIX and align time to acc/temp data
      mutate(
        unix = as.numeric(as.POSIXct(datetime)),
        # lag_diff = unix - lag(unix),
        unix_rounded = unix %/% 5 * 5,
        # unix_rounded = round(as.numeric(as.POSIXct(datetime)), digits = 0),
        unix_aligned = unix_rounded + rep_len(seq(0, 25, 5), length.out = nrow(.)),
        # datetime_aligned = as_datetime(unix_aligned, tz = "CET"),
        .before = score
      ) %>%
      select(-c(datetime, unix_rounded))

    print(glue::glue("ZM data resampled to {epoch_length} second epochs and aligned to raw/temp"))

    # join and write to .rds --------------------------------------------------

    left_join(zm_data, acc_temp, by = "unix_aligned") %>%
      mutate(
        id = mat_zm_files$id[i],
        .before = 1
      ) |>
      write_feather(tbl$destination_filename[i])

    print(glue::glue("Combined file has been compressed and written to\n{tbl$destination_filename[i]}"))

    # clear memory and garbage collect
    # rm(list = setdiff(ls(), c("mat_zm_files", "merge_resample", "make_index")))
    # gc(full = TRUE)
  }



# loop through all files --------------------------------------------------


for (i in 1:nrow(mat_zm_files)) {
  print(glue::glue("Processing id: {mat_zm_files$id[i]} ({i}/{nrow(mat_zm_files)})"))

  merge_resample(mat_zm_files)

  #   if (i %% 5 == 0) {
  #     rm(list = setdiff(ls(), c("mat_zm_files", "merge_resample", "make_index")))
  #     gc()
  #   }
}

beepr::beep(2)
cat("All done!\n")


# if (shutdown == TRUE) {
#   cat("All done! Powering off computer now.")
#   system("shutdown -h now")
# } else {
#   cat("All done!")
# }
