pacman::p_load(
  tidyverse,
  hms,
  lubridate,
  slider
)

# TODO This script has memory leaks from within the merge_resample function. 
# I have not figured out how to fix it yet.


# Specify paths -----------------------------------------------------------


mat_files_path <-
# on windows use the following path
  # "E:/screens_children_mat_files/followup"
# on linux use the following path
"/media/esben/My Passport/screens_children_mat_files/followup"

zm_files_path <-
  "data/raw/screens_zmachine/"

# choose "baseline" or "followup"
participant_info_path <-
  "followup"

# destination_path <-
#   ""


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
    destination_filename = str_replace(thigh, "\\d*_(\\d*).mat", "\\1.gz"),
    destination_filename = str_replace(destination_filename, "\\/0+", "/")
  ) %>%
  left_join(
    zm_files,
    by = "id"
  ) %>%
  drop_na()



# Build functions ---------------------------------------------------------

make_sample <-
  function(tbl) {
    tbl %>%
      mutate(
        sample = seq(1, nrow(.), 1),
        .before = 1
      )
  }

merge_resample <-
  function(tbl, epoch_length = 5) {
    thigh <- rmatio::read.mat(tbl$thigh[i])

    thigh_raw <-
      thigh$data$raw %>%
      as.data.frame() %>%
      as_tibble(.name_repair = ~ c("x_thigh", "y_thigh", "z_thigh")) %>%
      make_sample()

    thigh_temp <-
      thigh$data$temp %>%
      as.data.frame() %>%
      as_tibble(.name_repair = ~"temp_thigh") %>%
      make_sample()

    back <- rmatio::read.mat(tbl$back[i])

    back_raw <-
      back$data$raw %>%
      as.data.frame() %>%
      as_tibble(.name_repair = ~ c("x_back", "y_back", "z_back")) %>%
      make_sample()

    back_temp <-
      back$data$temp %>%
      as.data.frame() %>%
      as_tibble(.name_repair = ~"temp_back") %>%
      make_sample()

    combined_raw <-
      inner_join(thigh_raw, back_raw, by = "sample")

    combined_temp <-
      inner_join(thigh_temp, back_temp, by = "sample")

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
      select(-sample) %>%
      bind_cols( # TODO fix join!!! Is bind_cols robust?
        combined$temp %>%
          mutate(across(temp_thigh:temp_back, ~ slide_dbl(.x,
            mean,
            .after = epoch_length,
            .step = epoch_length
          ))) %>%
          drop_na() %>%
          select(-sample)
      ) %>%
      mutate(
        unix_aligned = seq(combined$start_time,
          nrow(combined$temp) + combined$start_time - 1,
          by = epoch_length
        ),
        datetime_aligned = as_datetime(unix_aligned, tz = "CET"),
        .before = 1
      )

    print(glue::glue("Raw and temp data resampled to {epoch_length} second epochs"))

    # resample zm data to "epoch_length" --------------------------------------

    zm_data <-
      vroom::vroom(tbl$zm_filenames[i], show_col_types = FALSE) %>%
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
        unix_rounded = round(as.numeric(as.POSIXct(datetime)), digits = -1),
        unix_aligned = unix_rounded + rep_len(seq(0, 25, 5), length.out = nrow(.)),
        datetime_aligned = as_datetime(unix_aligned, tz = "CET"),
        .before = score
      ) %>%
      select(-c(datetime, unix_rounded))

    print(glue::glue("ZM data resampled to {epoch_length} second epochs and aligned to raw/temp"))

    # join and write to .rds --------------------------------------------------

    inner_join(acc_temp, zm_data, by = c("datetime_aligned", "unix_aligned")) %>%
      vroom::vroom_write(tbl$destination_filename[i])

    print(glue::glue("Combined file has been compressed and written to\n{tbl$destination_filename[i]}"))

    rm(list = setdiff(ls(), c("mat_zm_files", "merge_resample", "make_sample")))
    gc(full = TRUE)
  }



# loop through all files --------------------------------------------------


for (i in 1:nrow(mat_zm_files)) {
  
  print(glue::glue("Processing id: {mat_zm_files$id[i]} ({i}/{nrow(mat_zm_files)})"))

  merge_resample(mat_zm_files)

  # if (i %% 5 == 0){
  #   rm(list = setdiff(ls(), c("mat_zm_files", "merge_resample", "make_sample")))
  #   gc()
  # }
  # This clean-up step does not seem to work...
}

beepr::beep(2)


# Things to try:
  # experiment with purrr::possibly and purrr::safely to map over `merge_resample`
  # do not write to disk inside "merge_resample". 
  # Try purrr::walk to write to disk in separate function?
