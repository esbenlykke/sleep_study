pacman::p_load(
  tidyverse,
  hms,
  lubridate,
  slider,
  feather,
  vroom
)

# TODO run this through once more. Na's produced during left_join()?


# Specify paths -----------------------------------------------------------

# Path to mat files:
mat_files_path <-
  # "/media/esben/My Passport/screens_children_mat_files/followup"
  "/media/esben/My Passport/screens_children_mat_files/baseline"


# Path to z machine files
zm_files_path <-
  "data/raw/screens_zmachine"

# choose "baseline" or "followup"
participant_info_path <-
  "baseline"

destination_path <-
  "data/test/combined"


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

epoch_length <- 5

# merge_resample <-
#   function(tbl, epoch_length = 5) { # epoch length is in seconds
thigh <- rmatio::read.mat(mat_zm_files$thigh[2])

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

back <- rmatio::read.mat(mat_zm_files$back[2])

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
  vroom::vroom(mat_zm_files$zm_filenames[2], show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  slice(rep(1:n(), each = 30 / epoch_length)) %>%
  # mutate(
  #   time = str_replace(time, ":\\d$", ":01") %>%
  #     as_hms(),
  #   date = as_date(date)
  # ) %>%
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
  ) |> 
  select(-c(datetime, unix, unix_rounded)) 

print(glue::glue("ZM data resampled to {epoch_length} second epochs and aligned to raw/temp"))

# join and write to .rds --------------------------------------------------

inner_join(zm_data, acc_temp, by = "unix_aligned") |>
  mutate(
    id = mat_zm_files$id[2],
    .before = 1
  ) |> 
  # filter(unix_aligned - lag(unix_aligned) != 5)
  write_feather(mat_zm_files$destination_filename[2])

print(glue::glue("Combined file has been compressed and written to '{mat_zm_files$destination_filename[2]}'"))
# }

test_files <- list.files("data/test/combined", ".feather", full.names = TRUE)

read_feather("data/test/combined/3404.feather")

test_files |> 
  map_dfr(read_feather)

test <- 
  read_feather("data/processed/bsl.feather")
