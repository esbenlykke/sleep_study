library(tidyverse)


# participant info --------------------------------------------------------


info_baseline <-
  readxl::read_excel("data/participant_info/screens_baseline_info.xlsx")

info_followup <-
  readxl::read_excel("data/participant_info/screens_followup_info.xlsx")


# screens filenames -------------------------------------------------------


fnames_baseline <- 
  tibble(filenames = list.files("/media/esbenlykke/My Passport/screens_cwa_files/Baseline/",
  full.names = TRUE
))
fnames_followup <- 
  tibble(filenames = list.files("/media/esbenlykke/My Passport/screens_cwa_files/Followup/",
  full.names = TRUE
))

clean_ids <-
  function(tbl) {
    tbl %>%
      mutate(
        sensor_id = parse_number(filenames),
        id = str_extract(filenames, "\\d{10}"),
        id = str_remove(id, "^0*") %>%
          as.numeric()
      ) %>%
      select(id, sensor_id, filenames) %>% 
      arrange(id)
  }

baseline <-
  fnames_baseline %>%
  clean_ids() 

followup <-
  fnames_followup %>%
  clean_ids()


# Join with participant info ----------------------------------------------

join_fnames_info <- function(x, y) {
  x %>%
    janitor::clean_names() %>%
    filter(age < 18) %>%
    pivot_longer(thigh:back,
      names_to = "sensor_location",
      values_to = "sensor_id"
    ) %>%
    left_join(y, by = c("id", "sensor_id")) %>%
    drop_na()
}

baseline_children <-
  info_baseline %>%
  join_fnames_info(baseline)%>%
  arrange(id)

followup_children <-
  info_followup %>%
  join_fnames_info(followup)%>%
  arrange(id)


# copy children to folder -------------------------------------------------

dir.create("/media/esbenlykke/My Passport/screens_cwa_children/baseline", recursive = TRUE)
dir.create("/media/esbenlykke/My Passport/screens_cwa_children/followup", recursive = TRUE)

baseline_children %>%
  pull(filenames) %>%
  file.copy(from = ., to = "/media/esbenlykke/My Passport/screens_cwa_children/baseline")

followup_children %>%
  pull(filenames) %>%
  file.copy(from = ., to = "/media/esbenlykke/My Passport/screens_cwa_children/followup", )
