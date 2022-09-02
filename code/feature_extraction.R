pacman::p_load(
  tidyverse,
  vroom,
  feather,
  slider
)

bsl <-
  read_feather("data/processed/bsl.feather")

fup <-
  read_feather("data/processed/fup.feather")

# Here we are calculating features in 30-sec overlapping rolling windows.

bsl_features <-
  bsl %>%
  group_by(id) %>%
  slice(1:100) %>%
  mutate(
    across(x_thigh:z_back,
      list(
        trim_mean = ~ slide_dbl(.x, ~ mean(.x, trim = .1), .after = 6),
        MAD = ~ slide_dbl(.x, ~ mad(.x), .after = 6),
        max = ~ slide_dbl(.x, ~ abs(max(.x)), .after = 6),
        IQR = ~ slide_dbl(.x, ~ IQR(.x), .after = 6, .step = 1)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  mutate(
    across(
      temp_thigh:temp_back,
      list(
        trim_mean = ~ slide_dbl(.x, ~ mean(.x, trim = .1), .after = 6),
        min = ~ slide_dbl(.x, ~ min(.x), .after = 6),
        max = ~ slide_dbl(.x, ~ abs(max(.x)), .after = 6),
        sd = ~ slide_dbl(.x, ~ sd(.x), .after = 6, .step = 1)
      )
    )
  )

# TODO next step is to create the sensor-independent features

# for details on angle calculations: https://www.nature.com/articles/s41598-018-31266-z
# angle_z = (1 / tan(a_z / (a_x^2 + a_y^2) * 180 / pi)) - from Van Hees

# this shit is gonna take forever on the full dataset
# consider furrr::future_map_* functions...


# furrr -------------------------------------------------------------------

extract_features <- 
  function(tbl) {
    tbl %>%
      group_by(id) %>%
      slice(1:100) %>%
      mutate(
        across(x_thigh:z_back,
               list(
                 trim_mean = ~ slide_dbl(.x, ~ mean(.x, trim = .1), .after = 6),
                 MAD = ~ slide_dbl(.x, ~ mad(.x), .after = 6),
                 max = ~ slide_dbl(.x, ~ abs(max(.x)), .after = 6),
                 IQR = ~ slide_dbl(.x, ~ IQR(.x), .after = 6, .step = 1)
               ),
               .names = "{.col}_{.fn}"
        )
      ) %>%
      mutate(
        across(
          temp_thigh:temp_back,
          list(
            trim_mean = ~ slide_dbl(.x, ~ mean(.x, trim = .1), .after = 6),
            min = ~ slide_dbl(.x, ~ min(.x), .after = 6),
            max = ~ slide_dbl(.x, ~ abs(max(.x)), .after = 6),
            sd = ~ slide_dbl(.x, ~ sd(.x), .after = 6, .step = 1)
          )
        )
      )
  }

