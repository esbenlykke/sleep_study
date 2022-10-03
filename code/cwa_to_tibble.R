library(tidyverse)
library(read.cwa)
library(slider)

acc_files <-
  list.files("data/raw/my_study_acc_data/cwa/")

test <- read_cwa("data/raw/my_study_acc_data/cwa/1_lower_back.cwa",
  xyz_only = FALSE,
  verbose = TRUE
) |>
  pluck("data")

# construct function to read in cwa and clean to be used with map_dfr(..., .id = )
# test_small <-
  test |>
  slice(1:1000) |> 
  mutate(
    across(X:temperature, ~ slide_dbl(.x,
                                       mean,
                                       .after = 50,
                                       .step = 50
    ))
  ) |>
    select(1:6) |> 
    drop_na()

  
  
  
# an attempt to avoid using drop_na() by using hop_index_vec()
  # does not seem worth it!
i <- 1:nrow(test_small)
idx <- slide_dfr(
  i,
  ~ tibble(start = .x[1], stop = .x[length(.x)]),
  .before = 50,
  .step = 50,
  .complete = FALSE
)

idx

test_small |>
  mutate(
    X = hop_index_vec(i, idx$start, idx$stop, .f = ~ mean(test_small$X), .ptype = double())
  )
