library(tidyverse)
library(visNetwork)
library(tidymodels)

# visNetwork --------------------------------------------------------------


in_bed_tree <-
  read_rds("~/sleep_study/data/models/fitted_models/in_bed_simple_CART_fit.rds")

in_bed_tree |>
  extract_fit_engine() |>
  visTree(
    height = "800px",
    nodesPopSize = TRUE,
    minNodeSize = 10,
    maxNodeSize = 30,
    fallenLeaves = TRUE,
    legend = FALSE,
    digits = 3,
    direction = "LR",
    colorVar = c(
      "#CC5500", "#8B0000", "#FFD700",
      "#228B22", "#654321", "#8B0000", "#FFDB58"
    )
  ) |>
  visNodes(font = list(
    color = "#EEE8D5",
    strokeWidth = 1
  )) |>
  visEdges(font = list(
    color = "#EEE8D5",
    strokeWidth = 1
  )) |> 
  visInteraction(dragNodes = FALSE, 
                 dragView = FALSE, 
                 zoomView = FALSE)