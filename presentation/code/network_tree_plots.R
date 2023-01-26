library(tidyverse)
library(igraph)
library(showtext)

in_bed_tree_data <-
  tibble(
    from = c(
      "clock_proxy_cos < 31e-18",
      "clock_proxy_cos < 31e-18",
      "sdmax >= 0.0031",
      "sdmax >= 0.0031",
      "clock_proxy_cos < 0.27",
      "clock_proxy_cos < 0.27",
      "temp < 33",
      "temp < 33",
      "incl >= 119",
      "incl >= 119",
      "sdmax >= 0.0046",
      "sdmax >= 0.0046",
      "macc_y >= 0.64",
      "macc_y >= 0.64"
    ),
    to = c(
      "sdmax >= 0.0031",
      "clock_proxy_cos < 0.27",
      "out-bed 53%",
      "temp < 33",
      "sdmax >= 0.0046",
      "in-bed 3%",
      "out-bed 1%",
      "incl >= 119",
      "out-bed 5%",
      "in-bed ~0%",
      "macc_y >= 0.64",
      "in-bed 1%",
      "out-bed 5%",
      "in-bed 32%"
    )
  )

g <-
  graph_from_data_frame(in_bed_tree_data, directed = TRUE)

coords <-
  layout_as_tree(g) |>
  as_tibble(.name_repair = ~ c("x", "y"))

output_df <-
  coords |>
  mutate(
    step = vertex_attr(g, "name"),
    label = step,
    x = x * -1
  )

plot_nodes <-
  output_df %>%
  mutate(
    xmin = x - 0.35,
    xmax = x + 0.35,
    ymin = y - 0.15,
    ymax = y + 0.15
  )

plot_edges <-
  in_bed_tree_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c("from", "to"),
    names_to = "s_e",
    values_to = "step"
  ) %>%
  left_join(plot_nodes, by = "step") %>%
  select(-c(label, y, xmin, xmax)) %>%
  mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
  select(-c(ymin, ymax))

font_add_google(name = "IBM Plex Serif", family = "ibm")
showtext_auto()

# p <-
plot_nodes |>
  ggplot(
    aes(
      xmin = xmin, ymin = ymin,
      xmax = xmax, ymax = ymax
    )
  ) +
  geom_rect(alpha = .5) +
  geom_text(
    data = plot_nodes,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    family = "ibm",
    color = "#EEE8D5"
  )


# flowchart package -------------------------------------------------------

# library(flowchart)
#
# test <-
#   tibble(
#   level = c(1, 2, 3, 4, 4, 5, 5, 6, 6),
#   group = c(NA, NA, NA, 1, 2, 1, 2, 1, 2),
#   text = c(
#     "Assessed for eligibility", "Not eligible\nfor inclusion",
#     "Randomized", "Group1", "Group2", "Excl 1", "Excl 2", "Analysis1",
#     "Analysis2"
#   ),
#   incexc = c(
#     "inc", "exc", "inc", "inc", "inc", "exc", "exc",
#     "inc", "inc"
#   )
# )
#
# flowchart(test)
#
# in_bed_tree_data <-
#   tibble(
#     level = c(1, 1, 2, 2, 3, 3, 4, 4, rep(5, 8)),
#     group = level,
#     text = c(
#       "clock_proxy_cos < 31e-18",
#       "sdmax >= 0.0031",
#       "clock_proxy_cos < 0.27",
#       "clock_proxy_cos < 0.27",
#       "temp < 33",
#       "incl >= 119",
#       "sdmax >= 0.0046",
#       "macc_y >= 0.64",
#       rep(c("yes", "no"), 4)
#     )
#   )
#
# flowchart(in_bed_tree_data)

# visNetwork --------------------------------------------------------------

library(arrow)
library(visNetwork)
library(tidymodels)

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