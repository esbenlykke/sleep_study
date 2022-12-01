#!/usr/bin/env Rscript

# Evaluate tuning results -------------------------------------------------


# autoplot(
#   grid_results,
#   rank_metric = "f_meas",
#   metric = "f_meas",
#   select_best = TRUE
# ) +
#   geom_text(aes(y = mean - .01, label = wflow_id), angle = 90, hjust = 1) +
#   lims(y = c(.86, 1)) +
#   labs(
#     title = "Best performing models from hyperparameter tuning on 5-fold CV",
#     subtitle = "RF, KNN, and tabnet were very slow...",
#     y = "F1 Metric",
#     x = "Model Rank"
#   ) +
#   scale_x_continuous(
#     breaks = seq(1, 10, 1)
#   ) +
#   theme_classic() +
#   theme(
#     legend.position = "none",
#     plot.title.position = "plot"
#   )
#
# ggsave("visuals/many_models_test.png", height = 4, width = 8)


# Finalizing model --------------------------------------------------------

# best_results <-
#   grid_results %>%
#   extract_workflow_set_result("random_forest") %>%
#   select_best(metric = "f_meas")
#
# ctrl_fit <- control_parsnip(verbosity = 2L)
#
# rf_fit <-
#   grid_results %>%
#   extract_workflow("random_forest") %>%
#   finalize_workflow(best_results) %>%
#   fit(train)
#
# rf_fit |>
#   augment(test) |>
#   select(datetime, in_bed, .pred_class) |>
#   pivot_longer(-datetime) |>
#   ggplot(aes(group = 1, color = name)) +
#   geom_step(aes(datetime, value)) +
#   facet_wrap(~ name, ncol = 1) +
#   theme_light()