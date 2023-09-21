library(tidyverse)
library(arrow)
library(patchwork)
library(showtext)
library(glue)
library(ggtext)

# Read in the data
ml_stats <-
  list.files("data/data_for_modelling/chained_classifiers/sleep_stats", full.names = TRUE) %>%
  read_csv(id = "model") %>%
  mutate(
    model = str_extract(model, "(?<=sleep_stats\\/).*(?=_stats.csv)")
  ) %>%
  select(-waso_min) %>%
  rename_with(.fn = ~ str_remove(.x, pattern = "_.*"), .cols = spt_hrs:waso_3) %>%
  mutate(
    diff_spt = (spt - zm_spt) * 60,
    diff_tst = (tst - zm_tst) * 60,
    diff_se = se - zm_se,
    diff_lps = lps - zm_lps,
    diff_waso = waso - zm_waso
  ) %>%
  rowwise() %>%
  mutate(
    avg_spt = mean(c(spt, zm_spt)),
    avg_tst = mean(c(tst, zm_tst)),
    avg_se = mean(c(se, zm_se)),
    avg_lps = mean(c(lps, zm_lps)),
    avg_waso = mean(c(waso, zm_waso))
  ) %>%
  ungroup() %>%
  relocate(type = sleep_type)

lstm_stats <-
  read_csv("data/data_for_modelling/lstm/stats/biLSTM_stats.csv") %>%
  drop_na() %>%
  mutate(
    diff_spt = (spt - zm_spt) * 60,
    diff_tst = (tst - zm_tst) * 60,
    diff_se = se - zm_se,
    diff_lps = lps - zm_lps,
    diff_waso = waso - zm_waso
  ) %>%
  rowwise() %>%
  mutate(
    avg_spt = mean(c(spt, zm_spt)),
    avg_tst = mean(c(tst, zm_tst)),
    avg_se = mean(c(se, zm_se)),
    avg_lps = mean(c(lps, zm_lps)),
    avg_waso = mean(c(waso, zm_waso))
  ) %>%
  ungroup() %>%
  relocate(type, model, .after = 1) %>%
  filter(!id %in% c(255704, 649105))

all_cors <-
  read_csv(here::here("data/processed/all_cors.csv"))

ba_data <-
  read_csv(here::here("data/processed/all_boostrap_ba_cis.csv"))


# List of models and variables
models <- c("biLSTM", "xgboost", "decision_tree", "logistic_regression", "neural_network")
variables <- c("spt", "tst", "se", "lps", "waso")
types <- c("raw", "median5", "median10")

# Function to plot ba and cor
plot_ba_cor <- function(model, variable, type) {
  bias_data <- ba_data %>%
    filter(model == {{ model }} & variable == paste0("diff_", {{ variable }}) & type == {{ type }})

  cor_data <- all_cors %>%
    filter(model == {{ model }} & correlation == {{ variable }} & type == {{ type }})

  stats_data <- if (model == "biLSTM") {
    lstm_stats
  } else {
    ml_stats %>%
      filter(
        model == {{ model }} & type == {{ type }} &
          !!sym(paste0("zm_", variable)) >= 0 & !!sym(variable) >= 0
        # & abs(!!sym(paste0("diff_", variable))) / max(abs(!!sym(paste0("diff_", variable))))  < .1
      )
  }

  x_coords <- stats_data %>% pull(!!sym(variable))
  y_coords <- stats_data %>% pull(!!sym(paste0("zm_", variable)))

  ba_plot <-
    ggplot() +
    geom_rect(
      data = bias_data, aes(
        xmin = -Inf, xmax = Inf,
        ymin = bias_ci_lower, ymax = bias_ci_upper
      ),
      fill = "grey", alpha = .5, linewidth = .1
    ) +
    geom_rect(
      data = bias_data, aes(
        xmin = -Inf, xmax = Inf,
        ymin = loa_lower_ci_lower, ymax = loa_lower_ci_upper
      ),
      fill = "grey", alpha = .5, linewidth = .1
    ) +
    geom_rect(
      data = bias_data, aes(
        xmin = -Inf, xmax = Inf,
        ymin = loa_upper_ci_lower, ymax = loa_upper_ci_upper
      ),
      fill = "grey", alpha = .5, linewidth = .1
    ) +
    geom_hline(data = bias_data, aes(yintercept = bias), color = "black", lty = 2) +
    geom_hline(data = bias_data, aes(yintercept = loa_lower), color = "black", lty = 2) +
    geom_hline(data = bias_data, aes(yintercept = loa_upper), color = "black", lty = 2) +
    geom_point(
      data = stats_data, aes(x = !!sym(paste0("avg_", variable)), y = !!sym(paste0("diff_", variable))),
      color = "grey20", fill = "#71C9DD", shape = 21, size = 2.5, stroke = .2, alpha = .7
    ) +
    labs(
      x = paste(
        "Mean of Model and ZM", str_to_upper(variable),
        switch(variable,
          "spt" = "(hrs)",
          "tst" = "(hrs)",
          "se" = "(%)",
          "lps" = "(min)",
          "waso" = "(min)"
        )
      ),
      y = paste(
        "Difference between\nModel and ZM", str_to_upper(variable),
        switch(variable,
          "spt" = "(hrs)",
          "tst" = "(hrs)",
          "se" = "(%)",
          "lps" = "(min)",
          "waso" = "(min)"
        )
      )
    ) +
    theme_classic() +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      strip.placement = "outside"
    )

  cor_plot <-
    stats_data %>%
    ggplot(aes(x = !!sym(variable), y = !!sym(paste0("zm_", variable)))) +
    geom_point(color = "grey20", fill = "#EB6F5D", shape = 21, size = 2.5, stroke = .2, alpha = .7) +
    geom_abline(slope = 1, intercept = 0, color = "black", lty = 2, linewidth = .5) +
    geom_smooth(method = "lm", color = "black", se = FALSE, linewidth = .5) +
    geom_label(
      data = cor_data, aes(x = -Inf, y = Inf, label = glue("r = {round(estimate, 2)}")),
      hjust = 0, vjust = 1, label.padding = unit(.3, "lines"), label.size = NA, alpha = .5, size = 4
    ) +
    labs(
      x = paste(
        "Model", str_to_upper(variable),
        switch(variable,
          "spt" = "(hrs)",
          "tst" = "(hrs)",
          "se" = "(%)",
          "lps" = "(min)",
          "waso" = "(min)"
        )
      ),
      y = paste(
        "ZM", str_to_upper(variable),
        switch(variable,
          "spt" = "(hrs)",
          "tst" = "(hrs)",
          "se" = "(%)",
          "lps" = "(min)",
          "waso" = "(min)"
        )
      )
    ) +
    theme_classic() +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_blank()
      # strip.text = element_blank()
    )

  return(ba_plot + cor_plot & plot_annotation(tag_levels = "A"))

  # ggsave(paste0("manuscript/visuals/ba_cor_", model, "_", variable, ".pdf"), height = 8, width = 8)
}

plot_names <-
  expand_grid(tibble(model = models), tibble(variable = variables), tibble(type = types)) %>%
  mutate(name = paste(model, variable, type, sep = "_")) %>%
  pull(name)

# Apply function to each combination of model and variable
plots <-
  expand_grid(tibble(model = models), tibble(variable = variables), tibble(type = types)) %>%
  pmap(plot_ba_cor) %>%
  set_names(plot_names)

plots$xgboost_spt_raw /
  plots$xgboost_tst_raw /
  plots$xgboost_se_raw /
  plots$xgboost_lps_raw /
  plots$xgboost_waso_raw

ggsave(filename = "~/projects/thesis/figures/raw_xgboost_ba_cor.pdf", height = 25, width = 20, units = "cm", dpi = 600)

plots$xgboost_spt_median5 /
  plots$xgboost_tst_median5 /
  plots$xgboost_se_median5 /
  plots$xgboost_lps_median5 /
  plots$xgboost_waso_median5

ggsave(filename = "manuscript/visuals/median_5_xgboost_ba_cor.pdf", height = 10, width = 8)

plots$xgboost_spt_raw /
  plots$xgboost_tst_raw /
  plots$xgboost_se_raw /
  plots$xgboost_lps_raw /
  plots$xgboost_waso_raw

ggsave(filename = "manuscript/visuals/median_10_xgboost_ba_cor.pdf", height = 10, width = 8)

plots$decision_tree_spt_raw /
  plots$decision_tree_tst_raw /
  plots$decision_tree_se_raw /
  plots$decision_tree_lps_raw /
  plots$decision_tree_waso_raw

ggsave(filename = "manuscript/visuals/raw_decision_tree_ba_cor.pdf", height = 10, width = 8)

plots$decision_tree_spt_median5 /
  plots$decision_tree_tst_median5 /
  plots$decision_tree_se_median5 /
  plots$decision_tree_lps_median5 /
  plots$decision_tree_waso_median5

ggsave(filename = "manuscript/visuals/median_5_decision_tree_ba_cor.pdf", height = 10, width = 8)

plots$decision_tree_spt_raw /
  plots$decision_tree_tst_raw /
  plots$decision_tree_se_raw /
  plots$decision_tree_lps_raw /
  plots$decision_tree_waso_raw

ggsave(filename = "manuscript/visuals/median_10_decision_tree_ba_cor.pdf", height = 10, width = 8)

plots$logistic_regression_spt_raw /
  plots$logistic_regression_tst_raw /
  plots$logistic_regression_se_raw /
  plots$logistic_regression_lps_raw /
  plots$logistic_regression_waso_raw

ggsave(filename = "manuscript/visuals/raw_logistic_regression_ba_cor.pdf", height = 10, width = 8)

plots$logistic_regression_spt_median5 /
  plots$logistic_regression_tst_median5 /
  plots$logistic_regression_se_median5 /
  plots$logistic_regression_lps_median5 /
  plots$logistic_regression_waso_median5

ggsave(filename = "manuscript/visuals/median_5_logistic_regression_ba_cor.pdf", height = 10, width = 8)

plots$logistic_regression_spt_raw /
  plots$logistic_regression_tst_raw /
  plots$logistic_regression_se_raw /
  plots$logistic_regression_lps_raw /
  plots$logistic_regression_waso_raw

ggsave(filename = "manuscript/visuals/median_10_logistic_regression_ba_cor.pdf", height = 10, width = 8)

plots$neural_network_spt_raw /
  plots$neural_network_tst_raw /
  plots$neural_network_se_raw /
  plots$neural_network_lps_raw /
  plots$neural_network_waso_raw

ggsave(filename = "manuscript/visuals/raw_neural_network_ba_cor.pdf", height = 10, width = 8)

plots$neural_network_spt_median5 /
  plots$neural_network_tst_median5 /
  plots$neural_network_se_median5 /
  plots$neural_network_lps_median5 /
  plots$neural_network_waso_median5

ggsave(filename = "manuscript/visuals/median_5_neural_network_ba_cor.pdf", height = 10, width = 8)

plots$neural_network_spt_raw /
  plots$neural_network_tst_raw /
  plots$neural_network_se_raw /
  plots$neural_network_lps_raw /
  plots$neural_network_waso_raw

ggsave(filename = "manuscript/visuals/median_10_neural_network_ba_cor.pdf", height = 10, width = 8)

plots$biLSTM_spt_raw /
  plots$biLSTM_tst_raw /
  plots$biLSTM_se_raw /
  plots$biLSTM_lps_raw /
  plots$biLSTM_waso_raw

ggsave(filename = "manuscript/visuals/raw_biLSTM_ba_cor.pdf", height = 10, width = 8)

plots$biLSTM_spt_median5 /
  plots$biLSTM_tst_median5 /
  plots$biLSTM_se_median5 /
  plots$biLSTM_lps_median5 /
  plots$biLSTM_waso_median5

ggsave(filename = "manuscript/visuals/median_5_biLSTM_ba_cor.pdf", height = 10, width = 8)

plots$biLSTM_spt_raw /
  plots$biLSTM_tst_raw /
  plots$biLSTM_se_raw /
  plots$biLSTM_lps_raw /
  plots$biLSTM_waso_raw

ggsave(filename = "manuscript/visuals/median_10_biLSTM_ba_cor.pdf", height = 10, width = 8)

