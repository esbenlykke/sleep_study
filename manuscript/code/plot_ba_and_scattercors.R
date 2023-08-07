library(tidyverse)
library(arrow)
library(patchwork)
library(showtext)
library(glue)
library(ggtext)

# model <- c("decision_tree", "logistic_regression", "neural_network", "xgboost", "biLSTM")
# sleep_parameter <- c("spt", "tst", "se", "lps", "waso")

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
    diff_waso = waso - zm_waso) %>% 
  rowwise() %>% 
  mutate(
    avg_spt = mean(c(spt, zm_spt)),
    avg_tst = mean(c(tst, zm_tst)),
    avg_se = mean(c(se, zm_se)),
    avg_lps = mean(c(lps, zm_lps)),
    avg_lps = mean(c(waso, zm_waso))
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
    avg_lps = mean(c(waso, zm_waso))
  ) %>%
  ungroup() %>%
  relocate(type, model, .after = 1) %>%
  filter(!id %in% c(255704, 649105))

all_cors <-
  read_csv(here::here("data/processed/all_cors.csv"))

ba_data <-
  read_csv(here::here("data/processed/all_boostrap_ba_cis.csv"))


### biLSTM SPT ###
lstm_bias_spt <-
  ba_data %>%
  filter(model == "biLSTM" & variable == "diff_spt")

lstm_cor_spt <-
  all_cors %>%
  filter(model == "biLSTM" & correlation == "spt")


ba_lstm_spt <-
  ggplot() +
  geom_rect(
    data = lstm_bias_spt, aes(
      xmin = -Inf, xmax = Inf,
      ymin = bias_ci_lower, ymax = bias_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_rect(
    data = lstm_bias_spt, aes(
      xmin = -Inf, xmax = Inf,
      ymin = loa_lower_ci_lower, ymax = loa_lower_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_rect(
    data = lstm_bias_spt, aes(
      xmin = -Inf, xmax = Inf,
      ymin = loa_upper_ci_lower, ymax = loa_upper_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_hline(data = lstm_bias_spt, aes(yintercept = bias), color = "grey25", lty = 2) +
  geom_hline(data = lstm_bias_spt, aes(yintercept = loa_lower), color = "grey25", lty = 2) +
  geom_hline(data = lstm_bias_spt, aes(yintercept = loa_upper), color = "grey25", lty = 2) +
  geom_point(
    data = lstm_stats, aes(avg_spt, diff_spt),
    color = "grey80", fill = "grey50", shape = 21, size = 2.5, stroke = .2
  ) +
  facet_wrap(~type,
    ncol = 1, strip.position = "top",
    labeller = labeller(type = c(
      "raw" = "Raw Predictions",
      "median5" = "5-Min Median",
      "median10" = "10-Min Median"
    ))
  ) +
  labs(
    x = "Mean of Model and ZM (min)",
    y = "Difference between Model and ZM (hrs) "
  ) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    strip.placement = "outside"
  )

cor_lstm_spt <-
  lstm_stats %>%
  ggplot(aes(spt, zm_spt)) +
  geom_point(color = "grey80", fill = "grey50", shape = 21, size = 2.5, stroke = .2) +
  geom_abline(slope = 1, intercept = 0, color = "grey25", lty = 2, linewidth = .5) +
  geom_smooth(method = "lm", color = "grey25", se = FALSE, linewidth = .5) +
  geom_text(data = lstm_cor_spt, aes(x = 6, y = 11.5, label = glue("r = {round(estimate, 2)}"))) +
  facet_wrap(~type,
    ncol = 1, strip.position = "right",
    labeller = labeller(type = c(
      "raw" = "Raw Predictions",
      "median5" = "5-Min Median",
      "median10" = "10-Min Median"
    ))
  ) +
  labs(
    x = "Model SPT (hrs)",
    y = "ZM SPT (hrs)"
  ) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
    # strip.text = element_blank()
  )

ba_lstm_spt + cor_lstm_spt & plot_annotation(tag_levels = "A")

# ggsave("manuscript/visuals/ba_cor_lstm_spt.pdf", height = 8, width = 8)

### biLSTM tst ###
lstm_bias_tst <-
  ba_data %>%
  filter(model == "biLSTM" & variable == "diff_tst")

lstm_cor_tst <-
  all_cors %>%
  filter(model == "biLSTM" & correlation == "tst")


ba_lstm_tst <-
  ggplot() +
  geom_rect(
    data = lstm_bias_tst, aes(
      xmin = -Inf, xmax = Inf,
      ymin = bias_ci_lower, ymax = bias_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_rect(
    data = lstm_bias_tst, aes(
      xmin = -Inf, xmax = Inf,
      ymin = loa_lower_ci_lower, ymax = loa_lower_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_rect(
    data = lstm_bias_tst, aes(
      xmin = -Inf, xmax = Inf,
      ymin = loa_upper_ci_lower, ymax = loa_upper_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_hline(data = lstm_bias_tst, aes(yintercept = bias), color = "grey25", lty = 2) +
  geom_hline(data = lstm_bias_tst, aes(yintercept = loa_lower), color = "grey25", lty = 2) +
  geom_hline(data = lstm_bias_tst, aes(yintercept = loa_upper), color = "grey25", lty = 2) +
  geom_point(
    data = lstm_stats, aes(avg_spt, diff_spt),
    color = "grey80", fill = "grey50", shape = 21, size = 2.5, stroke = .2
  ) +
  facet_wrap(~type,
             ncol = 1, strip.position = "top",
             labeller = labeller(type = c(
               "raw" = "Raw Predictions",
               "median5" = "5-Min Median",
               "median10" = "10-Min Median"
             ))
  ) +
  labs(
    x = "Mean of Model and ZM (min)",
    y = "Difference between Model and ZM (hrs) "
  ) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    strip.placement = "outside"
  )

cor_lstm_tst <-
  lstm_stats %>%
  ggplot(aes(spt, zm_tst)) +
  geom_point(color = "grey80", fill = "grey50", shape = 21, size = 2.5, stroke = .2) +
  geom_abline(slope = 1, intercept = 0, color = "grey25", lty = 2, linewidth = .5) +
  geom_smooth(method = "lm", color = "grey25", se = FALSE, linewidth = .5) +
  geom_text(data = lstm_cor_tst, aes(x = 6, y = 11.5, label = glue("r = {round(estimate, 2)}"))) +
  facet_wrap(~type,
             ncol = 1, strip.position = "right",
             labeller = labeller(type = c(
               "raw" = "Raw Predictions",
               "median5" = "5-Min Median",
               "median10" = "10-Min Median"
             ))
  ) +
  labs(
    x = "Model SPT (hrs)",
    y = "ZM SPT (hrs)"
  ) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
    # strip.text = element_blank()
  )

ba_lstm_tst + cor_lstm_tst & plot_annotation(tag_levels = "A")

# ggsave("manuscript/visuals/ba_cor_lstm_tst.pdf", height = 8, width = 8)

### xgboost ###

### biLSTM tst ###
xgb_bias_spt <-
  ba_data %>%
  filter(model == "xgboost" & variable == "diff_spt")

xgb_cor_spt <-
  all_cors %>%
  filter(model == "xgboost" & correlation == "spt")


ba_xgb_spt <-
  ggplot() +
  geom_rect(
    data = xgb_bias_spt, aes(
      xmin = -Inf, xmax = Inf,
      ymin = bias_ci_lower, ymax = bias_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_rect(
    data = xgb_bias_spt, aes(
      xmin = -Inf, xmax = Inf,
      ymin = loa_lower_ci_lower, ymax = loa_lower_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_rect(
    data = xgb_bias_spt, aes(
      xmin = -Inf, xmax = Inf,
      ymin = loa_upper_ci_lower, ymax = loa_upper_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_hline(data = xgb_bias_spt, aes(yintercept = bias), color = "grey25", lty = 2) +
  geom_hline(data = xgb_bias_spt, aes(yintercept = loa_lower), color = "grey25", lty = 2) +
  geom_hline(data = xgb_bias_spt, aes(yintercept = loa_upper), color = "grey25", lty = 2) +
  geom_point(
    data = ml_stats %>% filter(model == "xgboost"), aes(avg_spt, diff_spt),
    color = "grey80", fill = "grey50", shape = 21, size = 2.5, stroke = .2
  ) +
  facet_wrap(~type,
             ncol = 1, strip.position = "top",
             labeller = labeller(type = c(
               "raw" = "Raw Predictions",
               "median5" = "5-Min Median",
               "median10" = "10-Min Median"
             ))
  ) +
  labs(
    x = "Mean of Model and ZM (min)",
    y = "Difference between Model and ZM (hrs) "
  ) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    strip.placement = "outside"
  )

cor_xgb_spt <-
  ml_stats %>%
  filter(model == "xgboost") %>% 
  ggplot(aes(spt, zm_spt)) +
  geom_point(color = "grey80", fill = "grey50", shape = 21, size = 2.5, stroke = .2) +
  geom_abline(slope = 1, intercept = 0, color = "grey25", lty = 2, linewidth = .5) +
  geom_smooth(method = "lm", color = "grey25", se = FALSE, linewidth = .5) +
  geom_text(data = xgb_cor_spt, aes(x = 6, y = 11.5, label = glue("r = {round(estimate, 2)}"))) +
  facet_wrap(~type,
             ncol = 1, strip.position = "right",
             labeller = labeller(type = c(
               "raw" = "Raw Predictions",
               "median5" = "5-Min Median",
               "median10" = "10-Min Median"
             ))
  ) +
  labs(
    x = "Model SPT (hrs)",
    y = "ZM SPT (hrs)"
  ) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
    # strip.text = element_blank()
  )

ba_xgb_spt + cor_xgb_spt & plot_annotation(tag_levels = "A")

# ggsave("manuscript/visuals/ba_cor_xgb_spt.pdf", height = 8, width = 8)

### biLSTM TST ###
xgb_bias_tst <-
  ba_data %>%
  filter(model == "xgboost" & variable == "diff_tst")

xgb_cor_tst <-
  all_cors %>%
  filter(model == "xgboost" & correlation == "tst")


ba_xgb_tst <-
  ggplot() +
  geom_rect(
    data = xgb_bias_tst, aes(
      xmin = -Inf, xmax = Inf,
      ymin = bias_ci_lower, ymax = bias_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_rect(
    data = xgb_bias_tst, aes(
      xmin = -Inf, xmax = Inf,
      ymin = loa_lower_ci_lower, ymax = loa_lower_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_rect(
    data = xgb_bias_tst, aes(
      xmin = -Inf, xmax = Inf,
      ymin = loa_upper_ci_lower, ymax = loa_upper_ci_upper
    ),
    fill = "grey", alpha = .5, linewidth = .1
  ) +
  geom_hline(data = xgb_bias_tst, aes(yintercept = bias), color = "grey25", lty = 2) +
  geom_hline(data = xgb_bias_tst, aes(yintercept = loa_lower), color = "grey25", lty = 2) +
  geom_hline(data = xgb_bias_tst, aes(yintercept = loa_upper), color = "grey25", lty = 2) +
  geom_point(
    data = ml_stats %>% filter(model == "xgboost"), aes(avg_tst, diff_tst),
    color = "grey80", fill = "grey50", shape = 21, size = 2.5, stroke = .2
  ) +
  facet_wrap(~type,
             ncol = 1, strip.position = "top",
             labeller = labeller(type = c(
               "raw" = "Raw Predictions",
               "median5" = "5-Min Median",
               "median10" = "10-Min Median"
             ))
  ) +
  labs(
    x = "Mean of Model and ZM (min)",
    y = "Difference between Model and ZM (hrs) "
  ) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    strip.placement = "outside"
  )

cor_xgb_tst <-
  ml_stats %>%
  filter(model == "xgboost") %>% 
  ggplot(aes(tst, zm_tst)) +
  geom_point(color = "grey80", fill = "grey50", shape = 21, size = 2.5, stroke = .2) +
  geom_abline(slope = 1, intercept = 0, color = "grey25", lty = 2, linewidth = .5) +
  geom_smooth(method = "lm", color = "grey25", se = FALSE, linewidth = .5) +
  geom_text(data = xgb_cor_tst, aes(x = 6, y = 11.5, label = glue("r = {round(estimate, 2)}"))) +
  facet_wrap(~type,
             ncol = 1, strip.position = "right",
             labeller = labeller(type = c(
               "raw" = "Raw Predictions",
               "median5" = "5-Min Median",
               "median10" = "10-Min Median"
             ))
  ) +
  labs(
    x = "Model tst (hrs)",
    y = "ZM tst (hrs)"
  ) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
    # strip.text = element_blank()
  )

ba_xgb_tst + cor_xgb_tst & plot_annotation(tag_levels = "A")

# ggsave("manuscript/visuals/ba_cor_xgb_tst.pdf", height = 8, width = 8)