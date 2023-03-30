library(lme4)
library(simr)
library(tidyverse)
library(arrow)
library(gt)


crude_stats <-
  read_parquet(here::here("data/processed/crude_stats.parquet"))


get_diff_stats <-
  function(tbl) {
    tbl |>
      mutate(
        diff_spt_hrs = spt_hrs - zm_spt_hrs,
        diff_tst_hrs = tst_hrs - zm_tst_hrs,
        diff_se_percent = se_percent - zm_se_percent,
        diff_lps_min = lps_min - zm_lps_min,
        diff_waso_min = waso_min - zm_waso_min
      ) |>
      ungroup()
  }

diffs <- get_diff_stats(crude_stats)

spt <-
  diffs %>%
  filter(abs(diff_spt_hrs) < 5) %>%
  select(zm_spt_hrs) %>%
  summarise(spt_hrs_sd = sd(zm_spt_hrs), spt_hrs_mean = mean(zm_spt_hrs)) %>%
  mutate(model = "zm", .before = 1) %>%
  bind_rows(
    diffs %>%
      filter(abs(diff_spt_hrs) < 5) %>%
      select(model:noon_day, spt_hrs, zm_spt_hrs) %>%
      group_by(model) %>%
      summarise(across(spt_hrs, list(sd = sd, mean = mean)),
        .groups = "drop"
      )
  ) %>%
  rename(method = model) %>%
  transmute(
    method = method,
    SPT = glue::glue("{round(spt_hrs_mean, 2)} ({round(spt_hrs_sd, 2)})")
  )


tst <-
  diffs %>%
  filter(abs(diff_tst_hrs) < 5) %>%
  select(zm_tst_hrs) %>%
  summarise(tst_hrs_sd = sd(zm_tst_hrs), tst_hrs_mean = mean(zm_tst_hrs)) %>%
  mutate(model = "zm", .before = 1) %>%
  bind_rows(
    diffs %>%
      filter(abs(diff_tst_hrs) < 5) %>%
      select(model:noon_day, tst_hrs, zm_tst_hrs) %>%
      group_by(model) %>%
      summarise(across(tst_hrs, list(sd = sd, mean = mean)),
        .groups = "drop"
      )
  ) %>%
  rename(method = model) %>%
  transmute(
    method = method,
    TST = glue::glue("{round(tst_hrs_mean, 2)} ({round(tst_hrs_sd, 2)})")
  )

se <-
  diffs %>%
  filter(abs(diff_se_percent) < 30) %>%
  select(zm_se_percent) %>%
  summarise(se_percent_sd = sd(zm_se_percent), se_percent_mean = mean(zm_se_percent)) %>%
  mutate(model = "zm", .before = 1) %>%
  bind_rows(
    diffs %>%
      filter(abs(diff_se_percent) < 30) %>%
      select(model:noon_day, se_percent, zm_se_percent) %>%
      group_by(model) %>%
      summarise(across(se_percent, list(sd = sd, mean = mean)),
        .groups = "drop"
      )
  ) %>%
  rename(method = model) %>%
  transmute(
    method = method,
    SE = glue::glue("{round(se_percent_mean, 2)} ({round(se_percent_sd, 2)})")
  )

lps <-
  diffs %>%
  filter(abs(diff_lps_min) < 100) %>%
  select(zm_lps_min) %>%
  summarise(lps_min_sd = sd(zm_lps_min), lps_min_mean = mean(zm_lps_min)) %>%
  mutate(model = "zm", .before = 1) %>%
  bind_rows(
    diffs %>%
      filter(abs(diff_lps_min) < 100) %>%
      select(model:noon_day, lps_min, zm_lps_min) %>%
      group_by(model) %>%
      summarise(across(lps_min, list(sd = sd, mean = mean)),
        .groups = "drop"
      )
  ) %>%
  rename(method = model) %>%
  transmute(
    method = method,
    LPS = glue::glue("{round(lps_min_mean, 2)} ({round(lps_min_sd, 2)})")
  )


waso <-
  diffs %>%
  filter(abs(diff_waso_min) < 100) %>%
  select(zm_waso_min) %>%
  summarise(waso_min_sd = sd(zm_waso_min), waso_min_mean = mean(zm_waso_min)) %>%
  mutate(model = "zm", .before = 1) %>%
  bind_rows(
    diffs %>%
      filter(abs(diff_waso_min) < 100) %>%
      select(model:noon_day, waso_min, zm_waso_min) %>%
      group_by(model) %>%
      summarise(across(waso_min, list(sd = sd, mean = mean)),
        .groups = "drop"
      )
  ) %>%
  rename(method = model) %>%
  transmute(
    method = method,
    WASO = glue::glue("{round(waso_min_mean, 2)} ({round(waso_min_sd, 2)})")
  )



### Covariance between repeated measures ###
model_outcomes <- c(
  "spt_hrs", "tst_hrs", "se_percent", "lps_min", "waso_min"
)

zm_outcomes <-
  c("zm_spt_hrs", "zm_tst_hrs", "zm_se_percent", "zm_lps_min", "zm_waso_min")

# covariance values between the repeated measures for each outcome, grouped by model.
covariance <-
  diffs %>%
  select(model, id, noon_day, all_of(model_outcomes)) %>%
  drop_na() %>%
  pivot_longer(cols = all_of(model_outcomes), names_to = "outcome", values_to = "value") %>%
  group_by(id, outcome, model) %>%
  arrange(model, id, outcome, noon_day) %>%
  mutate(value_diff = value - lag(value)) %>%
  filter(!value_diff == "NaN" & !value == "Inf" & !value == "-Inf") %>%
  group_by(model, outcome) %>%
  filter((outcome == "lps_min" & value_diff < 100) |
    (outcome == "waso_min" & value_diff < 100) |
    (outcome == "spt_hrs" & value_diff < 5) |
    (outcome == "tst_hrs" & value_diff < 5) |
    (outcome == "se_percent" & value_diff < 30)) %>%
  summarize(
    mean_cov = cov(noon_day, value_diff, use = "pairwise.complete.obs"),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = outcome, values_from = mean_cov) %>%
  rename(method = model) %>%
  bind_rows(
    diffs %>%
      filter(model == "decision_tree") %>%
      select(id, noon_day, contains("zm")) %>%
      mutate(
        method = "zm",
        .before = 1
      ) %>%
      drop_na() %>%
      pivot_longer(cols = all_of(zm_outcomes), names_to = "outcome", values_to = "value") %>%
      group_by(id, outcome, method) %>%
      arrange(method, id, outcome, noon_day) %>%
      mutate(value_diff = value - lag(value)) %>%
      filter(!value_diff == "NaN" & !value == "Inf" & !value == "-Inf") %>%
      group_by(method, outcome) %>%
      summarize(
        mean_cov = cov(noon_day, value_diff, use = "pairwise.complete.obs"),
        .groups = "drop"
      ) %>%
      pivot_wider(names_from = outcome, values_from = mean_cov) %>%
      rename_with(.fn = ~ str_remove(.x, "zm_"))
  ) %>% 
  rename_with(-method, .fn = ~ str_c(.x, "_cov"))

reduce(list(spt, tst, se, lps, waso, covariance), left_join, by = "method") %>%
  gt() %>%
  tab_header(title = md("Descriptives of the sleep quality stats from all methods")) |>
  tab_style(
    style = cell_text(size = px(14)),
    locations = cells_body(
      columns = c(SPT:WASO)
    )
  ) |>
  tab_spanner(label = "SPTmmm", columns = 2) %>%
  tab_options(
    table.font.names = "ibm",
    table.font.color.light = "#EEE8D5",
    table.background.color = "#002B36"
  )

# An intraclass correlation coefficient (ICC) of 0.346 indicates that 34.6% of
# the total variation in the outcome variable can be attributed to the between-group
# (or between-subject) differences, while the remaining 65.4% of the variation is due
# to within-group (or within-subject) differences.


### MDES ###

# You have calculated a minimum detectable effect size (MDES) of 47 minutes with
# a 95% confidence interval ranging from 39.1  to 56.4 minutes. This result is based
# on a study with 75 subjects, each having 4 repeated measures, and with a desired
# statistical power of 80% and a significance level (alpha) of 0.05.
#
# The MDES represents the smallest effect size that your study is capable of
# detecting with the specified power and significance level. In this case, an
# effect size of 45 minutes means that if the true difference in sleep time
# (or the effect of the intervention or predictor) is equal to or greater than 45
# minutes, you have an 80% chance of detecting this effect as statistically
# significant at a 0.05 significance level.
#
# The confidence interval (39.1 - 56.4) provides a range of plausible values for
# the MDES. It indicates that you can be 95% confident that the true MDES lies
# within this interval. In other words, the smallest effect size that your study
# can detect with 80% power and a 0.05 significance level is likely to be between
# 29.1 and 47.2 minutes.
