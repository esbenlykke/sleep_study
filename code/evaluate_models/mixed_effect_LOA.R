library(tidyverse)
library(arrow)
library(patchwork)
library(showtext)


# TODO consider calculating 95% CI for bias


stats_files <-
  list.files("~/sleep_study/data/processed/stats_predictions", full.names = TRUE)

all_stats <-
  map(stats_files, read_parquet) |>
  set_names(c("logistic_regression", "neural_net", "decision_tree", "xgboost"))
# |>
#   map(
#     ~ filter(.x, (id != 1742705 | noon_day != 24) &
#         (id != 1044005 | noon_day != 16) &
#         (id != 1750904 | noon_day != 21) &
#         (id != 1718904 | noon_day != 8)
#     )
#   ) # TODO look into these lps_min outliers

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
      rowwise() |>
      mutate(
        avg_spt_hrs = mean(c(spt_hrs, zm_spt_hrs)),
        avg_tst_hrs = mean(c(tst_hrs, zm_tst_hrs)),
        avg_se_percent = mean(c(se_percent, zm_se_percent)),
        avg_lps_min = mean(c(lps_min, zm_lps_min)),
        avg_waso_min = mean(c(waso_min, zm_waso_min))
      ) |>
      ungroup()
  }


all_diffs <-
  all_stats |>
  map(get_diff_stats)

test <- all_diffs$logistic_regression

plot_qqs <- funtion(df){
df |> 
  select(id, noon_day, contains("diff")) |> 
  pivot_longer(-c(id:noon_day)) |> 
  ggplot(aes(sample = value)) +
  geom_qq(shape = 21, fill = "darkorange", alpha = .5) + 
  geom_qq_line() +
  facet_wrap(~ name, scales = "free") +
  theme_light()
}

# Differences are not normal. Use nonparametric approach for LOA. 

# test |> 
#   loa_lme(data = _,
#           diff = "diff_spt_hrs",
#           avg = "avg_spt_hrs",
#           id = "id",
#           replicates = 1000)


agree_np(x = "spt_hrs", 
         y = "zm_spt_hrs", 
         id = "id", 
         data = test, 
         delta = 1, 
         prop_bias = TRUE)
