library(tidyverse)
library(arrow)
library(lme4)


crude_stats <-
  crude_stats <-
  read_rds("data/processed/crude_stats_and_diffs.rds")

dc_spt <-
  crude_stats$decision_tree %>%
  filter(abs(diff_spt_hrs) < 5) %>%
  select(id, noon_day, spt_hrs, zm_spt_hrs) %>%
  mutate(spt_min = spt_hrs * 60) %>%
  group_by(id) %>%
  mutate(noon_day = 1:n())


# crude_stats %>% 
#   map(get_diff_stats) %>% 
#   write_rds("data/processed/crude_stats_and_diffs.rds")

# Fit a linear mixed model and calculate the MDC using the estimated within-subject SD:

get_mdc_icc <-
  function(tbl) {
    lmm <- lmer(spt_hrs ~ noon_day + (1 | id), data = tbl)
    var_components <- VarCorr(lmm)
    sd_between <- var_components$id[1] # Random intercept variance for subjects
    residual_variance <- sigma(lmm)^2 # Residual variance (square of residual standard deviation)
    icc <- sd_between / (sd_between + residual_variance)
    confidence_level <- 0.95
    z_score <- qnorm(confidence_level + (1 - confidence_level) / 2)
    sd_within <- sigma(lmm)
    
    mdc <- z_score * sqrt(2) * sd_within * sqrt(1 - icc)
    return(mdc)
  }

get_mdc_sem <-
  function(tbl) {
    lmm <- lmer(spt_hrs ~ noon_day + (1 | id), data = tbl)
    sd_within <- sigma(lmm)
    n <- length(unique(tbl$id))
    # # n <- 75
    sem <- sd_within / sqrt(n)
    mdc <- sem * 1.96 * sqrt(2)
    return(mdc)
  }

# Calculate MDC and CI95% by bootstrapping.

# Create bootstraps
set.seed(123)
boots <-
  rsample::bootstraps(dc_spt, times = 100)

# Calculate MDC for each bootstrapped data set
res_icc <-
  map_dbl(boots$splits, get_mdc_icc)

res_sem <-
  map_dbl(boots$splits, get_mdc_sem)

mdc_mean_icc <- mean(res_icc)
mdc_mean_sem <- mean(res_sem)


# Calculate bootstrap confidence intervals
mdc_ci_icc <- quantile(res_icc, c(0.025, 0.975))
mdc_ci_sem <- quantile(res_sem, c(0.025, 0.975))


glue::glue("MDC-ICC (CI95%)
           {round(mdc_mean_icc, 1)} hours ({round(mdc_ci_icc[[1]], 1)} - {round(mdc_ci_icc[[2]], 1)})")
glue::glue("MDC-SEM (CI95%)
           {round(mdc_mean_sem, 1)} minutes ({round(mdc_ci_sem[[1]], 1)} - {round(mdc_ci_sem[[2]], 1)})")



lmm <- lmer(spt_min ~ noon_day + (1 | id), data = dc_spt)
icc <- performance::icc(lmm)[[1]]
# MDES = (Z_β + Z_α/2) * σ / √(n * (k * (1 - ICC) + ICC))
(.84 + 1.96) * sd(dc_spt$spt_min) /sqrt(75) * (4.5 * (1 - icc) + icc)
