library(lme4)
library(tidyverse)
library(rlang)

# Get SD's from outcome measures

crude_stats <-
  read_rds("data/processed/crude_stats_and_diffs.rds")

dc_spt <-
  crude_stats$decision_tree %>%
  filter(abs(diff_spt_hrs) < 5) %>%
  select(id, noon_day, spt_hrs, zm_spt_hrs) %>%
  mutate(spt_min = spt_hrs * 60,
         zm_spt_min = zm_spt_hrs * 60) %>%
  group_by(id) %>%
  mutate(noon_day = 1:n())

# Fit a linear mixed model (LMM) to predict spt_min based on noon_day, with a 
# random intercept for each subject (id). Extract ICC to calculate MDES.

get_mdes_spt <-
  function(tbl) {
    lmm <- lmer(spt_min ~ noon_day + (1 | id), data = tbl)
    icc <- performance::icc(lmm)[[1]]
    sd_within <- sigma(lmm)
    # MDES = (Z_β + Z_α/2) * σ / √(n * (k * (1 - ICC) + ICC))
    mdes <- (.84 + 1.96) * sd_within / sqrt(75) * (4.5 * (1 - icc) + icc)
    return(mdes)
  }


# Create bootstraps
boots <-
  rsample::bootstraps(dc_spt, times = 100)

# Calculate MDC for each bootstrapped data set
res <-
  map_dbl(boots$splits, get_mdes_spt)

mdes_mean <- mean(res)

# Calculate bootstrap confidence intervals
mdes_ci <- quantile(res, c(0.025, 0.975))

glue::glue("MDC-ICC (CI95%)
           {round(mdes_mean, 1)} minutes ({round(mdes_ci[[1]], 1)} - {round(mdes_ci[[2]], 1)})")



get_mdes_zm <-
  function(tbl) {
    lmm <- lmer(zm_spt_min ~ noon_day + (1 | id), data = tbl)
    icc <- performance::icc(lmm)[[1]]
    sd_within <- sigma(lmm)
    # MDES = (Z_β + Z_α/2) * σ / √(n * (k * (1 - ICC) + ICC))
    mdes <- (.84 + 1.96) * sd_within / sqrt(75) * (4.5 * (1 - icc) + icc)
    return(mdes)
  }

res_zm <- 
  map_dbl(boots$splits, get_mdes_zm)

mdes_mean_zm <- mean(res_zm)

mdes_ci_zm <- quantile(res_zm, c(0.025, 0.975))

glue::glue("MDC-ICC (CI95%)
           {round(mdes_mean_zm, 1)} minutes ({round(mdes_ci_zm[[1]], 1)} - {round(mdes_ci_zm[[2]], 1)})")


### TESTING ###
data <- data.frame(
  id = rep(1:10, each = 4),
  noon_day = rep(1:4, 10),
  outcome1 = rnorm(40, mean = 5, sd = 2),
  outcome2 = rnorm(40, mean = 10, sd = 3),
  outcome3 = rnorm(40, mean = 15, sd = 4)
)

fit_lmer <- function(tbl, outcome_col) {
  # Capture the outcome as a quosure
  outcome_quo <- enquo(outcome_col)
  
  # Build the formula using paste() and as.formula()
  formula <- as.formula(paste0(quo_name(outcome_quo), " ~ noon_day + (1 | id)"))
  
  # Fit the linear mixed model
  lmm <- lmer(formula, data = tbl)
  
  return(lmm)
}

outcome_cols <- c("outcome1", "outcome2", "outcome3")

lmer_results <- map(outcome_cols, ~ fit_lmer(data, !!sym(.x))) %>% 
  set_names(outcome_cols)