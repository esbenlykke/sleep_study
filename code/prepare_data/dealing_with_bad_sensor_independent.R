# BASELINE
#   1: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to min; returning Inf
# ℹ The warning occurred in group 125: id = 604804, noon_day = 9.
# 2: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to max; returning -Inf
# ℹ The warning occurred in group 166: id = 757104, noon_day = 1.
# 3: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to min; returning Inf
# ℹ The warning occurred in group 166: id = 757104, noon_day = 1.
# 4: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to max; returning -Inf
# ℹ The warning occurred in group 167: id = 757104, noon_day = 2.
# 5: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to min; returning Inf
# ℹ The warning occurred in group 167: id = 757104, noon_day = 2.
# 6: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to max; returning -Inf
# ℹ The warning occurred in group 168: id = 757104, noon_day = 29.
# 7: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to min; returning Inf
# ℹ The warning occurred in group 168: id = 757104, noon_day = 29.
# 8: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to max; returning -Inf
# ℹ The warning occurred in group 462: id = 2596204, noon_day = 28.
# TODO overvej at bruge mean som static

# FOLLOWUP
# 1: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to max; returning -Inf
# ℹ The warning occurred in group 59: id = 447104, noon_day = 13.
# 2: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to max; returning -Inf
# ℹ The warning occurred in group 60: id = 447105, noon_day = 11.
# 3: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to min; returning Inf
# ℹ The warning occurred in group 60: id = 447105, noon_day = 11.
# 4: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to max; returning -Inf
# ℹ The warning occurred in group 61: id = 447105, noon_day = 13.
# 5: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to min; returning Inf
# ℹ The warning occurred in group 61: id = 447105, noon_day = 13.
# 6: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to max; returning -Inf
# ℹ The warning occurred in group 127: id = 1054804, noon_day = 31.
# 7: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to max; returning -Inf
# ℹ The warning occurred in group 159: id = 1262105, noon_day = 2.
# 8: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to min; returning Inf
# ℹ The warning occurred in group 159: id = 1262105, noon_day = 2.
# 9: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to max; returning -Inf
# ℹ The warning occurred in group 160: id = 1262105, noon_day = 3.
# 10: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to min; returning Inf
# ℹ The warning occurred in group 160: id = 1262105, noon_day = 3.
# 11: Problem while computing `proxy = if_else(...)`.
# ℹ no non-missing arguments to max; returning -Inf
# ℹ The warning occurred in group 208: id = 1587606, noon_day = 27.

# Problem IDs:
# 1   649105
# 2   7017604 
# 3   757104 
# 4   846005 
# 5   1055204
# 6   1091905
# 7   1292204
# 8   1377204
# 9   1528704
# 10  1742704
# 11  1783204
# 12  2304105
# 13  2584704
# 14  2584706
# 15  2627304
# 16  262730

x |>
  mutate(
    month = case_when(
      TRUE ~ month(datetime - hours(12))
    ),
    in_bed_72 = slide_sum(in_bed_pred, before = 36, after = 36),
    sleep_72 = slide_sum(sleep_pred, before = 36, after = 36),
    in_bed_sleep = if_else(in_bed_72 > 60 & sleep_72 > 60, 1, 0),
    in_bed_no_sleep = if_else(in_bed_72 > 60 & sleep_72 < 60, 1, 0)
  ) |>
group_by(id, noon_day, month) |>
  summarise(
    spt_hrs = ((max(row_number()[in_bed_72 == 60] + 30) - min(row_number()[in_bed_72 == 60] - 30)) * 10) / 60 / 60,
    tst_hrs = (sum(in_bed_sleep) * 10) / 60 / 60,
    se_percent = 100 * (tst_hrs / spt_hrs),
    lps_min = abs((((min(row_number()[sleep_72 == 60])) - min(row_number()[in_bed_72 == 60] - 30)) * 10) / 60),
    waso_min = (sum(in_bed_no_sleep) * 10) / 60,
    .groups = "drop"
  ) |>
  inner_join(zm_stats, by = c("id", "noon_day" = "day", "month")) |> filter(tst_hrs < 2)



x |>
  mutate(
    month = month(datetime - hours(12)),
    in_bed_72 = slide_sum(in_bed_pred, before = 36, after = 36),
    sleep_72 = slide_sum(sleep_pred, before = 36, after = 36),
    in_bed_sleep = if_else(in_bed_72 > 60 & sleep_72 > 60, 1, 0),
    in_bed_no_sleep = if_else(in_bed_72 > 60 & sleep_72 < 60, 1, 0)
  ) |>
  count(month, noon_day, id) |>
  arrange(id)
