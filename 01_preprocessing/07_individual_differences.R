# =============================================================================
# 07_individual_differences.R
#
# PURPOSE:
#   Aggregate baseline behaviour to the individual level.
#
# KEY UPDATE:
#   Adds average_daily_opens_baseline, computed as the participant-level mean
#   number of daily opens during baseline.
#
# INPUTS:
#   data/baseline_all_apps/session_time/time_per_day.csv
#   data/baseline_all_apps/opens/opens_per_day.csv
#   data/baseline_all_apps/session_time/time_per_day_avg.csv
#
# OUTPUTS:
#   data/baseline/individual_differences.csv
#
# Based on the uploaded individual-differences script. :contentReference[oaicite:0]{index=0}
# =============================================================================

Sys.setenv(TZ = "Europe/Copenhagen")

library(tidyverse)
library(lubridate)

dir.create("data/baseline", showWarnings = FALSE, recursive = TRUE)
dir.create("reports/07_individual_differences", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. LOAD BASELINE DAILY DATA
# =============================================================================

time_per_day <- read_csv(
  "data/baseline_all_apps/session_time/time_per_day.csv",
  show_col_types = FALSE
)

opens_per_day <- read_csv(
  "data/baseline_all_apps/opens/opens_per_day.csv",
  show_col_types = FALSE
)

time_per_day_avg <- read_csv(
  "data/baseline_all_apps/session_time/time_per_day_avg.csv",
  show_col_types = FALSE
)

# =============================================================================
# 2. AGGREGATE BASELINE BEHAVIOUR TO INDIVIDUAL LEVEL
# =============================================================================

session_time_id <- time_per_day %>%
  filter(!days_before_activation %in% c(-14, 0)) %>%
  group_by(ID) %>%
  summarise(
    total_time_minutes = mean(total_time_minutes, na.rm = TRUE),
    average_time_daily_baseline = mean(total_time_minutes, na.rm = TRUE),
    .groups = "drop"
  )

opens_id <- opens_per_day %>%
  filter(!days_before_activation %in% c(-14, 0)) %>%
  group_by(ID) %>%
  summarise(
    average_daily_opens_baseline = mean(daily_opens, na.rm = TRUE),
    median_daily_opens_baseline = median(daily_opens, na.rm = TRUE),
    sd_daily_opens_baseline = sd(daily_opens, na.rm = TRUE),
    daily_opens_median = median(daily_opens, na.rm = TRUE),
    sd_daily_opens = sd(daily_opens, na.rm = TRUE),
    .groups = "drop"
  )

session_length_id <- time_per_day_avg %>%
  filter(!days_before_activation %in% c(-14, 0)) %>%
  group_by(ID) %>%
  summarise(
    mean_time_minutes = mean(mean_time_minutes, na.rm = TRUE),
    average_session_length_baseline = mean(mean_time_minutes, na.rm = TRUE),
    .groups = "drop"
  )

combined_metrics <- opens_id %>%
  full_join(session_time_id, by = "ID") %>%
  full_join(session_length_id, by = "ID")

write_csv(
  combined_metrics,
  "data/baseline/individual_differences.csv"
)

message("Individual differences saved: ", nrow(combined_metrics), " participants")

# =============================================================================
# 3. REPORT
# =============================================================================

summary_report <- combined_metrics %>%
  summarise(
    n = n(),
    mean_daily_time_min = round(mean(total_time_minutes, na.rm = TRUE), 2),
    sd_daily_time_min = round(sd(total_time_minutes, na.rm = TRUE), 2),
    mean_daily_opens = round(mean(average_daily_opens_baseline, na.rm = TRUE), 2),
    sd_daily_opens = round(sd(average_daily_opens_baseline, na.rm = TRUE), 2),
    median_daily_opens = round(median(median_daily_opens_baseline, na.rm = TRUE), 2),
    mean_session_length_min = round(mean(mean_time_minutes, na.rm = TRUE), 2),
    sd_session_length_min = round(sd(mean_time_minutes, na.rm = TRUE), 2)
  )

write_csv(
  summary_report,
  "reports/07_individual_differences/individual_differences_summary.csv"
)

print(summary_report)

message("07_individual_differences.R complete")