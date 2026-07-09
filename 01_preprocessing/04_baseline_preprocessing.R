Sys.setenv(TZ = "Europe/Copenhagen")
# =============================================================================
# 04_baseline_preprocessing.R
#
# PURPOSE:
#   From the cleaned event-level dataset, compute session-level and daily
#   aggregates for the BASELINE period only (all 11 social-media apps).
#   Identical logic to the original baseline_preproc_all_apps.R but with
#   all dead code, diagnostic plots, and sleep estimation removed.
#
# INPUTS:
#   data/data_filt_step4_all_apps.csv
#
# OUTPUTS:
#   data/baseline/baseline_raw/baseline_sessions_filt3.csv
#   data/baseline/baseline_raw/baseline_opens_filt3.csv
#   data/baseline_all_apps/session_time/time_per_day.csv
#   data/baseline_all_apps/session_time/time_per_day_app.csv
#   data/baseline_all_apps/session_time/time_per_hour.csv
#   data/baseline_all_apps/session_time/time_per_hour2.csv
#   data/baseline_all_apps/session_time/time_per_day_avg.csv
#   data/baseline_all_apps/session_time/time_per_day_avg_app.csv
#   data/baseline_all_apps/opens/opens_per_day.csv
#   data/baseline_all_apps/opens/opens_per_day_app.csv
#   data/baseline_all_apps/opens/opens_per_hour.csv
#   data/baseline_all_apps/opens/opens_per_hour_no_app.csv
# =============================================================================

library(tidyverse)
library(lubridate)

dir.create("data/baseline/baseline_raw",          showWarnings = FALSE, recursive = TRUE)
dir.create("data/baseline_all_apps/session_time", showWarnings = FALSE, recursive = TRUE)
dir.create("data/baseline_all_apps/opens",        showWarnings = FALSE, recursive = TRUE)

TARGET_APPS <- c(
  "instagram", "linkedin", "facebook", "beReal", "snapchat",
  "pinterest", "twitch", "twitter", "youtube", "reddit", "tikTok"
)

# =============================================================================
# 1. LOAD AND ENFORCE TIMEZONE
# =============================================================================

data_all <- read_csv("data/data_filt_step4_all_apps.csv", show_col_types = FALSE) %>%
  mutate(
    time               = as.POSIXct(time,               tz = "Europe/Copenhagen"),
    time_at_activation = as.POSIXct(time_at_activation, tz = "Europe/Copenhagen"),
    date2              = as.Date(time, tz = "Europe/Copenhagen")
  )

# =============================================================================
# 2. IDENTIFY VALID ID×APP COMBOS
#    Keep only combos that have both openedApp AND closedApp in either period,
#    then filter to baseline only — this matches the original script exactly.
# =============================================================================

valid_id_apps_openclose <- data_all %>%
  filter(app %in% TARGET_APPS,
         period %in% c("baseline", "intervention")) %>%
  group_by(ID, app, period) %>%
  summarise(
    has_opened = any(resolution == "openedApp"),
    has_closed = any(resolution == "closedApp"),
    .groups = "drop"
  ) %>%
  filter(has_opened & has_closed) %>%
  group_by(ID, app) %>%
  summarise(n_periods = n(), .groups = "drop") %>%
  filter(n_periods > 0)

baseline_time <- data_all %>%
  filter(app %in% TARGET_APPS,
         period %in% c("baseline", "intervention")) %>%
  semi_join(valid_id_apps_openclose, by = c("ID", "app")) %>%
  filter(period == "baseline")

message("Baseline participants: ", n_distinct(baseline_time$ID))

# =============================================================================
# 3. SESSION MATCHING  (rules 1–4, identical to original)
#    Rule 1: openedApp immediately followed by closedApp, same app
#    Rule 2: one event intervenes between open and close, same app
#    Rule 3: two events intervene
#    Rule 4: one event intervenes, slightly relaxed timing
# =============================================================================

baseline_sessions <- baseline_time %>%
  arrange(ID, timestamp, desc(resolution == "openedApp")) %>%
  group_by(ID) %>%
  mutate(
    app_next     = lead(app),   app_next2   = lead(app, 2),   app_next3   = lead(app, 3),
    app_prev     = lag(app),    app_prev2   = lag(app, 2),

    resolution_next  = lead(resolution),  resolution_next2 = lead(resolution, 2),
    resolution_next3 = lead(resolution, 3), resolution_last = lag(resolution),
    resolution_last2 = lag(resolution, 2),

    timestamp_next  = lead(timestamp),  timestamp_next2 = lead(timestamp, 2),
    timestamp_next3 = lead(timestamp, 3), timestamp_last = lag(timestamp),
    timestamp_last2 = lag(timestamp, 2),

    time_next = lead(time), time_prev = lag(time),

    valid_open1 = resolution == "openedApp" &
      resolution_next == "closedApp" & app == app_next,

    valid_open2 = resolution == "openedApp" &
      app != app_next & app == app_next2 &
      resolution_next2 == "closedApp" &
      abs(timestamp_next - timestamp_next2) <= 4,

    valid_open3 = resolution == "openedApp" &
      app != app_next & app != app_next2 & app == app_next3 &
      resolution_next3 == "closedApp" &
      abs(timestamp - timestamp_next) <= 3 &
      abs(timestamp_next2 - timestamp_next3) <= 4,

    valid_open4 = resolution == "openedApp" &
      app != app_next & app == app_next2 &
      resolution_next2 == "closedApp" &
      abs(timestamp - timestamp_next) <= 3,

    close_ts = case_when(
      valid_open1 ~ timestamp_next,
      valid_open2 ~ timestamp_next2,
      valid_open3 ~ timestamp_next3,
      valid_open4 ~ timestamp_next2,
      TRUE ~ NA_real_
    ),

    time_diff_secs    = close_ts - timestamp,
    time_diff_minutes = time_diff_secs / 60
  ) %>%
  ungroup()

# Keep only matched sessions (rules 1–4) and apply 3-hour cap
baseline_sessions_filt <- baseline_sessions %>%
  filter(valid_open1 | valid_open2 | valid_open3 | valid_open4) %>%
  filter(time_diff_secs <= 10800)   # cap at 3 hours, same as original

# =============================================================================
# 4. TIMEZONE-CORRECT TIMESTAMPS AND COMPUTE DAYS_BEFORE_ACTIVATION
# =============================================================================

baseline_sessions_filt3 <- baseline_sessions_filt %>%
  mutate(
    timestamp            = with_tz(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"),
                                   tzone = "Europe/Copenhagen"),
    time_at_activation   = with_tz(time_at_activation,   tzone = "Europe/Copenhagen"),
    time_at_intervention = with_tz(time_at_intervention,  tzone = "Europe/Copenhagen"),
    date                 = floor_date(timestamp, unit = "day"),
    hour                 = hour(timestamp),
    date_at_activation   = floor_date(time_at_activation,   unit = "day"),
    date_at_intervention = floor_date(time_at_intervention, unit = "day"),
    days_before_activation     = round(as.numeric(
      difftime(date, date_at_intervention, units = "days")
    )),
    days_before_activation_num = days_before_activation
  )

write_csv(baseline_sessions_filt3,
          "data/baseline/baseline_raw/baseline_sessions_filt3.csv")

# =============================================================================
# 5. HELPER: PERIOD LABELLING  (used identically in every aggregate)
# =============================================================================

label_periods <- function(df) {
  df %>%
    mutate(
      period = case_when(
        has_baseline & has_intervention ~ "mixed",
        has_baseline                   ~ "baseline",
        has_intervention               ~ "intervention",
        has_outro                      ~ "outro",
        TRUE ~ NA_character_
      ),
      period2 = case_when(
        has_baseline & has_intervention ~ "intervention",
        has_baseline                   ~ "baseline",
        has_intervention               ~ "intervention",
        has_outro                      ~ "outro",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-has_baseline, -has_intervention, -has_outro)
}

# =============================================================================
# 6. SESSION-TIME AGGREGATES
# =============================================================================

make_time_agg <- function(df, group_vars, value_expr, value_col) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      !!value_col        := !!value_expr,
      date2              = last(date2),
      has_baseline       = any(period == "baseline"),
      has_intervention   = any(period == "intervention"),
      has_outro          = any(period == "outro"),
      condition          = last(condition),
      .groups = "drop"
    ) %>%
    label_periods()
}

time_per_day <- baseline_sessions_filt3 %>%
  group_by(ID, days_before_activation) %>%
  summarise(
    total_time_minutes = sum(time_diff_minutes, na.rm = TRUE),
    date2              = last(date2),
    has_baseline       = any(period == "baseline"),
    has_intervention   = any(period == "intervention"),
    has_outro          = any(period == "outro"),
    condition          = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

time_per_day_app <- baseline_sessions_filt3 %>%
  group_by(ID, app, days_before_activation) %>%
  summarise(
    total_time_minutes = sum(time_diff_minutes, na.rm = TRUE),
    date2              = last(date2),
    has_baseline       = any(period == "baseline"),
    has_intervention   = any(period == "intervention"),
    has_outro          = any(period == "outro"),
    condition          = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

time_per_hour <- baseline_sessions_filt3 %>%
  group_by(ID, app, days_before_activation, hour) %>%
  summarise(
    total_time_minutes = sum(time_diff_minutes, na.rm = TRUE),
    date2              = last(date2),
    has_baseline       = any(period == "baseline"),
    has_intervention   = any(period == "intervention"),
    has_outro          = any(period == "outro"),
    condition          = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

time_per_hour_2 <- baseline_sessions_filt3 %>%
  group_by(ID, days_before_activation, hour) %>%
  summarise(
    total_time_minutes = sum(time_diff_minutes, na.rm = TRUE),
    date2              = last(date2),
    has_baseline       = any(period == "baseline"),
    has_intervention   = any(period == "intervention"),
    has_outro          = any(period == "outro"),
    condition          = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

time_per_day_avg <- baseline_sessions_filt3 %>%
  group_by(ID, days_before_activation) %>%
  summarise(
    mean_time_minutes = mean(time_diff_minutes, na.rm = TRUE),
    date2             = last(date2),
    has_baseline      = any(period == "baseline"),
    has_intervention  = any(period == "intervention"),
    has_outro         = any(period == "outro"),
    condition         = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

time_per_day_avg_app <- baseline_sessions_filt3 %>%
  group_by(ID, app, days_before_activation) %>%
  summarise(
    mean_time_minutes = mean(time_diff_minutes, na.rm = TRUE),
    date2             = last(date2),
    has_baseline      = any(period == "baseline"),
    has_intervention  = any(period == "intervention"),
    has_outro         = any(period == "outro"),
    condition         = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

save_path_session <- "data/baseline_all_apps/session_time"
write_csv(time_per_day,         file.path(save_path_session, "time_per_day.csv"))
write_csv(time_per_day_app,     file.path(save_path_session, "time_per_day_app.csv"))
write_csv(time_per_hour,        file.path(save_path_session, "time_per_hour.csv"))
write_csv(time_per_hour_2,      file.path(save_path_session, "time_per_hour2.csv"))
write_csv(time_per_day_avg,     file.path(save_path_session, "time_per_day_avg.csv"))
write_csv(time_per_day_avg_app, file.path(save_path_session, "time_per_day_avg_app.csv"))
message("✓ Session-time files saved to ", save_path_session)

# =============================================================================
# 7. OPENS AGGREGATES
# =============================================================================

# Baseline opens: from the full baseline data (not session-matched)
baseline_raw <- data_all %>%
  filter(app %in% TARGET_APPS, period == "baseline") %>%
  semi_join(valid_id_apps_openclose, by = c("ID", "app"))

opens_df <- baseline_raw %>%
  mutate(
    time               = with_tz(time,               tzone = "Europe/Copenhagen"),
    time_at_activation = with_tz(time_at_activation, tzone = "Europe/Copenhagen"),
    time_at_intervention = with_tz(time_at_intervention, tzone = "Europe/Copenhagen"),
    date               = floor_date(time, unit = "day"),
    date_at_intervention = floor_date(time_at_intervention, unit = "day"),
    days_before_activation     = round(as.numeric(
      difftime(date, date_at_intervention, units = "days")
    )),
    days_before_activation_num = days_before_activation,
    date2              = as.Date(time, tz = "Europe/Copenhagen"),
    hour               = hour(time),
    open               = as.integer(resolution == "openedApp")
  ) %>%
  dplyr::select(
    ID, app, timestamp, time, resolution, open,
    period, condition, participation_day,
    time_at_activation, time_at_intervention,
    date, date2, hour, days_before_activation, days_before_activation_num
  )

write_csv(opens_df, "data/baseline/baseline_raw/baseline_opens_filt3.csv")

opens_per_day <- opens_df %>%
  group_by(ID, days_before_activation) %>%
  summarise(
    daily_opens      = sum(open, na.rm = TRUE),
    date2            = last(date2),
    has_baseline     = any(period == "baseline"),
    has_intervention = any(period == "intervention"),
    has_outro        = any(period == "outro"),
    condition        = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

opens_per_day_app <- opens_df %>%
  group_by(ID, app, days_before_activation) %>%
  summarise(
    daily_opens      = sum(open, na.rm = TRUE),
    date2            = last(date2),
    has_baseline     = any(period == "baseline"),
    has_intervention = any(period == "intervention"),
    has_outro        = any(period == "outro"),
    condition        = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

opens_per_hour <- opens_df %>%
  group_by(ID, app, days_before_activation, hour) %>%
  summarise(
    daily_opens      = sum(open, na.rm = TRUE),
    date2            = last(date2),
    has_baseline     = any(period == "baseline"),
    has_intervention = any(period == "intervention"),
    has_outro        = any(period == "outro"),
    condition        = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

opens_per_hour_no_app <- opens_df %>%
  group_by(ID, days_before_activation, hour) %>%
  summarise(
    daily_opens      = sum(open, na.rm = TRUE),
    date2            = last(date2),
    has_baseline     = any(period == "baseline"),
    has_intervention = any(period == "intervention"),
    has_outro        = any(period == "outro"),
    condition        = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

save_path_opens <- "data/baseline_all_apps/opens"
write_csv(opens_per_day,         file.path(save_path_opens, "opens_per_day.csv"))
write_csv(opens_per_day_app,     file.path(save_path_opens, "opens_per_day_app.csv"))
write_csv(opens_per_hour,        file.path(save_path_opens, "opens_per_hour.csv"))
write_csv(opens_per_hour_no_app, file.path(save_path_opens, "opens_per_hour_no_app.csv"))
message("✓ Opens files saved to ", save_path_opens)
