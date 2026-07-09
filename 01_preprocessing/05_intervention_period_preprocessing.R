Sys.setenv(TZ = "Europe/Copenhagen")
set.seed(1997)
# =============================================================================
# 05_intervention_period_preprocessing.R
#
# PURPOSE:
#   Build all session-level and daily aggregates for BOTH baseline and
#   intervention periods combined. This is the main dataset for all
#   intervention-effect models.
#
#   The group-tagging loop and the cleaned-final pipeline are kept
#   character-for-character identical to the original script to ensure
#   intervention_baseline_cleaned_final.csv is reproducible.
#
# INPUTS:
#   data/data_filt_step4_all_apps.csv
#
# OUTPUTS:
#   data/opens_df/intervention_baseline_cleaned_final.csv
#   data/baseline_intervention_outro/session_time/session_time_dataset_97_5_cutoff.csv
#   data/baseline_intervention_outro/session_time/time_per_day.csv
#   data/baseline_intervention_outro/session_time/time_per_day_app.csv
#   data/baseline_intervention_outro/session_time/time_per_hour.csv
#   data/baseline_intervention_outro/session_time/time_per_day_avg.csv
#   data/baseline_intervention_outro/session_time/time_per_day_avg_app.csv
#   data/baseline_intervention_outro/opens/opens_per_day.csv
#   data/baseline_intervention_outro/opens/opens_per_day_app.csv
#   data/baseline_intervention_outro/opens/opens_per_hour.csv
#   data/baseline_intervention_outro/opens/opens_per_period_app.csv
#   data/baseline_intervention_outro/opens/opens_per_day_night_morning.csv
#   data/regret_rate/intervention_app_for_regret.csv
# =============================================================================

library(tidyverse)
library(lubridate)

dir.create("data/opens_df",                                    showWarnings = FALSE, recursive = TRUE)
dir.create("data/baseline_intervention_outro/session_time",   showWarnings = FALSE, recursive = TRUE)
dir.create("data/baseline_intervention_outro/opens",          showWarnings = FALSE, recursive = TRUE)
dir.create("data/regret_rate",                                showWarnings = FALSE, recursive = TRUE)

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

intervention_baseline <- data_all %>%
  filter(period %in% c("baseline", "intervention"))

message("Unique periods: ", paste(unique(intervention_baseline$period), collapse = ", "))

# =============================================================================
# 2. GROUP TAGGING LOOP  ← KEPT EXACTLY AS IN ORIGINAL SCRIPT
#    Groups consecutive events for the same ID×app into sessions.
#    A new group starts when more than 70 seconds separate two openedApp events.
# =============================================================================

intervention_baseline_tagged <- intervention_baseline %>%
  arrange(ID, timestamp)           # first sort (matches original line 1)

intervention_baseline_tagged <- intervention_baseline %>%
  arrange(ID, app, timestamp)      # second sort overrides (matches original line 2)

# Initialize the group column
intervention_baseline_tagged$group_id <- NA_integer_

# Global counter for group tracking
global_group_counter <- 1

# Loop through each row of the dataframe
for (i in seq_len(nrow(intervention_baseline_tagged))) {

  # Check if this is the first row or a new ID/App (reset tracking)
  if (i == 1 ||
      intervention_baseline_tagged$ID[i]  != intervention_baseline_tagged$ID[i - 1] ||
      intervention_baseline_tagged$app[i] != intervention_baseline_tagged$app[i - 1]) {

    first_open_time <- intervention_baseline_tagged$timestamp[i]  # Reset the first open time
    intervention_baseline_tagged$group_id[i] <- global_group_counter
    global_group_counter <- global_group_counter + 1
    next  # Skip to the next iteration
  }

  # If the event is an app opening
  if (intervention_baseline_tagged$resolution[i] == "openedApp") {

    time_since_first_open <- intervention_baseline_tagged$timestamp[i] - first_open_time

    if (time_since_first_open > 70) {
      # Start a new group if more than 70 seconds have passed
      first_open_time <- intervention_baseline_tagged$timestamp[i]
      intervention_baseline_tagged$group_id[i] <- global_group_counter
      global_group_counter <- global_group_counter + 1
    } else {
      # Otherwise, assign to the current group
      intervention_baseline_tagged$group_id[i] <- global_group_counter - 1
    }

  } else {
    # Assign non-openedApp events to the last known group
    intervention_baseline_tagged$group_id[i] <- global_group_counter - 1
  }
}

# =============================================================================
# 3. DETECT REDUNDANT OPENS/CLOSES  ← KEPT EXACTLY AS IN ORIGINAL SCRIPT
# =============================================================================

intervention_baseline_cleaned <- intervention_baseline_tagged %>%
  group_by(ID, app, group_id) %>%
  mutate(
    resolution_next       = lead(resolution),
    openedapp_open_time   = ifelse(resolution == "openedApp", timestamp, NA),
    first_open_time       = min(openedapp_open_time, na.rm = TRUE),
    first_in_group        = ifelse(resolution == "openedApp" & timestamp == first_open_time, 1, 0),
    not_first_open_in_group = ifelse(resolution == "openedApp" & timestamp > first_open_time, 1, 0),
    last_closedapp_time   = max(ifelse(resolution == "closedApp", timestamp, NA), na.rm = TRUE),
    last_close_in_group   = ifelse(resolution == "closedApp" & timestamp == last_closedapp_time, 1, 0),
    not_last_close_in_group = ifelse(resolution == "closedApp" & timestamp < last_closedapp_time, 1, 0),
    not_last_close_in_group_2 = ifelse(
      resolution == "closedApp" & timestamp < last_closedapp_time &
        resolution_next != "closedApp", 1, 0)
  ) %>%
  ungroup()

# ---- Prepare Final Data & Export ----
intervention_baseline_cleaned_final <- intervention_baseline_cleaned %>%
  filter(not_first_open_in_group == 0 & not_last_close_in_group_2 == 0)

write_csv(intervention_baseline_cleaned_final,
          "data/opens_df/intervention_baseline_cleaned_final.csv")

intervention_baseline_cleaned_final = read_csv( "data/opens_df/intervention_baseline_cleaned_final.csv")

intervention_baseline_cleaned_final = read_csv("data/opens_df/intervention_baseline_cleaned_final.csv")
message("✓ intervention_baseline_cleaned_final.csv saved: ",
        n_distinct(intervention_baseline_cleaned_final$ID), " participants, ",
        nrow(intervention_baseline_cleaned_final), " rows")

# =============================================================================
# 4. HELPER: PERIOD LABELLING  (used identically in every aggregate)
# =============================================================================

label_periods <- function(df) {
  df %>%
    mutate(
      period = case_when(
        has_baseline & has_intervention ~ "mixed",
        has_baseline ~ "baseline",
        has_intervention ~ "intervention",
        has_outro ~ "outro",
        has_intervention & has_outro ~ "mixed",
        TRUE ~ NA_character_
      ),
      period2 = case_when(
        has_baseline & has_intervention ~ "intervention",
        has_baseline ~ "baseline",
        has_intervention ~ "intervention",
        has_outro ~ "outro",
        has_intervention & has_outro ~ "outro",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-has_baseline, -has_intervention, -has_outro)
}

# =============================================================================
# 5. SESSION-TIME DATASET  (rules 1–4, 3-hour cap)
# =============================================================================

intervention_baseline_cleaned_final_selected_apps <-
  intervention_baseline_cleaned_final %>%
  filter(app %in% TARGET_APPS)

valid_id_apps_openclose <- intervention_baseline_cleaned_final_selected_apps %>%
  group_by(ID, app, period) %>%
  summarise(
    has_opened = any(resolution == "openedApp"),
    has_closed = any(resolution == "closedApp"),
    .groups = "drop"
  ) %>%
  filter(has_opened & has_closed) %>%
  group_by(ID, app) %>%
  summarise(n_periods = n(), .groups = "drop") %>%
  filter(n_periods >= 2)

all_period_filtered <- intervention_baseline_cleaned_final_selected_apps %>%
  semi_join(valid_id_apps_openclose, by = c("ID", "app"))

message("Participants after open/close filter: ", n_distinct(all_period_filtered$ID))

all_period_sessions <- all_period_filtered %>%
  arrange(ID, timestamp) %>%
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

    time_next = lead(time), time_next2 = lead(time, 2), time_prev = lag(time),

    valid_open1 = resolution == "openedApp" &
      resolution_next == "closedApp" & app == app_next,

    valid_open2 = resolution == "openedApp" &
      app != app_next & app == app_next2 &
      resolution_next2 == "closedApp" &
      abs(timestamp_next - timestamp_next2) <= 3,

    valid_open3 = resolution == "openedApp" &
      app != app_next & app != app_next2 & app == app_next3 &
      resolution_next3 == "closedApp" &
      abs(timestamp - timestamp_next) <= 3 &
      abs(timestamp_next2 - timestamp_next3) <= 3,

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

    time_diff_secs         = close_ts - timestamp,
    time_diff_minutes      = time_diff_secs / 60,
    time_diff_minutes_round = round(time_diff_minutes),

    valid_open5 = resolution == "openedApp" &
      resolution_last == "closedApp" & app == app_prev &
      abs(timestamp - timestamp_last) <= 3,

    valid_open6 = resolution == "openedApp" &
      resolution_last2 == "closedApp" & app == app_prev2 &
      abs(timestamp - timestamp_last2) <= 3,

    unmatched_open = resolution == "openedApp" &
      !valid_open1 & !valid_open2 & !valid_open3 & !valid_open4
  ) %>%
  ungroup() %>%
  mutate(row_id = row_number())

all_period_sessions_filt <- all_period_sessions %>%
  filter(valid_open1 | valid_open2 | valid_open3 | valid_open4) %>%
  filter(time_diff_secs <= 10800)   # 3-hour cap

all_period_sessions_filt3 <- all_period_sessions_filt %>%
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

write_csv(all_period_sessions_filt3,
          "data/baseline_intervention_outro/session_time/session_time_dataset_97_5_cutoff.csv")

# =============================================================================
# 6. TIME-SPENT AGGREGATES
# =============================================================================

time_per_day <- all_period_sessions_filt3 %>%
  group_by(ID, days_before_activation_num) %>%
  summarise(
    total_time_minutes = sum(time_diff_minutes, na.rm = TRUE),
    date               = last(date2),
    has_baseline       = any(period == "baseline"),
    has_intervention   = any(period == "intervention"),
    has_outro          = any(period == "outro"),
    condition          = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

time_per_day_app <- all_period_sessions_filt3 %>%
  group_by(ID, app, days_before_activation_num) %>%
  summarise(
    total_time_minutes = sum(time_diff_minutes, na.rm = TRUE),
    date               = last(date2),
    has_baseline       = any(period == "baseline"),
    has_intervention   = any(period == "intervention"),
    has_outro          = any(period == "outro"),
    condition          = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

time_per_hour <- all_period_sessions_filt3 %>%
  group_by(ID, app, days_before_activation_num, hour) %>%
  summarise(
    total_time_minutes = sum(time_diff_minutes, na.rm = TRUE),
    date               = last(date2),
    has_baseline       = any(period == "baseline"),
    has_intervention   = any(period == "intervention"),
    has_outro          = any(period == "outro"),
    condition          = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

time_per_day_avg <- all_period_sessions_filt3 %>%
  group_by(ID, days_before_activation_num) %>%
  summarise(
    mean_time_minutes = mean(time_diff_minutes, na.rm = TRUE),
    date              = last(date2),
    has_baseline      = any(period == "baseline"),
    has_intervention  = any(period == "intervention"),
    has_outro         = any(period == "outro"),
    condition         = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

time_per_day_avg_app <- all_period_sessions_filt3 %>%
  group_by(ID, app, days_before_activation_num) %>%
  summarise(
    mean_time_minutes = mean(time_diff_minutes, na.rm = TRUE),
    date              = last(date2),
    has_baseline      = any(period == "baseline"),
    has_intervention  = any(period == "intervention"),
    has_outro         = any(period == "outro"),
    condition         = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

save_path_session <- "data/baseline_intervention_outro/session_time"
write_csv(time_per_day,         file.path(save_path_session, "time_per_day.csv"))
write_csv(time_per_day_app,     file.path(save_path_session, "time_per_day_app.csv"))
write_csv(time_per_hour,        file.path(save_path_session, "time_per_hour.csv"))
write_csv(time_per_day_avg,     file.path(save_path_session, "time_per_day_avg.csv"))
write_csv(time_per_day_avg_app, file.path(save_path_session, "time_per_day_avg_app.csv"))
message("✓ Session-time files saved to ", save_path_session)

# =============================================================================
# 7. OPENS AGGREGATES
# =============================================================================

opens_df_ap <- intervention_baseline_cleaned_final %>%
  arrange(ID, timestamp) %>%
  group_by(ID) %>%
  mutate(
    open            = as.integer(resolution == "openedApp"),
    open_or_dismiss = as.integer(resolution %in% c("openedApp","dismissedAppOpening"))
  ) %>%
  ungroup() %>%
  filter(resolution %in% c("openedApp", "dismissedAppOpening"))

opens_df_ap2 <- opens_df_ap %>%
  dplyr::select(ID, app, timestamp, time, resolution, open, open_or_dismiss,
                period, condition, participation_day,
                time_at_activation, time_at_intervention) %>%
  mutate(
    time               = with_tz(time,               tzone = "Europe/Copenhagen"),
    time_at_activation = with_tz(time_at_activation, tzone = "Europe/Copenhagen"),
    time_at_intervention = with_tz(time_at_intervention, tzone = "Europe/Copenhagen"),
    date               = floor_date(time, unit = "day"),
    date_at_activation = floor_date(time_at_activation, unit = "day"),
    date_at_intervention = floor_date(time_at_intervention, unit = "day"),
    days_before_activation     = round(as.numeric(
      difftime(date, date_at_intervention, units = "days")
    )),
    days_before_activation_num = days_before_activation,
    hour               = hour(time)
  )

opens_per_day <- opens_df_ap2 %>%
  group_by(ID, days_before_activation_num) %>%
  summarise(
    daily_opens      = sum(open, na.rm = TRUE),
    date             = last(date),
    has_baseline     = any(period == "baseline"),
    has_intervention = any(period == "intervention"),
    has_outro        = any(period == "outro"),
    condition        = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

opens_per_day_app <- opens_df_ap2 %>%
  group_by(ID, app, days_before_activation_num) %>%
  summarise(
    daily_opens      = sum(open, na.rm = TRUE),
    date             = last(date),
    has_baseline     = any(period == "baseline"),
    has_intervention = any(period == "intervention"),
    has_outro        = any(period == "outro"),
    condition        = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

opens_per_hour <- opens_df_ap2 %>%
  group_by(ID, app, days_before_activation_num, hour) %>%
  summarise(
    daily_opens      = sum(open, na.rm = TRUE),
    date             = last(date),
    has_baseline     = any(period == "baseline"),
    has_intervention = any(period == "intervention"),
    has_outro        = any(period == "outro"),
    condition        = last(condition),
    .groups = "drop"
  ) %>%
  label_periods()

opens_per_period_app <- opens_df_ap2 %>%
  group_by(ID, app, period) %>%
  summarise(
    daily_opens                = sum(open, na.rm = TRUE),
    condition                  = last(condition),
    days_before_activation_num = last(days_before_activation_num),
    has_baseline               = any(period == "baseline"),
    has_intervention           = any(period == "intervention"),
    has_outro                  = any(period == "outro"),
    .groups = "drop"
  ) %>%
  label_periods()

opens_per_day_night_morning <- opens_df_ap2 %>%
  mutate(
    day_id        = if_else(hour < 5L,
                            as.Date(date) - days(1),
                            as.Date(date)),
    opens_night   = if_else(hour >= 18L | hour < 5L, open, 0L),
    opens_morning = if_else(hour >= 5L & hour < 12L,  open, 0L)
  ) %>%
  arrange(ID, day_id, time) %>%
  group_by(ID, day_id) %>%
  summarise(
    opens_night                = sum(opens_night,   na.rm = TRUE),
    opens_morning              = sum(opens_morning, na.rm = TRUE),
    daily_opens                = sum(open,          na.rm = TRUE),
    days_before_activation_num = last(days_before_activation_num),
    date                       = last(day_id),
    has_baseline               = any(period == "baseline",     na.rm = TRUE),
    has_intervention           = any(period == "intervention", na.rm = TRUE),
    has_outro                  = any(period == "outro",        na.rm = TRUE),
    condition                  = last(condition),
    .groups = "drop"
  ) %>%
  label_periods() %>%
  arrange(ID, day_id)

save_path_opens <- "data/baseline_intervention_outro/opens"
write_csv(opens_per_day,              file.path(save_path_opens, "opens_per_day.csv"))
write_csv(opens_per_day_app,          file.path(save_path_opens, "opens_per_day_app.csv"))
write_csv(opens_per_hour,             file.path(save_path_opens, "opens_per_hour.csv"))
write_csv(opens_per_period_app,       file.path(save_path_opens, "opens_per_period_app.csv"))
write_csv(opens_per_day_night_morning, file.path(save_path_opens, "opens_per_day_night_morning.csv"))
message("✓ Opens files saved to ", save_path_opens)

# =============================================================================
# 8. REGRET / DISMISSAL DATASET
#    Intervention period only: every openedApp and dismissedAppOpening event
#    with break covariates pre-attached.
# =============================================================================

intervention_app_for_regret <- intervention_baseline_cleaned_final %>%
  filter(period == "intervention",
         resolution %in% c("openedApp", "dismissedAppOpening")) %>%
  mutate(
    time               = with_tz(time,               tzone = "Europe/Copenhagen"),
    time_at_intervention = with_tz(time_at_intervention, tzone = "Europe/Copenhagen"),
    date               = as.Date(time, tz = "Europe/Copenhagen"),
    date_at_intervention = as.Date(time_at_intervention, tz = "Europe/Copenhagen"),
    days_before_activation_num = round(as.numeric(
      difftime(date, date_at_intervention, units = "days")
    )),
    day_of_week     = weekdays(date),
    weekend         = as.integer(day_of_week %in% c("Saturday", "Sunday")),
    fall_break      = as.integer(
      date >= as.Date("2024-10-14") & date <= as.Date("2024-10-18")
    ),
    christmas_break = as.integer(
      date >= as.Date("2024-12-23") & date <= as.Date("2025-01-02")
    )
  )

write_csv(intervention_app_for_regret,
          "data/regret_rate/intervention_app_for_regret.csv")

message("✓ Regret dataset saved: ",
        n_distinct(intervention_app_for_regret$ID), " participants, ",
        nrow(intervention_app_for_regret), " events")
