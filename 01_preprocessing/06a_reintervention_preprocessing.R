# =============================================================================
# 02_analysis/appendix/06a_reintervention_preprocessing.R
#
# PURPOSE:
#   Build a clean appendix dataset for the Planning reintervention analysis.
#
#   The Planning intervention asks participants to choose a planned session
#   length. When the planned time has passed, the app may trigger a
#   reintervention. This script prepares two datasets:
#
#     1. Reintervention decision events:
#        openedApp and dismissedAppOpening events in the Planning condition.
#
#     2. Planned-session duration records:
#        openedApp events that can be matched to an immediate closedApp event
#        for the same app, allowing estimation of how much of the planned time
#        was used before the app was closed.
#
# IMPORTANT:
#   This script reads intervention_baseline_cleaned_final.csv rather than
#   intervention_app_for_regret.csv because the planned-time analysis requires
#   closedApp events. Some regret/dismissal files intentionally contain only
#   openedApp and dismissedAppOpening events, which is not sufficient here.
#
# INPUTS:
#   data/opens_df/intervention_baseline_cleaned_final.csv
#   data/participants.csv
#
# OUTPUTS:
#   data/regret_rate/reintervention_data_for_appendix.csv
#   data/regret_rate/reintervention_sessions_for_appendix.csv
#   tables/reintervention/reintervention_preprocessing_audit.csv
# =============================================================================

Sys.setenv(TZ = "Europe/Copenhagen")

library(tidyverse)
library(lubridate)

# ---- Directories ------------------------------------------------------------
dir.create("data/regret_rate", showWarnings = FALSE, recursive = TRUE)
dir.create("tables/reintervention", showWarnings = FALSE, recursive = TRUE)

# ---- Helpers ----------------------------------------------------------------
parse_event_time <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(with_tz(x, tzone = "Europe/Copenhagen"))
  }
  if (is.numeric(x)) {
    return(with_tz(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"),
                   tzone = "Europe/Copenhagen"))
  }
  parsed <- suppressWarnings(ymd_hms(x, tz = "UTC", quiet = TRUE))
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  }
  with_tz(parsed, tzone = "Europe/Copenhagen")
}

make_app_label <- function(app) {
  case_when(
    app == "facebook" ~ "Facebook",
    app == "tikTok" ~ "TikTok",
    app == "snapchat" ~ "Snapchat",
    app == "instagram" ~ "Instagram",
    app == "youtube" ~ "YouTube",
    TRUE ~ "Other"
  )
}

# ---- Load data --------------------------------------------------------------
events <- read_csv(
  "data/opens_df/intervention_baseline_cleaned_final.csv",
  show_col_types = FALSE
)

participants <- read_csv(
  "data/participants.csv",
  show_col_types = FALSE
) %>%
  dplyr::select(
    -any_of(c(
      "condition",
      "cond",
      "condition.x",
      "condition.y",
      "condition_x",
      "condition_y"
    ))
  )

required_event_cols <- c(
  "ID",
  "app",
  "timestamp",
  "time",
  "period",
  "condition",
  "resolution",
  "interventionType",
  "isReIntervention",
  "timeIntervalUntilReIntervention"
)

missing_event_cols <- setdiff(required_event_cols, names(events))
if (length(missing_event_cols) > 0) {
  stop(
    "Missing required event columns: ",
    paste(missing_event_cols, collapse = ", "),
    call. = FALSE
  )
}

# ---- Prepare all Planning intervention events -------------------------------
# Keep closedApp here because it is needed to estimate planned-session length.
planning_events <- events %>%
  mutate(
    timestamp = parse_event_time(timestamp),
    time = parse_event_time(time),
    time_at_activation = parse_event_time(time_at_activation),
    time_at_intervention = parse_event_time(time_at_intervention),
    date = as.Date(time, tz = "Europe/Copenhagen"),
    date_at_activation = as.Date(time_at_activation, tz = "Europe/Copenhagen"),
    date_at_intervention = as.Date(time_at_intervention, tz = "Europe/Copenhagen"),
    days_before_activation = round(as.numeric(
      difftime(date, date_at_intervention, units = "days")
    )),
    days_before_activation_num = days_before_activation,
    day_of_week = weekdays(date),
    weekend = as.integer(day_of_week %in% c("Saturday", "Sunday")),
    fall_break = as.integer(
      date >= as.Date("2024-10-14") &
        date <= as.Date("2024-10-18")
    ),
    christmas_break = as.integer(
      date >= as.Date("2024-12-23") &
        date <= as.Date("2025-01-02")
    ),
    interventionType = na_if(interventionType, ""),
    isReIntervention = case_when(
      isReIntervention %in% c(TRUE, "TRUE", "true", "1", 1) ~ 1L,
      isReIntervention %in% c(FALSE, "FALSE", "false", "0", 0) ~ 0L,
      TRUE ~ NA_integer_
    ),
    timeIntervalUntilReIntervention = as.numeric(timeIntervalUntilReIntervention),
    regret = as.integer(resolution == "dismissedAppOpening"),
    app2 = make_app_label(app),
    app2 = factor(
      app2,
      levels = c("TikTok", "Snapchat", "Instagram", "Facebook", "YouTube", "Other")
    )
  ) %>%
  filter(
    period == "intervention",
    condition == "intervention"
  ) %>%
  arrange(ID, timestamp)

# ---- Identify opened sessions with an immediate matched close ----------------
# This follows the original logic: an openedApp event counts as a planned-session
# record only if the next event for the participant is a closedApp for the same app.
planning_events <- planning_events %>%
  group_by(ID) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  mutate(
    resolution_next = lead(resolution),
    app_next = lead(app),
    timestamp_next = lead(timestamp),
    time_diff_seconds = as.numeric(difftime(timestamp_next, timestamp, units = "secs")),
    count_time = resolution == "openedApp" &
      resolution_next == "closedApp" &
      app_next == app &
      !is.na(interventionType) &
      !is.na(timeIntervalUntilReIntervention) &
      timeIntervalUntilReIntervention > 0,
    time_diff_seconds_round = round(time_diff_seconds),
    difference_time = time_diff_seconds_round - timeIntervalUntilReIntervention
  ) %>%
  ungroup()

# ---- Decision-event dataset -------------------------------------------------
# These are the events used for dismissal/reintervention summaries.
reintervention_decisions <- planning_events %>%
  filter(
    resolution %in% c("openedApp", "dismissedAppOpening"),
    !is.na(interventionType)
  ) %>%
  left_join(participants, by = "ID") %>%
  mutate(
    day_of_week = factor(
      day_of_week,
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ),
    gender = factor(gender, levels = c("mand", "kvinde")),
    region = as.factor(region),
    weekend = factor(weekend),
    fall_break = factor(fall_break),
    christmas_break = factor(christmas_break),
    interventionType = as.factor(interventionType),
    isReIntervention = factor(isReIntervention, levels = c(0, 1)),
    app = as.factor(app),
    condition = factor(condition, levels = c("control", "intervention", "default")),
    klassetrin_short2 = case_when(
      klassetrin %in% c("7. klasse", "8. klasse", "9. klasse", "10. klasse") ~ "Primary Education",
      klassetrin %in% c("1. år på ungdomsuddannelse", "2. år på ungdomsuddannelse") ~ "Secondary Education",
      klassetrin == "Efterskole" ~ "Boarding School",
      TRUE ~ "Others"
    ),
    klassetrin_short2 = factor(
      klassetrin_short2,
      levels = c("Primary Education", "Secondary Education", "Boarding School", "Others")
    )
  )

# ---- Planned-session dataset ------------------------------------------------
# The tolerance rule treats a session as using the full planned interval if the
# close occurs within 10 seconds before the scheduled reintervention time.
SESSION_TOLERANCE_SECONDS <- 10

reintervention_sessions <- planning_events %>%
  filter(
    count_time,
    !is.na(timeIntervalUntilReIntervention),
    timeIntervalUntilReIntervention > 0,
    !is.na(time_diff_seconds),
    time_diff_seconds >= 0
  ) %>%
  mutate(
    time_diff_reint = time_diff_seconds - timeIntervalUntilReIntervention,
    valid_time_match = time_diff_reint <= SESSION_TOLERANCE_SECONDS,
    percentage_used_raw = time_diff_seconds / timeIntervalUntilReIntervention,
    percentage_used = if_else(
      time_diff_seconds >= timeIntervalUntilReIntervention - SESSION_TOLERANCE_SECONDS,
      1,
      percentage_used_raw
    ),
    percentage_used = pmin(percentage_used, 1),
    percentage_used = pmax(percentage_used, 0),
    planned_minutes = timeIntervalUntilReIntervention / 60,
    actual_minutes = time_diff_seconds / 60
  ) %>%
  filter(valid_time_match) %>%
  left_join(participants, by = "ID") %>%
  mutate(
    isReIntervention = factor(isReIntervention, levels = c(0, 1)),
    app2 = factor(
      as.character(app2),
      levels = c("TikTok", "Snapchat", "Instagram", "Facebook", "YouTube", "Other")
    ),
    klassetrin_short2 = case_when(
      klassetrin %in% c("7. klasse", "8. klasse", "9. klasse", "10. klasse") ~ "Primary Education",
      klassetrin %in% c("1. år på ungdomsuddannelse", "2. år på ungdomsuddannelse") ~ "Secondary Education",
      klassetrin == "Efterskole" ~ "Boarding School",
      TRUE ~ "Others"
    ),
    klassetrin_short2 = factor(
      klassetrin_short2,
      levels = c("Primary Education", "Secondary Education", "Boarding School", "Others")
    )
  )

# ---- Audit ------------------------------------------------------------------
preprocessing_audit <- tibble(
  metric = c(
    "Planning intervention events",
    "Decision events",
    "Participants with decision events",
    "OpenedApp decision events",
    "DismissedAppOpening decision events",
    "Planned sessions with matched close",
    "Participants with matched planned sessions"
  ),
  value = c(
    nrow(planning_events),
    nrow(reintervention_decisions),
    n_distinct(reintervention_decisions$ID),
    sum(reintervention_decisions$resolution == "openedApp", na.rm = TRUE),
    sum(reintervention_decisions$resolution == "dismissedAppOpening", na.rm = TRUE),
    nrow(reintervention_sessions),
    n_distinct(reintervention_sessions$ID)
  )
)

print(preprocessing_audit)

# ---- Save -------------------------------------------------------------------
write_csv(
  reintervention_decisions,
  "data/regret_rate/reintervention_data_for_appendix.csv"
)

write_csv(
  reintervention_sessions,
  "data/regret_rate/reintervention_sessions_for_appendix.csv"
)

write_csv(
  preprocessing_audit,
  "tables/reintervention/reintervention_preprocessing_audit.csv"
)

message("Reintervention preprocessing complete.")
