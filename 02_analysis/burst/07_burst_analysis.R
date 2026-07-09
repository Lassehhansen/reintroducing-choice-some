Sys.setenv(TZ = "Europe/Copenhagen")

# =============================================================================
# 02_analysis/burst/07_burst_analysis.R
#
# PURPOSE:
#   Fit three burst-structure models:
#     1. Daily burst rate        (truncated NB2 glmmTMB)
#     2. Daily multi-app bursts  (NB2 glmmTMB)
#     3. Mean burst length       (truncated NB2 glmmTMB)
#
#   Also builds hourly heatmap data for figure2_burst_figure.R.
#
# MODEL UPDATE:
#   Burst models now include the same period-specific nonlinear time control
#   used in the main ITS models:
#
#     ns(time3, df = 2):period2 + period2 * condition
#
# EFFECT EXTRACTION:
#   Condition-specific baseline-to-intervention contrasts are extracted using
#   the same delta-method contrast logic as the main/app/moderation scripts:
#
#     estimate = L %*% beta
#     se       = sqrt(L %*% V %*% t(L))
#
# INPUTS:
#   data/opens_df/intervention_baseline_cleaned_final.csv
#   data/participants.csv
#
# OUTPUTS:
#   models/burst/m_burst_rate.rds
#   models/burst/m_burst_multiapp.rds
#   models/burst/m_burst_length.rds
#   tables/burst/burst_model_checks.csv
#   tables/burst/burst_effects.{xlsx,tex,csv}
#   tables/burst/burst_effects_validation_flags.csv
#   data/burst/data_heatmap_weekday.csv
#   data/burst/data_heatmap_weekend.csv
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs(
  "models/burst",
  "tables/burst",
  "data/burst"
)

if (!exists("COND_LEVELS_RAW")) {
  COND_LEVELS_RAW <- c("control", "intervention", "default")
}

cond_levels <- c("Reflection", "Planning", "Waiting")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

message("Loading event data...")

events <- read_csv(
  "data/opens_df/intervention_baseline_cleaned_final.csv",
  show_col_types = FALSE
)

if (!inherits(events$time, "POSIXct")) {
  events <- events %>%
    mutate(
      time = ymd_hms(time, tz = "UTC", quiet = TRUE)
    )
}

if (!inherits(events$time_at_activation, "POSIXct")) {
  events <- events %>%
    mutate(
      time_at_activation = ymd_hms(time_at_activation, tz = "UTC", quiet = TRUE)
    )
}

if (!inherits(events$time_at_intervention, "POSIXct")) {
  events <- events %>%
    mutate(
      time_at_intervention = ymd_hms(time_at_intervention, tz = "UTC", quiet = TRUE)
    )
}

events <- events %>%
  mutate(
    time = with_tz(time, tzone = "Europe/Copenhagen"),
    time_at_activation = with_tz(time_at_activation, tzone = "Europe/Copenhagen"),
    time_at_intervention = with_tz(time_at_intervention, tzone = "Europe/Copenhagen")
  )

if (!"timestamp" %in% names(events)) {
  events <- events %>%
    mutate(
      timestamp = time
    )
}

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

# =============================================================================
# 2. BUILD BURST-LEVEL DATA
# =============================================================================

BURST_GAP_MINS <- 5L

burst_events <- events %>%
  filter(
    resolution %in% c("openedApp", "dismissedAppOpening"),
    period %in% c("baseline", "intervention")
  ) %>%
  arrange(ID, time) %>%
  group_by(ID) %>%
  mutate(
    gap_to_prev = as.numeric(difftime(time, lag(time), units = "mins")),
    new_burst = is.na(gap_to_prev) | gap_to_prev > BURST_GAP_MINS,
    burst_id = cumsum(new_burst)
  ) %>%
  ungroup() %>%
  mutate(
    burst_uid = paste(ID, burst_id, sep = "_"),
    calendar_date = as.Date(time, tz = "Europe/Copenhagen"),
    day_type = if_else(
      weekdays(calendar_date) %in% c("Saturday", "Sunday"),
      "Weekend",
      "Weekday"
    ),
    date_at_intervention = as.Date(
      time_at_intervention,
      tz = "Europe/Copenhagen"
    ),
    days_before_activation_num = round(as.numeric(
      difftime(calendar_date, date_at_intervention, units = "days")
    )),
    period2 = if_else(period == "baseline", "baseline", "intervention"),
    fall_break = as.integer(
      calendar_date >= as.Date("2024-10-14") &
        calendar_date <= as.Date("2024-10-18")
    ),
    is_fall_break = fall_break == 1L,
    is_christmas = calendar_date >= as.Date("2024-12-23") &
      calendar_date <= as.Date("2025-01-02"),
    is_break = is_fall_break | is_christmas,
    weekend = as.integer(day_type == "Weekend"),
    time2 = if_else(
      days_before_activation_num < 0,
      as.numeric(days_before_activation_num),
      if_else(
        days_before_activation_num > 0,
        as.numeric(days_before_activation_num - 3),
        NA_real_
      )
    ),
    time3 = time2 + 11,
    condition_label = factor(
      recode(
        condition,
        control = "Reflection",
        intervention = "Planning",
        default = "Waiting"
      ),
      levels = cond_levels
    )
  ) %>%
  filter(
    !days_before_activation_num %in% c(-14, 0, 29),
    period2 %in% c("baseline", "intervention")
  ) %>%
  left_join(
    participants,
    by = "ID"
  ) %>%
  add_klassetrin_short2()

# =============================================================================
# 3. BURST SUMMARY DATA
# =============================================================================

burst_model_data <- burst_events %>%
  group_by(
    ID,
    burst_uid,
    burst_id,
    calendar_date,
    days_before_activation_num,
    time2,
    time3,
    period2,
    condition,
    condition_label,
    day_type,
    fall_break,
    weekend,
    gender,
    region,
    klassetrin_short2,
    is_break
  ) %>%
  summarise(
    burst_length = n(),
    n_apps_in_burst = n_distinct(app),
    multiapp_burst = as.integer(n_apps_in_burst > 1),
    .groups = "drop"
  ) %>%
  mutate(
    period2 = factor(period2, levels = c("baseline", "intervention")),
    condition = factor(condition, levels = COND_LEVELS_RAW),
    fall_break = factor(fall_break),
    weekend = factor(weekend),
    gender = factor(gender, levels = c("mand", "kvinde")),
    region = as.factor(region),
    klassetrin_short2 = factor(
      klassetrin_short2,
      levels = c(
        "Primary Education",
        "Secondary Education",
        "Boarding School",
        "Others"
      )
    ),
    ID = as.factor(ID),
    day_id = paste(ID, calendar_date)
  ) %>%
  drop_na(
    burst_length,
    n_apps_in_burst,
    multiapp_burst,
    period2,
    condition,
    time3,
    gender,
    fall_break,
    region,
    klassetrin_short2,
    weekend,
    ID,
    day_id
  ) %>%
  droplevels()

mean_len_per_user_day <- burst_model_data %>%
  group_by(ID, calendar_date) %>%
  summarise(
    mean_burst_len = mean(burst_length, na.rm = TRUE),
    .groups = "drop"
  )

multiapp_per_user_day <- burst_model_data %>%
  group_by(ID, calendar_date) %>%
  summarise(
    n_multiapp = sum(multiapp_burst, na.rm = TRUE),
    .groups = "drop"
  )

user_day_model <- burst_model_data %>%
  group_by(
    ID,
    calendar_date,
    days_before_activation_num,
    time2,
    time3,
    period2,
    condition,
    condition_label,
    day_type,
    fall_break,
    weekend,
    gender,
    region,
    klassetrin_short2
  ) %>%
  summarise(
    burst_rate = n_distinct(burst_uid),
    .groups = "drop"
  ) %>%
  left_join(
    mean_len_per_user_day,
    by = c("ID", "calendar_date")
  ) %>%
  left_join(
    multiapp_per_user_day,
    by = c("ID", "calendar_date")
  ) %>%
  mutate(
    n_multiapp = replace_na(n_multiapp, 0L),
    period2 = factor(period2, levels = c("baseline", "intervention")),
    condition = factor(condition, levels = COND_LEVELS_RAW),
    fall_break = factor(fall_break),
    weekend = factor(weekend),
    gender = factor(gender, levels = c("mand", "kvinde")),
    region = as.factor(region),
    klassetrin_short2 = factor(
      klassetrin_short2,
      levels = c(
        "Primary Education",
        "Secondary Education",
        "Boarding School",
        "Others"
      )
    ),
    ID = as.factor(ID),
    day_id = paste(ID, calendar_date)
  ) %>%
  drop_na(
    burst_rate,
    n_multiapp,
    mean_burst_len,
    period2,
    condition,
    time3,
    gender,
    fall_break,
    region,
    klassetrin_short2,
    weekend,
    ID
  ) %>%
  droplevels()

#### sanity check


burst_sanity_check <- burst_model_data %>%
  summarise(
    n_bursts = n(),
    n_single_app_bursts = sum(n_apps_in_burst == 1, na.rm = TRUE),
    n_multi_app_bursts = sum(n_apps_in_burst > 1, na.rm = TRUE),
    pct_multi_app_bursts = 100 * mean(n_apps_in_burst > 1, na.rm = TRUE),
    median_burst_length = median(burst_length, na.rm = TRUE),
    mean_burst_length = mean(burst_length, na.rm = TRUE),
    max_burst_length = max(burst_length, na.rm = TRUE)
  )

print(burst_sanity_check)

write_csv(
  burst_sanity_check,
  "tables/burst/burst_sanity_check.csv"
)

user_day_sanity_check <- user_day_model %>%
  summarise(
    n_user_days = n(),
    n_ids = n_distinct(ID),
    mean_burst_rate = mean(burst_rate, na.rm = TRUE),
    mean_multiapp_bursts = mean(n_multiapp, na.rm = TRUE),
    mean_burst_len = mean(mean_burst_len, na.rm = TRUE),
    pct_user_days_with_multiapp = 100 * mean(n_multiapp > 0, na.rm = TRUE)
  )

print(user_day_sanity_check)

write_csv(
  user_day_sanity_check,
  "tables/burst/user_day_sanity_check.csv"
)

#####


message(
  "Burst data: ",
  nrow(user_day_model),
  " user-days, ",
  n_distinct(user_day_model$ID),
  " participants"
)

print(table(user_day_model$condition, user_day_model$period2))
print(summary(user_day_model$burst_rate))
print(summary(user_day_model$n_multiapp))
print(summary(user_day_model$time3))

# =============================================================================
# 4. FIT BURST MODELS
# =============================================================================

ctrl <- glmmTMBControl(
  optCtrl = list(
    iter.max = 1e5,
    eval.max = 1e5
  )
)

message("Fitting burst-rate model with time control...")

m_rate <- glmmTMB(
  burst_rate ~
    ns(time3, df = 2) +
    period2 * condition +
    gender + fall_break +
    region + klassetrin_short2 +
    weekend +
    (1 | ID),
  family = truncated_nbinom2(link = "log"),
  data = user_day_model,
  control = ctrl
)

saveRDS(
  m_rate,
  "models/burst/m_burst_rate.rds"
)

message("  AIC: ", round(AIC(m_rate), 1))

message("Fitting multi-app burst model with time control...")

m_multi <- glmmTMB(
  n_multiapp ~
    ns(time3, df = 2) +
    period2 * condition +
    gender + fall_break +
    region + klassetrin_short2 +
    weekend +
    (1 | ID),
  family = nbinom2(link = "log"),
  data = user_day_model,
  control = ctrl
)

saveRDS(
  m_multi,
  "models/burst/m_burst_multiapp.rds"
)

message("  AIC: ", round(AIC(m_multi), 1))

message("Fitting burst-length model with time control...")

m_len <- glmmTMB(
  burst_length ~
    ns(time3, df = 2) +
    period2 * condition +
    gender + fall_break +
    region + klassetrin_short2 +
    weekend +
    (1 | ID) +
    (1 | day_id),
  family = truncated_nbinom2(link = "log"),
  data = burst_model_data,
  control = ctrl
)

saveRDS(
  m_len,
  "models/burst/m_burst_length.rds"
)

message("  AIC: ", round(AIC(m_len), 1))

# =============================================================================
# 5. MODEL CHECKS
# =============================================================================

check_glmmtmb_model <- function(.model, model_label) {
  tibble(
    model = model_label,
    AIC = AIC(.model),
    BIC = BIC(.model),
    logLik = as.numeric(logLik(.model)),
    nobs = nobs(.model),
    convergence = .model$fit$convergence,
    pdHess = .model$sdr$pdHess,
    message = paste(.model$fit$message, collapse = " | ")
  )
}

burst_model_checks <- bind_rows(
  check_glmmtmb_model(m_rate, "burst_rate"),
  check_glmmtmb_model(m_multi, "multiapp_burst_count"),
  check_glmmtmb_model(m_len, "mean_burst_length")
)

print(burst_model_checks)

write_csv(
  burst_model_checks,
  "tables/burst/burst_model_checks.csv"
)

# =============================================================================
# 6. EXTRACT EFFECTS WITH DELTA METHOD
# =============================================================================

extract_burst_change_delta_method <- function(model, metric_label, model_label) {
  
  b <- fixef(model)$cond
  V <- as.matrix(vcov(model)$cond)
  nms <- names(b)
  
  if (!"period2intervention" %in% nms) {
    stop(
      "`period2intervention` not found in burst model.",
      call. = FALSE
    )
  }
  
  L <- matrix(
    0,
    nrow = 3,
    ncol = length(b),
    dimnames = list(
      c("Reflection", "Planning", "Waiting"),
      nms
    )
  )
  
  L["Reflection", "period2intervention"] <- 1
  L["Planning", "period2intervention"] <- 1
  L["Waiting", "period2intervention"] <- 1
  
  int_plan <- intersect(
    c(
      "period2intervention:conditionintervention",
      "conditionintervention:period2intervention"
    ),
    nms
  )[1]
  
  int_wait <- intersect(
    c(
      "period2intervention:conditiondefault",
      "conditiondefault:period2intervention"
    ),
    nms
  )[1]
  
  if (!is.na(int_plan)) {
    L["Planning", int_plan] <- 1
  }
  
  if (!is.na(int_wait)) {
    L["Waiting", int_wait] <- 1
  }
  
  estimate_log <- as.numeric(L %*% b)
  se_log <- sqrt(diag(L %*% V %*% t(L)))
  z <- estimate_log / se_log
  p_value <- 2 * pnorm(-abs(z))
  
  tibble(
    model = model_label,
    metric = metric_label,
    condition = factor(
      rownames(L),
      levels = c("Reflection", "Planning", "Waiting")
    ),
    estimate_log = estimate_log,
    se_log = se_log,
    conf.low_log = estimate_log - 1.96 * se_log,
    conf.high_log = estimate_log + 1.96 * se_log,
    rr = exp(estimate_log),
    rr_low = exp(conf.low_log),
    rr_high = exp(conf.high_log),
    ratio = rr,
    ratio_lo = rr_low,
    ratio_hi = rr_high,
    pct = 100 * (rr - 1),
    pct_low = 100 * (rr_low - 1),
    pct_high = 100 * (rr_high - 1),
    pct_change = pct,
    pct_lo = pct_low,
    pct_hi = pct_high,
    z = z,
    p = p_value,
    p_value = p_value,
    significant = rr_low > 1 | rr_high < 1
  )
}

burst_effects <- bind_rows(
  extract_burst_change_delta_method(
    model = m_rate,
    metric_label = "Daily burst rate",
    model_label = "burst_rate"
  ),
  extract_burst_change_delta_method(
    model = m_multi,
    metric_label = "Daily multi-app bursts",
    model_label = "multiapp_burst_count"
  ),
  extract_burst_change_delta_method(
    model = m_len,
    metric_label = "Mean burst length",
    model_label = "mean_burst_length"
  )
)

burst_effects_validation <- burst_effects %>%
  mutate(
    ci_excludes_1 = rr_low > 1 | rr_high < 1,
    p_sig = p_value < 0.05,
    mismatch = ci_excludes_1 != p_sig,
    bad_bounds = rr_low > rr | rr > rr_high,
    bad_log_bounds = conf.low_log > estimate_log | estimate_log > conf.high_log
  ) %>%
  filter(
    mismatch | bad_bounds | bad_log_bounds
  )

print(burst_effects, n = Inf)
print(burst_effects_validation, n = Inf)

save_model_table(
  burst_effects,
  "burst_effects",
  "tables/burst",
  caption = "Condition-specific baseline-to-intervention changes in burst outcomes",
  label = "tab:burst_effects"
)

write_csv(
  burst_effects,
  "tables/burst/burst_effects.csv"
)

write_csv(
  burst_effects_validation,
  "tables/burst/burst_effects_validation_flags.csv"
)

# =============================================================================
# 7. BUILD HOURLY HEATMAP DATA
# =============================================================================

# =============================================================================
# 7. BUILD HOURLY HEATMAP DATA
# =============================================================================

MIN_USERS_HEATMAP <- 20L
ACTIVE_HOURS <- 6:26
TZ_HEATMAP <- "Europe/Copenhagen"

# -----------------------------------------------------------------------------
# 7.0 Helper functions
# -----------------------------------------------------------------------------

condition_to_label <- function(x) {
  factor(
    dplyr::recode(
      x,
      control = "Reflection",
      intervention = "Planning",
      default = "Waiting"
    ),
    levels = cond_levels
  )
}

make_heatmap_day_vars <- function(df) {
  df %>%
    mutate(
      time = with_tz(time, tzone = TZ_HEATMAP),
      time_at_intervention = with_tz(
        time_at_intervention,
        tzone = TZ_HEATMAP
      ),
      calendar_date = as.Date(floor_date(time, unit = "day")),
      date_at_intervention = as.Date(floor_date(time_at_intervention, unit = "day")),
      raw_hour = hour(time),
      behavioral_date = if_else(
        raw_hour < 3L,
        calendar_date - days(1),
        calendar_date
      ),
      display_hour = if_else(
        raw_hour < 3L,
        raw_hour + 24L,
        raw_hour
      ),
      days_before_activation_num = round(as.numeric(
        difftime(
          behavioral_date,
          date_at_intervention,
          units = "days"
        )
      )),
      dow = wday(behavioral_date, week_start = 1),
      
      # Monday to Thursday = weekday.
      # Friday to Sunday = leisure day, matching the manuscript caption.
      day_type = factor(
        if_else(dow >= 5L, "Weekend/Friday", "Weekday"),
        levels = c("Weekday", "Weekend/Friday")
      ),
      
      is_fall_break = behavioral_date >= as.Date("2024-10-14") &
        behavioral_date <= as.Date("2024-10-18"),
      is_christmas = behavioral_date >= as.Date("2024-12-23") &
        behavioral_date <= as.Date("2025-01-02"),
      is_break = is_fall_break | is_christmas,
      
      period2 = case_when(
        period == "baseline" ~ "baseline",
        period == "intervention" ~ "intervention",
        TRUE ~ NA_character_
      ),
      
      condition_label = condition_to_label(condition)
    )
}

clean_heatmap_days <- function(df) {
  df %>%
    filter(
      period2 %in% c("baseline", "intervention"),
      !is.na(ID),
      !is.na(condition_label),
      !is.na(behavioral_date),
      !is.na(day_type),
      !days_before_activation_num %in% c(-14, 0, 29),
      !is_break
    ) %>%
    mutate(
      period2 = factor(
        period2,
        levels = c("baseline", "intervention")
      ),
      condition_label = factor(
        condition_label,
        levels = cond_levels
      ),
      day_type = factor(
        day_type,
        levels = c("Weekday", "Weekend/Friday")
      )
    )
}

# -----------------------------------------------------------------------------
# 7.1 Prepare all eligible events and opened-app events
# -----------------------------------------------------------------------------

heatmap_events_clean <- events %>%
  filter(
    period %in% c("baseline", "intervention"),
    !is.na(ID),
    !is.na(time),
    !is.na(time_at_intervention)
  ) %>%
  make_heatmap_day_vars() %>%
  clean_heatmap_days()

heatmap_opens_clean <- heatmap_events_clean %>%
  filter(
    resolution == "openedApp",
    display_hour %in% ACTIVE_HOURS
  )

stopifnot(
  all(heatmap_opens_clean$resolution == "openedApp"),
  all(!heatmap_opens_clean$is_break),
  all(heatmap_opens_clean$display_hour %in% ACTIVE_HOURS),
  all(heatmap_opens_clean$day_type %in% c("Weekday", "Weekend/Friday"))
)

message(
  "Observed heatmap opened-app events: ",
  nrow(heatmap_opens_clean),
  " rows, ",
  n_distinct(heatmap_opens_clean$ID),
  " participants"
)

# -----------------------------------------------------------------------------
# 7.2 Build the eligible participant-day grid
# -----------------------------------------------------------------------------
# Goal: create true zero-opening participant-day-hour cells.
# Preferred source: user_day_model, if it has day-level columns.
# Fallback source: observed eligible participant-days from events.

message("Columns in user_day_model: ", paste(names(user_day_model), collapse = ", "))

can_use_user_day_model <- all(
  c("ID", "condition", "period", "behavioral_date") %in% names(user_day_model)
)

if (can_use_user_day_model) {
  
  message("Using user_day_model to define eligible heatmap participant-days.")
  
  eligible_user_days <- user_day_model %>%
    mutate(
      behavioral_date = as.Date(behavioral_date),
      period2 = case_when(
        period == "baseline" ~ "baseline",
        period == "intervention" ~ "intervention",
        TRUE ~ NA_character_
      ),
      dow = wday(behavioral_date, week_start = 1),
      day_type = factor(
        if_else(dow >= 5L, "Weekend/Friday", "Weekday"),
        levels = c("Weekday", "Weekend/Friday")
      ),
      is_fall_break = behavioral_date >= as.Date("2024-10-14") &
        behavioral_date <= as.Date("2024-10-18"),
      is_christmas = behavioral_date >= as.Date("2024-12-23") &
        behavioral_date <= as.Date("2025-01-02"),
      is_break_heatmap = is_fall_break | is_christmas,
      condition_label = condition_to_label(condition)
    ) %>%
    filter(
      period2 %in% c("baseline", "intervention"),
      !is.na(ID),
      !is.na(condition_label),
      !is.na(behavioral_date),
      !is.na(day_type),
      !is_break_heatmap
    )
  
  if ("days_before_activation_num" %in% names(eligible_user_days)) {
    eligible_user_days <- eligible_user_days %>%
      filter(!days_before_activation_num %in% c(-14, 0, 29))
  }
  
  if ("include_day" %in% names(eligible_user_days)) {
    eligible_user_days <- eligible_user_days %>%
      filter(include_day)
  }
  
  eligible_user_days <- eligible_user_days %>%
    distinct(
      ID,
      condition_label,
      period2,
      behavioral_date,
      day_type
    )
  
} else {
  
  warning(
    "user_day_model does not contain ID, condition, period, and behavioral_date. ",
    "Falling back to eligible participant-days derived from events. ",
    "Zeros therefore mean no openedApp events during active hours on days with observed event logging."
  )
  
  # Use all eligible event-observed participant-days as the observation grid.
  # This avoids the earlier hard stop and still prevents creating calendar days
  # for participants without any observed data.
  eligible_user_days <- heatmap_events_clean %>%
    distinct(
      ID,
      condition_label,
      period2,
      behavioral_date,
      day_type
    )
}

eligible_user_days <- eligible_user_days %>%
  mutate(
    period2 = factor(
      period2,
      levels = c("baseline", "intervention")
    ),
    condition_label = factor(
      condition_label,
      levels = cond_levels
    ),
    day_type = factor(
      day_type,
      levels = c("Weekday", "Weekend/Friday")
    )
  ) %>%
  filter(
    period2 %in% c("baseline", "intervention"),
    !is.na(ID),
    !is.na(condition_label),
    !is.na(behavioral_date),
    !is.na(day_type)
  ) %>%
  distinct(
    ID,
    condition_label,
    period2,
    behavioral_date,
    day_type
  )

message(
  "Eligible heatmap participant-days: ",
  nrow(eligible_user_days),
  " rows, ",
  n_distinct(eligible_user_days$ID),
  " participants"
)

heatmap_day_audit <- eligible_user_days %>%
  count(
    condition_label,
    period2,
    day_type,
    name = "n_participant_days"
  ) %>%
  arrange(
    condition_label,
    period2,
    day_type
  )

print(heatmap_day_audit, n = Inf)

write_csv(
  heatmap_day_audit,
  "tables/burst/heatmap_day_audit.csv"
)

# Full participant-day-hour grid, including zero-opening hours.
heatmap_full_grid <- eligible_user_days %>%
  tidyr::expand_grid(
    display_hour = ACTIVE_HOURS
  )

stopifnot(
  all(heatmap_full_grid$display_hour %in% ACTIVE_HOURS),
  all(heatmap_full_grid$day_type %in% c("Weekday", "Weekend/Friday")),
  all(heatmap_full_grid$period2 %in% c("baseline", "intervention"))
)

# -----------------------------------------------------------------------------
# 7.3 Count observed openings and join onto the full grid
# -----------------------------------------------------------------------------

observed_hourly_counts <- heatmap_opens_clean %>%
  group_by(
    ID,
    condition_label,
    period2,
    behavioral_date,
    display_hour,
    day_type
  ) %>%
  summarise(
    n_opens = n(),
    .groups = "drop"
  )

# Missing joined values are interpreted as zero completed app openings
# within eligible participant-day-hour cells.
hourly_counts <- heatmap_full_grid %>%
  left_join(
    observed_hourly_counts,
    by = c(
      "ID",
      "condition_label",
      "period2",
      "behavioral_date",
      "display_hour",
      "day_type"
    )
  ) %>%
  mutate(
    n_opens = tidyr::replace_na(n_opens, 0L),
    period2 = factor(
      period2,
      levels = c("baseline", "intervention")
    ),
    condition_label = factor(
      condition_label,
      levels = cond_levels
    ),
    day_type = factor(
      day_type,
      levels = c("Weekday", "Weekend/Friday")
    )
  )

stopifnot(
  all(hourly_counts$display_hour %in% ACTIVE_HOURS),
  all(!is.na(hourly_counts$n_opens)),
  all(hourly_counts$day_type %in% c("Weekday", "Weekend/Friday"))
)

message(
  "Zero-filled heatmap hourly cells: ",
  nrow(hourly_counts),
  " rows, ",
  n_distinct(hourly_counts$ID),
  " participants"
)

# -----------------------------------------------------------------------------
# 7.4 Compute within-person hourly changes
# -----------------------------------------------------------------------------

user_hour_means <- hourly_counts %>%
  group_by(
    ID,
    condition_label,
    period2,
    display_hour,
    day_type
  ) %>%
  summarise(
    mean_opens = mean(n_opens, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )

user_hour_wide <- user_hour_means %>%
  pivot_wider(
    names_from = period2,
    values_from = c(mean_opens, n_days)
  ) %>%
  filter(
    !is.na(mean_opens_baseline),
    !is.na(mean_opens_intervention)
  ) %>%
  mutate(
    delta = mean_opens_intervention - mean_opens_baseline,
    pct_change_raw = case_when(
      mean_opens_baseline > 0 ~ delta / mean_opens_baseline,
      mean_opens_baseline == 0 & mean_opens_intervention == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

# -----------------------------------------------------------------------------
# 7.5 Summarise heatmap cells
# -----------------------------------------------------------------------------
# median_pct_change_unclipped is for reporting.
# median_pct_change_plot is clipped for the heatmap color scale only.

heatmap_data <- user_hour_wide %>%
  group_by(
    condition_label,
    display_hour,
    day_type
  ) %>%
  summarise(
    median_pct_change_raw = median(pct_change_raw, na.rm = TRUE),
    median_delta = median(delta, na.rm = TRUE),
    mean_baseline = mean(mean_opens_baseline, na.rm = TRUE),
    mean_intervention = mean(mean_opens_intervention, na.rm = TRUE),
    n_users = sum(!is.na(pct_change_raw)),
    n_unique = n_distinct(ID),
    .groups = "drop"
  ) %>%
  mutate(
    median_pct_change_raw = if_else(
      is.nan(median_pct_change_raw),
      NA_real_,
      median_pct_change_raw
    ),
    median_delta = if_else(
      is.nan(median_delta),
      NA_real_,
      median_delta
    ),
    
    report_cell = n_users >= MIN_USERS_HEATMAP & mean_baseline > 0.05,
    
    # Uncapped percentage change, used for both reporting and plotting.
    median_pct_change = if_else(
      report_cell,
      100 * median_pct_change_raw,
      NA_real_
    ),
    
    # Backward-compatible names, if the plotting script expects these.
    mean_pct_change = median_pct_change,
    median_pct_change_plot = median_pct_change,
    median_pct_change_unclipped = median_pct_change,
    
    hour = as.integer(if_else(display_hour >= 24L, display_hour - 24L, display_hour)),
    condition_label = factor(
      condition_label,
      levels = cond_levels
    ),
    day_type = factor(
      day_type,
      levels = c("Weekday", "Weekend/Friday")
    )
  ) %>%
  dplyr::select(
    condition_label,
    day_type,
    hour,
    display_hour,
    median_pct_change,
    mean_pct_change,
    median_pct_change_plot,
    median_pct_change_unclipped,
    median_delta,
    mean_baseline,
    mean_intervention,
    n_users,
    n_unique,
    report_cell
  ) %>%
  arrange(
    day_type,
    condition_label,
    display_hour
  )

data_heatmap_weekday <- heatmap_data %>%
  filter(day_type == "Weekday") %>%
  arrange(
    condition_label,
    display_hour
  )

data_heatmap_weekend <- heatmap_data %>%
  filter(day_type == "Weekend/Friday") %>%
  arrange(
    condition_label,
    display_hour
  )

write_csv(
  data_heatmap_weekday,
  "data/burst/data_heatmap_weekday.csv"
)

write_csv(
  data_heatmap_weekend,
  "data/burst/data_heatmap_weekend.csv"
)

message(
  "Heatmap weekday rows: ",
  nrow(data_heatmap_weekday)
)

message(
  "Heatmap weekend/Friday rows: ",
  nrow(data_heatmap_weekend)
)

# -----------------------------------------------------------------------------
# 7.6 Window-level summaries for manuscript text
# -----------------------------------------------------------------------------
# These are computed directly from zero-filled participant-day-hour data.
# Use these values for manuscript percentages, not clipped heatmap values.

hourly_counts_windowed <- hourly_counts %>%
  mutate(
    time_window = case_when(
      day_type == "Weekday" & display_hour >= 6  & display_hour < 9  ~ "Morning",
      day_type == "Weekday" & display_hour >= 9  & display_hour < 14 ~ "School hours",
      day_type == "Weekday" & display_hour >= 14 & display_hour < 18 ~ "After school",
      day_type == "Weekday" & display_hour >= 18 & display_hour < 23 ~ "Evening",
      day_type == "Weekday" & display_hour >= 23 & display_hour <= 26 ~ "Late night",
      
      day_type == "Weekend/Friday" & display_hour >= 6  & display_hour < 11 ~ "Morning",
      day_type == "Weekend/Friday" & display_hour >= 11 & display_hour < 17 ~ "Afternoon",
      day_type == "Weekend/Friday" & display_hour >= 17 & display_hour < 21 ~ "Evening",
      day_type == "Weekend/Friday" & display_hour >= 21 & display_hour <= 26 ~ "Night",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(time_window)) %>%
  mutate(
    time_window = factor(
      time_window,
      levels = c(
        "Morning",
        "School hours",
        "After school",
        "Evening",
        "Late night",
        "Afternoon",
        "Night"
      )
    )
  )

user_window_means <- hourly_counts_windowed %>%
  group_by(
    ID,
    condition_label,
    period2,
    day_type,
    time_window,
    behavioral_date
  ) %>%
  summarise(
    n_opens_window = sum(n_opens, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    ID,
    condition_label,
    period2,
    day_type,
    time_window
  ) %>%
  summarise(
    mean_opens_window = mean(n_opens_window, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )

window_wide <- user_window_means %>%
  pivot_wider(
    names_from = period2,
    values_from = c(mean_opens_window, n_days)
  ) %>%
  filter(
    !is.na(mean_opens_window_baseline),
    !is.na(mean_opens_window_intervention)
  ) %>%
  mutate(
    delta = mean_opens_window_intervention - mean_opens_window_baseline,
    pct_change_raw = case_when(
      mean_opens_window_baseline > 0 ~ delta / mean_opens_window_baseline,
      mean_opens_window_baseline == 0 & mean_opens_window_intervention == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

heatmap_window_summary <- window_wide %>%
  group_by(
    condition_label,
    day_type,
    time_window
  ) %>%
  summarise(
    median_pct_change = 100 * median(pct_change_raw, na.rm = TRUE),
    median_delta = median(delta, na.rm = TRUE),
    mean_baseline = mean(mean_opens_window_baseline, na.rm = TRUE),
    mean_intervention = mean(mean_opens_window_intervention, na.rm = TRUE),
    n_users = sum(!is.na(pct_change_raw)),
    .groups = "drop"
  ) %>%
  mutate(
    median_pct_change = if_else(
      is.nan(median_pct_change),
      NA_real_,
      median_pct_change
    ),
    median_delta = if_else(
      is.nan(median_delta),
      NA_real_,
      median_delta
    ),
    report_cell = n_users >= MIN_USERS_HEATMAP & mean_baseline > 0.05,
    median_pct_change = if_else(
      report_cell,
      median_pct_change,
      NA_real_
    )
  ) %>%
  arrange(
    day_type,
    condition_label,
    time_window
  )

write_csv(
  heatmap_window_summary,
  "tables/burst/heatmap_window_summary.csv"
)

print(heatmap_window_summary, n = Inf)

# -----------------------------------------------------------------------------
# 7.7 Audit summaries
# -----------------------------------------------------------------------------

heatmap_summary <- heatmap_data %>%
  group_by(
    condition_label,
    day_type
  ) %>%
  summarise(
    mean_pct_plot = round(mean(median_pct_change_plot, na.rm = TRUE), 1),
    median_pct_plot = round(median(median_pct_change_plot, na.rm = TRUE), 1),
    mean_pct_unclipped = round(mean(median_pct_change_unclipped, na.rm = TRUE), 1),
    median_pct_unclipped = round(median(median_pct_change_unclipped, na.rm = TRUE), 1),
    n_grey = sum(is.na(median_pct_change_plot)),
    mean_n_users = round(mean(n_users, na.rm = TRUE), 0),
    min_n_users = min(n_users, na.rm = TRUE),
    .groups = "drop"
  )

print(heatmap_summary, n = Inf)

write_csv(
  heatmap_summary,
  "tables/burst/heatmap_summary.csv"
)

print(data_heatmap_weekday, n = Inf)
print(data_heatmap_weekend, n = Inf)