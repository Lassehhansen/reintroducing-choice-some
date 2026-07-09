Sys.setenv(TZ = "Europe/Copenhagen")

# =============================================================================
# 02_analysis/burst/07_burst_analysis.R
#
# PURPOSE:
#   Fit burst-structure models using the old-model definition, but in the
#   cleaned project structure.
#
# OLD-MATCHING BURST DEFINITION:
#   - Uses openedApp only
#   - Excludes period == "mixed"
#   - Excludes days_before_activation_num %in% c(-14, 0, 29)
#   - Uses a 10-minute burst window
#   - Defines bursts across apps within ID x date x period x condition
#   - Aggregates to user-day for burst_rate, n_multiapp, mean_burst_len
#   - Joins weekend/fall_break from opens_per_day, as in the old script
#
# MODELS:
#   1. Daily burst rate:
#        truncated NB2, user-day outcome burst_rate
#   2. Daily multi-app bursts:
#        NB2, user-day outcome n_multiapp
#   3. Mean burst length:
#        Gamma, user-day outcome mean_burst_len
#
# INPUTS:
#   data/opens_df/intervention_baseline_cleaned_final.csv
#   data/baseline_intervention_outro/opens/opens_per_day.csv
#   data/demographic/demographic_data.csv
#   data/survey/klassetrin_clean.csv
#
# OUTPUTS:
#   models/burst/m_burst_rate.rds
#   models/burst/m_burst_multiapp.rds
#   models/burst/m_burst_length.rds
#   tables/burst/burst_rate_coefficients.{xlsx,tex}
#   tables/burst/burst_multiapp_coefficients.{xlsx,tex}
#   tables/burst/burst_length_coefficients.{xlsx,tex}
#   tables/burst/burst_condition_effects.{xlsx,tex,csv}
#   data/burst/user_day_model.csv
#   data/burst/burst_metrics.csv
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs(
  "models/burst",
  "tables/burst",
  "data/burst"
)

condition_labels <- c(
  "control"      = "Reflection",
  "intervention" = "Planning",
  "default"      = "Waiting"
)

cond_levels <- c("Reflection", "Planning", "Waiting")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

intervention_baseline_cleaned_final <- read_csv(
  "data/opens_df/intervention_baseline_cleaned_final.csv",
  show_col_types = FALSE
)

demografi_clean <- read_csv(
  "data/demographic/demographic_data.csv",
  show_col_types = FALSE
)

klassetrin_clean <- read_csv(
  "data/survey/klassetrin_clean.csv",
  show_col_types = FALSE
)

opens_per_day <- read_csv(
  "data/baseline_intervention_outro/opens/opens_per_day.csv",
  show_col_types = FALSE
)

# =============================================================================
# 2. PREPARE OPEN EVENTS
# =============================================================================

opens_df_ap2 <- intervention_baseline_cleaned_final %>%
  filter(
    resolution %in% c("openedApp", "dismissedAppOpening")
  ) %>%
  arrange(ID, timestamp) %>%
  group_by(ID) %>%
  mutate(
    open = ifelse(resolution == "openedApp", 1, 0)
  ) %>%
  ungroup() %>%
  dplyr::select(
    ID,
    app,
    timestamp,
    time,
    resolution,
    open,
    period,
    condition,
    participation_day,
    time_at_activation,
    time_at_intervention
  ) %>%
  mutate(
    time = with_tz(time, tzone = "Europe/Copenhagen"),
    time_at_activation = with_tz(time_at_activation, tzone = "Europe/Copenhagen"),
    time_at_intervention = with_tz(time_at_intervention, tzone = "Europe/Copenhagen"),
    date = floor_date(time, unit = "day"),
    date_at_intervention = floor_date(time_at_intervention, unit = "day"),
    days_before_activation_num = round(
      as.numeric(difftime(date, date_at_intervention, units = "days"))
    ),
    hour = hour(time),
    app_clean = case_when(
      str_detect(app, "snap") ~
        "Snapchat",
      str_detect(app, "tik|tiktok") ~
        "TikTok",
      str_detect(app, "inst") ~
        "Instagram",
      str_detect(app, "yout") ~
        "YouTube",
      str_detect(app, "face") ~
        "Facebook",
      str_detect(app, "pint") ~
        "Pinterest",
      str_detect(app, "twit|x\\.") ~
        "Twitter/X",
      str_detect(app, "reddit") ~
        "Reddit",
      TRUE ~
        "Other"
    ),
    condition_label = factor(
      dplyr::recode(condition, !!!condition_labels),
      levels = cond_levels
    ),
    period2 = case_when(
      period == "baseline" ~
        "baseline",
      period == "intervention" ~
        "intervention",
      period == "mixed" ~
        "intervention",
      TRUE ~
        NA_character_
    )
  ) %>%
  filter(
    resolution == "openedApp",
    period2 %in% c("baseline", "intervention"),
    period != "mixed",
    !days_before_activation_num %in% c(-14, 0, 29)
  )

message(
  "Opens: ",
  nrow(opens_df_ap2),
  " | Users: ",
  n_distinct(opens_df_ap2$ID)
)

# =============================================================================
# 3. BUILD BURSTS
# =============================================================================

BURST_WINDOW_MINS <- 10L

burst_data <- opens_df_ap2 %>%
  arrange(ID, date, time) %>%
  group_by(ID, date, period, condition) %>%
  mutate(
    gap_mins = as.numeric(
      difftime(time, lag(time), units = "mins")
    ),
    new_burst = is.na(gap_mins) | gap_mins > BURST_WINDOW_MINS,
    burst_id = cumsum(new_burst)
  ) %>%
  ungroup()

burst_metrics <- burst_data %>%
  group_by(
    ID,
    condition,
    condition_label,
    period,
    period2,
    date,
    days_before_activation_num,
    burst_id
  ) %>%
  summarise(
    burst_length = n(),
    n_unique_apps = n_distinct(app_clean),
    .groups = "drop"
  ) %>%
  mutate(
    is_multiapp = burst_length > 1L & n_unique_apps > 1L,
    day_id = paste(ID, date, sep = "_")
  )

user_day <- burst_metrics %>%
  group_by(
    ID,
    condition,
    condition_label,
    period2,
    date,
    days_before_activation_num
  ) %>%
  summarise(
    burst_rate = n(),
    n_multiapp = sum(is_multiapp),
    pct_multiapp = mean(is_multiapp),
    mean_burst_len = mean(burst_length),
    mean_apps = mean(n_unique_apps),
    .groups = "drop"
  )

user_day %>%
  group_by(condition_label, period2) %>%
  summarise(
    pct_zero_burst_rate = mean(burst_rate == 0) * 100,
    mean_burst_rate = mean(burst_rate),
    .groups = "drop"
  ) %>%
  print()

user_day %>%
  group_by(condition_label, period2) %>%
  summarise(
    pct_zero_multiapp = mean(n_multiapp == 0) * 100,
    mean_multiapp = mean(n_multiapp),
    .groups = "drop"
  ) %>%
  print()

# =============================================================================
# 4. MERGE COVARIATES
# =============================================================================

covariate_lookup <- opens_per_day %>%
  mutate(
    date = as.Date(date, tz = "Europe/Copenhagen"),
    day_of_week = weekdays(date),
    weekend = as.integer(day_of_week %in% c("Saturday", "Sunday")),
    fall_break = as.integer(
      date >= as.Date("2024-10-14") &
        date <= as.Date("2024-10-18")
    )
  ) %>%
  dplyr::select(
    ID,
    days_before_activation_num,
    weekend,
    fall_break
  ) %>%
  distinct()

add_burst_covariates <- function(df) {
  df %>%
    left_join(
      covariate_lookup,
      by = c("ID", "days_before_activation_num")
    ) %>%
    left_join(
      demografi_clean %>%
        dplyr::select(ID, gender, region),
      by = "ID"
    ) %>%
    left_join(
      klassetrin_clean %>%
        dplyr::select(ID, klassetrin),
      by = "ID"
    ) %>%
    mutate(
      klassetrin_short2 = case_when(
        klassetrin %in% c(
          "7. klasse",
          "8. klasse",
          "9. klasse",
          "10. klasse"
        ) ~
          "Primary Education",
        klassetrin %in% c(
          "1. år på ungdomsuddannelse",
          "2. år på ungdomsuddannelse"
        ) ~
          "Secondary Education",
        klassetrin == "Efterskole" ~
          "Boarding School",
        TRUE ~
          "Others"
      ),
      condition = factor(
        condition,
        levels = c("control", "intervention", "default")
      ),
      period2 = factor(
        period2,
        levels = c("baseline", "intervention")
      ),
      gender = factor(
        gender,
        levels = c("mand", "kvinde")
      ),
      region = as.factor(region),
      ID = as.factor(ID),
      weekend = as.factor(weekend),
      fall_break = as.factor(fall_break),
      klassetrin_short2 = factor(
        klassetrin_short2,
        levels = c(
          "Primary Education",
          "Secondary Education",
          "Boarding School",
          "Others"
        )
      )
    ) %>%
    filter(
      !is.na(gender),
      !is.na(region),
      !is.na(klassetrin_short2),
      !is.na(weekend),
      !is.na(fall_break)
    )
}

user_day_model <- add_burst_covariates(user_day)
burst_model_data <- add_burst_covariates(burst_metrics)

message("user_day_model: ", nrow(user_day_model))
message("burst_model_data: ", nrow(burst_model_data))

write_csv(
  user_day_model,
  "data/burst/user_day_model.csv"
)

write_csv(
  burst_model_data,
  "data/burst/burst_metrics.csv"
)

# =============================================================================
# 5. AUDIT
# =============================================================================

burst_audit <- user_day_model %>%
  summarise(
    n_rows = n(),
    n_ids = n_distinct(ID),
    min_day = min(days_before_activation_num, na.rm = TRUE),
    max_day = max(days_before_activation_num, na.rm = TRUE),
    mean_burst_rate = mean(burst_rate, na.rm = TRUE),
    mean_multiapp = mean(n_multiapp, na.rm = TRUE),
    mean_burst_len = mean(mean_burst_len, na.rm = TRUE),
    max_burst_rate = max(burst_rate, na.rm = TRUE),
    max_multiapp = max(n_multiapp, na.rm = TRUE),
    max_mean_burst_len = max(mean_burst_len, na.rm = TRUE)
  )

print(burst_audit)

user_day_model %>%
  count(condition, period2) %>%
  arrange(condition, period2) %>%
  print(n = Inf)

user_day_model %>%
  count(gender, useNA = "ifany") %>%
  print(n = Inf)

user_day_model %>%
  count(weekend, fall_break) %>%
  arrange(weekend, fall_break) %>%
  print(n = Inf)

# =============================================================================
# 6. FIT MODELS
# =============================================================================

ctrl <- glmmTMBControl(
  optCtrl = list(
    iter.max = 2e4,
    eval.max = 2e4
  )
)

cat("\n--- Model 1: Burst rate, truncated NB2 ---\n")

m_rate <- glmmTMB(
  burst_rate ~
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 + weekend +
    (1 | ID),
  family = truncated_nbinom2(link = "log"),
  data = user_day_model,
  control = ctrl
)

summary(m_rate)

cat("AIC:", round(AIC(m_rate), 1), "\n")

saveRDS(
  m_rate,
  "models/burst/m_burst_rate.rds"
)

cat("\n--- Model 2: Multi-app burst count, NB2 ---\n")

m_multi <- glmmTMB(
  n_multiapp ~
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 + weekend +
    (1 | ID),
  family = nbinom2(link = "log"),
  data = user_day_model,
  control = ctrl
)

summary(m_multi)

cat("AIC:", round(AIC(m_multi), 1), "\n")

saveRDS(
  m_multi,
  "models/burst/m_burst_multiapp.rds"
)

cat("\n--- Model 3: Mean burst length, Gamma ---\n")

m_len <- glmmTMB(
  mean_burst_len ~
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 + weekend +
    (1 | ID),
  family = Gamma(link = "log"),
  data = user_day_model,
  control = ctrl
)

summary(m_len)

cat("AIC:", round(AIC(m_len), 1), "\n")

saveRDS(
  m_len,
  "models/burst/m_burst_length.rds"
)

# =============================================================================
# 7. SAVE MODEL TABLES
# =============================================================================

tidy_rate <- broom.mixed::tidy(
  m_rate,
  effects = "fixed",
  conf.int = TRUE
) %>%
  mutate(
    rr = exp(estimate),
    rr_low = exp(conf.low),
    rr_high = exp(conf.high),
    pct = 100 * (rr - 1),
    pct_low = 100 * (rr_low - 1),
    pct_high = 100 * (rr_high - 1),
    model = "burst_rate"
  )

tidy_multi <- broom.mixed::tidy(
  m_multi,
  effects = "fixed",
  conf.int = TRUE
) %>%
  mutate(
    rr = exp(estimate),
    rr_low = exp(conf.low),
    rr_high = exp(conf.high),
    pct = 100 * (rr - 1),
    pct_low = 100 * (rr_low - 1),
    pct_high = 100 * (rr_high - 1),
    model = "multiapp_burst_count"
  )

tidy_len <- broom.mixed::tidy(
  m_len,
  effects = "fixed",
  conf.int = TRUE
) %>%
  mutate(
    rr = exp(estimate),
    rr_low = exp(conf.low),
    rr_high = exp(conf.high),
    pct = 100 * (rr - 1),
    pct_low = 100 * (rr_low - 1),
    pct_high = 100 * (rr_high - 1),
    model = "mean_burst_length"
  )

save_model_table(
  tidy_rate,
  "burst_rate_coefficients",
  "tables/burst",
  caption = "Burst rate model coefficients",
  label = "tab:burst_rate_coefficients"
)

save_model_table(
  tidy_multi,
  "burst_multiapp_coefficients",
  "tables/burst",
  caption = "Multi-app burst count model coefficients",
  label = "tab:burst_multiapp_coefficients"
)

save_model_table(
  tidy_len,
  "burst_length_coefficients",
  "tables/burst",
  caption = "Mean burst length model coefficients",
  label = "tab:burst_length_coefficients"
)

burst_coefficients <- bind_rows(
  tidy_rate,
  tidy_multi,
  tidy_len
)

write_csv(
  burst_coefficients,
  "tables/burst/burst_model_coefficients.csv"
)

# =============================================================================
# 8. CONDITION-SPECIFIC EFFECTS
# =============================================================================

burst_effects <- bind_rows(
  extract_change_delta_method(m_rate) %>%
    mutate(model = "burst_rate"),
  
  extract_change_delta_method(m_multi) %>%
    mutate(model = "multiapp_burst_count"),
  
  extract_change_delta_method(m_len) %>%
    mutate(model = "mean_burst_length")
) %>%
  dplyr::select(
    model,
    condition,
    estimate_log,
    se_log,
    conf.low_log,
    conf.high_log,
    rr,
    rr_low,
    rr_high,
    pct,
    pct_low,
    pct_high,
    z,
    p
  )

print(burst_effects, n = Inf)

write_csv(
  burst_effects,
  "tables/burst/burst_effects.csv"
)

save_model_table(
  burst_effects,
  "burst_effects",
  "tables/burst",
  caption = "Burst model condition-specific baseline-to-intervention effects",
  label = "tab:burst_effects"
)

# =============================================================================
# 9. HEATMAP DATA FOR FIGURES
# =============================================================================

heatmap_events <- opens_df_ap2 %>%
  mutate(
    hour = lubridate::hour(time),
    weekday = weekdays(date),
    day_type = if_else(
      weekday %in% c("Saturday", "Sunday"),
      "Weekend",
      "Weekday"
    ),
    condition_label = factor(
      dplyr::recode(condition, !!!condition_labels),
      levels = cond_levels
    )
  )

data_heatmap_weekday <- heatmap_events %>%
  filter(day_type == "Weekday") %>%
  count(condition, condition_label, period2, hour, name = "n_opens") %>%
  group_by(condition, condition_label, period2) %>%
  mutate(
    prop_opens = n_opens / sum(n_opens)
  ) %>%
  ungroup()

data_heatmap_weekend <- heatmap_events %>%
  filter(day_type == "Weekend") %>%
  count(condition, condition_label, period2, hour, name = "n_opens") %>%
  group_by(condition, condition_label, period2) %>%
  mutate(
    prop_opens = n_opens / sum(n_opens)
  ) %>%
  ungroup()

write_csv(
  data_heatmap_weekday,
  "data/burst/data_heatmap_weekday.csv"
)

write_csv(
  data_heatmap_weekend,
  "data/burst/data_heatmap_weekend.csv"
)

message("Burst analysis complete.")