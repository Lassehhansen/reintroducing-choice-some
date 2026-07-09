# =============================================================================
# 02_analysis/app_specific/06a_app_specific_daily_time.R
#
# PURPOSE:
#   Fit app-specific ITS models for total daily time.
#
# MODEL:
#   total_time_minutes_smooth ~
#     ns(time3, df = 2):period2 +
#     period2 * condition +
#     gender + fall_break + region + klassetrin_short2 +
#     (1 | ID2)
#
# NOTE:
#   App-specific smoothing is grouped by ID2 × period2 × app.
#   Gamma outcomes are filtered to strictly positive smoothed values.
#
# Based on the uploaded combined app-specific script. :contentReference[oaicite:0]{index=0}
# =============================================================================

Sys.setenv(TZ = "Europe/Copenhagen")

source("02_analysis/shared_utils.R")

make_output_dirs(
  "models/app_specific",
  "tables/app_specific",
  "figures/app_specific"
)

TARGET_APPS <- c("instagram", "tikTok", "snapchat", "youtube", "facebook")
COND_LEVELS_ENG <- c("Reflection", "Planning", "Waiting")

participants <- read_csv(
  "data/participants.csv",
  show_col_types = FALSE
) %>%
  dplyr::select(
    -any_of(c(
      "condition", "cond",
      "condition.x", "condition.y",
      "condition_x", "condition_y"
    ))
  )

smooth_app_mean <- function(df, value_col, out_col) {
  df %>%
    group_by(ID2, period2, app) %>%
    arrange(days_before_activation_num, .by_group = TRUE) %>%
    mutate(
      !!out_col := zoo::rollmean(
        .data[[value_col]],
        k = 3,
        fill = NA,
        align = "right"
      )
    ) %>%
    ungroup()
}

make_time3_after_smoothing <- function(df) {
  df %>%
    mutate(
      time2 = ifelse(
        days_before_activation_num < 0,
        days_before_activation_num,
        ifelse(days_before_activation_num > 0, days_before_activation_num - 3, NA_real_)
      ),
      time3 = time2 + 11
    )
}

get_existing_term <- function(tidy_df, term_names) {
  out <- tidy_df %>%
    filter(term %in% term_names) %>%
    slice_head(n = 1) %>%
    transmute(
      beta = estimate,
      se = std.error,
      z = statistic,
      p = p.value,
      lo = conf.low,
      hi = conf.high
    )
  
  if (nrow(out) == 0) {
    tibble(beta = 0, se = 0, z = 0, p = NA_real_, lo = 0, hi = 0)
  } else {
    out
  }
}


time_app_prepped <- read_csv(
  "data/baseline_intervention_outro/session_time/time_per_day_app.csv",
  show_col_types = FALSE
) %>%
  left_join(participants, by = "ID") %>%
  add_break_covariates(date_col = "date") %>%
  filter(
    period != "mixed",
    !days_before_activation_num %in% c(-14, 0, 29),
    period2 %in% c("baseline", "intervention"),
    app %in% TARGET_APPS
  ) %>%
  add_klassetrin_short2() %>%
  factorise_model_data() %>%
  mutate(app = factor(app, levels = TARGET_APPS)) %>%
  smooth_app_mean("total_time_minutes", "total_time_minutes_smooth") %>%
  dplyr::select(
    total_time_minutes_smooth,
    days_before_activation_num,
    period2,
    condition,
    fall_break,
    ID2,
    app,
    gender,
    region,
    klassetrin_short2
  ) %>%
  drop_na() %>%
  make_time3_after_smoothing() %>%
  drop_na(time3) %>%
  filter(total_time_minutes_smooth > 0)

glm_time_instagram <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + region +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = time_app_prepped %>% filter(app == "instagram"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)


# ######
# 
# instagram_dat <- time_app_prepped %>%
#   filter(app == "instagram") %>%
#   droplevels()
# 
# # =============================================================================
# # 1. Diagnose why fall_break and klassetrin_short2 destabilize the model
# # =============================================================================
# 
# instagram_dat %>%
#   count(fall_break, period2, condition) %>%
#   group_by(period2, condition) %>%
#   mutate(prop = n / sum(n)) %>%
#   ungroup() %>%
#   arrange(fall_break, period2, condition) %>%
#   print(n = Inf)
# 
# instagram_dat %>%
#   count(klassetrin_short2, period2, condition) %>%
#   group_by(klassetrin_short2) %>%
#   mutate(prop_within_klassetrin = n / sum(n)) %>%
#   ungroup() %>%
#   arrange(klassetrin_short2, period2, condition) %>%
#   print(n = Inf)
# 
# instagram_dat %>%
#   count(region, klassetrin_short2, gender) %>%
#   arrange(n) %>%
#   print(n = Inf)
# 
# instagram_dat %>%
#   count(condition, period2, gender, region, klassetrin_short2, fall_break) %>%
#   arrange(n) %>%
#   filter(n < 10) %>%
#   print(n = Inf)

glm_time_tiktok <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = time_app_prepped %>% filter(app == "tikTok"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)

glm_time_snapchat <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = time_app_prepped %>% filter(app == "snapchat"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)

glm_time_youtube <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = time_app_prepped %>% filter(app == "youtube"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)

glm_time_facebook <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = time_app_prepped %>% filter(app == "facebook"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)

saveRDS(glm_time_instagram, "models/app_specific/glm_time_instagram.rds")
saveRDS(glm_time_tiktok,    "models/app_specific/glm_time_tikTok.rds")
saveRDS(glm_time_snapchat,  "models/app_specific/glm_time_snapchat.rds")
saveRDS(glm_time_youtube,   "models/app_specific/glm_time_youtube.rds")
saveRDS(glm_time_facebook,  "models/app_specific/glm_time_facebook.rds")

app_changes_time <- bind_rows(
  extract_change_delta_method(glm_time_instagram, "instagram"),
  extract_change_delta_method(glm_time_tiktok,    "tikTok"),
  extract_change_delta_method(glm_time_snapchat,  "snapchat"),
  extract_change_delta_method(glm_time_youtube,   "youtube"),
  extract_change_delta_method(glm_time_facebook,  "facebook")
) %>%
  mutate(
    app = factor(
      app,
      levels = TARGET_APPS
    ),
    condition2 = factor(
      condition2,
      levels = COND_LEVELS_ENG
    )
  )

save_model_table(
  app_changes_time,
  "app_changes_time",
  "tables/app_specific",
  caption = "App-specific intervention effect on daily total time",
  label = "tab:app_changes_time"
)

write_csv(
  app_changes_time,
  "tables/app_specific/app_changes_time.csv"
)

app_changes_time %>%
  dplyr::select(
    app,
    condition,
    condition2,
    estimate_log,
    se_log,
    z,
    p_value,
    conf.low_log,
    conf.high_log,
    rr,
    rr_low,
    rr_high,
    pct,
    pct_low,
    pct_high,
    significant
  ) %>%
  print(n = Inf)

time_app_prepped %>%
  count(app, period2, condition) %>%
  arrange(app, period2, condition) %>%
  print(n = Inf)

message("06a_app_specific_daily_time.R complete")