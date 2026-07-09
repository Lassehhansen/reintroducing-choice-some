# =============================================================================
# 02_analysis/app_specific/06c_app_specific_session_duration.R
#
# PURPOSE:
#   Fit app-specific ITS models for average session duration.
#
# MODEL:
#   mean_time_minutes_smooth ~
#     ns(time3, df = 2):period2 +
#     period2 * condition +
#     gender + fall_break + region + klassetrin_short2 +
#     (1 | ID2)
#
# NOTE:
#   No additional old-script valid-participants filter is applied.
#   Gamma outcomes are filtered to strictly positive smoothed values.
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

extract_period_change_by_condition_app <- function(model, app_label = NA_character_) {
  tidy <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
  
  bP <- get_existing_term(tidy, "period2intervention")
  bP_int <- get_existing_term(
    tidy,
    c(
      "period2intervention:conditionintervention",
      "conditionintervention:period2intervention"
    )
  )
  bP_def <- get_existing_term(
    tidy,
    c(
      "period2intervention:conditiondefault",
      "conditiondefault:period2intervention"
    )
  )
  
  bind_rows(
    tibble(
      app = app_label,
      condition = "control",
      condition2 = "Reflection",
      estimate_log = bP$beta,
      se_log = bP$se,
      z = bP$z,
      p_value = bP$p,
      conf.low_log = bP$lo,
      conf.high_log = bP$hi
    ),
    tibble(
      app = app_label,
      condition = "intervention",
      condition2 = "Planning",
      estimate_log = bP$beta + bP_int$beta,
      se_log = sqrt(bP$se^2 + bP_int$se^2),
      z = (bP$beta + bP_int$beta) / sqrt(bP$se^2 + bP_int$se^2),
      p_value = 2 * pnorm(-abs((bP$beta + bP_int$beta) / sqrt(bP$se^2 + bP_int$se^2))),
      conf.low_log = bP$lo + bP_int$lo,
      conf.high_log = bP$hi + bP_int$hi
    ),
    tibble(
      app = app_label,
      condition = "default",
      condition2 = "Waiting",
      estimate_log = bP$beta + bP_def$beta,
      se_log = sqrt(bP$se^2 + bP_def$se^2),
      z = (bP$beta + bP_def$beta) / sqrt(bP$se^2 + bP_def$se^2),
      p_value = 2 * pnorm(-abs((bP$beta + bP_def$beta) / sqrt(bP$se^2 + bP_def$se^2))),
      conf.low_log = bP$lo + bP_def$lo,
      conf.high_log = bP$hi + bP_def$hi
    )
  ) %>%
    mutate(
      rr = exp(estimate_log),
      rr_low = exp(conf.low_log),
      rr_high = exp(conf.high_log),
      pct = 100 * (rr - 1),
      pct_low = 100 * (rr_low - 1),
      pct_high = 100 * (rr_high - 1),
      app = factor(app, levels = TARGET_APPS),
      condition2 = factor(condition2, levels = COND_LEVELS_ENG),
      p_value = round(p_value, 4)
    )
}

session_app_prepped <- read_csv(
  "data/baseline_intervention_outro/session_time/time_per_day_avg_app.csv",
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
  smooth_app_mean("mean_time_minutes", "mean_time_minutes_smooth") %>%
  dplyr::select(
    mean_time_minutes_smooth,
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
  filter(mean_time_minutes_smooth > 0)

glm_session_instagram <- glmer(
  mean_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = session_app_prepped %>% filter(app == "instagram"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
)

glm_session_tiktok <- glmer(
  mean_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = session_app_prepped %>% filter(app == "tikTok"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
)

### GLM SESSION SNAPCHAT ONLY WORKS WITHOUT REGION; OR ELSE CONVERGENCE PROBLEMS
glm_session_snapchat <- glmer(
  mean_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    #gender + region +
    gender + klassetrin_short2 + fall_break + 
    #gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = session_app_prepped %>% filter(app == "snapchat"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
)

glm_session_youtube <- glmer(
  mean_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = session_app_prepped %>% filter(app == "youtube"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
)

glm_session_facebook <- glmer(
  mean_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = session_app_prepped %>% filter(app == "facebook"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
)

saveRDS(glm_session_instagram, "models/app_specific/glm_session_instagram.rds")
saveRDS(glm_session_tiktok,    "models/app_specific/glm_session_tikTok.rds")
saveRDS(glm_session_snapchat,  "models/app_specific/glm_session_snapchat.rds")
saveRDS(glm_session_youtube,   "models/app_specific/glm_session_youtube.rds")
saveRDS(glm_session_facebook,  "models/app_specific/glm_session_facebook.rds")

app_changes_session <- bind_rows(
  extract_change_delta_method(glm_session_instagram, "instagram"),
  extract_change_delta_method(glm_session_tiktok,    "tikTok"),
  extract_change_delta_method(glm_session_snapchat,  "snapchat"),
  extract_change_delta_method(glm_session_youtube,   "youtube"),
  extract_change_delta_method(glm_session_facebook,  "facebook")
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
  app_changes_session,
  "app_changes_session",
  "tables/app_specific",
  caption = "App-specific intervention effect on average session duration",
  label = "tab:app_changes_session"
)

write_csv(
  app_changes_session,
  "tables/app_specific/app_changes_session.csv"
)

app_changes_session %>%
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
message("06c_app_specific_session_duration.R complete")


# =============================================================================
# OPTIONAL VALIDATION CHECK AFTER RUNNING 06a, 06b, AND 06c
# =============================================================================

validate_delta_table <- function(df) {
  df %>%
    mutate(
      ci_excludes_1 = rr_low > 1 | rr_high < 1,
      p_sig = p_value < 0.05,
      mismatch = ci_excludes_1 != p_sig
    ) %>%
    filter(mismatch) %>%
    dplyr::select(
      app,
      condition,
      condition2,
      estimate_log,
      se_log,
      p_value,
      rr,
      rr_low,
      rr_high,
      ci_excludes_1,
      p_sig
    )
}

validate_delta_table(app_changes_time)
validate_delta_table(app_changes_opens)
validate_delta_table(app_changes_session)


# And I would add one slightly stricter audit before finalizing Figure 4:

delta_table_audit <- bind_rows(
  app_changes_time %>%
    mutate(outcome = "Daily Activity"),
  app_changes_opens %>%
    mutate(outcome = "Sessions"),
  app_changes_session %>%
    mutate(outcome = "Session Length")
) %>%
  mutate(
    ci_excludes_1 = rr_low > 1 | rr_high < 1,
    p_sig = p_value < 0.05,
    mismatch = ci_excludes_1 != p_sig,
    bad_bounds = rr_low > rr | rr > rr_high,
    bad_log_bounds = conf.low_log > estimate_log | estimate_log > conf.high_log
  )

delta_table_audit %>%
  summarise(
    n_rows = n(),
    n_mismatches = sum(mismatch, na.rm = TRUE),
    n_bad_bounds = sum(bad_bounds, na.rm = TRUE),
    n_bad_log_bounds = sum(bad_log_bounds, na.rm = TRUE),
    n_missing = sum(if_any(
      c(
        estimate_log,
        se_log,
        p_value,
        conf.low_log,
        conf.high_log,
        rr,
        rr_low,
        rr_high
      ),
      is.na
    ))
  )

delta_table_audit %>%
  filter(mismatch | bad_bounds | bad_log_bounds) %>%
  print(n = Inf)
