Sys.setenv(TZ = "Europe/Copenhagen")

# =============================================================================
# 02_analysis/app_specific/06_app_specific_analysis.R
#
# PURPOSE:
#   Fit app-specific ITS models for five apps across four outcomes:
#     1. Total daily time
#     2. Daily opens
#     3. Average session duration
#     4. Dismissal probability during intervention
#
# KEY DECISION FOR NEW MANUSCRIPT:
#   App-specific baseline/intervention models now use the same ITS formula
#   structure as the main models:
#
#     outcome ~ ns(time3, df = 2):period2 +
#       period2 * condition +
#       gender + fall_break + region + klassetrin_short2 +
#       (1 | ID2)
#
#   The one exception is dismissal probability, because it is not a
#   baseline/intervention ITS model. It is fitted in the intervention period only
#   and models dismissal/opening probability by condition over intervention time.
#
# IMPORTANT FIX RETAINED:
#   App-specific smoothing must be grouped by ID2 x period2 x app.
#   Do NOT use apply_rolling_smooth() directly here, because the generic version
#   groups only by ID2 x period2 and would mix app rows inside the rolling window.
#
# INPUTS:
#   data/baseline_intervention_outro/session_time/time_per_day_app.csv
#   data/baseline_intervention_outro/session_time/time_per_day_avg_app.csv
#   data/baseline_intervention_outro/opens/opens_per_day_app.csv
#   data/regret_rate/intervention_app_for_regret.csv
#   data/participants.csv
#
# OUTPUTS:
#   models/app_specific/glm_time_{app}.rds
#   models/app_specific/glm_opens_{app}.rds
#   models/app_specific/glm_session_{app}.rds
#   models/app_specific/glm_regret_{app}.rds
#   tables/app_specific/app_changes_time.{xlsx,tex,csv}
#   tables/app_specific/app_changes_opens.{xlsx,tex,csv}
#   tables/app_specific/app_changes_session.{xlsx,tex,csv}
#   tables/app_specific/dismissal_probability_by_app.{xlsx,tex,csv}
#   tables/app_specific/app_specific_model_frame_audit.csv
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs(
  "models/app_specific",
  "tables/app_specific",
  "figures/app_specific"
)

TARGET_APPS <- c(
  "instagram",
  "tikTok",
  "snapchat",
  "youtube",
  "facebook"
)

COND_LEVELS_RAW <- c(
  "control",
  "intervention",
  "default"
)

COND_LEVELS_ENG <- c(
  "Reflection",
  "Planning",
  "Waiting"
)

# =============================================================================
# 1. LOAD PARTICIPANTS
# =============================================================================

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
# 2. APP-SPECIFIC HELPERS
# =============================================================================

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

smooth_app_median <- function(df, value_col, out_col) {
  df %>%
    group_by(ID2, period2, app) %>%
    arrange(days_before_activation_num, .by_group = TRUE) %>%
    mutate(
      !!out_col := zoo::rollmedian(
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
        ifelse(
          days_before_activation_num > 0,
          days_before_activation_num - 3,
          NA
        )
      ),
      time3 = time2 + 11
    )
}

prep_app_its_data <- function(
    df,
    raw_outcome,
    smooth_outcome,
    smoother = c("mean", "median"),
    include_id_in_model_frame = FALSE
) {
  smoother <- match.arg(smoother)
  
  out <- df %>%
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
    mutate(
      app = factor(app, levels = TARGET_APPS)
    )
  
  if (smoother == "mean") {
    out <- out %>%
      smooth_app_mean(raw_outcome, smooth_outcome)
  }
  
  if (smoother == "median") {
    out <- out %>%
      smooth_app_median(raw_outcome, smooth_outcome)
  }
  
  keep_cols <- c(
    smooth_outcome,
    "days_before_activation_num",
    "period2",
    "condition",
    "weekend",
    "fall_break",
    "ID2",
    "day_of_week",
    "app",
    "gender",
    "region",
    "klassetrin_short2"
  )
  
  if (include_id_in_model_frame) {
    keep_cols <- c(keep_cols, "ID")
  }
  
  out %>%
    dplyr::select(any_of(keep_cols)) %>%
    drop_na() %>%
    make_time3_after_smoothing()
}

fit_gamma_app_main_formula <- function(df, outcome, app_name, max_fun = 1e5) {
  glmer(
    as.formula(
      paste0(
        outcome,
        " ~ ns(time3, df = 2):period2 + ",
        "period2 * condition + ",
        "gender + fall_break + region + klassetrin_short2 + ",
        "(1 | ID2)"
      )
    ),
    family = Gamma(link = "log"),
    data = df %>% filter(app == app_name),
    control = glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = max_fun)
    )
  )
}

fit_nb2_app_main_formula <- function(df, outcome, app_name) {
  glmmTMB(
    as.formula(
      paste0(
        outcome,
        " ~ ns(time3, df = 2):period2 + ",
        "period2 * condition + ",
        "gender + fall_break + region + klassetrin_short2 + ",
        "(1 | ID2)"
      )
    ),
    family = nbinom2(link = "log"),
    data = df %>% filter(app == app_name),
    control = glmmTMBControl(
      optCtrl = list(
        iter.max = 1e5,
        eval.max = 1e5
      )
    )
  )
}

get_term_app <- function(tidy_df, term_name) {
  out <- tidy_df %>%
    filter(term == term_name) %>%
    transmute(
      beta = estimate,
      se = std.error,
      z = statistic,
      p = p.value,
      lo = conf.low,
      hi = conf.high
    )
  
  if (nrow(out) == 0) {
    tibble(
      beta = 0,
      se = 0,
      z = 0,
      p = NA_real_,
      lo = 0,
      hi = 0
    )
  } else {
    out
  }
}

extract_period_change_by_condition_app <- function(model, app_label = NA_character_) {
  tidy <- broom.mixed::tidy(
    model,
    conf.int = TRUE
  )
  
  bP <- get_term_app(
    tidy,
    "period2intervention"
  )
  
  bP_int <- bind_rows(
    get_term_app(tidy, "period2intervention:conditionintervention"),
    get_term_app(tidy, "conditionintervention:period2intervention")
  ) %>%
    filter(!(beta == 0 & se == 0 & is.na(p))) %>%
    slice_head(n = 1)
  
  bP_def <- bind_rows(
    get_term_app(tidy, "period2intervention:conditiondefault"),
    get_term_app(tidy, "conditiondefault:period2intervention")
  ) %>%
    filter(!(beta == 0 & se == 0 & is.na(p))) %>%
    slice_head(n = 1)
  
  if (nrow(bP_int) == 0) {
    bP_int <- tibble(
      beta = 0,
      se = 0,
      z = 0,
      p = NA_real_,
      lo = 0,
      hi = 0
    )
  }
  
  if (nrow(bP_def) == 0) {
    bP_def <- tibble(
      beta = 0,
      se = 0,
      z = 0,
      p = NA_real_,
      lo = 0,
      hi = 0
    )
  }
  
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
      p_value = 2 * pnorm(
        -abs(
          (bP$beta + bP_int$beta) /
            sqrt(bP$se^2 + bP_int$se^2)
        )
      ),
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
      p_value = 2 * pnorm(
        -abs(
          (bP$beta + bP_def$beta) /
            sqrt(bP$se^2 + bP_def$se^2)
        )
      ),
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

# =============================================================================
# 3. DAILY TIME BY APP
# =============================================================================

message("\n--- Daily time by app ---")

time_app_prepped <- read_csv(
  "data/baseline_intervention_outro/session_time/time_per_day_app.csv",
  show_col_types = FALSE
) %>%
  prep_app_its_data(
    raw_outcome = "total_time_minutes",
    smooth_outcome = "total_time_minutes_smooth",
    smoother = "mean",
    include_id_in_model_frame = TRUE
  )

time_app_models <- lapply(
  setNames(TARGET_APPS, TARGET_APPS),
  function(app_name) {
    message("  Fitting time model: ", app_name)
    
    model <- fit_gamma_app_main_formula(
      df = time_app_prepped,
      outcome = "total_time_minutes_smooth",
      app_name = app_name,
      max_fun = 1e5
    )
    
    saveRDS(
      model,
      file.path(
        "models/app_specific",
        paste0("glm_time_", app_name, ".rds")
      )
    )
    
    model
  }
)

app_changes_time <- bind_rows(
  lapply(
    TARGET_APPS,
    function(app_name) {
      extract_period_change_by_condition_app(
        time_app_models[[app_name]],
        app_name
      )
    }
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

# =============================================================================
# 4. DAILY OPENS BY APP
# =============================================================================

message("\n--- Daily opens by app ---")

opens_app_prepped <- read_csv(
  "data/baseline_intervention_outro/opens/opens_per_day_app.csv",
  show_col_types = FALSE
) %>%
  prep_app_its_data(
    raw_outcome = "daily_opens",
    smooth_outcome = "daily_opens_smooth",
    smoother = "median",
    include_id_in_model_frame = FALSE
  )

opens_app_models <- lapply(
  setNames(TARGET_APPS, TARGET_APPS),
  function(app_name) {
    message("  Fitting opens model: ", app_name)
    
    model <- fit_nb2_app_main_formula(
      df = opens_app_prepped,
      outcome = "daily_opens_smooth",
      app_name = app_name
    )
    
    saveRDS(
      model,
      file.path(
        "models/app_specific",
        paste0("glm_opens_", app_name, ".rds")
      )
    )
    
    model
  }
)

app_changes_opens <- bind_rows(
  lapply(
    TARGET_APPS,
    function(app_name) {
      extract_period_change_by_condition_app(
        opens_app_models[[app_name]],
        app_name
      )
    }
  )
)

save_model_table(
  app_changes_opens,
  "app_changes_opens",
  "tables/app_specific",
  caption = "App-specific intervention effect on daily access attempts",
  label = "tab:app_changes_opens"
)

write_csv(
  app_changes_opens,
  "tables/app_specific/app_changes_opens.csv"
)

# =============================================================================
# 5. SESSION DURATION BY APP
# =============================================================================

message("\n--- Session duration by app ---")

session_app_prepped <- read_csv(
  "data/baseline_intervention_outro/session_time/time_per_day_avg_app.csv",
  show_col_types = FALSE
) %>%
  prep_app_its_data(
    raw_outcome = "mean_time_minutes",
    smooth_outcome = "mean_time_minutes_smooth",
    smoother = "mean",
    include_id_in_model_frame = FALSE
  )

session_app_models <- lapply(
  setNames(TARGET_APPS, TARGET_APPS),
  function(app_name) {
    message("  Fitting session model: ", app_name)
    
    model <- fit_gamma_app_main_formula(
      df = session_app_prepped,
      outcome = "mean_time_minutes_smooth",
      app_name = app_name,
      max_fun = 1e6
    )
    
    saveRDS(
      model,
      file.path(
        "models/app_specific",
        paste0("glm_session_", app_name, ".rds")
      )
    )
    
    model
  }
)

app_changes_session <- bind_rows(
  lapply(
    TARGET_APPS,
    function(app_name) {
      extract_period_change_by_condition_app(
        session_app_models[[app_name]],
        app_name
      )
    }
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

# =============================================================================
# 6. DISMISSAL PROBABILITY BY APP
# =============================================================================

message("\n--- Dismissal probability by app ---")

regret_data_app <- read_csv(
  "data/regret_rate/intervention_app_for_regret.csv",
  show_col_types = FALSE
) %>%
  filter(
    !is.na(interventionType),
    interventionType != "",
    !is.na(condition),
    resolution %in% c("openedApp", "dismissedAppOpening"),
    app %in% TARGET_APPS
  ) %>%
  mutate(
    regret = as.integer(resolution == "dismissedAppOpening"),
    date = as.Date(date, tz = "Europe/Copenhagen"),
    day_of_week = weekdays(date),
    weekend = as.factor(
      as.integer(day_of_week %in% c("Saturday", "Sunday"))
    ),
    fall_break = as.factor(
      as.integer(
        date >= as.Date("2024-10-14") &
          date <= as.Date("2024-10-18")
      )
    ),
    christmas_break = as.factor(
      as.integer(
        date >= as.Date("2024-12-23") &
          date <= as.Date("2025-01-02")
      )
    ),
    condition = factor(condition, levels = COND_LEVELS_RAW)
  ) %>%
  left_join(participants, by = "ID") %>%
  add_klassetrin_short2() %>%
  mutate(
    ID2 = as.factor(as.numeric(as.factor(ID))),
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
    )
  ) %>%
  drop_na(
    regret,
    condition,
    days_before_activation_num,
    ID2,
    weekend,
    fall_break,
    region,
    klassetrin_short2
  )

fit_regret_app <- function(app_name) {
  glmer(
    regret ~
      condition +
      ns(days_before_activation_num, df = 2):condition +
      klassetrin_short2 +
      weekend + fall_break +
      region +
      (1 | ID2),
    family = binomial(),
    data = regret_data_app %>% filter(app == app_name),
    control = glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 1e6)
    )
  )
}

regret_app_models <- lapply(
  setNames(TARGET_APPS, TARGET_APPS),
  function(app_name) {
    message("  Fitting dismissal model: ", app_name)
    
    model <- fit_regret_app(app_name)
    
    saveRDS(
      model,
      file.path(
        "models/app_specific",
        paste0("glm_regret_", app_name, ".rds")
      )
    )
    
    model
  }
)

extract_regret_by_condition_app <- function(model, app_label = NA_character_) {
  tidy <- broom.mixed::tidy(
    model,
    conf.int = TRUE
  )
  
  b0 <- get_term_app(tidy, "(Intercept)")
  b_plan <- get_term_app(tidy, "conditionintervention")
  b_wait <- get_term_app(tidy, "conditiondefault")
  
  bind_rows(
    tibble(
      app = app_label,
      condition = "control",
      condition2 = "Reflection",
      estimate_log = b0$beta,
      se_log = b0$se,
      z = b0$z,
      p_value = b0$p,
      conf.low_log = b0$lo,
      conf.high_log = b0$hi
    ),
    tibble(
      app = app_label,
      condition = "intervention",
      condition2 = "Planning",
      estimate_log = b0$beta + b_plan$beta,
      se_log = sqrt(b0$se^2 + b_plan$se^2),
      z = (b0$beta + b_plan$beta) / sqrt(b0$se^2 + b_plan$se^2),
      p_value = 2 * pnorm(
        -abs(
          (b0$beta + b_plan$beta) /
            sqrt(b0$se^2 + b_plan$se^2)
        )
      ),
      conf.low_log = b0$lo + b_plan$lo,
      conf.high_log = b0$hi + b_plan$hi
    ),
    tibble(
      app = app_label,
      condition = "default",
      condition2 = "Waiting",
      estimate_log = b0$beta + b_wait$beta,
      se_log = sqrt(b0$se^2 + b_wait$se^2),
      z = (b0$beta + b_wait$beta) / sqrt(b0$se^2 + b_wait$se^2),
      p_value = 2 * pnorm(
        -abs(
          (b0$beta + b_wait$beta) /
            sqrt(b0$se^2 + b_wait$se^2)
        )
      ),
      conf.low_log = b0$lo + b_wait$lo,
      conf.high_log = b0$hi + b_wait$hi
    )
  ) %>%
    mutate(
      prob = plogis(estimate_log),
      prob_low = plogis(conf.low_log),
      prob_high = plogis(conf.high_log),
      pct = 100 * prob,
      pct_low = 100 * prob_low,
      pct_high = 100 * prob_high,
      app = factor(app, levels = TARGET_APPS),
      condition2 = factor(condition2, levels = COND_LEVELS_ENG),
      p_value = round(p_value, 4)
    )
}

dismissal_effects <- bind_rows(
  lapply(
    TARGET_APPS,
    function(app_name) {
      extract_regret_by_condition_app(
        regret_app_models[[app_name]],
        app_name
      )
    }
  )
)

dismissal_effects_no_facebook <- dismissal_effects %>%
  filter(app != "facebook")

save_model_table(
  dismissal_effects,
  "dismissal_probability_by_app",
  "tables/app_specific",
  caption = "App-specific dismissal probability by condition",
  label = "tab:app_dismissal_probability"
)

write_csv(
  dismissal_effects,
  "tables/app_specific/dismissal_effects.csv"
)

write_csv(
  dismissal_effects_no_facebook,
  "tables/app_specific/dismissal_effects_no_facebook.csv"
)

# =============================================================================
# 7. AUDIT OUTPUTS
# =============================================================================

audit_app_specific <- bind_rows(
  time_app_prepped %>%
    count(app, name = "n_rows") %>%
    mutate(outcome = "time"),
  
  opens_app_prepped %>%
    count(app, name = "n_rows") %>%
    mutate(outcome = "opens"),
  
  session_app_prepped %>%
    count(app, name = "n_rows") %>%
    mutate(outcome = "session"),
  
  regret_data_app %>%
    count(app, name = "n_rows") %>%
    mutate(outcome = "dismissal")
) %>%
  dplyr::select(
    outcome,
    app,
    n_rows
  ) %>%
  arrange(
    outcome,
    app
  )

print(audit_app_specific, n = Inf)

write_csv(
  audit_app_specific,
  "tables/app_specific/app_specific_model_frame_audit.csv"
)

message("\n=== 06_app_specific_analysis.R complete ===")