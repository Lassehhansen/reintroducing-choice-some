# =============================================================================
# 02_analysis/app_specific/06b_app_specific_daily_opens.R
#
# PURPOSE:
#   Fit app-specific ITS models for daily opens.
#
# MODEL:
#   daily_opens_smooth ~
#     ns(time3, df = 2):period2 +
#     period2 * condition +
#     gender + fall_break + region + klassetrin_short2 +
#     (1 | ID2)
#
# NOTE:
#   App-specific smoothing is grouped by ID2 × period2 × app.
#   Opens are smoothed with rolling median, matching the count-outcome logic.
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

opens_app_prepped <- read_csv(
  "data/baseline_intervention_outro/opens/opens_per_day_app.csv",
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
  smooth_app_median("daily_opens", "daily_opens_smooth") %>%
  dplyr::select(
    daily_opens_smooth,
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
  drop_na(time3)

glm_opens_instagram <- glmmTMB(
  daily_opens_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = nbinom2(link = "log"),
  data = opens_app_prepped %>% filter(app == "instagram"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e5, eval.max = 1e5))
)

glm_opens_tiktok <- glmmTMB(
  daily_opens_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = nbinom2(link = "log"),
  data = opens_app_prepped %>% filter(app == "tikTok"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e5, eval.max = 1e5))
)

glm_opens_snapchat <- glmmTMB(
  daily_opens_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = nbinom2(link = "log"),
  data = opens_app_prepped %>% filter(app == "snapchat"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e5, eval.max = 1e5))
)

glm_opens_youtube <- glmmTMB(
  daily_opens_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = nbinom2(link = "log"),
  data = opens_app_prepped %>% filter(app == "youtube"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e5, eval.max = 1e5))
)

glm_opens_facebook <- glmmTMB(
  daily_opens_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break + region + klassetrin_short2 +
    (1 | ID2),
  family = nbinom2(link = "log"),
  data = opens_app_prepped %>% filter(app == "facebook"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e5, eval.max = 1e5))
)

saveRDS(glm_opens_instagram, "models/app_specific/glm_opens_instagram.rds")
saveRDS(glm_opens_tiktok,    "models/app_specific/glm_opens_tikTok.rds")
saveRDS(glm_opens_snapchat,  "models/app_specific/glm_opens_snapchat.rds")
saveRDS(glm_opens_youtube,   "models/app_specific/glm_opens_youtube.rds")
saveRDS(glm_opens_facebook,  "models/app_specific/glm_opens_facebook.rds")

app_changes_opens <- bind_rows(
  extract_change_delta_method(glm_opens_instagram, "instagram"),
  extract_change_delta_method(glm_opens_tiktok,    "tikTok"),
  extract_change_delta_method(glm_opens_snapchat,  "snapchat"),
  extract_change_delta_method(glm_opens_youtube,   "youtube"),
  extract_change_delta_method(glm_opens_facebook,  "facebook")
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

app_changes_opens %>%
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

message("06b_app_specific_daily_opens.R complete")