# =============================================================================
# 02_analysis/moderation/04a_moderation_baseline_time_use_daily_time.R
#
# PURPOSE:
#   Test whether baseline daily time spent moderates intervention effects on
#   total daily time spent.
#
# OUTCOME:
#   total_time_minutes_smooth
#
# MODERATOR:
#   average_time_daily_baseline_s
#
# MODEL:
#   Gamma GLMM with log link.
#
# VALIDATION MODEL COMPARISON:
#   Compares the original main-effect model against the moderated model using
#   AIC and BIC. No extra no-moderation block models are fitted.
#
# EFFECT EXTRACTION:
#   Condition-specific baseline-to-intervention contrasts at -1, 0, +1 SD of
#   baseline use, using delta-method standard errors from the full model vcov.
#
# Based on the uploaded baseline-use moderation script. :contentReference[oaicite:1]{index=1}
# =============================================================================

Sys.setenv(TZ = "Europe/Copenhagen")

source("02_analysis/shared_utils.R")

make_output_dirs(
  "models/moderation",
  "tables/moderation",
  "figures/moderation"
)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

time_per_day <- read_csv(
  "data/baseline_intervention_outro/session_time/time_per_day.csv",
  show_col_types = FALSE
)

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

ind_diff <- read_csv(
  "data/baseline/individual_differences.csv",
  show_col_types = FALSE
) %>%
  mutate(
    average_time_daily_baseline = case_when(
      "average_time_daily_baseline" %in% names(.) ~ average_time_daily_baseline,
      "total_time_minutes" %in% names(.) ~ total_time_minutes,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(
    ID,
    average_time_daily_baseline
  )

# =============================================================================
# 2. PREPARE MODEL DATA
# =============================================================================

time_smoothed <- time_per_day %>%
  left_join(participants, by = "ID") %>%
  left_join(ind_diff, by = "ID") %>%
  add_break_covariates("date") %>%
  add_klassetrin_short2() %>%
  filter(
    period != "mixed",
    !days_before_activation_num %in% c(-14, 0, 29),
    period2 %in% c("baseline", "intervention")
  ) %>%
  factorise_model_data() %>%
  apply_rolling_smooth("total_time_minutes", "total_time_minutes_smooth") %>%
  make_time3() %>%
  dplyr::select(
    total_time_minutes_smooth,
    days_before_activation_num,
    period2,
    condition,
    time3,
    time2,
    fall_break,
    ID2,
    ID,
    gender,
    region,
    klassetrin_short2,
    average_time_daily_baseline
  ) %>%
  drop_na(
    total_time_minutes_smooth,
    period2,
    condition,
    time3,
    fall_break,
    ID2,
    gender,
    region,
    klassetrin_short2,
    average_time_daily_baseline
  ) %>%
  filter(
    total_time_minutes_smooth > 0
  ) %>%
  mutate(
    average_time_daily_baseline_s = as.numeric(scale(average_time_daily_baseline)),
    condition = factor(condition, levels = c("control", "intervention", "default")),
    period2 = factor(period2, levels = c("baseline", "intervention"))
  ) %>%
  droplevels()

message(
  "Moderation data: ",
  nrow(time_smoothed),
  " rows, ",
  n_distinct(time_smoothed$ID2),
  " participants"
)

print(table(time_smoothed$condition, time_smoothed$period2))
print(summary(time_smoothed$total_time_minutes_smooth))
print(summary(time_smoothed$average_time_daily_baseline_s))

# =============================================================================
# 3. SAFE HELPERS
# =============================================================================

all_interaction_orders <- function(vars) {
  if (length(vars) == 1) {
    return(vars)
  }
  
  unlist(
    lapply(
      seq_along(vars),
      function(i) {
        paste(vars[i], all_interaction_orders(vars[-i]), sep = ":")
      }
    ),
    use.names = FALSE
  )
}

add_term_if_exists <- function(L, term, value = 1) {
  if (term %in% names(L)) {
    L[term] <- L[term] + value
  }
  
  L
}

add_interaction_if_exists <- function(L, vars, value = 1) {
  for (term in all_interaction_orders(vars)) {
    L <- add_term_if_exists(L, term, value)
  }
  
  L
}

extract_moderated_change_delta <- function(
    model,
    moderator,
    moderator_label,
    mod_values = c(-1, 0, 1),
    mod_labels = c("Light", "Average", "Heavy")
) {
  b <- fixef(model)
  V <- as.matrix(vcov(model))
  nms <- names(b)
  
  if (!"period2intervention" %in% nms) {
    stop("period2intervention not found in model.", call. = FALSE)
  }
  
  get_one_contrast <- function(condition_raw, condition_label, mod_value, mod_label) {
    L <- setNames(rep(0, length(b)), nms)
    
    L <- add_term_if_exists(L, "period2intervention", 1)
    L <- add_interaction_if_exists(L, c("period2intervention", moderator), mod_value)
    
    if (condition_raw == "intervention") {
      L <- add_interaction_if_exists(L, c("period2intervention", "conditionintervention"), 1)
      L <- add_interaction_if_exists(L, c("period2intervention", "conditionintervention", moderator), mod_value)
    }
    
    if (condition_raw == "default") {
      L <- add_interaction_if_exists(L, c("period2intervention", "conditiondefault"), 1)
      L <- add_interaction_if_exists(L, c("period2intervention", "conditiondefault", moderator), mod_value)
    }
    
    estimate_log <- as.numeric(sum(L * b))
    se_log <- sqrt(as.numeric(t(L) %*% V %*% L))
    z <- estimate_log / se_log
    p_value <- 2 * pnorm(-abs(z))
    
    tibble(
      moderator = moderator_label,
      condition = condition_raw,
      condition2 = condition_label,
      mod_s = mod_value,
      mod_level = mod_label,
      estimate_log = estimate_log,
      se_log = se_log,
      conf.low_log = estimate_log - 1.96 * se_log,
      conf.high_log = estimate_log + 1.96 * se_log,
      rr = exp(estimate_log),
      rr_low = exp(conf.low_log),
      rr_high = exp(conf.high_log),
      pct = 100 * (rr - 1),
      pct_low = 100 * (rr_low - 1),
      pct_high = 100 * (rr_high - 1),
      z = z,
      p_value = p_value,
      significant = rr_low > 1 | rr_high < 1
    )
  }
  
  crossing(
    condition = c("control", "intervention", "default"),
    mod_i = seq_along(mod_values)
  ) %>%
    mutate(
      condition2 = recode(
        condition,
        "control" = "Reflection",
        "intervention" = "Planning",
        "default" = "Waiting"
      ),
      mod_s = mod_values[mod_i],
      mod_level = mod_labels[mod_i]
    ) %>%
    pmap_dfr(
      function(condition, mod_i, condition2, mod_s, mod_level) {
        get_one_contrast(condition, condition2, mod_s, mod_level)
      }
    ) %>%
    mutate(
      condition2 = factor(condition2, levels = c("Reflection", "Planning", "Waiting")),
      mod_level = factor(mod_level, levels = mod_labels)
    )
}

validate_mod_table <- function(df) {
  df %>%
    mutate(
      ci_excludes_1 = rr_low > 1 | rr_high < 1,
      p_sig = p_value < 0.05,
      mismatch = ci_excludes_1 != p_sig,
      bad_bounds = rr_low > rr | rr > rr_high,
      bad_log_bounds = conf.low_log > estimate_log | estimate_log > conf.high_log
    ) %>%
    filter(mismatch | bad_bounds | bad_log_bounds)
}

check_glmer_model <- function(.model, model_label) {
  tibble(
    model = model_label,
    AIC = AIC(.model),
    BIC = BIC(.model),
    logLik = as.numeric(logLik(.model)),
    nobs = nobs(.model),
    max_abs_gradient = if (!is.null(.model@optinfo$derivs$gradient)) {
      max(abs(.model@optinfo$derivs$gradient))
    } else {
      NA_real_
    },
    is_singular = isSingular(.model, tol = 1e-4),
    messages = paste(.model@optinfo$conv$lme4$messages, collapse = " | ")
  )
}

fit_tidy <- function(model) {
  broom.mixed::tidy(
    model,
    effects = "fixed",
    conf.int = TRUE
  ) %>%
    mutate(
      rr = exp(estimate),
      rr_low = exp(conf.low),
      rr_high = exp(conf.high),
      pct = 100 * (rr - 1),
      pct_low = 100 * (rr_low - 1),
      pct_high = 100 * (rr_high - 1)
    )
}

extract_moderation_terms <- function(model, moderator, moderator_label) {
  fit_tidy(model) %>%
    filter(
      str_detect(term, "period2intervention") &
        str_detect(term, moderator)
    ) %>%
    mutate(
      moderator = moderator_label
    ) %>%
    dplyr::select(
      moderator,
      term,
      estimate,
      std.error,
      conf.low,
      conf.high,
      statistic,
      p.value,
      rr,
      rr_low,
      rr_high,
      pct,
      pct_low,
      pct_high
    )
}

# =============================================================================
# 4. FIT ORIGINAL AND MODERATION MODELS
# =============================================================================

message("Fitting original daily-time model...")

glm_time_original <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2) +
    period2 * condition +
    gender + fall_break +
    region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = time_smoothed,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 1e5)
  )
)

message("Fitting baseline-time-use moderation model...")

glm_time_mod_baseline_time <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2) +
    period2 * condition * average_time_daily_baseline_s +
    gender + fall_break +
    region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = time_smoothed,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 1e5)
  )
)

saveRDS(glm_time_original, "models/moderation/glm_time_original_for_baseline_time_comparison.rds")
saveRDS(glm_time_mod_baseline_time, "models/moderation/glm_time_mod_baseline_time.rds")

model_comparison <- bind_rows(
  check_glmer_model(glm_time_original, "original"),
  check_glmer_model(glm_time_mod_baseline_time, "moderated")
) %>%
  mutate(
    original_AIC = AIC[model == "original"][1],
    original_BIC = BIC[model == "original"][1],
    delta_AIC_vs_original = AIC - original_AIC,
    delta_BIC_vs_original = BIC - original_BIC
  ) %>%
  dplyr::select(
    -original_AIC,
    -original_BIC
  )

print(model_comparison)

print(model_comparison)

write_csv(
  model_comparison,
  "tables/moderation/time_mod_baseline_time_model_comparison.csv"
)

# =============================================================================
# 5. TABLES
# =============================================================================

tidy_time_mod_baseline_time <- fit_tidy(glm_time_mod_baseline_time)

baseline_time_effects <- extract_moderated_change_delta(
  model = glm_time_mod_baseline_time,
  moderator = "average_time_daily_baseline_s",
  moderator_label = "Baseline daily time",
  mod_values = c(-1, 0, 1),
  mod_labels = c("Light", "Average", "Heavy")
)

baseline_time_moderation_terms <- extract_moderation_terms(
  glm_time_mod_baseline_time,
  "average_time_daily_baseline_s",
  "Baseline daily time"
)

save_model_table(
  tidy_time_mod_baseline_time,
  "time_mod_baseline_time_coefficients",
  "tables/moderation",
  caption = "Daily time moderation by baseline daily time",
  label = "tab:time_mod_baseline_time_coef"
)

save_model_table(
  baseline_time_effects,
  "time_mod_baseline_time_condition_effects",
  "tables/moderation",
  caption = "Daily time: baseline-to-intervention change by baseline daily time and condition",
  label = "tab:time_mod_baseline_time_effects"
)

save_model_table(
  baseline_time_moderation_terms,
  "time_mod_baseline_time_interaction_terms",
  "tables/moderation",
  caption = "Daily time: baseline daily-time moderation terms",
  label = "tab:time_mod_baseline_time_interactions"
)

write_csv(tidy_time_mod_baseline_time, "tables/moderation/time_mod_baseline_time_coefficients.csv")
write_csv(baseline_time_effects, "tables/moderation/time_mod_baseline_time_condition_effects.csv")
write_csv(baseline_time_moderation_terms, "tables/moderation/time_mod_baseline_time_interaction_terms.csv")

print(validate_mod_table(baseline_time_effects))

# =============================================================================
# 6. FIGURE
# =============================================================================

p_time_baseline_time <- baseline_time_effects %>%
  ggplot(
    aes(
      x = rr,
      y = mod_level,
      xmin = rr_low,
      xmax = rr_high,
      color = condition2,
      fill = condition2
    )
  ) +
  geom_vline(xintercept = 1, linewidth = 0.6) +
  geom_errorbarh(
    height = 0.35,
    linewidth = 0.75,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 3.5,
    shape = 21,
    position = position_dodge(width = 0.55)
  ) +
  scale_x_log10(
    limits = c(0.3, 2),
    breaks = c(0.5, 1, 1.5, 2),
    labels = c("0.5×", "1×", "1.5×", "2×")
  ) +
  scale_color_manual(values = COND_COLOURS, name = NULL) +
  scale_fill_manual(values = COND_COLOURS, name = NULL) +
  labs(
    x = "Rate ratio",
    y = "",
    title = "Daily time by baseline daily time"
  ) +
  custom_general_theme_word() +
  theme(
    axis.title.x = element_text(size = 12, family = BODY_FONT),
    legend.position = "bottom",
    legend.text = element_text(size = 11, family = BODY_FONT)
  )

p_time_baseline_time

ggsave(
  "figures/moderation/time_mod_baseline_time_effects.png",
  p_time_baseline_time,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/moderation/time_mod_baseline_time_effects.svg",
  p_time_baseline_time,
  width = 8,
  height = 5,
  bg = "white"
)

message("04a_moderation_baseline_time_use_daily_time.R complete")