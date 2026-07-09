# =============================================================================
# 02_analysis/moderation/05a_moderation_daily_time_addiction_selfcontrol.R
#
# PURPOSE:
#   Test whether intervention effects on total daily time are moderated by:
#     1. social-media addiction
#     2. self-control
#
# OUTCOME:
#   total_time_minutes_smooth
#
# NOTE:
#   The addiction model uses ns(time3, df = 2) instead of
#   ns(time3, df = 2):period2 because the fully period-specific spline version
#   was numerically unstable for this moderation model.
#
# VALIDATION MODEL COMPARISON:
#   Compares the corresponding original model against each moderated model using
#   AIC and BIC.
#
# Based on uploaded daily-time moderation scripts. :contentReference[oaicite:2]{index=2} :contentReference[oaicite:3]{index=3}
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

# =============================================================================
# 2. PREPARE MODEL DATA
# =============================================================================

time_smoothed <- time_per_day %>%
  left_join(participants, by = "ID") %>%
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
    addiction_index,
    self_control_index,
    gender,
    region,
    klassetrin_short2
  ) %>%
  drop_na(
    total_time_minutes_smooth,
    period2,
    condition,
    time3,
    addiction_index,
    self_control_index,
    gender,
    fall_break,
    region,
    klassetrin_short2,
    ID2
  ) %>%
  filter(
    total_time_minutes_smooth > 0
  ) %>%
  mutate(
    addiction_index_s = as.numeric(scale(addiction_index)),
    self_control_index_s = as.numeric(scale(self_control_index)),
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
print(summary(time_smoothed$addiction_index_s))
print(summary(time_smoothed$self_control_index_s))

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
    mod_labels = c("Low", "Average", "High")
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

make_mod_figure <- function(effects_df, title_text) {
  
  x_limits <- range(
    effects_df$rr_low,
    effects_df$rr_high,
    na.rm = TRUE
  )
  
  x_lower <- min(0.3, floor(x_limits[1] * 10) / 10)
  x_upper <- max(2, ceiling(x_limits[2] * 10) / 10)
  
  x_breaks <- c(0.3, 0.5, 1, 1.5, 2, 3)
  x_breaks <- x_breaks[x_breaks >= x_lower & x_breaks <= x_upper]
  
  effects_df %>%
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
    geom_vline(
      xintercept = 1,
      linewidth = 0.6
    ) +
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
      limits = c(x_lower, x_upper),
      breaks = x_breaks,
      labels = paste0(x_breaks, "×")
    ) +
    scale_color_manual(
      values = COND_COLOURS,
      name = NULL
    ) +
    scale_fill_manual(
      values = COND_COLOURS,
      name = NULL
    ) +
    labs(
      x = "Rate ratio",
      y = "",
      title = title_text
    ) +
    custom_general_theme_word() +
    theme(
      axis.title.x = element_text(
        size = 12,
        family = BODY_FONT
      ),
      legend.position = "bottom",
      legend.text = element_text(
        size = 11,
        family = BODY_FONT
      )
    )
}
# =============================================================================
# 4. ADDICTION MODEL
# =============================================================================

message("Fitting original daily-time model for addiction comparison...")

glm_time_original_addiction_spec <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2) +
    period2 * condition + fall_break +
    gender + 
    region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = time_smoothed,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 1e5)
  )
)

message("Fitting addiction moderation model for daily time...")

glm_time_mod_addiction <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2) +
    period2 * condition * addiction_index_s + fall_break +
    gender + 
    region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = time_smoothed,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 1e5)
  )
)

saveRDS(glm_time_original_addiction_spec, "models/moderation/glm_time_original_addiction_spec.rds")
saveRDS(glm_time_mod_addiction, "models/moderation/glm_time_mod_addiction.rds")

time_addiction_model_comparison <- bind_rows(
  check_glmer_model(glm_time_original_addiction_spec, "original"),
  check_glmer_model(glm_time_mod_addiction, "moderated")
) %>%
  mutate(
    delta_AIC_vs_original = AIC - AIC[model == "original"],
    delta_BIC_vs_original = BIC - BIC[model == "original"]
  )

print(time_addiction_model_comparison)

write_csv(
  time_addiction_model_comparison,
  "tables/moderation/time_mod_addiction_model_comparison.csv"
)

tidy_time_mod_addiction <- fit_tidy(glm_time_mod_addiction)

time_addiction_effects <- extract_moderated_change_delta(
  model = glm_time_mod_addiction,
  moderator = "addiction_index_s",
  moderator_label = "Social-media addiction",
  mod_values = c(-1, 0, 1),
  mod_labels = c("Low", "Average", "High")
)

time_addiction_moderation_terms <- extract_moderation_terms(
  glm_time_mod_addiction,
  "addiction_index_s",
  "Social-media addiction"
)

save_model_table(
  tidy_time_mod_addiction,
  "time_mod_addiction_coefficients",
  "tables/moderation",
  caption = "Daily time moderation by social-media addiction",
  label = "tab:time_mod_addiction_coef"
)

save_model_table(
  time_addiction_effects,
  "time_mod_addiction_condition_effects",
  "tables/moderation",
  caption = "Daily time: baseline-to-intervention change by addiction level and condition",
  label = "tab:time_mod_addiction_effects"
)

save_model_table(
  time_addiction_moderation_terms,
  "time_mod_addiction_interaction_terms",
  "tables/moderation",
  caption = "Daily time: addiction moderation terms",
  label = "tab:time_mod_addiction_interactions"
)

write_csv(tidy_time_mod_addiction, "tables/moderation/time_mod_addiction_coefficients.csv")
write_csv(time_addiction_effects, "tables/moderation/time_mod_addiction_condition_effects.csv")
write_csv(time_addiction_moderation_terms, "tables/moderation/time_mod_addiction_interaction_terms.csv")

print(validate_mod_table(time_addiction_effects))

p_time_addiction <- make_mod_figure(
  time_addiction_effects,
  "Daily time by social-media addiction"
)

ggsave(
  "figures/moderation/time_mod_addiction_effects.png",
  p_time_addiction,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/moderation/time_mod_addiction_effects.svg",
  p_time_addiction,
  width = 8,
  height = 5,
  bg = "white"
)

# =============================================================================
# 5. SELF-CONTROL MODEL
# =============================================================================

message("Fitting original daily-time model for self-control comparison...")

glm_time_original_selfcontrol_spec <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
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

message("Fitting self-control moderation model for daily time...")

glm_time_mod_selfcontrol <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition * self_control_index_s +
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

saveRDS(glm_time_original_selfcontrol_spec, "models/moderation/glm_time_original_selfcontrol_spec.rds")
saveRDS(glm_time_mod_selfcontrol, "models/moderation/glm_time_mod_selfcontrol.rds")

time_selfcontrol_model_comparison <- bind_rows(
  check_glmer_model(glm_time_original_selfcontrol_spec, "original"),
  check_glmer_model(glm_time_mod_selfcontrol, "moderated")
) %>%
  mutate(
    delta_AIC_vs_original = AIC - AIC[model == "original"],
    delta_BIC_vs_original = BIC - BIC[model == "original"]
  )

print(time_selfcontrol_model_comparison)

write_csv(
  time_selfcontrol_model_comparison,
  "tables/moderation/time_mod_selfcontrol_model_comparison.csv"
)

tidy_time_mod_selfcontrol <- fit_tidy(glm_time_mod_selfcontrol)

time_selfcontrol_effects <- extract_moderated_change_delta(
  model = glm_time_mod_selfcontrol,
  moderator = "self_control_index_s",
  moderator_label = "Self-control",
  mod_values = c(-1, 0, 1),
  mod_labels = c("Low", "Average", "High")
)

time_selfcontrol_moderation_terms <- extract_moderation_terms(
  glm_time_mod_selfcontrol,
  "self_control_index_s",
  "Self-control"
)

save_model_table(
  tidy_time_mod_selfcontrol,
  "time_mod_selfcontrol_coefficients",
  "tables/moderation",
  caption = "Daily time moderation by self-control",
  label = "tab:time_mod_selfcontrol_coef"
)

save_model_table(
  time_selfcontrol_effects,
  "time_mod_selfcontrol_condition_effects",
  "tables/moderation",
  caption = "Daily time: baseline-to-intervention change by self-control level and condition",
  label = "tab:time_mod_selfcontrol_effects"
)

save_model_table(
  time_selfcontrol_moderation_terms,
  "time_mod_selfcontrol_interaction_terms",
  "tables/moderation",
  caption = "Daily time: self-control moderation terms",
  label = "tab:time_mod_selfcontrol_interactions"
)

write_csv(tidy_time_mod_selfcontrol, "tables/moderation/time_mod_selfcontrol_coefficients.csv")
write_csv(time_selfcontrol_effects, "tables/moderation/time_mod_selfcontrol_condition_effects.csv")
write_csv(time_selfcontrol_moderation_terms, "tables/moderation/time_mod_selfcontrol_interaction_terms.csv")

print(validate_mod_table(time_selfcontrol_effects))

p_time_selfcontrol <- make_mod_figure(
  time_selfcontrol_effects,
  "Daily time by self-control"
)

ggsave(
  "figures/moderation/time_mod_selfcontrol_effects.png",
  p_time_selfcontrol,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/moderation/time_mod_selfcontrol_effects.svg",
  p_time_selfcontrol,
  width = 8,
  height = 5,
  bg = "white"
)

# =============================================================================
# 6. COMBINED OUTPUTS
# =============================================================================

time_moderation_effects <- bind_rows(
  time_addiction_effects %>% mutate(outcome = "Daily time"),
  time_selfcontrol_effects %>% mutate(outcome = "Daily time")
)

time_moderation_terms <- bind_rows(
  time_addiction_moderation_terms %>% mutate(outcome = "Daily time"),
  time_selfcontrol_moderation_terms %>% mutate(outcome = "Daily time")
)

write_csv(
  time_moderation_effects,
  "tables/moderation/time_moderation_condition_effects_combined.csv"
)

write_csv(
  time_moderation_terms,
  "tables/moderation/time_moderation_interaction_terms_combined.csv"
)

message("05a_moderation_daily_time_addiction_selfcontrol.R complete")