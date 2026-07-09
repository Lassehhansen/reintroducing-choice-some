# =============================================================================
# 03_figures/figure1_main_figure.R
#
# Fixed for the current pipeline:
#   - reads the three saved main models from the new main-effect scripts
#   - rebuilds Figure 1B data robustly from extract_change_delta_method()
#   - standardizes column names across time / opens / session outputs
#   - uses manuscript-aligned Figure 1B labels:
#       Daily social media time
#       Daily app-opening attempts
#       Average session duration
#   - uses "Ratio" rather than "Rate Ratio" because total time and session
#     duration are not rates
#   - rebuilds dismissal Panel C with the current participants/covariates pipeline
# =============================================================================

Sys.setenv(TZ = "Europe/Copenhagen")

source("02_analysis/shared_utils.R")

library(tidyverse)
library(lubridate)
library(lme4)
library(splines)
library(ggeffects)
library(broom.mixed)
library(patchwork)

make_output_dirs(
  "figures/main",
  "tables/main"
)

# =============================================================================
# 1. LOAD SAVED MAIN MODELS
# =============================================================================

glm_time_model <- readRDS(
  "models/main/glm_sumtime_model.rds"
)

glm_opens_model <- readRDS(
  "models/main/glm_opens_model.rds"
)

glm_session_model <- readRDS(
  "models/main/glm_session_duration_model.rds"
)

# =============================================================================
# 2. ROBUST FIGURE-1B DATA CREATION
# =============================================================================

figure1b_facet_levels <- c(
  "Avg. session duration",
  "Daily openings",
  "Daily time"
)

standardize_change_table <- function(df, facet_label) {
  
  df <- df %>%
    as_tibble()
  
  if (!"condition2" %in% names(df)) {
    if ("condition" %in% names(df)) {
      df <- df %>%
        rename(condition2 = condition)
    } else {
      stop("No condition or condition2 column found in extracted effect table.", call. = FALSE)
    }
  }
  
  if (!"condition" %in% names(df)) {
    df <- df %>%
      mutate(
        condition = recode(
          as.character(condition2),
          "Reflection" = "control",
          "Planning" = "intervention",
          "Waiting" = "default",
          .default = as.character(condition2)
        )
      )
  }
  
  if (!"p_value" %in% names(df) && "p" %in% names(df)) {
    df <- df %>%
      rename(p_value = p)
  }
  
  if (!"p" %in% names(df) && "p_value" %in% names(df)) {
    df <- df %>%
      mutate(p = p_value)
  }
  
  if (!"rr_low" %in% names(df) && "rr_lo" %in% names(df)) {
    df <- df %>%
      rename(rr_low = rr_lo)
  }
  
  if (!"rr_high" %in% names(df) && "rr_hi" %in% names(df)) {
    df <- df %>%
      rename(rr_high = rr_hi)
  }
  
  if (!"pct" %in% names(df)) {
    df <- df %>%
      mutate(pct = 100 * (rr - 1))
  }
  
  if (!"pct_low" %in% names(df)) {
    df <- df %>%
      mutate(pct_low = 100 * (rr_low - 1))
  }
  
  if (!"pct_high" %in% names(df)) {
    df <- df %>%
      mutate(pct_high = 100 * (rr_high - 1))
  }
  
  if (!"estimate_log" %in% names(df) && "log_est" %in% names(df)) {
    df <- df %>%
      rename(estimate_log = log_est)
  }
  
  if (!"se_log" %in% names(df) && "log_se" %in% names(df)) {
    df <- df %>%
      rename(se_log = log_se)
  }
  
  if (!"conf.low_log" %in% names(df) && "log_lo" %in% names(df)) {
    df <- df %>%
      rename(conf.low_log = log_lo)
  }
  
  if (!"conf.high_log" %in% names(df) && "log_hi" %in% names(df)) {
    df <- df %>%
      rename(conf.high_log = log_hi)
  }
  
  df %>%
    mutate(
      facet = facet_label,
      condition2 = case_when(
        as.character(condition2) %in% c("Reflection", "Planning", "Waiting") ~ as.character(condition2),
        as.character(condition2) == "control" ~ "Reflection",
        as.character(condition2) == "intervention" ~ "Planning",
        as.character(condition2) == "default" ~ "Waiting",
        condition == "control" ~ "Reflection",
        condition == "intervention" ~ "Planning",
        condition == "default" ~ "Waiting",
        TRUE ~ as.character(condition2)
      ),
      condition2 = factor(
        condition2,
        levels = c("Reflection", "Planning", "Waiting")
      ),
      condition = factor(
        condition,
        levels = c("control", "intervention", "default")
      ),
      facet = factor(
        facet,
        levels = figure1b_facet_levels
      ),
      significant = rr_low > 1 | rr_high < 1
    ) %>%
    dplyr::select(
      facet,
      condition,
      condition2,
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
      p,
      p_value,
      significant
    )
}

time_change <- extract_change_delta_method(glm_time_model) %>%
  standardize_change_table("Daily time")

opens_change <- extract_change_delta_method(glm_opens_model) %>%
  standardize_change_table("Daily openings")

session_change <- extract_change_delta_method(glm_session_model) %>%
  standardize_change_table("Avg. session duration")

vis_together <- bind_rows(
  time_change,
  opens_change,
  session_change
)

write_csv(
  vis_together,
  "tables/main/vis_together_main_effects.csv"
)

save_model_table(
  vis_together,
  "vis_together_main_effects",
  "tables/main",
  caption = "Main effect estimates used in Figure 1B forest plot",
  label = "tab:vis_together"
)

cat("\n=== MAIN EFFECTS SUMMARY USED IN FIGURE 1B ===\n")

vis_together %>%
  dplyr::select(
    facet,
    condition2,
    rr,
    rr_low,
    rr_high,
    pct,
    pct_low,
    pct_high,
    z,
    p_value
  ) %>%
  arrange(facet, condition2) %>%
  print(n = Inf)

validation_vis_together <- vis_together %>%
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

print(validation_vis_together, n = Inf)

write_csv(
  validation_vis_together,
  "tables/main/vis_together_main_effects_validation_flags.csv"
)

# =============================================================================
# 3. PANEL A: DAILY TIME ITS TRAJECTORY
# =============================================================================

p1 <- predict_response(
  glm_time_model,
  terms = c("time3[all]", "period2", "condition"),
  margin = "empirical",
  type = "response",
  ci_level = 0.95,
  interval = "prediction"
)

p1_df <- as.data.frame(p1) %>%
  as_tibble() %>%
  filter(
    (x <= 10 & group == "baseline") |
      (x > 10 & group == "intervention")
  )

p_sum_time_model_2 <- ggplot(
  p1_df,
  aes(
    x = x,
    y = predicted,
    group = factor(facet)
  )
) +
  geom_rect(
    aes(
      xmin = 10,
      xmax = 11,
      ymin = -Inf,
      ymax = 240
    ),
    fill = "#DADADA",
    alpha = 0.05
  ) +
  geom_segment(
    aes(
      x = 10,
      xend = 10,
      y = -Inf,
      yend = 240
    ),
    linetype = "dashed",
    color = "#000000",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(
      x = 11,
      xend = 11,
      y = -Inf,
      yend = 240
    ),
    linetype = "dashed",
    color = "#000000",
    linewidth = 0.5
  ) +
  geom_point(
    aes(color = factor(facet)),
    size = 1.5
  ) +
  geom_line(
    aes(color = factor(facet)),
    linewidth = 0.8
  ) +
  geom_ribbon(
    aes(
      ymin = conf.low,
      ymax = conf.high,
      fill = factor(facet)
    ),
    alpha = 0.1
  ) +
  geom_text(
    data = p1_df %>% filter(x == max(x), facet == "control"),
    aes(
      label = "Reflection",
      color = factor(facet)
    ),
    hjust = -0.2,
    vjust = 0,
    size = 4,
    show.legend = FALSE,
    family = BOLD_FONT,
    fontface = "bold"
  ) +
  geom_text(
    data = p1_df %>% filter(x == max(x), facet == "default"),
    aes(
      label = "Waiting",
      color = factor(facet)
    ),
    hjust = -0.3,
    vjust = 0.35,
    size = 4,
    show.legend = FALSE,
    family = BOLD_FONT,
    fontface = "bold"
  ) +
  geom_text(
    data = p1_df %>% filter(x == max(x), facet == "intervention"),
    aes(
      label = "Planning",
      color = factor(facet)
    ),
    hjust = -0.25,
    vjust = -0.2,
    size = 4,
    show.legend = FALSE,
    family = BOLD_FONT,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 10.5,
    y = 255,
    label = "Intervention starts",
    color = "#000000",
    size = 4.4,
    family = BOLD_FONT,
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = c(1, 11, 21, 31),
    labels = function(x) paste0("Day ", x + 4),
    expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = ""
  ) +
  scale_y_continuous(
    limits = c(30, 257),
    breaks = seq(60, 240, by = 60),
    labels = c("1 hour", "2 hours", "3 hours", "4 hours")
  ) +
  scale_color_manual(
    values = c(
      "control" = "#737373",
      "intervention" = "#9e0b1d",
      "default" = "#8dadbf"
    ),
    breaks = c("control", "intervention", "default"),
    labels = c("Reflection", "Planning", "Waiting"),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      "control" = 16,
      "intervention" = 16,
      "default" = 16
    ),
    breaks = c("control", "intervention", "default"),
    labels = c("Reflection", "Planning", "Waiting"),
    name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "control" = "#737373",
      "intervention" = "#9e0b1d",
      "default" = "#8dadbf"
    ),
    breaks = c("control", "intervention", "default"),
    labels = c("Reflection", "Planning", "Waiting"),
    name = NULL
  ) +
  guides(
    color = guide_legend(override.aes = list(shape = 16, fill = NA)),
    shape = "none",
    fill = "none"
  ) +
  coord_cartesian(
    xlim = c(0, 36),
    clip = "off"
  ) +
  custom_general_theme_word() +
  theme(
    plot.margin = margin(t = 10, r = 80, b = 0, l = -10),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.ticks.y = element_line(color = "#000000"),
    axis.line.x = element_line(color = "#000000"),
    axis.text.y = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.text.x = element_text(
      size = 12,
      family = BODY_FONT,
      color = "#000000",
      margin = margin(t = 5, r = 0, b = -10, l = 0)
    ),
    panel.grid.major.y = element_line(
      color = "#000000",
      linewidth = 0.05
    ),
    legend.position = "bottom",
    legend.text = element_text(
      size = 14,
      family = BODY_FONT,
      color = "#000000"
    ),
    legend.box.margin = margin(t = -5, b = 15)
  )

# =============================================================================
# 4. PANEL B: ESTIMATED INTERVENTION-VERSUS-BASELINE RATIOS
# =============================================================================

p_effect_v <- vis_together %>%
  mutate(
    facet = factor(
      facet,
      levels = figure1b_facet_levels
    ),
    condition2 = factor(
      condition2,
      levels = c("Reflection", "Planning", "Waiting")
    )
  ) %>%
  ggplot(
    aes(
      x = rr,
      y = facet,
      color = condition2
    )
  ) +
  geom_errorbarh(
    aes(
      xmin = rr_low,
      xmax = rr_high
    ),
    height = 0.3,
    linewidth = 0.5,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 3,
    position = position_dodge(width = 0.55)
  ) +
  scale_color_manual(
    values = c(
      "Reflection" = "#737373",
      "Planning" = "#9e0b1d",
      "Waiting" = "#8dadbf"
    )
  ) +
  labs(
    y = "",
    x = "Ratio",
    color = ""
  ) +
  scale_x_log10(
    limits = c(0.2, 4),
    breaks = c(0.2, 0.5, 1, 2, 4),
    labels = c("0.2", "0.5", "1.0", "2.0", "4.0"),
    expand = c(0, 0)
  ) +
  geom_vline(
    xintercept = 1,
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = c(1.5, 2.5, 3.5),
    linewidth = 0.05
  ) +
  custom_general_theme_word() +
  theme(
    legend.position = "none",
    legend.text = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    strip.text = element_text(
      size = 13,
      family = BOLD_FONT,
      color = "#000000",
      margin = margin(b = 10),
      hjust = 0.52
    ),
    axis.text.y = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.text.x = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.title.x = element_text(
      size = 14,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.title = element_text(
      size = 12,
      family = BODY_FONT
    ),
    axis.line.x = element_line(
      color = "#000000",
      linetype = "solid",
      linewidth = 0.3
    ),
    axis.line.y = element_blank(),
    plot.margin = margin(r = 10, l = -10, t = -10),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    strip.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# =============================================================================
# 5. PANEL C: DISMISSAL PROBABILITY
# =============================================================================

intervention_app <- read_csv(
  "data/regret_rate/intervention_app_for_regret.csv",
  show_col_types = FALSE
)

participants <- read_csv(
  "data/participants.csv",
  show_col_types = FALSE
) %>%
  dplyr::select(
    -any_of(c(
      "condition",
      "condition.x",
      "condition.y",
      "condition_x",
      "condition_y",
      "cond"
    ))
  )

if (!"days_before_activation_num" %in% names(intervention_app)) {
  if ("days_before_activation" %in% names(intervention_app)) {
    intervention_app <- intervention_app %>%
      mutate(days_before_activation_num = as.numeric(days_before_activation))
  } else {
    stop(
      "Neither days_before_activation_num nor days_before_activation exists in intervention_app_for_regret.csv.",
      call. = FALSE
    )
  }
}

if (!"date" %in% names(intervention_app)) {
  intervention_app <- intervention_app %>%
    mutate(
      time = with_tz(as.POSIXct(time, tz = "UTC"), tzone = "Europe/Copenhagen"),
      date = as.Date(time, tz = "Europe/Copenhagen")
    )
}

regret_data_merge <- intervention_app %>%
  filter(
    #isReIntervention == 0,
    !is.na(interventionType),
    interventionType != "",
    !is.na(condition),
    resolution %in% c("dismissedAppOpening", "openedApp")
  ) %>%
  mutate(
    regret = as.integer(resolution == "dismissedAppOpening"),
    date = as.Date(date, tz = "Europe/Copenhagen"),
    day_of_week = weekdays(date),
    weekend = as.factor(as.integer(day_of_week %in% c("Saturday", "Sunday"))),
    fall_break = as.factor(as.integer(
      date >= as.Date("2024-10-14") &
        date <= as.Date("2024-10-18")
    )),
    condition = factor(
      condition,
      levels = c("control", "intervention", "default")
    ),
    ID = as.factor(ID),
    app = as.factor(app)
  ) %>%
  left_join(
    participants,
    by = "ID"
  ) %>%
  add_klassetrin_short2() %>%
  mutate(
    gender = factor(
      gender,
      levels = c("mand", "kvinde")
    ),
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
    klassetrin_short2,
    weekend,
    fall_break,
    region,
    ID,
    app
  )

dismissal_model_audit <- intervention_app %>%
  mutate(
    is_reintervention = case_when(
      isReIntervention %in% c(TRUE, "TRUE", "true", "1", 1) ~ 1L,
      isReIntervention %in% c(FALSE, "FALSE", "false", "0", 0) ~ 0L,
      is.na(isReIntervention) ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(
    !is.na(interventionType),
    interventionType != "",
    !is.na(condition),
    resolution %in% c("dismissedAppOpening", "openedApp")
  ) %>%
  count(condition, is_reintervention, resolution, name = "n")

write_csv(
  dismissal_model_audit,
  "tables/main/dismissal_model_reintervention_audit.csv"
)

message(
  "Dismissal model data: ",
  nrow(regret_data_merge),
  " rows, ",
  n_distinct(regret_data_merge$ID),
  " participants"
)

regret_model_simple_glmer_v2 <- glmer(
  regret ~
    condition +
    ns(days_before_activation_num, df = 2):condition +
    klassetrin_short2 +
    weekend +
    fall_break +
    region +
    (1 | ID) +
    (1 | app),
  family = binomial(),
  data = regret_data_merge,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 1e5)
  )
)

saveRDS(
  regret_model_simple_glmer_v2,
  "models/main/regret_model_figure1.rds"
)

regret_model_simple_glmer_v2_tidy <- broom.mixed::tidy(
  regret_model_simple_glmer_v2,
  conf.int = TRUE
)

save_model_table(
  regret_model_simple_glmer_v2_tidy,
  "dismissal_model_coefficients",
  "tables/main",
  caption = "Dismissal probability model used in Figure 1C",
  label = "tab:dismissal_coef"
)

regret_report <- regret_model_simple_glmer_v2_tidy %>%
  filter(
    term %in% c(
      "conditionintervention",
      "conditiondefault"
    )
  ) %>%
  transmute(
    condition = recode(
      term,
      "conditionintervention" = "Planning vs Reflection",
      "conditiondefault" = "Waiting vs Reflection"
    ),
    beta = estimate,
    se = std.error,
    z = statistic,
    p = p.value,
    odds_ratio = exp(estimate),
    odds_ratio_low = exp(conf.low),
    odds_ratio_high = exp(conf.high),
    report = sprintf(
      "%s: OR = %.2f, 95%% CI %.2f to %.2f; beta = %.3f, SE = %.3f, z = %.2f, p %s",
      condition,
      odds_ratio,
      odds_ratio_low,
      odds_ratio_high,
      beta,
      se,
      z,
      if_else(
        p < 0.001,
        "< 0.001",
        paste0("= ", sprintf("%.3f", p))
      )
    )
  )

cat("\n=== DISMISSAL PROBABILITY: CONDITION EFFECTS ===\n")
cat(paste(regret_report$report, collapse = "\n"), "\n")

save_model_table(
  regret_report,
  "dismissal_condition_effects",
  "tables/main",
  caption = "Dismissal probability: condition effects",
  label = "tab:dismissal_effects"
)

p_regret <- predict_response(
  regret_model_simple_glmer_v2,
  terms = c("condition"),
  margin = "empirical",
  type = "response",
  ci_level = 0.95,
  interval = "prediction"
)

regret_df <- as.data.frame(p_regret) %>%
  as_tibble() %>%
  transmute(
    condition2 = factor(
      x,
      levels = c("control", "intervention", "default"),
      labels = c("Reflection", "Planning", "Waiting")
    ),
    condition3 = factor(
      x,
      levels = rev(c("control", "intervention", "default")),
      labels = rev(c("Reflection", "Planning", "Waiting"))
    ),
    predicted = predicted,
    conf.low = conf.low,
    conf.high = conf.high,
    pct = 100 * predicted,
    pct_low = 100 * conf.low,
    pct_high = 100 * conf.high
  )

write_csv(
  regret_df,
  "tables/main/figure1_dismissal_predictions.csv"
)

p_regret_effect <- ggplot(
  regret_df,
  aes(
    y = condition3,
    x = pct,
    color = condition3
  )
) +
  geom_errorbarh(
    aes(
      xmin = pct_low,
      xmax = pct_high
    ),
    height = 0.18,
    linewidth = 0.9
  ) +
  geom_point(
    size = 3.0
  ) +
  scale_color_manual(
    values = c(
      "Reflection" = "#737373",
      "Planning" = "#9e0b1d",
      "Waiting" = "#8dadbf"
    )
  ) +
  labs(
    x = "Dismissal probability",
    y = "",
    color = ""
  ) +
  scale_x_continuous(
    limits = c(0, 40),
    breaks = c(0, 10, 20, 30, 40),
    labels = c("0%", "10%", "20%", "30%", "40%"),
    expand = c(0, 0),
    position = "bottom"
  ) +
  geom_hline(
    yintercept = c(1.5, 2.5, 3.5),
    linewidth = 0.05
  ) +
  custom_general_theme_word() +
  theme(
    legend.position = "none",
    legend.text = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    strip.text = element_text(
      size = 13,
      family = BOLD_FONT,
      color = "#000000",
      margin = margin(b = 10),
      hjust = 0.52
    ),
    axis.text.y = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.text.x = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.title.x = element_text(
      size = 14,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.title = element_text(
      size = 12,
      family = BODY_FONT
    ),
    axis.line.x = element_line(
      color = "#000000",
      linetype = "solid",
      linewidth = 0.3
    ),
    axis.line.y = element_blank(),
    plot.margin = margin(r = 10, l = -10, t = -10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# =============================================================================
# 6. COMBINE AND SAVE FIGURE
# =============================================================================

p_effect_v_B <- p_effect_v +
  labs(
    title = "Estimated changes",
    color = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(
      size = 12,
      margin = margin(b = -2),
      family = BOLD_FONT,
      hjust = 0.5
    )
  )

p_regret_effect_C <- p_regret_effect +
  labs(
    title = "Dismissal probability"
  ) +
  theme(
    plot.title = element_text(
      size = 12,
      color = "black",
      margin = margin(b = -2),
      family = BOLD_FONT,
      hjust = 0.5
    )
  )

p_bottom <- (p_effect_v_B | p_regret_effect_C) +
  plot_layout(
    widths = c(0.5, 0.5)
  )

p_ABC <- p_sum_time_model_2 /
  p_bottom +
  plot_layout(
    heights = c(1.8, 1)
  ) +
  plot_annotation(
    tag_levels = "A"
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "pt"),
    plot.background = element_rect(fill = "white", color = NA)
  )

p_ABC

save_figure(
  p_sum_time_model_2,
  "figure1_panel_A_time_trajectory",
  "figures/main",
  width = 1200 / 96,
  height = 500 / 96
)

save_figure(
  p_effect_v_B,
  "figure1_panel_B_effect_forest",
  "figures/main",
  width = 600 / 96,
  height = 400 / 96
)

save_figure(
  p_regret_effect_C,
  "figure1_panel_C_dismissal",
  "figures/main",
  width = 400 / 96,
  height = 300 / 96
)

save_figure(
  p_ABC,
  "figure1_main",
  "figures/main",
  width = 1200 / 96,
  height = 900 / 96
)

ggsave(
  "figures/main/figure1_main.png",
  p_ABC,
  width = 1200 / 96,
  height = 900 / 96,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/main/figure1_main.svg",
  p_ABC,
  width = 1200 / 96,
  height = 900 / 96,
  bg = "white"
)

message("\n=== figure1_main_figure.R complete ===")