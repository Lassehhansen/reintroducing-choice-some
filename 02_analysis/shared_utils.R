Sys.setenv(TZ = "Europe/Copenhagen")
# =============================================================================
# shared_utils.R
#
# SOURCE THIS AT THE TOP OF EVERY ANALYSIS SCRIPT:
#   source("02_analysis/shared_utils.R")
#
# What this file provides:
#   - Libraries
#   - Colour palette and condition labels
#   - make_output_dirs()
#   - add_break_covariates()
#   - add_klassetrin_short2()
#   - factorise_model_data()
#   - apply_rolling_smooth()     -- rolling MEAN  (time, session duration)
#   - apply_rolling_median()     -- rolling MEDIAN (opens / count outcomes)
#   - make_time3()
#   - custom_general_theme_word()
#   - extract_change_delta_method()
#   - format_rr_report()
#   - save_model_table()
#   - save_figure()
#
# What this file does NOT do:
#   - Load data          (do that explicitly in each script)
#   - Join data          (do that explicitly in each script)
#   - Assert anything    (read the error R gives you directly)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(lme4)
  library(lmerTest)
  library(glmmTMB)
  library(broom.mixed)
  library(ggeffects)
  library(splines)
  library(zoo)
  library(patchwork)
  library(knitr)
  library(kableExtra)
  library(writexl)
  library(showtext)
  library(sysfonts)
})

# -- Fonts ---------------------------------------------------------------------
if (file.exists("kfst_fonts/FrutigerNextPro-Regular.otf")) {
  font_add("FrutigerNextPro",     "kfst_fonts/FrutigerNextPro-Regular.otf")
  font_add("FrutigerNextProBold", "kfst_fonts/FrutigerNextPro-Bold.otf")
  showtext_auto()
  BODY_FONT <- "FrutigerNextPro"
  BOLD_FONT <- "FrutigerNextProBold"
} else {
  BODY_FONT <- "sans"
  BOLD_FONT <- "sans"
}

# -- Colours and labels --------------------------------------------------------
COND_COLOURS <- c(
  "control"      = "#737373",
  "intervention" = "#9e0b1d",
  "default"      = "#8dadbf",
  "Reflection"   = "#737373",
  "Planning"     = "#9e0b1d",
  "Waiting"      = "#8dadbf"
)

COND_LABELS <- c(
  "control"      = "Reflection",
  "intervention" = "Planning",
  "default"      = "Waiting"
)

COND_LEVELS_ENG <- c("Reflection", "Planning", "Waiting")
COND_LEVELS_RAW <- c("control", "intervention", "default")

# -- Directory helper ----------------------------------------------------------
make_output_dirs <- function(...) {
  for (p in c(...)) dir.create(p, showWarnings = FALSE, recursive = TRUE)
}

# -- Break covariates ----------------------------------------------------------
# Adds: day_of_week, weekend, fall_break, christmas_break
# date_col must be the name of the date column in df (default "date")
add_break_covariates <- function(df, date_col = "date") {
  df %>%
    mutate(
      date            = as.Date(.data[[date_col]], tz = "Europe/Copenhagen"),
      day_of_week     = weekdays(date),
      weekend         = as.integer(day_of_week %in% c("Saturday", "Sunday")),
      fall_break      = as.integer(
        date >= as.Date("2024-10-14") & date <= as.Date("2024-10-18")
      ),
      christmas_break = as.integer(
        date >= as.Date("2024-12-23") & date <= as.Date("2025-01-02")
      )
    )
}

# -- Klassetrin recoding -------------------------------------------------------
# Requires a column named `klassetrin` to already be in df
add_klassetrin_short2 <- function(df) {
  df %>%
    mutate(
      klassetrin_short2 = case_when(
        klassetrin %in% c("7. klasse", "8. klasse",
                          "9. klasse", "10. klasse")           ~ "Primary Education",
        klassetrin %in% c("1. år på ungdomsuddannelse",
                          "2. år på ungdomsuddannelse")        ~ "Secondary Education",
        klassetrin == "Efterskole"                              ~ "Boarding School",
        TRUE                                                    ~ "Others"
      ),
      klassetrin_short2 = factor(
        klassetrin_short2,
        levels = c("Primary Education", "Secondary Education",
                   "Boarding School", "Others")
      )
    )
}

# -- Factor levels for modelling -----------------------------------------------
factorise_model_data <- function(df) {
  df %>%
    mutate(
      day_of_week = factor(day_of_week,
                           levels = c("Monday", "Tuesday", "Wednesday",
                                      "Thursday", "Friday", "Saturday", "Sunday")),
      period      = factor(period,  levels = c("baseline", "mixed", "intervention")),
      period2     = factor(period2, levels = c("baseline", "intervention")),
      gender      = factor(gender,  levels = c("mand", "kvinde")),
      region      = as.factor(region),
      klassetrin_short2 = factor(
        klassetrin_short2,
        levels = c("Primary Education", "Secondary Education",
                   "Boarding School", "Others")
      ),
      weekend         = as.factor(weekend),
      fall_break      = as.factor(fall_break),
      christmas_break = as.factor(christmas_break),
      ID              = as.factor(ID),
      ID3             = as.numeric(as.factor(ID)),
      ID2             = as.factor(ID3),
      condition       = factor(condition, levels = COND_LEVELS_RAW)
    )
}

# -- Rolling 3-day MEAN smooth -------------------------------------------------
# Use for continuous outcomes: total time, session duration.
# value_col: column to smooth (character string)
# out_col:   name for the new column (defaults to value_col + "_smooth")
apply_rolling_smooth <- function(df, value_col, out_col = NULL) {
  if (is.null(out_col)) out_col <- paste0(value_col, "_smooth")
  df %>%
    group_by(ID2, period2) %>%
    arrange(days_before_activation_num, .by_group = TRUE) %>%
    mutate(!!out_col := zoo::rollmean(.data[[value_col]], k = 3,
                                      fill = NA, align = "right")) %>%
    ungroup()
}

# -- Rolling 3-day MEDIAN smooth -----------------------------------------------
# Use for count outcomes: daily opens, burst counts.
# Rolling median is more appropriate than rolling mean for count data because
# it is robust to the spikes that are common in discrete count series, and it
# preserves the integer-like character of the values better.
# value_col: column to smooth (character string)
# out_col:   name for the new column (defaults to value_col + "_smooth")
apply_rolling_median <- function(df, value_col, out_col = NULL) {
  if (is.null(out_col)) out_col <- paste0(value_col, "_smooth")
  df %>%
    group_by(ID2, period2) %>%
    arrange(days_before_activation_num, .by_group = TRUE) %>%
    mutate(!!out_col := zoo::rollmedian(.data[[value_col]], k = 3,
                                        fill = NA, align = "right")) %>%
    ungroup()
}

# -- Time index for spline -----------------------------------------------------
# Creates time2 and time3 from days_before_activation_num
make_time3 <- function(df) {
  df %>%
    mutate(
      time2 = ifelse(days_before_activation_num < 0,
                     days_before_activation_num,
                     ifelse(days_before_activation_num > 0,
                            days_before_activation_num - 3,
                            NA)),
      time3 = time2 + 11
    )
}

# -- ggplot theme --------------------------------------------------------------
custom_general_theme_word <- function() {
  theme(
    panel.border       = element_blank(),
    axis.line.y        = element_blank(),
    axis.line.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    axis.text.y        = element_text(size = 12, family = BODY_FONT,
                                      color = "#000000", face = "bold"),
    axis.text.x        = element_text(size = 12, family = BODY_FONT,
                                      color = "#000000", face = "bold"),
    panel.grid.major.y = element_line(color = "#000000", linetype = "solid"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.title.x       = element_text(size = 12, family = BODY_FONT,
                                      color = "#000000", face = "bold"),
    axis.title.y       = element_blank(),
    plot.margin        = margin(t = 0, r = 12, b = 12, l = 5),
    legend.position    = "none",
    plot.title         = element_text(hjust = 0.5, size = 14,
                                      family = BODY_FONT, face = "bold"),
    strip.text         = element_text(size = 14, family = BODY_FONT,
                                      face = "bold", color = "#000000"),
    strip.background   = element_rect(fill = "white", color = NA)
  )
}

# -- Delta-method condition effects --------------------------------------------
# Returns a tibble with one row per condition (Reflection / Planning / Waiting)
# and columns: condition, estimate_log, se_log, conf.low_log, conf.high_log,
#              rr, rr_low, rr_high, pct, pct_low, pct_high, z, p
extract_change_delta_method <- function(model) {
  b <- if (inherits(model, "glmmTMB")) fixef(model)$cond else fixef(model)
  V <- if (inherits(model, "glmmTMB")) as.matrix(vcov(model)$cond) else as.matrix(vcov(model))
  nms <- names(b)

  L <- matrix(0, nrow = 3, ncol = length(b),
               dimnames = list(c("Reflection", "Planning", "Waiting"), nms))

  L["Reflection", "period2intervention"] <- 1
  L["Planning",   "period2intervention"] <- 1
  L["Waiting",    "period2intervention"] <- 1

  int_plan <- intersect(c("period2intervention:conditionintervention",
                           "conditionintervention:period2intervention"), nms)[1]
  int_wait <- intersect(c("period2intervention:conditiondefault",
                           "conditiondefault:period2intervention"), nms)[1]

  if (!is.na(int_plan)) L["Planning", int_plan] <- 1
  if (!is.na(int_wait)) L["Waiting",  int_wait] <- 1

  est <- as.numeric(L %*% b)
  se  <- sqrt(diag(L %*% V %*% t(L)))

  tibble(
    condition     = rownames(L),
    estimate_log  = est,
    se_log        = se,
    conf.low_log  = est - 1.96 * se,
    conf.high_log = est + 1.96 * se,
    rr            = exp(est),
    rr_low        = exp(est - 1.96 * se),
    rr_high       = exp(est + 1.96 * se),
    pct           = 100 * (rr - 1),
    pct_low       = 100 * (rr_low - 1),
    pct_high      = 100 * (rr_high - 1),
    z             = est / se,
    p             = 2 * pnorm(-abs(z))
  )
}

# -- Paper-ready effect report string -----------------------------------------
format_rr_report <- function(change_df) {
  change_df %>%
    mutate(
      report = sprintf(
        "%s: RR = %.2f, 95%% CI [%.2f, %.2f]; log-est = %.3f, SE = %.3f, z = %.2f, p %s",
        condition, rr, rr_low, rr_high, estimate_log, se_log, z,
        if_else(p < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", p)))
      )
    )
}

# -- Table export: Excel + LaTeX -----------------------------------------------
save_model_table <- function(tbl, stem, dir,
                             caption = stem,
                             label   = paste0("tab:", stem)) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  xlsx_path <- file.path(dir, paste0(stem, ".xlsx"))
  write_xlsx(tbl, xlsx_path)

  tex_path <- file.path(dir, paste0(stem, ".tex"))
  latex_tbl <- tbl %>%
    kable(format = "latex", booktabs = TRUE, digits = 3,
          caption = caption, label = label) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
  writeLines(as.character(latex_tbl), tex_path)

  message("  Saved: ", xlsx_path, " | ", tex_path)
  invisible(list(xlsx = xlsx_path, tex = tex_path))
}

# -- Figure export: PNG + SVG --------------------------------------------------
save_figure <- function(plot, stem, dir,
                        width  = 1200 / 96,
                        height = 500  / 96,
                        dpi    = 300) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  png_path <- file.path(dir, paste0(stem, ".png"))
  svg_path <- file.path(dir, paste0(stem, ".svg"))

  ggsave(png_path, plot, width = width, height = height, dpi = dpi, units = "in")
  ggsave(svg_path, plot, width = width, height = height, units = "in", device = "svg")

  message("  Saved: ", png_path, " | ", svg_path)
  invisible(list(png = png_path, svg = svg_path))
}

# -- Extract one coefficient row from a tidy model table ----------------------
# Returns a single-row tibble with beta, se, z, p, lo, hi.
# If the term is not found, returns zeros so arithmetic downstream still works.
get_term_full <- function(tidy_df, term_name) {
  out <- tidy_df %>%
    filter(term == term_name) %>%
    transmute(
      beta = estimate,
      se   = std.error,
      z    = statistic,
      p    = p.value,
      lo   = conf.low,
      hi   = conf.high
    )
  if (nrow(out) == 0)
    tibble(beta = 0, se = 0, z = 0, p = NA_real_, lo = 0, hi = 0)
  else
    out
}

# -- Extract period change by condition (ITS models) --------------------------
# For Gamma / NB2 ITS models with period2 * condition interaction.
# Returns rate ratios (rr) and % change for each condition.
extract_period_change_by_condition <- function(model, app_label = NA_character_) {
  tidy <- broom.mixed::tidy(model, conf.int = TRUE)

  bP     <- get_term_full(tidy, "period2intervention")
  bP_int <- get_term_full(tidy, "period2intervention:conditionintervention")
  bP_def <- get_term_full(tidy, "period2intervention:conditiondefault")

  bind_rows(
    tibble(
      app           = app_label,
      condition     = "control",
      condition2    = "Reflection",
      estimate_log  = bP$beta,
      se_log        = bP$se,
      z             = bP$z,
      p_value       = bP$p,
      conf.low_log  = bP$lo,
      conf.high_log = bP$hi
    ),
    tibble(
      app           = app_label,
      condition     = "intervention",
      condition2    = "Planning",
      estimate_log  = bP$beta + bP_int$beta,
      se_log        = sqrt(bP$se^2 + bP_int$se^2),
      z             = (bP$beta + bP_int$beta) / sqrt(bP$se^2 + bP_int$se^2),
      p_value       = 2 * pnorm(-abs(
        (bP$beta + bP_int$beta) / sqrt(bP$se^2 + bP_int$se^2)
      )),
      conf.low_log  = bP$lo + bP_int$lo,
      conf.high_log = bP$hi + bP_int$hi
    ),
    tibble(
      app           = app_label,
      condition     = "default",
      condition2    = "Waiting",
      estimate_log  = bP$beta + bP_def$beta,
      se_log        = sqrt(bP$se^2 + bP_def$se^2),
      z             = (bP$beta + bP_def$beta) / sqrt(bP$se^2 + bP_def$se^2),
      p_value       = 2 * pnorm(-abs(
        (bP$beta + bP_def$beta) / sqrt(bP$se^2 + bP_def$se^2)
      )),
      conf.low_log  = bP$lo + bP_def$lo,
      conf.high_log = bP$hi + bP_def$hi
    )
  ) %>%
    mutate(
      rr       = exp(estimate_log),
      rr_low   = exp(conf.low_log),
      rr_high  = exp(conf.high_log),
      pct      = 100 * (rr - 1),
      pct_low  = 100 * (rr_low - 1),
      pct_high = 100 * (rr_high - 1),
      p_value  = round(p_value, 4),
      condition2 = factor(condition2, levels = COND_LEVELS_ENG)
    )
}

# -- Extract dismissal probability by condition (binomial models) -------------
# For binomial glmer where the intercept = control log-odds.
# Returns absolute dismissal probability (prob) for each condition.
extract_regret_by_condition <- function(model, app_label = NA_character_) {
  tidy <- broom.mixed::tidy(model, conf.int = TRUE)

  bI    <- get_term_full(tidy, "(Intercept)")
  bPlan <- get_term_full(tidy, "conditionintervention")
  bWait <- get_term_full(tidy, "conditiondefault")

  bind_rows(
    tibble(
      app           = app_label,
      condition     = "control",
      condition2    = "Reflection",
      estimate_log  = bI$beta,
      se_log        = bI$se,
      z             = bI$z,
      p_value       = bI$p,
      conf.low_log  = bI$lo,
      conf.high_log = bI$hi
    ),
    tibble(
      app           = app_label,
      condition     = "intervention",
      condition2    = "Planning",
      estimate_log  = bI$beta + bPlan$beta,
      se_log        = sqrt(bI$se^2 + bPlan$se^2),
      z             = (bI$beta + bPlan$beta) / sqrt(bI$se^2 + bPlan$se^2),
      p_value       = 2 * pnorm(-abs(
        (bI$beta + bPlan$beta) / sqrt(bI$se^2 + bPlan$se^2)
      )),
      conf.low_log  = bI$lo + bPlan$lo,
      conf.high_log = bI$hi + bPlan$hi
    ),
    tibble(
      app           = app_label,
      condition     = "default",
      condition2    = "Waiting",
      estimate_log  = bI$beta + bWait$beta,
      se_log        = sqrt(bI$se^2 + bWait$se^2),
      z             = (bI$beta + bWait$beta) / sqrt(bI$se^2 + bWait$se^2),
      p_value       = 2 * pnorm(-abs(
        (bI$beta + bWait$beta) / sqrt(bI$se^2 + bWait$se^2)
      )),
      conf.low_log  = bI$lo + bWait$lo,
      conf.high_log = bI$hi + bWait$hi
    )
  ) %>%
    mutate(
      prob      = plogis(estimate_log),
      prob_low  = plogis(conf.low_log),
      prob_high = plogis(conf.high_log),
      pct       = 100 * prob,
      pct_low   = 100 * prob_low,
      pct_high  = 100 * prob_high,
      p_value   = round(p_value, 4),
      condition2 = factor(condition2, levels = COND_LEVELS_ENG)
    )
}


get_term <- function(df, term_name) {
  out <- df %>% filter(term == term_name) %>% transmute(beta = estimate, lo = conf.low, hi = conf.high)
  if (nrow(out) == 0) tibble(beta = 0, lo = 0, hi = 0) else out
}



extract_change_delta_method <- function(model, app_label = NA_character_) {
  b <- if (inherits(model, "glmmTMB")) {
    fixef(model)$cond
  } else {
    fixef(model)
  }
  
  V <- if (inherits(model, "glmmTMB")) {
    as.matrix(vcov(model)$cond)
  } else {
    as.matrix(vcov(model))
  }
  
  nms <- names(b)
  
  if (!"period2intervention" %in% nms) {
    stop(
      "`period2intervention` not found. ",
      "`extract_change_delta_method()` is only for baseline/intervention ITS models.",
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
  L["Planning",   "period2intervention"] <- 1
  L["Waiting",    "period2intervention"] <- 1
  
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
  
  tibble(
    app = app_label,
    condition2 = rownames(L),
    condition = recode(
      condition2,
      "Reflection" = "control",
      "Planning" = "intervention",
      "Waiting" = "default"
    ),
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
    z = estimate_log / se_log,
    p_value = 2 * pnorm(-abs(z)),
    significant = rr_low > 1 | rr_high < 1
  ) %>%
    mutate(
      condition2 = factor(
        condition2,
        levels = c("Reflection", "Planning", "Waiting")
      )
    )
}
