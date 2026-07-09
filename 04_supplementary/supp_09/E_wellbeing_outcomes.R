Sys.setenv(TZ = "Europe/Copenhagen")

# =============================================================================
# 04_supplementary/appendix_E_wellbeing/E_wellbeing_outcomes.R
#
# PURPOSE:
#   Estimate standardized baseline-to-post changes in survey outcomes by
#   condition and produce Appendix Figure E7.
#
# DATA PIPELINE:
#   Uses the cleaned survey files from 01_survey_preprocessing.R and follows the
#   old analysis logic: Survey 1 vs Survey 2, eligible experiment participants,
#   condition-specific within-person change models.
#
# STANDARDIZATION:
#   Each outcome is standardized using the baseline mean and baseline SD:
#
#     outcome_z = (outcome - baseline_mean) / baseline_sd
#
#   Therefore, all plotted estimates are standardized changes from baseline in
#   baseline-SD units.
#
# MODEL:
#   outcome_z ~ survey_nr_fact * cond_fact + (1 | response_id)
#
# ESTIMAND:
#   For each condition and outcome:
#
#     standardized post-intervention - baseline change
#
#   Extracted using model-matrix contrasts and the delta method:
#
#     estimate = L %*% beta
#     se       = sqrt(L %*% V %*% t(L))
#
# INPUTS:
#   data/experiment_list.xlsx
#   data/survey/survey_data_clean_long.csv
#
# OUTPUTS:
#   models/supplementary/wellbeing_<outcome>.rds
#   figures/supplementary/appendix_E7_wellbeing_change.{png,svg}
#   tables/supplementary/appendix_E_wellbeing_model_estimates.{xlsx,tex}
#   tables/supplementary/appendix_E_reported_effects.csv
#   tables/supplementary/appendix_E_plot_data.csv
#   tables/supplementary/appendix_E_plot_data_validation_flags.csv
#   tables/supplementary/appendix_E_model_checks.csv
#   tables/supplementary/appendix_E_data_audit.csv
# =============================================================================

source("02_analysis/shared_utils.R")

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(broom.mixed)

make_output_dirs(
  "models/supplementary",
  "figures/supplementary",
  "tables/supplementary"
)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

id_list <- readxl::read_excel(
  "data/experiment_list.xlsx"
)

eligible <- id_list %>%
  transmute(
    response_id = as.character(ID)
  ) %>%
  filter(
    !is.na(response_id),
    response_id != ""
  ) %>%
  distinct() %>%
  pull(response_id)

survey_long_raw <- read_csv(
  "data/survey/survey_data_clean_long.csv",
  show_col_types = FALSE
)

stopifnot(
  "response_id" %in% names(survey_long_raw),
  "survey_nr" %in% names(survey_long_raw),
  "cond" %in% names(survey_long_raw),
  "retention_index" %in% names(survey_long_raw)
)

standardise_condition <- function(cond, condition = NULL) {
  case_when(
    !is.null(condition) &
      as.character(condition) %in% c("control", "intervention", "default") ~ as.character(condition),
    as.character(cond) %in% c("control", "intervention", "default") ~ as.character(cond),
    suppressWarnings(as.integer(cond)) %in% c(1L, 2L) ~ "control",
    suppressWarnings(as.integer(cond)) %in% c(5L, 6L) ~ "intervention",
    suppressWarnings(as.integer(cond)) %in% c(3L, 4L) ~ "default",
    TRUE ~ NA_character_
  )
}

survey_long <- survey_long_raw %>%
  mutate(
    response_id = as.character(response_id),
    cond_raw = cond,
    condition = if ("condition" %in% names(.)) {
      standardise_condition(
        cond = cond_raw,
        condition = condition
      )
    } else {
      standardise_condition(
        cond = cond_raw
      )
    }
  ) %>%
  filter(
    response_id %in% eligible,
    survey_nr %in% c(1, 2),
    !is.na(condition)
  ) %>%
  mutate(
    survey_nr = as.integer(survey_nr),
    survey_nr_fact = factor(
      survey_nr,
      levels = c(1, 2),
      labels = c("Baseline", "Post-intervention")
    ),
    cond_fact = factor(
      condition,
      levels = c("control", "intervention", "default"),
      labels = c("Reflection", "Planning", "Waiting")
    ),
    response_id = factor(response_id)
  )

message(
  "Survey analysis data: ",
  nrow(survey_long),
  " rows, ",
  n_distinct(survey_long$response_id),
  " participants"
)

print(
  survey_long %>%
    count(
      survey_nr_fact,
      cond_fact
    )
)

# =============================================================================
# 2. OUTCOMES
# =============================================================================

outcomes <- c(
  fo_mo = "FOMO",
  kontrolover_so_me = "Perceived Control",
  retention_index = "Perceived Overuse",
  socialconnection = "Social Connection",
  tilfredsmed_so_me = "Satisfaction with Social Media",
  trivsel = "Well-being"
)

outcome_facet_labels <- c(
  "FOMO" = "FOMO",
  "Perceived Control" = "Control over SoMe",
  "Perceived Overuse" = "Overuse",
  "Social Connection" = "Social Connection",
  "Satisfaction with Social Media" = "Satisfaction",
  "Well-being" = "Well-being"
)

outcome_facet_order <- c(
  "FOMO",
  "Control over SoMe",
  "Overuse",
  "Social Connection",
  "Satisfaction",
  "Well-being"
)

available_outcomes <- names(outcomes)[names(outcomes) %in% names(survey_long)]

if (length(available_outcomes) == 0) {
  stop(
    "None of the requested survey outcomes are present in survey_data_clean_long.csv.",
    call. = FALSE
  )
}

missing_outcomes <- setdiff(
  names(outcomes),
  available_outcomes
)

if (length(missing_outcomes) > 0) {
  warning(
    paste0(
      "These outcomes are missing and will be skipped: ",
      paste(missing_outcomes, collapse = ", ")
    ),
    call. = FALSE
  )
}

outcomes <- outcomes[available_outcomes]

retention_audit <- survey_long %>%
  summarise(
    n_retention_nonmissing = sum(!is.na(retention_index)),
    mean_retention = mean(retention_index, na.rm = TRUE),
    sd_retention = sd(retention_index, na.rm = TRUE),
    min_retention = min(retention_index, na.rm = TRUE),
    max_retention = max(retention_index, na.rm = TRUE)
  )

print(retention_audit)

# =============================================================================
# 3. HELPERS
# =============================================================================

baseline_standardize <- function(data, outcome_var) {
  
  df <- data %>%
    mutate(
      outcome_raw = suppressWarnings(as.numeric(.data[[outcome_var]]))
    )
  
  baseline_stats <- df %>%
    filter(
      survey_nr == 1
    ) %>%
    summarise(
      baseline_mean = mean(outcome_raw, na.rm = TRUE),
      baseline_sd = sd(outcome_raw, na.rm = TRUE),
      n_baseline_nonmissing = sum(!is.na(outcome_raw)),
      .groups = "drop"
    )
  
  baseline_mean <- baseline_stats$baseline_mean[[1]]
  baseline_sd <- baseline_stats$baseline_sd[[1]]
  
  if (is.na(baseline_sd) || baseline_sd <= 0) {
    stop(
      paste0(
        "Baseline SD is missing or zero for outcome: ",
        outcome_var
      ),
      call. = FALSE
    )
  }
  
  df %>%
    mutate(
      outcome_z = (outcome_raw - baseline_mean) / baseline_sd,
      baseline_mean = baseline_mean,
      baseline_sd = baseline_sd,
      n_baseline_nonmissing = baseline_stats$n_baseline_nonmissing[[1]]
    )
}

fit_wellbeing_model <- function(outcome_var, data) {
  
  outcome_label <- unname(outcomes[outcome_var])
  
  df_model <- baseline_standardize(
    data = data,
    outcome_var = outcome_var
  ) %>%
    drop_na(
      outcome_z,
      survey_nr_fact,
      cond_fact,
      response_id
    ) %>%
    droplevels()
  
  if (n_distinct(df_model$survey_nr_fact) < 2) {
    stop(
      paste0(
        "Only one survey wave available after filtering for outcome: ",
        outcome_var
      ),
      call. = FALSE
    )
  }
  
  if (n_distinct(df_model$cond_fact) < 2) {
    stop(
      paste0(
        "Fewer than two conditions available after filtering for outcome: ",
        outcome_var
      ),
      call. = FALSE
    )
  }
  
  model <- lmer(
    outcome_z ~ survey_nr_fact * cond_fact + (1 | response_id),
    data = df_model,
    REML = FALSE,
    control = lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 1e5)
    )
  )
  
  tidy_model <- broom.mixed::tidy(
    model,
    conf.int = TRUE
  ) %>%
    mutate(
      outcome = outcome_var,
      outcome_label = outcome_label,
      baseline_mean = unique(df_model$baseline_mean),
      baseline_sd = unique(df_model$baseline_sd),
      n_obs = nrow(df_model),
      n_participants = n_distinct(df_model$response_id),
      n_baseline_nonmissing = unique(df_model$n_baseline_nonmissing)
    )
  
  saveRDS(
    model,
    file.path(
      "models/supplementary",
      paste0("wellbeing_", outcome_var, ".rds")
    )
  )
  
  list(
    model = model,
    tidy = tidy_model,
    data = df_model,
    outcome = outcome_var,
    outcome_label = outcome_label
  )
}

make_reference_row <- function(model_data, condition_value, survey_value) {
  tibble(
    survey_nr_fact = factor(
      survey_value,
      levels = levels(model_data$survey_nr_fact)
    ),
    cond_fact = factor(
      condition_value,
      levels = levels(model_data$cond_fact)
    ),
    response_id = model_data$response_id[1]
  )
}

extract_change_model_matrix <- function(model, model_data, outcome_var, outcome_label) {
  
  b <- fixef(model)
  V <- as.matrix(vcov(model))
  
  tt <- delete.response(
    terms(model)
  )
  
  contrasts_arg <- attr(
    model.matrix(model),
    "contrasts"
  )
  
  map_dfr(
    levels(model_data$cond_fact),
    function(condition_value) {
      
      nd_baseline <- make_reference_row(
        model_data = model_data,
        condition_value = condition_value,
        survey_value = "Baseline"
      )
      
      nd_post <- make_reference_row(
        model_data = model_data,
        condition_value = condition_value,
        survey_value = "Post-intervention"
      )
      
      X_baseline <- model.matrix(
        tt,
        nd_baseline,
        contrasts.arg = contrasts_arg
      )
      
      X_post <- model.matrix(
        tt,
        nd_post,
        contrasts.arg = contrasts_arg
      )
      
      common_cols <- intersect(
        colnames(X_baseline),
        names(b)
      )
      
      X_baseline <- X_baseline[, common_cols, drop = FALSE]
      X_post <- X_post[, common_cols, drop = FALSE]
      
      b_use <- b[common_cols]
      V_use <- V[common_cols, common_cols, drop = FALSE]
      
      L <- X_post - X_baseline
      
      estimate <- as.numeric(
        L %*% b_use
      )
      
      se <- sqrt(
        as.numeric(
          L %*% V_use %*% t(L)
        )
      )
      
      statistic <- estimate / se
      p_value <- 2 * pnorm(-abs(statistic))
      
      tibble(
        outcome = outcome_var,
        outcome_label = outcome_label,
        condition_label = condition_value,
        estimate = estimate,
        std.error = se,
        conf.low = estimate - 1.96 * se,
        conf.high = estimate + 1.96 * se,
        statistic = statistic,
        p.value = p_value,
        sig = p_value < 0.05
      )
    }
  )
}

extract_interactions <- function(tidy_df) {
  tidy_df %>%
    filter(
      effect == "fixed",
      str_detect(
        term,
        "survey_nr_fact.*cond_fact|cond_fact.*survey_nr_fact"
      )
    ) %>%
    transmute(
      outcome,
      outcome_label,
      term,
      condition = case_when(
        str_detect(term, "Planning") ~ "Planning",
        str_detect(term, "Waiting") ~ "Waiting",
        TRUE ~ NA_character_
      ),
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high,
      baseline_mean,
      baseline_sd,
      n_obs,
      n_participants,
      n_baseline_nonmissing,
      sig = p.value < 0.05,
      report = sprintf(
        "%s — %s vs Reflection difference in standardized change: beta = %.2f, 95%% CI [%.2f, %.2f], p %s",
        outcome_label,
        condition,
        estimate,
        conf.low,
        conf.high,
        if_else(
          p.value < 0.001,
          "< 0.001",
          paste0("= ", sprintf("%.3f", p.value))
        )
      )
    )
}

validate_effect_table <- function(df) {
  df %>%
    mutate(
      ci_excludes_0 = conf.low > 0 | conf.high < 0,
      p_sig = p.value < 0.05,
      mismatch = ci_excludes_0 != p_sig,
      bad_bounds = conf.low > estimate | estimate > conf.high
    ) %>%
    filter(
      mismatch | bad_bounds
    )
}

# =============================================================================
# 4. FIT MODELS
# =============================================================================

model_objects <- purrr::map(
  names(outcomes),
  fit_wellbeing_model,
  data = survey_long
)

names(model_objects) <- names(outcomes)

all_results <- purrr::map_dfr(
  model_objects,
  "tidy"
)

model_checks <- purrr::imap_dfr(
  model_objects,
  function(obj, outcome_var) {
    tibble(
      outcome = outcome_var,
      outcome_label = obj$outcome_label,
      AIC = AIC(obj$model),
      BIC = BIC(obj$model),
      logLik = as.numeric(logLik(obj$model)),
      nobs = nobs(obj$model),
      n_participants = n_distinct(obj$data$response_id),
      is_singular = isSingular(obj$model, tol = 1e-4),
      max_abs_gradient = if (!is.null(obj$model@optinfo$derivs$gradient)) {
        max(abs(obj$model@optinfo$derivs$gradient))
      } else {
        NA_real_
      },
      messages = paste(
        obj$model@optinfo$conv$lme4$messages,
        collapse = " | "
      )
    )
  }
)

write_csv(
  model_checks,
  "tables/supplementary/appendix_E_model_checks.csv"
)

print(
  model_checks,
  n = Inf
)

save_model_table(
  all_results %>%
    filter(
      effect == "fixed"
    ) %>%
    dplyr::select(
      outcome_label,
      term,
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high,
      baseline_mean,
      baseline_sd,
      n_obs,
      n_participants,
      n_baseline_nonmissing
    ),
  "appendix_E_wellbeing_model_estimates",
  "tables/supplementary",
  caption = "Appendix E: Well-being model estimates using baseline-SD standardized outcomes",
  label = "tab:appendix_E_wellbeing"
)

# =============================================================================
# 5. INTERACTION EFFECTS
# =============================================================================

interaction_effects <- extract_interactions(
  all_results
)

cat("\n=== APPENDIX E: STANDARDIZED INTERACTION EFFECTS ===\n")

if (nrow(interaction_effects) > 0) {
  cat(
    paste(
      interaction_effects$report,
      collapse = "\n"
    ),
    "\n"
  )
} else {
  cat("No interaction terms found.\n")
}

write_csv(
  interaction_effects,
  "tables/supplementary/appendix_E_reported_effects.csv"
)

# =============================================================================
# 6. CONDITION-SPECIFIC STANDARDIZED CHANGES
# =============================================================================

plot_data <- purrr::imap_dfr(
  model_objects,
  function(obj, outcome_var) {
    extract_change_model_matrix(
      model = obj$model,
      model_data = obj$data,
      outcome_var = outcome_var,
      outcome_label = obj$outcome_label
    )
  }
) %>%
  mutate(
    condition_label = factor(
      condition_label,
      levels = c(
        "Reflection",
        "Planning",
        "Waiting"
      )
    ),
    outcome_label = factor(
      outcome_label,
      levels = unname(outcomes)
    ),
    outcome_facet = recode(
      as.character(outcome_label),
      !!!outcome_facet_labels
    ),
    outcome_facet = factor(
      outcome_facet,
      levels = outcome_facet_order
    ),
    p_value_category = case_when(
      p.value < 0.001 ~ "p < 0.001 ***",
      p.value < 0.01 ~ "p < 0.01 **",
      p.value < 0.05 ~ "p < 0.05 *",
      TRUE ~ "ns"
    ),
    p_value_category = factor(
      p_value_category,
      levels = c(
        "ns",
        "p < 0.05 *",
        "p < 0.01 **",
        "p < 0.001 ***"
      )
    )
  )

plot_data_validation <- validate_effect_table(
  plot_data
)

print(
  plot_data,
  n = Inf
)

print(
  plot_data_validation,
  n = Inf
)

stopifnot(
  "retention_index" %in% plot_data$outcome,
  "Overuse" %in% as.character(plot_data$outcome_facet)
)

write_csv(
  plot_data,
  "tables/supplementary/appendix_E_plot_data.csv"
)

write_csv(
  plot_data_validation,
  "tables/supplementary/appendix_E_plot_data_validation_flags.csv"
)

# =============================================================================
# 7. DATA AUDIT
# =============================================================================

data_audit <- survey_long %>%
  summarise(
    n_rows = n(),
    n_participants = n_distinct(response_id),
    n_wave_1 = sum(survey_nr == 1),
    n_wave_2 = sum(survey_nr == 2),
    n_retention_nonmissing = sum(!is.na(retention_index)),
    n_fo_mo_nonmissing = sum(!is.na(fo_mo)),
    n_kontrolover_nonmissing = sum(!is.na(kontrolover_so_me)),
    n_socialconnection_nonmissing = sum(!is.na(socialconnection)),
    n_tilfreds_nonmissing = sum(!is.na(tilfredsmed_so_me)),
    n_trivsel_nonmissing = sum(!is.na(trivsel))
  )

write_csv(
  data_audit,
  "tables/supplementary/appendix_E_data_audit.csv"
)

print(
  data_audit
)

# =============================================================================
# 8. FIGURE E7
# =============================================================================

p_value_colors <- c(
  "p < 0.001 ***" = "#c1121f",
  "p < 0.01 **" = "#c1121f",
  "p < 0.05 *" = "#c1121f",
  "ns" = "#737373"
)

fig_E7 <- ggplot(
  plot_data,
  aes(
    x = condition_label,
    y = estimate,
    fill = p_value_category
  )
) +
  geom_point(
    shape = 23,
    size = 5
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.02,
    linewidth = 0.55
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "#000000",
    linewidth = 0.5
  ) +
  coord_flip() +
  facet_wrap(
    ~ outcome_facet,
    nrow = 3,
    strip.position = "top"
  ) +
  scale_y_continuous(
    limits = c(-0.8, 0.8),
    breaks = seq(-0.5, 0.5, by = 0.5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = p_value_colors,
    drop = FALSE
  ) +
  labs(
    title = "",
    x = "",
    y = "Standardized change from baseline",
    fill = ""
  ) +
  theme_classic() +
  scale_x_discrete(
    labels = tools::toTitleCase
  ) +
  custom_general_theme_word() +
  theme(
    panel.border = element_rect(
      fill = NA,
      color = "#000000",
      linewidth = 0.5
    ),
    text = element_text(
      size = 14,
      color = "#000000",
      family = BOLD_FONT
    ),
    axis.title = element_text(
      size = 14,
      color = "#000000",
      family = BOLD_FONT
    ),
    axis.text = element_text(
      size = 12,
      color = "#000000",
      family = BOLD_FONT
    ),
    strip.text.x = element_text(
      size = 14,
      color = "#000000",
      family = BOLD_FONT
    ),
    legend.position = "none",
    axis.title.y = element_text(
      size = 14,
      color = "#000000",
      family = BODY_FONT
    ),
    strip.text.x.top = element_text(
      size = 12,
      family = BOLD_FONT,
      color = "#000000",
      angle = 0
    ),
    panel.grid.major.x = element_line(
      color = "#000000",
      linetype = "dashed"
    ),
    panel.grid.major.y = element_blank(),
    axis.ticks.y = element_line(
      color = "#000000"
    ),
    panel.spacing.y = unit(
      0.5,
      "lines"
    ),
    plot.margin = margin(
      t = 0,
      r = 12,
      b = 10,
      l = 0
    ),
    strip.background = element_rect(
      fill = "white",
      color = "#000000"
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

fig_E7

save_figure(
  fig_E7,
  "appendix_E7_wellbeing_change",
  "figures/supplementary",
  width = 1200 / 96,
  height = 500 / 96
)

ggsave(
  "figures/supplementary/appendix_E7_wellbeing_change.png",
  fig_E7,
  width = 1200 / 96,
  height = 500 / 96,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/supplementary/appendix_E7_wellbeing_change.svg",
  fig_E7,
  width = 1200 / 96,
  height = 500 / 96,
  bg = "white"
)

message("\n=== appendix_E_wellbeing_outcomes.R complete ===")