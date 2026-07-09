Sys.setenv(TZ = "Europe/Copenhagen")

# =============================================================================
# 04_supplementary/appendix_F_retention/F_retention_analysis.R
#
# PURPOSE:
#   Produce retention / active-data coverage figures and summary tables for
#   Supplementary Appendix F.
#
# KEY DECISIONS:
#   - Retention is operationalized as daily active participants with at least one
#     openedApp event.
#   - The analysis is restricted to participants observed in BOTH baseline and
#     intervention.
#   - Only baseline and intervention days are shown.
#   - Day 1-14 = baseline; Day 15-42 = intervention.
#
# IMPORTANT FIX:
#   An individual cannot be counted as both baseline and intervention on the same
#   analytic day. The script first creates a canonical day_model and then derives
#   period_model ONLY from day_model. It also collapses to one row per ID x day
#   before counting active participants.
#
# INPUTS:
#   data/data_filt_step4_all_apps.csv
#   data/survey/survey_data_clean_long.csv
#
# OUTPUTS:
#   figures/supplementary/appendix_F_retention_count.{png,svg}
#   figures/supplementary/appendix_F_retention_rate.{png,svg}
#   tables/supplementary/appendix_F_retention_daily.csv
#   tables/supplementary/appendix_F_retention_summary.{xlsx,tex,csv}
#   tables/supplementary/appendix_F_retention_sample_audit.csv
#   tables/supplementary/appendix_F_retention_condition_audit.csv
#   tables/supplementary/appendix_F_retention_id_day_conflicts.csv
#
# SOURCE:
#   Adapted from the uploaded retention script. :contentReference[oaicite:0]{index=0}
# =============================================================================

source("02_analysis/shared_utils.R")

library(tidyverse)
library(lubridate)

make_output_dirs(
  "figures/supplementary",
  "tables/supplementary"
)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

data_filt <- read_csv(
  "data/data_filt_step4_all_apps.csv",
  show_col_types = FALSE
) %>%
  mutate(
    ID = as.character(ID),
    time = as.POSIXct(time, tz = "Europe/Copenhagen"),
    time = lubridate::with_tz(time, tzone = "Europe/Copenhagen"),
    date = as.Date(time, tz = "Europe/Copenhagen")
  )

stopifnot(
  "ID" %in% names(data_filt),
  "resolution" %in% names(data_filt)
)

survey_baseline <- read_csv(
  "data/survey/survey_data_clean_long.csv",
  show_col_types = FALSE
) %>%
  filter(
    survey_nr == 1
  ) %>%
  transmute(
    ID = as.character(response_id),
    survey_cond = cond
  ) %>%
  distinct(
    ID,
    .keep_all = TRUE
  )

# =============================================================================
# 2. SAFE HELPERS
# =============================================================================

get_chr_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) {
    as.character(df[[col]])
  } else {
    rep(default, nrow(df))
  }
}

get_int_col <- function(df, col, default = NA_integer_) {
  if (col %in% names(df)) {
    suppressWarnings(as.integer(df[[col]]))
  } else {
    rep(default, nrow(df))
  }
}

standardise_condition_raw <- function(x) {
  case_when(
    as.character(x) %in% c("control", "intervention", "default") ~ as.character(x),
    as.character(x) %in% c("Reflection", "reflection", "Refleksion") ~ "control",
    as.character(x) %in% c("Planning", "planning", "Planlægning") ~ "intervention",
    as.character(x) %in% c("Waiting", "waiting", "Vente") ~ "default",
    suppressWarnings(as.integer(x)) %in% c(1L, 2L) ~ "control",
    suppressWarnings(as.integer(x)) %in% c(5L, 6L) ~ "intervention",
    suppressWarnings(as.integer(x)) %in% c(3L, 4L) ~ "default",
    TRUE ~ NA_character_
  )
}

condition_vec <- get_chr_col(data_filt, "condition")
participation_day_vec <- get_int_col(data_filt, "participation_day")
days_before_activation_vec <- get_int_col(data_filt, "days_before_activation_num")

# =============================================================================
# 3. STANDARDISE DAY, PERIOD, CONDITION
# =============================================================================

retention_raw <- data_filt %>%
  mutate(
    condition_raw = condition_vec,
    participation_day = participation_day_vec,
    days_before_activation_num = days_before_activation_vec,
    day_model = case_when(
      !is.na(participation_day) ~ participation_day,
      !is.na(days_before_activation_num) & days_before_activation_num < 0L ~ days_before_activation_num + 15L,
      !is.na(days_before_activation_num) & days_before_activation_num > 0L ~ days_before_activation_num + 14L,
      TRUE ~ NA_integer_
    ),
    period_model = case_when(
      !is.na(day_model) & day_model >= 1L & day_model <= 14L ~ "baseline",
      !is.na(day_model) & day_model >= 15L & day_model <= 42L ~ "intervention",
      TRUE ~ NA_character_
    ),
    condition = standardise_condition_raw(condition_raw),
    condition2 = factor(
      condition,
      levels = COND_LEVELS_RAW,
      labels = COND_LEVELS_ENG
    ),
    period_label = factor(
      period_model,
      levels = c("baseline", "intervention"),
      labels = c("Baseline", "Intervention")
    )
  ) %>%
  filter(
    !is.na(ID),
    !is.na(resolution),
    period_model %in% c("baseline", "intervention"),
    day_model >= 1L,
    day_model <= 42L,
    condition %in% COND_LEVELS_RAW
  )

period_day_audit <- retention_raw %>%
  summarise(
    n_rows = n(),
    n_ids = n_distinct(ID),
    min_day = min(day_model, na.rm = TRUE),
    max_day = max(day_model, na.rm = TRUE),
    n_baseline_rows = sum(period_model == "baseline", na.rm = TRUE),
    n_intervention_rows = sum(period_model == "intervention", na.rm = TRUE),
    n_opened_rows = sum(resolution == "openedApp", na.rm = TRUE)
  )

print(period_day_audit)

# =============================================================================
# 4. ID-DAY CONFLICT AUDIT
# =============================================================================

id_day_conflicts <- retention_raw %>%
  filter(
    resolution == "openedApp"
  ) %>%
  distinct(
    ID,
    day_model,
    period_model
  ) %>%
  count(
    ID,
    day_model,
    name = "n_periods_on_same_day"
  ) %>%
  filter(
    n_periods_on_same_day > 1L
  )

write_csv(
  id_day_conflicts,
  "tables/supplementary/appendix_F_retention_id_day_conflicts.csv"
)

print(id_day_conflicts, n = Inf)

stopifnot(
  nrow(id_day_conflicts) == 0
)

# =============================================================================
# 5. KEEP ONLY PARTICIPANTS OBSERVED IN BOTH BASELINE AND INTERVENTION
# =============================================================================

valid_participants <- retention_raw %>%
  filter(
    resolution == "openedApp"
  ) %>%
  distinct(
    ID,
    period_model
  ) %>%
  count(
    ID,
    name = "n_periods"
  ) %>%
  filter(
    n_periods == 2L
  ) %>%
  pull(ID)

participant_condition <- retention_raw %>%
  filter(
    ID %in% valid_participants,
    resolution == "openedApp"
  ) %>%
  count(
    ID,
    condition,
    condition2,
    name = "n_events"
  ) %>%
  group_by(
    ID
  ) %>%
  slice_max(
    order_by = n_events,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  dplyr::select(
    ID,
    condition,
    condition2
  )

condition_audit <- retention_raw %>%
  filter(
    ID %in% valid_participants,
    resolution == "openedApp"
  ) %>%
  distinct(
    ID,
    condition
  ) %>%
  count(
    ID,
    name = "n_conditions"
  ) %>%
  summarise(
    n_valid_ids = n(),
    n_ids_with_multiple_conditions = sum(n_conditions > 1L),
    max_conditions_per_id = max(n_conditions, na.rm = TRUE)
  )

print(condition_audit)

write_csv(
  condition_audit,
  "tables/supplementary/appendix_F_retention_condition_audit.csv"
)

retention_events_id_day <- retention_raw %>%
  filter(
    ID %in% valid_participants,
    resolution == "openedApp"
  ) %>%
  dplyr::select(
    -condition,
    -condition2
  ) %>%
  left_join(
    participant_condition,
    by = "ID"
  ) %>%
  left_join(
    survey_baseline,
    by = "ID"
  ) %>%
  group_by(
    ID,
    day_model
  ) %>%
  summarise(
    period_model = first(period_model),
    period_label = first(period_label),
    condition = first(condition),
    condition2 = first(condition2),
    survey_cond = first(survey_cond),
    n_events = n(),
    .groups = "drop"
  ) %>%
  mutate(
    condition2 = factor(
      condition2,
      levels = COND_LEVELS_ENG
    ),
    period_label = factor(
      period_model,
      levels = c("baseline", "intervention"),
      labels = c("Baseline", "Intervention")
    )
  )

id_day_final_check <- retention_events_id_day %>%
  distinct(
    ID,
    day_model,
    period_model
  ) %>%
  count(
    ID,
    day_model,
    name = "n_periods_on_same_day"
  ) %>%
  filter(
    n_periods_on_same_day > 1L
  )

stopifnot(
  nrow(id_day_final_check) == 0
)

sample_audit <- tibble(
  step = c(
    "raw_events",
    "baseline_intervention_events",
    "opened_app_events_in_baseline_intervention",
    "participants_with_opened_app_in_both_periods",
    "final_id_day_opened_app_rows"
  ),
  n_rows = c(
    nrow(data_filt),
    nrow(retention_raw),
    retention_raw %>%
      filter(resolution == "openedApp") %>%
      nrow(),
    length(valid_participants),
    nrow(retention_events_id_day)
  ),
  n_ids = c(
    n_distinct(data_filt$ID),
    n_distinct(retention_raw$ID),
    retention_raw %>%
      filter(resolution == "openedApp") %>%
      summarise(n = n_distinct(ID)) %>%
      pull(n),
    length(valid_participants),
    n_distinct(retention_events_id_day$ID)
  )
)

print(sample_audit)

write_csv(
  sample_audit,
  "tables/supplementary/appendix_F_retention_sample_audit.csv"
)

# =============================================================================
# 6. BUILD DAILY RETENTION DATA
# =============================================================================

condition_denominators <- participant_condition %>%
  count(
    condition2,
    name = "n_condition_total"
  ) %>%
  mutate(
    condition2 = factor(
      condition2,
      levels = COND_LEVELS_ENG
    )
  )

day_grid <- condition_denominators %>%
  dplyr::select(
    condition2
  ) %>%
  tidyr::crossing(
    day_model = 1:42
  )

retention_daily <- retention_events_id_day %>%
  group_by(
    day_model,
    period_model,
    period_label,
    condition2
  ) %>%
  summarise(
    n_participants = n_distinct(ID),
    n_events = sum(n_events, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(
    day_grid,
    by = c("day_model", "condition2")
  ) %>%
  mutate(
    period_model = case_when(
      is.na(period_model) & day_model <= 14L ~ "baseline",
      is.na(period_model) & day_model >= 15L ~ "intervention",
      TRUE ~ period_model
    ),
    period_label = factor(
      period_model,
      levels = c("baseline", "intervention"),
      labels = c("Baseline", "Intervention")
    ),
    n_participants = replace_na(n_participants, 0L),
    n_events = replace_na(n_events, 0L)
  ) %>%
  left_join(
    condition_denominators,
    by = "condition2"
  ) %>%
  mutate(
    pct_retained = 100 * n_participants / n_condition_total,
    condition2 = factor(
      condition2,
      levels = COND_LEVELS_ENG
    )
  ) %>%
  arrange(
    condition2,
    day_model
  )

daily_period_conflicts <- retention_daily %>%
  distinct(
    condition2,
    day_model,
    period_model
  ) %>%
  count(
    condition2,
    day_model,
    name = "n_periods_on_same_day"
  ) %>%
  filter(
    n_periods_on_same_day > 1L
  )

stopifnot(
  nrow(daily_period_conflicts) == 0
)

write_csv(
  retention_daily,
  "tables/supplementary/appendix_F_retention_daily.csv"
)

print(
  retention_daily %>%
    group_by(
      condition2,
      period_label
    ) %>%
    summarise(
      n_condition_total = first(n_condition_total),
      mean_n = round(mean(n_participants, na.rm = TRUE), 1),
      min_n = min(n_participants, na.rm = TRUE),
      max_n = max(n_participants, na.rm = TRUE),
      mean_pct = round(mean(pct_retained, na.rm = TRUE), 1),
      .groups = "drop"
    ),
  n = Inf
)

# =============================================================================
# 7. SUMMARY TABLE
# =============================================================================

retention_summary <- retention_daily %>%
  group_by(
    condition2,
    period_label
  ) %>%
  summarise(
    n_condition_total = first(n_condition_total),
    mean_n_participants = round(mean(n_participants, na.rm = TRUE), 1),
    min_n = min(n_participants, na.rm = TRUE),
    max_n = max(n_participants, na.rm = TRUE),
    mean_retention_pct = round(mean(pct_retained, na.rm = TRUE), 1),
    min_retention_pct = round(min(pct_retained, na.rm = TRUE), 1),
    max_retention_pct = round(max(pct_retained, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(
    condition2,
    period_label
  )

write_csv(
  retention_summary,
  "tables/supplementary/appendix_F_retention_summary.csv"
)

save_model_table(
  retention_summary,
  "appendix_F_retention_summary",
  "tables/supplementary",
  caption = "Appendix F: Daily active participant coverage among participants observed in both baseline and intervention",
  label = "tab:appendix_F_retention"
)

cat("\n=== APPENDIX F: RETENTION SUMMARY ===\n")
print(retention_summary, n = Inf)

# =============================================================================
# 8. SHARED VISUAL THEME
# =============================================================================

retention_theme <- function() {
  custom_general_theme_word() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(
        size = 13,
        family = BODY_FONT,
        color = "#000000"
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
      axis.title.y = element_text(
        size = 14,
        family = BODY_FONT,
        color = "#000000"
      ),
      axis.line.x = element_line(
        color = "#000000",
        linetype = "solid",
        linewidth = 0.3
      ),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line(
        color = "#000000"
      ),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_line(
        color = "#000000",
        linewidth = 0.05
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(
        fill = "white",
        color = NA
      ),
      strip.text = element_text(
        size = 13,
        family = BOLD_FONT,
        color = "#000000",
        margin = margin(b = 10),
        hjust = 0.52
      ),
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.margin = margin(
        t = 10,
        r = 40,
        b = 10,
        l = 10
      )
    )
}

# =============================================================================
# 9. FIGURE F1: ACTIVE PARTICIPANT COUNT
# =============================================================================

fig_F_count <- ggplot(
  retention_daily,
  aes(
    x = day_model,
    y = n_participants,
    color = condition2,
    group = condition2
  )
) +
  annotate(
    "rect",
    xmin = 1,
    xmax = 14,
    ymin = -Inf,
    ymax = Inf,
    fill = "#DADADA",
    alpha = 0.15
  ) +
  annotate(
    "rect",
    xmin = 14,
    xmax = 42,
    ymin = -Inf,
    ymax = Inf,
    fill = "#DADADA",
    alpha = 0.04
  ) +
  geom_vline(
    xintercept = 14,
    linetype = "dashed",
    color = "#000000",
    linewidth = 0.5
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 1.8
  ) +
  annotate(
    "text",
    x = 7,
    y = Inf,
    vjust = 1.5,
    label = "Baseline",
    size = 4,
    color = "#000000",
    family = BOLD_FONT,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 28,
    y = Inf,
    vjust = 1.5,
    label = "Intervention",
    size = 4,
    color = "#000000",
    family = BOLD_FONT,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = COND_COLOURS,
    name = NULL,
    breaks = COND_LEVELS_ENG
  ) +
  scale_x_continuous(
    limits = c(1, 42),
    breaks = seq(7, 42, by = 7),
    labels = function(x) paste0("Day ", x),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "",
    y = "Active participants"
  ) +
  retention_theme()

fig_F_count

save_figure(
  fig_F_count,
  "appendix_F_retention_count",
  "figures/supplementary",
  width = 1200 / 96,
  height = 500 / 96
)

ggsave(
  "figures/supplementary/appendix_F_retention_count.png",
  fig_F_count,
  width = 1200 / 96,
  height = 500 / 96,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/supplementary/appendix_F_retention_count.svg",
  fig_F_count,
  width = 1200 / 96,
  height = 500 / 96,
  bg = "white"
)

# =============================================================================
# 10. FIGURE F2: ACTIVE PARTICIPANT RATE
# =============================================================================

fig_F_pct <- ggplot(
  retention_daily,
  aes(
    x = day_model,
    y = pct_retained,
    color = condition2,
    group = condition2
  )
) +
  annotate(
    "rect",
    xmin = 1,
    xmax = 14,
    ymin = -Inf,
    ymax = 100,
    fill = "#DADADA",
    alpha = 0.15
  ) +
  annotate(
    "rect",
    xmin = 14,
    xmax = 42,
    ymin = -Inf,
    ymax = 100,
    fill = "#DADADA",
    alpha = 0.04
  ) +
  geom_hline(
    yintercept = c(50, 75, 100),
    linetype = "dashed",
    color = "#000000",
    linewidth = 0.25
  ) +
  geom_vline(
    xintercept = 14,
    linetype = "dashed",
    color = "#000000",
    linewidth = 0.5
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 1.8
  ) +
  annotate(
    "text",
    x = 7,
    y = 107,
    label = "Baseline",
    size = 4,
    color = "#000000",
    family = BOLD_FONT,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 28,
    y = 107,
    label = "Intervention",
    size = 4,
    color = "#000000",
    family = BOLD_FONT,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = COND_COLOURS,
    name = NULL,
    breaks = COND_LEVELS_ENG
  ) +
  scale_x_continuous(
    limits = c(1, 42),
    breaks = seq(7, 42, by = 7),
    labels = function(x) paste0("Day ", x),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Active participants"
  ) +
  retention_theme() +
  theme(
    legend.position = "none"
  )

fig_F_pct

save_figure(
  fig_F_pct,
  "appendix_F_retention_rate",
  "figures/supplementary",
  width = 1200 / 96,
  height = 500 / 96
)

ggsave(
  "figures/supplementary/appendix_F_retention_rate.png",
  fig_F_pct,
  width = 1200 / 96,
  height = 500 / 96,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/supplementary/appendix_F_retention_rate.svg",
  fig_F_pct,
  width = 1200 / 96,
  height = 500 / 96,
  bg = "white"
)

message("\n=== appendix_F_retention_analysis.R complete ===")