Sys.setenv(TZ = "Europe/Copenhagen")
# =============================================================================
# 04_supplementary/appendix_C_reinterventions/C_reintervention_analysis.R
#
# PURPOSE:
#   Produce statistics and figures for Appendix C:
#   Re-interventions and adherence to self-imposed time limits.
#
#   Reported in text:
#   - Re-intervention frequency (~10% of sessions)
#   - Dismissal rate at re-intervention vs initial entry
#   - Planned duration adherence (% of planned time actually used)
#   - Sessions that triggered vs did not trigger re-intervention
#
# INPUTS:
#   data/regret_rate/intervention_app_for_regret.csv
#   data/demographic/demographic_data.csv
#   data/survey/survey_data_clean_long.csv
#   data/survey/klassetrin_clean.csv
#
# OUTPUTS:
#   tables/supplementary/appendix_C_reintervention_stats.{xlsx,tex}
#   tables/supplementary/appendix_C_dismissal_by_type.{xlsx,tex}
#   figures/supplementary/appendix_C_reintervention_dismissal.{png,svg}
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs("figures/supplementary", "tables/supplementary")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

intervention_app <- read_csv(
  "data/regret_rate/intervention_app_for_regret.csv",
  show_col_types = FALSE
) %>%
  mutate(
    time = with_tz(as.POSIXct(time, tz = "UTC"),
                   tzone = "Europe/Copenhagen"),
    date = as.Date(time, tz = "Europe/Copenhagen")
  )

covs <- load_covariates()

# Keep only Planning condition (interventionType involves time-limit)
planning_data <- intervention_app %>%
  filter(
    condition == "intervention",
    !is.na(interventionType), interventionType != "",
    resolution %in% c("openedApp","dismissedAppOpening")
  ) %>%
  mutate(
    dismissed     = as.integer(resolution == "dismissedAppOpening"),
    is_reint      = as.integer(isReIntervention == 1),
    # interventionDuration is the planned duration in seconds
    planned_s     = as.numeric(interventionDuration),
    planned_min   = planned_s / 60
  )

# =============================================================================
# 2. RE-INTERVENTION FREQUENCY
# =============================================================================

# Total sessions (initial entry = isReIntervention == 0)
n_initial   <- sum(planning_data$is_reint == 0, na.rm = TRUE)
n_reint     <- sum(planning_data$is_reint == 1, na.rm = TRUE)
pct_reint   <- round(100 * n_reint / (n_initial + n_reint), 1)

# =============================================================================
# 3. DISMISSAL RATE: INITIAL vs RE-INTERVENTION
# =============================================================================

dismissal_by_type <- planning_data %>%
  group_by(is_reint) %>%
  summarise(
    n_events      = n(),
    n_dismissed   = sum(dismissed, na.rm = TRUE),
    dismissal_pct = round(100 * n_dismissed / n_events, 1),
    .groups = "drop"
  ) %>%
  mutate(event_type = if_else(is_reint == 0,
                              "Initial entry", "Re-intervention"))

cat("\n=== APPENDIX C: DISMISSAL RATES ===\n")
print(dismissal_by_type %>%
        dplyr::select(event_type, n_events, n_dismissed, dismissal_pct))

# =============================================================================
# 4. DISMISSAL RATE BY APP (re-intervention only, Planning condition)
# =============================================================================

dismissal_reint_by_app <- planning_data %>%
  filter(is_reint == 1,
         app %in% c("instagram","tikTok","snapchat","youtube","facebook")) %>%
  mutate(app = recode(app, tikTok = "TikTok", snapchat = "Snapchat",
                      instagram = "Instagram", youtube = "YouTube",
                      facebook = "Facebook")) %>%
  group_by(app) %>%
  summarise(
    n_events      = n(),
    n_dismissed   = sum(dismissed, na.rm = TRUE),
    dismissal_pct = round(100 * n_dismissed / n_events, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(dismissal_pct))

# =============================================================================
# 5. PLANNING ADHERENCE
#    timeIntervalUntilReIntervention = time elapsed before re-intervention
#    or session end, in milliseconds
# =============================================================================

adherence_data <- planning_data %>%
  filter(is_reint == 0, !is.na(planned_min), planned_min > 0) %>%
  mutate(
    elapsed_min  = as.numeric(timeIntervalUntilReIntervention) / 60000,
    pct_used     = if_else(planned_min > 0,
                           100 * elapsed_min / planned_min,
                           NA_real_),
    triggered_reint = !is.na(elapsed_min) &
                      elapsed_min >= planned_min * 0.98  # reached limit
  )

mean_pct_used      <- round(mean(adherence_data$pct_used, na.rm = TRUE), 1)
pct_triggered      <- round(100 * mean(adherence_data$triggered_reint,
                                       na.rm = TRUE), 1)

adherence_by_group <- adherence_data %>%
  group_by(triggered_reint) %>%
  summarise(
    n               = n(),
    mean_pct_used   = round(mean(pct_used,    na.rm = TRUE), 1),
    mean_planned    = round(mean(planned_min, na.rm = TRUE), 2),
    mean_elapsed    = round(mean(elapsed_min, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(group = if_else(triggered_reint,
                         "Reached limit (re-intervention triggered)",
                         "Ended before limit"))

cat("\n=== APPENDIX C: PLANNING ADHERENCE ===\n")
cat("Overall mean % of planned time used:", mean_pct_used, "%\n")
cat("% sessions that reached the planned limit:", pct_triggered, "%\n")
print(adherence_by_group %>%
        dplyr::select(group, n, mean_planned, mean_elapsed, mean_pct_used))

# =============================================================================
# 6. SAVE SUMMARY TABLES
# =============================================================================

reint_summary <- tibble(
  metric        = c("Total initial entries",
                    "Total re-interventions",
                    "Re-intervention rate (%)",
                    "Dismissal at initial entry (%)",
                    "Dismissal at re-intervention (%)",
                    "Mean % planned time used (all sessions)",
                    "% sessions reaching planned limit"),
  value = c(n_initial, n_reint, pct_reint,
            dismissal_by_type$dismissal_pct[dismissal_by_type$is_reint == 0],
            dismissal_by_type$dismissal_pct[dismissal_by_type$is_reint == 1],
            mean_pct_used,
            pct_triggered)
)

save_model_table(
  reint_summary, "appendix_C_reintervention_stats", "tables/supplementary",
  caption = "Appendix C: Re-intervention frequency and planning adherence",
  label   = "tab:appendix_C_reint_stats"
)

save_model_table(
  bind_rows(
    dismissal_by_type %>%
      dplyr::select(event_type, n_events, n_dismissed, dismissal_pct),
    dismissal_reint_by_app %>%
      rename(event_type = app) %>%
      dplyr::select(event_type, n_events, n_dismissed, dismissal_pct)
  ),
  "appendix_C_dismissal_by_type", "tables/supplementary",
  caption = "Appendix C: Dismissal rate by event type and app",
  label   = "tab:appendix_C_dismissal"
)

# =============================================================================
# 7. FIGURE  — Dismissal rate: initial entry vs re-intervention by app
# =============================================================================

dismissal_plot_data <- bind_rows(
  dismissal_by_type %>%
    transmute(app = event_type, dismissal_pct,
              type = "Overall"),
  dismissal_reint_by_app %>%
    mutate(type = "By app (re-intervention only)") %>%
    dplyr::select(app, dismissal_pct, type)
) %>%
  mutate(app = fct_reorder(app, dismissal_pct))

fig_C <- ggplot(
  dismissal_by_type %>%
    mutate(event_type = factor(event_type,
                               levels = c("Initial entry","Re-intervention"))),
  aes(x = event_type, y = dismissal_pct, fill = event_type)
) +
  geom_col(width = 0.6, color = "black") +
  geom_text(aes(label = paste0(dismissal_pct, "%")),
            vjust = -0.4, size = 4.5, family = BODY_FONT, fontface = "bold") +
  scale_fill_manual(
    values = c("Initial entry" = "#8dadbf", "Re-intervention" = "#9e0b1d"),
    guide  = "none"
  ) +
  scale_y_continuous(
    limits = c(0, 55),
    breaks = c(0, 10, 20, 30, 40, 50),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Dismissal rate (%)",
    subtitle = "Planning condition only"
  ) +
  custom_general_theme_word() +
  theme(
    axis.title.y = element_text(size = 12, family = BODY_FONT),
    axis.text.x  = element_text(size = 13, family = BODY_FONT, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#000000", linetype = "dashed")
  )

save_figure(fig_C, "appendix_C_reintervention_dismissal",
            "figures/supplementary", width = 800/96, height = 450/96)

message("\n=== appendix_C_reintervention_analysis complete ===")
