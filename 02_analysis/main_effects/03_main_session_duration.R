Sys.setenv(TZ = "Europe/Copenhagen")

# =============================================================================
# 02_analysis/main_effects/03_main_session_duration.R
#
# Closest possible match to the old session-duration script WITHOUT adding the
# old valid_participants filter.
#
# Remaining expected difference:
#   old script: 8336 rows, 240 participants
#   new script without valid_participants filter: likely 8347 rows, 241 participants
#
# This is intentional. The old script explicitly removed participants without
# both baseline and intervention in time_per_day_avg before smoothing. We are
# not doing that here.
#
# Everything else below mirrors the old script as closely as possible:
#   - same input: time_per_day_avg.csv
#   - same covariate source: participants.csv
#   - same break covariates
#   - same filter: period != mixed, remove -14/0/29, period2 baseline/intervention
#   - same factor levels
#   - same rolling mean by ID2 × period2
#   - same final model-frame variables as the old model
#   - same drop_na() position relative to time3 creation as old script
#
# Old session-duration script selected only:
#   mean_time_minutes_smooth, days_before_activation_num, period2, condition,
#   weekend, fall_break, ID2, day_of_week, gender, region, klassetrin_short2
#
# It did NOT include addiction_index, self_control_index, trivsel, age, or ID
# in the final model-frame drop_na(). That is the main cleanup here.
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs(
  "models/main",
  "tables/main",
  "figures/main"
)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

time_per_day_avg <- read_csv(
  "data/baseline_intervention_outro/session_time/time_per_day_avg.csv",
  show_col_types = FALSE
)

participants <- read_csv(
  "data/participants.csv",
  show_col_types = FALSE
)

# =============================================================================
# 2. JOIN, FILTER, SMOOTH
# =============================================================================

session_data <- time_per_day_avg %>%
  left_join(participants, by = "ID") %>%
  add_break_covariates("date") %>%
  filter(
    period != "mixed",
    !days_before_activation_num %in% c(-14, 0, 29),
    period2 %in% c("baseline", "intervention")
  ) %>%
  add_klassetrin_short2() %>%
  factorise_model_data()

session_smoothed <- session_data %>%
  group_by(ID2, period2) %>%
  arrange(days_before_activation_num, .by_group = TRUE) %>%
  mutate(
    mean_time_minutes_smooth = zoo::rollmean(
      mean_time_minutes,
      k = 3,
      fill = NA,
      align = "right"
    )
  ) %>%
  ungroup() %>%
  dplyr::select(
    mean_time_minutes_smooth,
    days_before_activation_num,
    period2,
    condition,
    weekend,
    fall_break,
    ID2,
    day_of_week,
    gender,
    region,
    klassetrin_short2
  ) %>%
  drop_na() %>%
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

message(
  "Model data: ",
  nrow(session_smoothed),
  " rows, ",
  n_distinct(session_smoothed$ID2),
  " participants"
)

print(table(session_smoothed$condition, session_smoothed$period2))
print(summary(session_smoothed$mean_time_minutes_smooth))

# =============================================================================
# 3. AUDIT AGAINST EXPECTED OLD-SCRIPT DIFFERENCE
# =============================================================================

session_audit <- session_smoothed %>%
  summarise(
    n_rows = n(),
    n_ids = n_distinct(ID2),
    min_day = min(days_before_activation_num, na.rm = TRUE),
    max_day = max(days_before_activation_num, na.rm = TRUE),
    n_baseline = sum(period2 == "baseline"),
    n_intervention = sum(period2 == "intervention"),
    n_control = sum(condition == "control"),
    n_intervention_condition = sum(condition == "intervention"),
    n_default = sum(condition == "default"),
    n_missing_outcome = sum(is.na(mean_time_minutes_smooth)),
    n_missing_time3 = sum(is.na(time3)),
    n_zero_or_negative_outcome = sum(mean_time_minutes_smooth <= 0, na.rm = TRUE)
  )

print(session_audit)

session_smoothed %>%
  count(condition, period2) %>%
  arrange(condition, period2) %>%
  print(n = Inf)

session_smoothed %>%
  count(klassetrin_short2) %>%
  arrange(klassetrin_short2) %>%
  print(n = Inf)

# =============================================================================
# 4. FIT MODEL
# =============================================================================

message("Fitting main session-duration model...")

glm_session_model <- glmer(
  mean_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break +
    region + klassetrin_short2 +
    (1 | ID2),
  family = Gamma(link = "log"),
  data = session_smoothed,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 1e6)
  )
)

summary(glm_session_model)

message("Max absolute gradient: ",
        round(max(abs(glm_session_model@optinfo$derivs$gradient)), 6))

saveRDS(glm_session_model, "models/main/glm_session_duration_model.rds")
message("Model saved: models/main/glm_session_duration_model.rds")

# =============================================================================
# 4. EXTRACT EFFECTS
# =============================================================================

tidy_session <- broom.mixed::tidy(glm_session_model, conf.int = TRUE) %>%
  mutate(
    rr       = exp(estimate),
    rr_low   = exp(conf.low),
    rr_high  = exp(conf.high),
    pct      = 100 * (rr - 1),
    pct_low  = 100 * (rr_low - 1),
    pct_high = 100 * (rr_high - 1)
  )

save_model_table(
  tidy_session, "session_duration_model_coefficients", "tables/main",
  caption = "Main model: average session duration (Gamma GLMM)",
  label   = "tab:session_duration_coefficients"
)

session_change        <- extract_change_delta_method(glm_session_model)
session_change_report <- format_rr_report(session_change)

cat("\n=== SESSION DURATION: CONDITION EFFECTS ===\n")
cat(paste(session_change_report$report, collapse = "\n"), "\n")

save_model_table(
  session_change, "session_duration_condition_effects", "tables/main",
  caption = "Baseline to intervention change in session duration by condition",
  label   = "tab:session_duration_effects"
)

p2_session <- predict_response(
  glm_session_model,
  terms    = c("period2", "condition"),
  margin   = "empirical",
  type     = "response",
  ci_level = 0.95,
  interval = "prediction"
)

p2_session_df <- as.data.frame(p2_session) %>%
  pivot_wider(
    id_cols     = group,
    names_from  = x,
    values_from = c(predicted, conf.low, conf.high),
    names_glue  = "{.value}_{x}"
  ) %>%
  mutate(
    predicted_diff       = predicted_intervention - predicted_baseline,
    predicted_pct_change = 100 * predicted_diff / predicted_baseline
  )

save_model_table(
  p2_session_df, "session_duration_predicted_change", "tables/main",
  caption = "Predicted average session duration by period and condition",
  label   = "tab:session_duration_predicted"
)

# =============================================================================
# 5. PREDICTED TIME SERIES
# =============================================================================

p1_session <- predict_response(
  glm_session_model,
  terms    = c("time3[all]", "period2", "condition"),
  margin   = "empirical",
  type     = "response",
  ci_level = 0.95,
  interval = "prediction"
)

p1_session_df <- as.data.frame(p1_session) %>%
  filter((x <= 10 & group == "baseline") |
         (x  > 10 & group == "intervention"))

# =============================================================================
# 6. FIGURE
# =============================================================================

p_session <- ggplot(p1_session_df,
  aes(x = x, y = predicted, group = factor(facet))) +
  geom_segment(aes(x = 10, xend = 10, y = -Inf, yend = Inf),
               linetype = "dashed", color = "#000000", linewidth = 0.7) +
  geom_segment(aes(x = 11, xend = 11, y = -Inf, yend = Inf),
               linetype = "dashed", color = "#000000", linewidth = 0.7) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(facet)),
              alpha = 0.1) +
  geom_point(aes(color = factor(facet)), size = 1.5) +
  geom_line( aes(color = factor(facet)), linewidth = 0.8) +
  geom_text(
    data = p1_session_df %>% filter(x == max(x), facet == "control"),
    aes(label = "Reflection", color = factor(facet)),
    hjust = -0.2, size = 4, family = BOLD_FONT, show.legend = FALSE
  ) +
  geom_text(
    data = p1_session_df %>% filter(x == max(x), facet == "default"),
    aes(label = "Waiting", color = factor(facet)),
    hjust = -0.2, size = 4, family = BOLD_FONT, show.legend = FALSE
  ) +
  geom_text(
    data = p1_session_df %>% filter(x == max(x), facet == "intervention"),
    aes(label = "Planning", color = factor(facet)),
    hjust = -0.2, size = 4, family = BOLD_FONT, show.legend = FALSE
  ) +
  annotate("text", x = 10.5, y = Inf, vjust = 1.5,
           label = "Interventions Start", color = "#000000",
           size = 4, family = BOLD_FONT) +
  scale_x_continuous(
    breaks = c(1, 11, 21, 31),
    labels = function(x) paste0("Day ", x + 4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = function(x) paste0(round(x), " mins")) +
  scale_color_manual(values = COND_COLOURS, breaks = COND_LEVELS_RAW,
                     labels = unname(COND_LABELS), name = NULL) +
  scale_fill_manual(values = COND_COLOURS, guide = "none") +
  labs(x = "", y = "Average session duration (minutes)") +
  coord_cartesian(xlim = c(0, 36), clip = "off") +
  custom_general_theme_word() +
  theme(
    plot.margin        = margin(t = 10, r = 80, b = 0, l = -10),
    axis.ticks.x       = element_line(color = "#000000"),
    axis.line.x        = element_line(color = "#000000"),
    axis.text.y        = element_text(size = 13, family = BODY_FONT),
    axis.text.x        = element_text(size = 12, family = BODY_FONT,
                                      margin = margin(t = 5)),
    panel.grid.major.y = element_line(color = "#000000", linewidth = 0.05),
    legend.position    = "bottom",
    legend.text        = element_text(size = 14, family = BODY_FONT),
    legend.box.margin  = margin(t = -5, b = 15)
  )

save_figure(p_session, "session_duration_its_predicted", "figures/main")

message("\n=== 03_main_session_duration.R complete ===")
