Sys.setenv(TZ = "Europe/Copenhagen")
# =============================================================================
# 02_analysis/main_effects/01_main_daily_time.R
#
# MODEL:
#   glmer(total_time_minutes_smooth ~
#           ns(time3, df=2):period2 + period2 * condition +
#           gender + fall_break + region + klassetrin_short2 +
#           (1 | ID2),
#         family = Gamma(link="log"))
#
# INPUTS:
#   data/baseline_intervention_outro/session_time/time_per_day.csv
#   data/participants.csv      (from 02_demographic_preprocessing.R)
#   data/baseline/individual_differences.csv
#
# OUTPUTS:
#   models/main/glm_sumtime_model.rds
#   tables/main/daily_time_model_coefficients.{xlsx,tex}
#   tables/main/daily_time_condition_effects.{xlsx,tex}
#   tables/main/daily_time_predicted_change.{xlsx,tex}
#   figures/main/daily_time_its_predicted.{png,svg}
#   figures/main/daily_time_its_predicted_ribbon.{png,svg}
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs("models/main", "tables/main", "figures/main")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

time_per_day <- read_csv(
  "data/baseline_intervention_outro/session_time/time_per_day.csv",
  show_col_types = FALSE
)

covariates <- read_csv(
  "data/covariates.csv",
  show_col_types = FALSE
) %>%
  dplyr::select(
    -any_of(c(
      "condition",
      "condition.x",
      "condition.y",
      "condition_x",
      "condition_y"
    ))
  )

ind_diff <- read_csv(
  "data/baseline/individual_differences.csv",
  show_col_types = FALSE
) %>%
  mutate(
    heavy_user = factor(
      as.integer(
        total_time_minutes >= quantile(total_time_minutes, 0.5, na.rm = TRUE)
      ),
      levels = c(0, 1)
    ),
    average_time_daily = total_time_minutes
  ) %>%
  dplyr::select(ID, heavy_user, average_time_daily)

time_smoothed <- time_per_day %>%
  left_join(covariates, by = "ID") %>%
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
    weekend,
    fall_break,
    ID2,
    ID,
    day_of_week,
    addiction_index,
    self_control_index,
    trivsel,
    age,
    gender,
    region,
    klassetrin_short2
  ) %>%
  drop_na()

message(
  "Model data: ",
  nrow(time_smoothed),
  " rows, ",
  n_distinct(time_smoothed$ID2),
  " participants"
)
print(table(time_smoothed$condition, time_smoothed$period2))
print(summary(time_smoothed$total_time_minutes_smooth))

# =============================================================================
# 3. FIT MODEL
# =============================================================================

message("Fitting main daily-time model...")

glm_time_model <- glmer(
  total_time_minutes_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break +
    region + klassetrin_short2 +
    (1 | ID2),
  family  = Gamma(link = "log"),
  data    = time_smoothed,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)

summary(glm_time_model)

message("Max absolute gradient: ",
        round(max(abs(glm_time_model@optinfo$derivs$gradient)), 6))

saveRDS(glm_time_model, "models/main/glm_sumtime_model.rds")
message("Model saved: models/main/glm_sumtime_model.rds")

# =============================================================================
# 4. EXTRACT EFFECTS
# =============================================================================

# Full coefficient table
tidy_time <- broom.mixed::tidy(glm_time_model, conf.int = TRUE) %>%
  mutate(
    rr       = exp(estimate),
    rr_low   = exp(conf.low),
    rr_high  = exp(conf.high),
    pct      = 100 * (rr - 1),
    pct_low  = 100 * (rr_low - 1),
    pct_high = 100 * (rr_high - 1)
  )

save_model_table(
  tidy_time, "daily_time_model_coefficients", "tables/main",
  caption = "Main model: daily total time spent (Gamma GLMM)",
  label   = "tab:daily_time_coefficients"
)

# Condition-specific change (delta method)
time_change        <- extract_change_delta_method(glm_time_model)
time_change_report <- format_rr_report(time_change)

cat("\n=== DAILY TIME: CONDITION EFFECTS ===\n")
cat(paste(time_change_report$report, collapse = "\n"), "\n")

save_model_table(
  time_change, "daily_time_condition_effects", "tables/main",
  caption = "Baseline to intervention change in daily time by condition",
  label   = "tab:daily_time_effects"
)

# Predicted marginal means by period × condition
p2_preds <- predict_response(
  glm_time_model,
  terms    = c("period2", "condition"),
  margin   = "empirical",
  type     = "response",
  ci_level = 0.95,
  interval = "prediction"
)

p2_df <- as.data.frame(p2_preds) %>%
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
  p2_df, "daily_time_predicted_change", "tables/main",
  caption = "Predicted daily time by period and condition",
  label   = "tab:daily_time_predicted"
)

# =============================================================================
# 5. PREDICTED TIME SERIES
# =============================================================================

p1_preds <- predict_response(
  glm_time_model,
  terms    = c("time3[all]", "period2", "condition"),
  margin   = "empirical",
  type     = "response",
  ci_level = 0.95,
  interval = "prediction"
)

p1_df <- as.data.frame(p1_preds) %>%
  filter((x <= 10 & group == "baseline") |
         (x  > 10 & group == "intervention")) %>%
  mutate(predicted_hour = predicted / 60)

# =============================================================================
# 6. FIGURES
# =============================================================================

p_time_base <- ggplot(p1_df,
  aes(x = x, y = predicted, group = factor(facet))) +
  geom_segment(aes(x = 10, xend = 10, y = -Inf, yend = 240),
               linetype = "dashed", color = "#000000", linewidth = 0.7) +
  geom_segment(aes(x = 11, xend = 11, y = -Inf, yend = 240),
               linetype = "dashed", color = "#000000", linewidth = 0.7) +
  geom_point(aes(color = factor(facet)), size = 1.5) +
  geom_line( aes(color = factor(facet)), linewidth = 0.8) +
  geom_text(
    data = p1_df %>% filter(x == max(x), facet == "control"),
    aes(label = "Reflection", color = factor(facet)),
    hjust = -0.2, vjust = 0, size = 4,
    family = BOLD_FONT, fontface = "bold", show.legend = FALSE
  ) +
  geom_text(
    data = p1_df %>% filter(x == max(x), facet == "default"),
    aes(label = "Waiting", color = factor(facet)),
    hjust = -0.3, vjust = 0.35, size = 4,
    family = BOLD_FONT, fontface = "bold", show.legend = FALSE
  ) +
  geom_text(
    data = p1_df %>% filter(x == max(x), facet == "intervention"),
    aes(label = "Planning", color = factor(facet)),
    hjust = -0.25, vjust = -0.2, size = 4,
    family = BOLD_FONT, fontface = "bold", show.legend = FALSE
  ) +
  annotate("text", x = 10.5, y = 255,
           label = "Interventions Start",
           color = "#000000", size = 4.4,
           family = BOLD_FONT, fontface = "bold") +
  scale_x_continuous(
    breaks = c(1, 11, 21, 31),
    labels = function(x) paste0("Day ", x + 4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(30, 257),
    breaks = seq(60, 240, by = 60),
    labels = c("1 hour", "2 hours", "3 hours", "4 hours")
  ) +
  scale_color_manual(values = COND_COLOURS, breaks = COND_LEVELS_RAW,
                     labels = unname(COND_LABELS), name = NULL) +
  labs(x = "", y = "") +
  coord_cartesian(xlim = c(0, 36), clip = "off") +
  custom_general_theme_word() +
  theme(
    plot.margin        = margin(t = 10, r = 80, b = 0, l = -10),
    axis.ticks.x       = element_line(color = "#000000"),
    axis.line.x        = element_line(color = "#000000"),
    axis.text.y        = element_text(size = 13, family = BODY_FONT,
                                      color = "#000000"),
    axis.text.x        = element_text(size = 12, family = BODY_FONT,
                                      color = "#000000",
                                      margin = margin(t = 5)),
    panel.grid.major.y = element_line(color = "#000000", linewidth = 0.05),
    legend.position    = "bottom",
    legend.text        = element_text(size = 14, family = BODY_FONT,
                                      color = "#000000"),
    legend.box.margin  = margin(t = -5, b = 15)
  )

p_time_ribbon <- p_time_base +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(facet)),
              alpha = 0.1) +
  scale_fill_manual(values = COND_COLOURS, guide = "none")

save_figure(p_time_base,   "daily_time_its_predicted",        "figures/main")
save_figure(p_time_ribbon, "daily_time_its_predicted_ribbon", "figures/main")

message("\n=== 01_main_daily_time.R complete ===")
