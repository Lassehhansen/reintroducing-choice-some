Sys.setenv(TZ = "Europe/Copenhagen")
# =============================================================================
# 02_analysis/main_effects/02_main_daily_opens.R
#
# MODEL:
#   glmmTMB(daily_opens_smooth ~
#             ns(time3, df=2):period2 + period2 * condition +
#             gender + fall_break + region + klassetrin_short2 +
#             (1 | ID2),
#           family = nbinom2(link="log"))
#
# NOTE: opens are smoothed with a 3-day rolling MEDIAN (not mean) because
#       daily opens are count data with spikes. rollmedian is more robust.
#
# INPUTS:
#   data/baseline_intervention_outro/opens/opens_per_day.csv
#   data/participants.csv      (from 02_demographic_preprocessing.R)
#
# OUTPUTS:
#   models/main/glm_opens_model.rds
#   tables/main/daily_opens_model_coefficients.{xlsx,tex}
#   tables/main/daily_opens_condition_effects.{xlsx,tex}
#   tables/main/daily_opens_predicted_change.{xlsx,tex}
#   figures/main/daily_opens_its_predicted.{png,svg}
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs("models/main", "tables/main", "figures/main")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

opens_per_day <- read_csv(
  "data/baseline_intervention_outro/opens/opens_per_day.csv",
  show_col_types = FALSE
)

participants <- read_csv("data/participants.csv", show_col_types = FALSE)

# =============================================================================
# 2. JOIN, FILTER, SMOOTH
# =============================================================================

opens_smoothed <- opens_per_day %>%
  left_join(participants, by = "ID") %>%
  add_break_covariates("date") %>%
  add_klassetrin_short2() %>%
  filter(
    period  != "mixed",
    !days_before_activation_num %in% c(-14, 0, 29),
    period2 %in% c("baseline", "intervention")
  ) %>%
  factorise_model_data() %>%
  apply_rolling_median("daily_opens", "daily_opens_smooth") %>%   # median for counts
  make_time3() %>%
  dplyr::select(
    daily_opens_smooth, days_before_activation_num,
    period2, condition, time3, time2,
    weekend, fall_break, ID2, ID, day_of_week,
    addiction_index, self_control_index, trivsel,
    age, gender, region, klassetrin_short2
  ) %>%
  drop_na()

message("Model data: ", nrow(opens_smoothed), " rows, ",
        n_distinct(opens_smoothed$ID2), " participants")
print(table(opens_smoothed$condition, opens_smoothed$period2))
print(summary(opens_smoothed$daily_opens_smooth))

# =============================================================================
# 3. FIT MODEL
# =============================================================================

message("Fitting main daily-opens model...")

glm_opens_model <- glmmTMB(
  daily_opens_smooth ~
    ns(time3, df = 2):period2 +
    period2 * condition +
    gender + fall_break +
    region + klassetrin_short2 +
    (1 | ID2),
  data    = opens_smoothed,
  family  = nbinom2(link = "log"),
  control = glmmTMBControl(optCtrl = list(iter.max = 1e5, eval.max = 1e5))
)

summary(glm_opens_model)

saveRDS(glm_opens_model, "models/main/glm_opens_model.rds")
message("Model saved: models/main/glm_opens_model.rds")

# =============================================================================
# 4. EXTRACT EFFECTS
# =============================================================================

tidy_opens <- broom.mixed::tidy(glm_opens_model, conf.int = TRUE) %>%
  mutate(
    rr       = exp(estimate),
    rr_low   = exp(conf.low),
    rr_high  = exp(conf.high),
    pct      = 100 * (rr - 1),
    pct_low  = 100 * (rr_low - 1),
    pct_high = 100 * (rr_high - 1)
  )

save_model_table(
  tidy_opens, "daily_opens_model_coefficients", "tables/main",
  caption = "Main model: daily access attempts (NB2 GLMM)",
  label   = "tab:daily_opens_coefficients"
)

opens_change        <- extract_change_delta_method(glm_opens_model)
opens_change_report <- format_rr_report(opens_change)

cat("\n=== DAILY OPENS: CONDITION EFFECTS ===\n")
cat(paste(opens_change_report$report, collapse = "\n"), "\n")

save_model_table(
  opens_change, "daily_opens_condition_effects", "tables/main",
  caption = "Baseline to intervention change in daily opens by condition",
  label   = "tab:daily_opens_effects"
)

p2_opens <- predict_response(
  glm_opens_model,
  terms    = c("period2", "condition"),
  margin   = "empirical",
  type     = "response",
  ci_level = 0.95,
  interval = "prediction"
)

p2_opens_df <- as.data.frame(p2_opens) %>%
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
  p2_opens_df, "daily_opens_predicted_change", "tables/main",
  caption = "Predicted daily opens by period and condition",
  label   = "tab:daily_opens_predicted"
)

# =============================================================================
# 5. PREDICTED TIME SERIES
# =============================================================================

p1_opens <- predict_response(
  glm_opens_model,
  terms    = c("time3[all]", "period2", "condition"),
  margin   = "empirical",
  type     = "response",
  ci_level = 0.95,
  interval = "prediction"
)

p1_opens_df <- as.data.frame(p1_opens) %>%
  filter((x <= 10 & group == "baseline") |
         (x  > 10 & group == "intervention"))

# =============================================================================
# 6. FIGURE
# =============================================================================

p_opens <- ggplot(p1_opens_df,
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
    data = p1_opens_df %>% filter(x == max(x), facet == "control"),
    aes(label = "Reflection", color = factor(facet)),
    hjust = -0.2, size = 4, family = BOLD_FONT, show.legend = FALSE
  ) +
  geom_text(
    data = p1_opens_df %>% filter(x == max(x), facet == "default"),
    aes(label = "Waiting", color = factor(facet)),
    hjust = -0.2, size = 4, family = BOLD_FONT, show.legend = FALSE
  ) +
  geom_text(
    data = p1_opens_df %>% filter(x == max(x), facet == "intervention"),
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
  scale_color_manual(values = COND_COLOURS, breaks = COND_LEVELS_RAW,
                     labels = unname(COND_LABELS), name = NULL) +
  scale_fill_manual(values = COND_COLOURS, guide = "none") +
  labs(x = "", y = "Daily access attempts") +
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

save_figure(p_opens, "daily_opens_its_predicted", "figures/main")

message("\n=== 02_main_daily_opens.R complete ===")
