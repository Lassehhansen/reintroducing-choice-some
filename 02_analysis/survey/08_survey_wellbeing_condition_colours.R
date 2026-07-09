Sys.setenv(TZ = "Europe/Copenhagen")
# =============================================================================
# 02_analysis/survey/08_survey_wellbeing.R
#
# PURPOSE:
#   Test whether the intervention affected self-reported survey outcomes
#   from baseline to post-intervention.
#   Outcomes: addiction index, self-control index, FoMO, social connection,
#   satisfaction with social media use, well-being (trivsel), perceived
#   overuse (retention_index), and perceived control over social media use
#   (kontrolover_so_me).
#
#   Mixed-effects models with survey wave as a within-person factor.
#
# INPUTS:
#   data/experiment_list.xlsx
#   data/survey/survey_data_clean_wide.csv
#   data/survey/survey_data_clean_long.csv
#   data/baseline/individual_differences.csv
#
# OUTPUTS:
#   tables/survey/wellbeing_model_results.{xlsx,tex}
#   tables/survey/wellbeing_descriptive_by_wave.{xlsx,tex}
#   figures/survey/wellbeing_change_by_condition.{png,svg}
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs("tables/survey", "figures/survey")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

id_list <- readxl::read_excel("00_data/experiment_list.xlsx")
eligible <- unique(id_list$ID)

survey_wide <- read_csv("data/survey/survey_data_clean_wide.csv",
                        show_col_types = FALSE) %>%
  mutate(
    condition = case_when(
      cond %in% c(1, 2) ~ "control",
      cond %in% c(5, 6) ~ "intervention",
      cond %in% c(3, 4) ~ "default",
      TRUE ~ NA_character_
    )
  )

survey_long <- read_csv("data/survey/survey_data_clean_long.csv",
                        show_col_types = FALSE) %>%
  mutate(
    condition = case_when(
      cond %in% c(1, 2) ~ "control",
      cond %in% c(5, 6) ~ "intervention",
      cond %in% c(3, 4) ~ "default",
      TRUE ~ NA_character_
    )
  )

# Filter to eligible experiment participants
survey_long_elig <- survey_long %>%
  filter(response_id %in% eligible,
         survey_nr %in% c(1, 2))

# =============================================================================
# 2. DESCRIPTIVE TABLE BY WAVE
# =============================================================================

wellbeing_outcomes <- c(
  "addiction_index",
  "self_control_index",
  "fo_mo",
  "socialconnection",
  "trivsel",
  "tilfredsmed_so_me",
  "retention_index",
  "kontrolover_so_me"
)

outcome_labels <- c(
  addiction_index = "Social-media addiction",
  self_control_index = "Self-control",
  fo_mo = "Fear of missing out",
  socialconnection = "Social connection",
  trivsel = "Well-being",
  tilfredsmed_so_me = "Satisfaction with social media",
  retention_index = "Perceived overuse",
  kontrolover_so_me = "Perceived control over social media"
)

desc_by_wave <- survey_long_elig %>%
  filter(survey_nr %in% c(1, 2)) %>%
  pivot_longer(cols = all_of(wellbeing_outcomes),
               names_to = "outcome", values_to = "value") %>%
  group_by(outcome, survey_nr, condition) %>%
  summarise(
    n    = sum(!is.na(value)),
    mean = round(mean(value, na.rm = TRUE), 3),
    sd   = round(sd(value,   na.rm = TRUE), 3),
    .groups = "drop"
  )

save_model_table(
  desc_by_wave, "wellbeing_descriptive_by_wave", "tables/survey",
  caption = "Well-being outcomes by survey wave and condition (mean, SD)",
  label   = "tab:wellbeing_descriptive"
)

# =============================================================================
# 3. MIXED-EFFECTS MODELS
#    For each outcome: lmer(outcome ~ survey_nr_fact * condition + (1|ID))
# =============================================================================

survey_long_elig <- survey_long_elig %>%
  mutate(
    survey_nr_fact = factor(survey_nr, levels = c(1, 2),
                            labels = c("Baseline","Post-intervention")),
    cond_fact = factor(condition, levels = COND_LEVELS_RAW)
  )

run_wellbeing_model <- function(outcome, data) {
  formula_str <- paste0(outcome,
    " ~ survey_nr_fact + cond_fact + survey_nr_fact:cond_fact + (1|response_id)")
  tryCatch({
    m <- lmer(as.formula(formula_str), data = data)
    tidy_m <- broom.mixed::tidy(m, conf.int = TRUE) %>%
      filter(effect == "fixed") %>%
      mutate(outcome = outcome)
    tidy_m
  }, error = function(e) {
    warning("Model failed for ", outcome, ": ", conditionMessage(e))
    NULL
  })
}

all_results <- purrr::map_dfr(wellbeing_outcomes,
                               run_wellbeing_model,
                               data = survey_long_elig)

if (!is.null(all_results) && nrow(all_results) > 0) {
  cat("\n=== WELL-BEING MODELS: KEY INTERACTION TERMS ===\n")
  interaction_terms <- all_results %>%
    filter(str_detect(term, "survey_nr_fact.*cond_fact|cond_fact.*survey_nr_fact")) %>%
    dplyr::select(outcome, term, estimate, std.error, statistic, p.value,
                  conf.low, conf.high)
  print(interaction_terms)

  save_model_table(
    all_results, "wellbeing_model_results", "tables/survey",
    caption = "Well-being outcomes: mixed-effects model results",
    label   = "tab:wellbeing_models"
  )
}

# =============================================================================
# 4. FIGURE: condition-specific baseline-to-post change estimates
# =============================================================================

# This mirrors the original survey_comparison1_2.Rmd visualization logic:
# for each outcome and condition, estimate the within-condition change from
# baseline to post-intervention using a mixed model with participant random
# intercepts. Points are colored by intervention condition, not by p-value.

condition_plot_levels <- c("Reflection", "Waiting", "Planning")

condition_colours_eng <- c(
  "Reflection" = COND_COLOURS[["control"]],
  "Planning" = COND_COLOURS[["intervention"]],
  "Waiting" = COND_COLOURS[["default"]]
)

fit_change_within_condition <- function(data, outcome) {
  data %>%
    filter(!is.na(.data[[outcome]])) %>%
    group_split(cond_fact) %>%
    purrr::map_dfr(function(df_cond) {
      if (n_distinct(df_cond$response_id) < 2 ||
          n_distinct(df_cond$survey_nr_fact) < 2) {
        return(NULL)
      }

      model <- lmer(
        as.formula(paste0(outcome, " ~ survey_nr_fact + (1 | response_id)")),
        data = df_cond
      )

      broom.mixed::tidy(model, conf.int = TRUE) %>%
        filter(str_detect(term, "^survey_nr_fact")) %>%
        mutate(
          outcome = outcome,
          outcome_label = outcome_labels[[outcome]],
          condition = as.character(unique(df_cond$condition)),
          condition2 = recode(
            condition,
            "control" = "Reflection",
            "intervention" = "Planning",
            "default" = "Waiting"
          )
        )
    })
}

condition_change_results <- purrr::map_dfr(
  wellbeing_outcomes,
  fit_change_within_condition,
  data = survey_long_elig
) %>%
  mutate(
    p_value_category = case_when(
      p.value < 0.001 ~ "p < 0.001 ***",
      p.value < 0.01 ~ "p < 0.01 **",
      p.value < 0.05 ~ "p < 0.05 *",
      TRUE ~ "ns"
    ),
    p_value_category = factor(
      p_value_category,
      levels = c("ns", "p < 0.05 *", "p < 0.01 **", "p < 0.001 ***")
    ),
    condition2 = factor(condition2, levels = condition_plot_levels),
    outcome_label = factor(
      outcome_label,
      levels = unname(outcome_labels[wellbeing_outcomes])
    )
  )

save_model_table(
  condition_change_results,
  "wellbeing_condition_change_results",
  "tables/survey",
  caption = "Condition-specific baseline-to-post-intervention change estimates for survey outcomes",
  label = "tab:wellbeing_condition_change_results"
)

write_csv(
  condition_change_results,
  "tables/survey/wellbeing_condition_change_results.csv"
)

# Retain a descriptive mean-change table for auditability.
survey_change <- survey_long_elig %>%
  dplyr::select(response_id, condition, survey_nr, all_of(wellbeing_outcomes)) %>%
  pivot_longer(
    cols = all_of(wellbeing_outcomes),
    names_to = "outcome",
    values_to = "value"
  ) %>%
  mutate(
    condition2 = factor(
      recode(condition,
             "control" = "Reflection",
             "intervention" = "Planning",
             "default" = "Waiting"),
      levels = condition_plot_levels
    ),
    outcome_label = recode(outcome, !!!outcome_labels)
  ) %>%
  filter(!is.na(condition2)) %>%
  dplyr::select(response_id, condition2, outcome, outcome_label, survey_nr, value) %>%
  pivot_wider(
    names_from = survey_nr,
    values_from = value,
    names_prefix = "survey_"
  ) %>%
  mutate(
    change = survey_2 - survey_1
  )

change_summary <- survey_change %>%
  group_by(condition2, outcome, outcome_label) %>%
  summarise(
    n = sum(!is.na(change)),
    mean_change = mean(change, na.rm = TRUE),
    se = sd(change, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    outcome_label = factor(
      outcome_label,
      levels = unname(outcome_labels[wellbeing_outcomes])
    )
  )

save_model_table(
  change_summary,
  "wellbeing_change_by_condition_summary",
  "tables/survey",
  caption = "Descriptive baseline-to-post-intervention change in survey outcomes by condition",
  label = "tab:wellbeing_change_summary"
)

p_wellbeing <- ggplot(
  condition_change_results,
  aes(x = condition2, y = estimate, fill = condition2)
) +
  geom_point(
    shape = 23,
    size = 5,
    color = "#000000"
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = condition2),
    width = 0.02,
    linewidth = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "#000000",
    linewidth = 0.5
  ) +
  coord_flip() +
  facet_wrap(
    ~ outcome_label,
    nrow = 4,
    strip.position = "top"
  ) +
  scale_y_continuous(
    limits = c(-0.8, 0.8),
    breaks = seq(-0.5, 0.5, by = 0.5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = condition_colours_eng, name = NULL) +
  scale_color_manual(values = condition_colours_eng, guide = "none") +
  labs(
    title = "",
    x = "",
    y = "Estimate",
    fill = ""
  ) +
  custom_general_theme_word() +
  theme(
    panel.border = element_rect(fill = NA, color = "#000000", linewidth = 0.5),
    panel.background = element_rect(fill = NA, color = "#000000", linewidth = 0.5),
    strip.background = element_rect(fill = NA, color = "#000000", linewidth = 0.5),
    legend.position = "none",
    strip.text.x = element_text(
      size = 12,
      family = BOLD_FONT,
      color = "#000000",
      angle = 0
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
    axis.text = element_text(
      size = 12,
      family = BODY_FONT,
      color = "#000000"
    ),
    panel.grid.major.x = element_line(
      color = "#000000",
      linetype = "dashed",
      linewidth = 0.15
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_line(color = "#000000"),
    panel.spacing.y = unit(0.5, "lines"),
    plot.margin = margin(t = 0, r = 12, b = 10, l = -20)
  )

save_figure(
  p_wellbeing,
  "wellbeing_change_by_condition",
  "figures/survey",
  width = 12,
  height = 8
)

message("\n=== 08_survey_wellbeing.R complete ===")
