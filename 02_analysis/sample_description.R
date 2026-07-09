Sys.setenv(TZ = "Europe/Copenhagen")
# =============================================================================
# sample_description.R
#
# PURPOSE:
#   Produce every number reported in the Participants section of the paper
#   and the supplementary demographics table (Table S1).
#
# INPUTS:
#   data/data_filt_step4_all_apps.csv        -- cleaned event-level dataset
#   data/participants.csv                    -- one row per participant
#   data/baseline/individual_differences.csv -- baseline behaviour per person
#   data/baseline_intervention_outro/session_time/time_per_day.csv
#   data/baseline_intervention_outro/opens/opens_per_day.csv
#   data/baseline_intervention_outro/session_time/time_per_day_avg.csv
#   reports/03_app_preprocessing/exclusion_summary.csv
#   data/survey/survey_data_clean_long.csv   -- for survey completion counts
#
# OUTPUTS:
#   reports/sample_description/sample_sizes.csv
#   reports/sample_description/attrition_by_condition.csv
#   reports/sample_description/demographics_table.xlsx
#   reports/sample_description/demographics_table.tex
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs("reports/sample_description")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

events        <- read_csv("data/data_filt_step4_all_apps.csv",
                          show_col_types = FALSE)
participants  <- read_csv("data/participants.csv",
                          show_col_types = FALSE)
ind_diff      <- read_csv("data/baseline/individual_differences.csv",
                          show_col_types = FALSE)
time_per_day  <- read_csv(
  "data/baseline_intervention_outro/session_time/time_per_day.csv",
  show_col_types = FALSE)
opens_per_day <- read_csv(
  "data/baseline_intervention_outro/opens/opens_per_day.csv",
  show_col_types = FALSE)
time_per_day_avg <- read_csv(
  "data/baseline_intervention_outro/session_time/time_per_day_avg.csv",
  show_col_types = FALSE)
survey_long   <- read_csv("data/survey/survey_data_clean_long.csv",
                          show_col_types = FALSE)

# =============================================================================
# 2. ANALYTIC SAMPLE SIZES
# =============================================================================

# --- 2a. Behavioural dataset (all apps, any event) ---------------------------
n_behavioural <- n_distinct(events$ID)
n_by_cond_beh <- events %>%
  distinct(ID, condition) %>%
  count(condition, name = "n")

# --- 2b. Opens model (opens_per_day after preprocessing filters) -------------
opens_model_data <- opens_per_day %>%
  left_join(participants, by = "ID") %>%
  add_break_covariates("date") %>%
  add_klassetrin_short2() %>%
  filter(
    period  != "mixed",
    !days_before_activation_num %in% c(-14, 0, 29),
    period2 %in% c("baseline", "intervention")
  ) %>%
  factorise_model_data() %>%
  apply_rolling_median("daily_opens", "daily_opens_smooth") %>%
  make_time3() %>%
  dplyr::select(
    daily_opens_smooth, days_before_activation_num,
    period2, condition, time3, ID2, ID,
    gender, fall_break, region, klassetrin_short2
  ) %>%
  drop_na()

n_opens_model      <- n_distinct(opens_model_data$ID)
n_opens_by_cond    <- opens_model_data %>%
  distinct(ID, condition) %>%
  count(condition, name = "n")

# --- 2c. Total daily time model ----------------------------------------------
time_model_data <- time_per_day %>%
  left_join(participants, by = "ID") %>%
  add_break_covariates("date") %>%
  add_klassetrin_short2() %>%
  filter(
    period  != "mixed",
    !days_before_activation_num %in% c(-14, 0, 29),
    period2 %in% c("baseline", "intervention")
  ) %>%
  factorise_model_data() %>%
  apply_rolling_smooth("total_time_minutes", "total_time_minutes_smooth") %>%
  make_time3() %>%
  dplyr::select(
    total_time_minutes_smooth, days_before_activation_num,
    period2, condition, time3, ID2, ID,
    gender, fall_break, region, klassetrin_short2
  ) %>%
  drop_na()

n_time_model    <- n_distinct(time_model_data$ID)
n_time_by_cond  <- time_model_data %>%
  distinct(ID, condition) %>%
  count(condition, name = "n")

# --- 2d. Average session duration model --------------------------------------
session_model_data <- time_per_day_avg %>%
  left_join(participants, by = "ID") %>%
  add_break_covariates("date") %>%
  add_klassetrin_short2() %>%
  filter(
    period  != "mixed",
    !days_before_activation_num %in% c(-14, 0, 29),
    period2 %in% c("baseline", "intervention")
  ) %>%
  factorise_model_data() %>%
  apply_rolling_smooth("mean_time_minutes", "mean_time_minutes_smooth") %>%
  make_time3() %>%
  dplyr::select(
    mean_time_minutes_smooth, days_before_activation_num,
    period2, condition, time3, ID2, ID,
    gender, fall_break, region, klassetrin_short2
  ) %>%
  drop_na()

n_session_model   <- n_distinct(session_model_data$ID)
n_session_by_cond <- session_model_data %>%
  distinct(ID, condition) %>%
  count(condition, name = "n")

# --- 2e. Survey completion ---------------------------------------------------
n_survey_baseline <- survey_long %>%
  filter(survey_nr == 1, response_id %in% unique(events$ID)) %>%
  n_distinct(.$response_id)

n_survey_followup <- survey_long %>%
  filter(survey_nr == 2, response_id %in% unique(events$ID)) %>%
  n_distinct(.$response_id)

# Fix: count distinct IDs properly
n_survey_baseline <- survey_long %>%
  filter(survey_nr == 1, response_id %in% unique(events$ID)) %>%
  pull(response_id) %>% n_distinct()

n_survey_followup <- survey_long %>%
  filter(survey_nr == 2, response_id %in% unique(events$ID)) %>%
  pull(response_id) %>% n_distinct()

# =============================================================================
# 3. ATTRITION DURING INTERVENTION PERIOD
# =============================================================================

# Active = had at least one openedApp event in the final 7 days of intervention
# Intervention window: days_before_activation_num 0 to 27
# Final 7 days: days 21–27

final_7_active <- events %>%
  filter(
    period == "intervention",
    participation_day >= (max(participation_day[period == "intervention"],
                              na.rm = TRUE) - 6)
  ) %>%
  distinct(ID) %>%
  pull(ID)

# Use event-level data to find intervention participants and those still active
int_participants <- events %>%
  filter(period == "intervention") %>%
  distinct(ID, condition)

n_int_total <- nrow(int_participants)

attrition_by_cond <- int_participants %>%
  mutate(
    active_end = ID %in% final_7_active,
    condition2 = dplyr::recode(condition,
                        control      = "Reflection",
                        intervention = "Planning",
                        default      = "Waiting")
  ) %>%
  group_by(condition2) %>%
  summarise(
    n_total       = n(),
    n_active_end  = sum(active_end),
    n_attrited    = n_total - n_active_end,
    attrition_pct = round(100 * n_attrited / n_total, 1),
    .groups = "drop"
  )

overall_attrition <- attrition_by_cond %>%
  summarise(
    condition2    = "Overall",
    n_total       = sum(n_total),
    n_active_end  = sum(n_active_end),
    n_attrited    = sum(n_attrited),
    attrition_pct = round(100 * n_attrited / n_total, 1)
  )

attrition_table <- bind_rows(overall_attrition, attrition_by_cond)

# =============================================================================
# 4. PRINT ALL KEY NUMBERS
# =============================================================================

cat("\n")
cat("=============================================================\n")
cat("SAMPLE SIZES\n")
cat("=============================================================\n")
cat("Behavioural dataset (all apps):          n =", n_behavioural, "\n")
cat("  Reflection:                            n =",
    n_by_cond_beh$n[n_by_cond_beh$condition == "control"], "\n")
cat("  Planning:                              n =",
    n_by_cond_beh$n[n_by_cond_beh$condition == "intervention"], "\n")
cat("  Waiting:                               n =",
    n_by_cond_beh$n[n_by_cond_beh$condition == "default"], "\n")
cat("\n")
cat("Opens model (access attempts):           n =", n_opens_model, "\n")
cat("  Reflection:                            n =",
    n_opens_by_cond$n[n_opens_by_cond$condition == "control"], "\n")
cat("  Planning:                              n =",
    n_opens_by_cond$n[n_opens_by_cond$condition == "intervention"], "\n")
cat("  Waiting:                               n =",
    n_opens_by_cond$n[n_opens_by_cond$condition == "default"], "\n")
cat("\n")
cat("Total daily time model:                  n =", n_time_model, "\n")
cat("  Reflection:                            n =",
    n_time_by_cond$n[n_time_by_cond$condition == "control"], "\n")
cat("  Planning:                              n =",
    n_time_by_cond$n[n_time_by_cond$condition == "intervention"], "\n")
cat("  Waiting:                               n =",
    n_time_by_cond$n[n_time_by_cond$condition == "default"], "\n")
cat("\n")
cat("Average session duration model:          n =", n_session_model, "\n")
cat("  Reflection:                            n =",
    n_session_by_cond$n[n_session_by_cond$condition == "control"], "\n")
cat("  Planning:                              n =",
    n_session_by_cond$n[n_session_by_cond$condition == "intervention"], "\n")
cat("  Waiting:                               n =",
    n_session_by_cond$n[n_session_by_cond$condition == "default"], "\n")
cat("\n")
cat("Survey completion:\n")
cat("  Baseline survey:                       n =", n_survey_baseline, "\n")
cat("  Post-intervention survey:              n =", n_survey_followup, "\n")
cat("\n")
cat("=============================================================\n")
cat("ATTRITION\n")
cat("=============================================================\n")
print(attrition_table)

# =============================================================================
# 5. DEMOGRAPHICS TABLE
# =============================================================================

demo <- participants %>%
  filter(ID %in% unique(events$ID)) %>%
  left_join(survey_long %>% filter(survey_nr == 1) %>% rename("ID" = "response_id") %>%  select(ID, condition)) %>% 
  mutate(
    condition2 = factor(
      dplyr::recode(condition,
             control      = "Reflection",
             intervention = "Planning",
             default      = "Waiting"),
      levels = c("Reflection", "Planning", "Waiting")
    ),
    gender_label = dplyr::recode(as.character(gender),
                          mand   = "Male",
                          kvinde = "Female"),
    age = as.numeric(age)
  )

fmt_mean_sd <- function(x)
  sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))

fmt_n_pct <- function(x, total)
  sprintf("%d (%.1f%%)", sum(x, na.rm = TRUE),
          100 * sum(x, na.rm = TRUE) / total)

make_demo_col <- function(df) {
  n <- nrow(df)
  tibble(
    Variable = c(
      "N",
      "Age, mean (SD)",
      "Female, n (%)",
      "Male, n (%)",
      "Primary Education (7.–10. klasse), n (%)",
      "Secondary Education (Ungdomsuddannelse), n (%)",
      "Boarding School (Efterskole), n (%)",
      "Other / Missing, n (%)",
      "Addiction index, mean (SD)",
      "Self-control index, mean (SD)",
      "Daily time at baseline (min), mean (SD)",
      "Daily opens at baseline, median (SD)"
    ),
    Value = c(
      as.character(n),
      fmt_mean_sd(df$age),
      fmt_n_pct(df$gender_label == "Female", n),
      fmt_n_pct(df$gender_label == "Male", n),
      fmt_n_pct(df$klassetrin_short2 == "Primary Education", n),
      fmt_n_pct(df$klassetrin_short2 == "Secondary Education", n),
      fmt_n_pct(df$klassetrin_short2 == "Boarding School", n),
      fmt_n_pct(df$klassetrin_short2 == "Others", n),
      fmt_mean_sd(df$addiction_index),
      fmt_mean_sd(df$self_control_index),
      fmt_mean_sd(df$total_time_minutes),
      sprintf("%.1f (%.1f)",
              median(df$daily_opens_median, na.rm = TRUE),
              sd(df$daily_opens_median,     na.rm = TRUE))
    )
  )
}

# Merge baseline behaviour into demographics
demo_with_baseline <- demo %>%
  left_join(
    ind_diff %>% dplyr::select(ID, total_time_minutes, daily_opens_median),
    by = "ID"
  )

# Build one column per condition + overall
col_overall    <- make_demo_col(demo_with_baseline) %>% rename(Overall = Value)
col_reflection <- make_demo_col(demo_with_baseline %>%
                                  filter(condition2 == "Reflection")) %>%
  rename(Reflection = Value)
col_planning   <- make_demo_col(demo_with_baseline %>%
                                  filter(condition2 == "Planning")) %>%
  rename(Planning = Value)
col_waiting    <- make_demo_col(demo_with_baseline %>%
                                  filter(condition2 == "Waiting")) %>%
  rename(Waiting = Value)

demographics_table <- col_overall %>%
  left_join(col_reflection, by = "Variable") %>%
  left_join(col_planning,   by = "Variable") %>%
  left_join(col_waiting,    by = "Variable")

cat("\n=============================================================\n")
cat("DEMOGRAPHICS TABLE\n")
cat("=============================================================\n")
print(demographics_table, n = Inf)

# =============================================================================
# 6. BALANCE TESTS  (printed alongside the table)
# =============================================================================

cat("\n=== BALANCE TESTS (ANOVA / chi-square) ===\n")

run_anova <- function(var, label) {
  f <- as.formula(paste(var, "~ condition2"))
  m <- aov(f, data = demo_with_baseline)
  p <- summary(m)[[1]]$`Pr(>F)`[1]
  cat(sprintf("  %-45s F-test p = %.3f\n", label, p))
}

run_chisq <- function(var, label) {
  tbl <- table(demo_with_baseline[[var]], demo_with_baseline$condition2)
  p   <- chisq.test(tbl, simulate.p.value = TRUE)$p.value
  cat(sprintf("  %-45s chi-sq p = %.3f\n", label, p))
}

run_anova("age",                  "Age")
run_chisq("gender_label",         "Gender")
run_chisq("klassetrin_short2",    "School type")
run_anova("addiction_index",      "Addiction index")
run_anova("self_control_index",   "Self-control index")
run_anova("total_time_minutes",   "Daily time at baseline")
run_anova("daily_opens_median",   "Daily opens at baseline")

# =============================================================================
# 7. SAVE OUTPUTS
# =============================================================================

# Sample sizes summary
sample_sizes <- tibble(
  outcome = c("Behavioural dataset (all apps)",
              "Opens model",
              "Total daily time model",
              "Average session duration model",
              "Baseline survey completed",
              "Post-intervention survey completed"),
  n_total = c(n_behavioural, n_opens_model,
              n_time_model, n_session_model,
              n_survey_baseline, n_survey_followup),
  n_reflection = c(
    n_by_cond_beh$n[n_by_cond_beh$condition == "control"],
    n_opens_by_cond$n[n_opens_by_cond$condition == "control"],
    n_time_by_cond$n[n_time_by_cond$condition == "control"],
    n_session_by_cond$n[n_session_by_cond$condition == "control"],
    NA, NA
  ),
  n_planning = c(
    n_by_cond_beh$n[n_by_cond_beh$condition == "intervention"],
    n_opens_by_cond$n[n_opens_by_cond$condition == "intervention"],
    n_time_by_cond$n[n_time_by_cond$condition == "intervention"],
    n_session_by_cond$n[n_session_by_cond$condition == "intervention"],
    NA, NA
  ),
  n_waiting = c(
    n_by_cond_beh$n[n_by_cond_beh$condition == "default"],
    n_opens_by_cond$n[n_opens_by_cond$condition == "default"],
    n_time_by_cond$n[n_time_by_cond$condition == "default"],
    n_session_by_cond$n[n_session_by_cond$condition == "default"],
    NA, NA
  )
)

write_csv(sample_sizes,    "reports/sample_description/sample_sizes.csv")
write_csv(attrition_table, "reports/sample_description/attrition_by_condition.csv")

# Demographics table — Excel + LaTeX
write_xlsx(demographics_table,
           "reports/sample_description/demographics_table.xlsx")

latex_tbl <- demographics_table %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    align     = c("l", "r", "r", "r", "r"),
    caption   = "Baseline characteristics by experimental condition",
    label     = "demographics"
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  add_header_above(c(" " = 1, "Overall" = 1,
                     "Reflection" = 1, "Planning" = 1, "Waiting" = 1))

writeLines(as.character(latex_tbl),
           "reports/sample_description/demographics_table.tex")

message("\n✓ All outputs saved to reports/sample_description/")
