
Sys.setenv(TZ = "Europe/Copenhagen")

# =============================================================================
# 04_supplementary/sample_characteristics/sample_characteristics.R
#
# PURPOSE:
#   Generate a baseline characteristics table for the analytic sample,
#   including demographic, behavioural, and psychological variables,
#   broken down by experimental condition.
#
# INPUTS:
#   data/experiment_list.xlsx
#   data/demographic/demographic_data.csv
#   data/survey/survey_data_clean_long.csv
#   data/survey/klassetrin_clean.csv
#   data/baseline/individual_differences.csv
#
# OUTPUTS:
#   tables/supplementary/table1_sample_characteristics.{xlsx,tex}
#   tables/supplementary/table1_balance_tests.{xlsx,tex}
#   figures/supplementary/sample_age_distribution.{png,svg}
#   figures/supplementary/sample_baseline_use_by_condition.{png,svg}
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs(
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
  transmute(ID = as.character(ID)) %>%
  filter(
    !is.na(ID),
    ID != ""
  ) %>%
  distinct() %>%
  pull(ID)

demografi <- read_csv(
  "data/demographic/demographic_data.csv",
  show_col_types = FALSE
) %>%
  mutate(
    ID = as.character(ID)
  )

survey_long <- read_csv(
  "data/survey/survey_data_clean_long.csv",
  show_col_types = FALSE
) %>%
  mutate(
    response_id = as.character(response_id)
  ) %>%
  filter(
    survey_nr == 1,
    response_id %in% eligible
  ) %>%
  rename(
    ID = response_id
  ) %>%
  dplyr::select(
    -any_of("cond")
  )

klassetrin <- read_csv(
  "data/survey/klassetrin_clean.csv",
  show_col_types = FALSE
) %>%
  mutate(
    ID = as.character(ID)
  )

ind_diff <- read_csv(
  "data/baseline/individual_differences.csv",
  show_col_types = FALSE
) %>%
  mutate(
    ID = as.character(ID)
  )

# =============================================================================
# 2. MERGE INTO ONE PARTICIPANT-LEVEL DATASET
# =============================================================================

participants <- survey_long %>%
  left_join(
    demografi,
    by = "ID"
  ) %>%
  left_join(
    klassetrin,
    by = "ID"
  ) %>%
  left_join(
    ind_diff,
    by = "ID"
  ) %>%
  filter(
    ID %in% eligible
  ) %>%
  add_klassetrin_short2() %>%
  mutate(
    condition2 = factor(
      condition,
      levels = COND_LEVELS_RAW,
      labels = COND_LEVELS_ENG
    ),
    gender_label = recode(
      as.character(gender),
      mand = "Male",
      kvinde = "Female",
      .default = NA_character_
    ),
    education_label = case_when(
      klassetrin_short2 %in% c(
        "Primary Education",
        "Secondary Education",
        "Boarding School"
      ) ~ klassetrin_short2,
      TRUE ~ "Other / Missing"
    ),
    education_label = factor(
      education_label,
      levels = c(
        "Primary Education",
        "Secondary Education",
        "Boarding School",
        "Other / Missing"
      )
    ),
    age = as.numeric(age)
  )

N_total <- nrow(participants)

cat("Total eligible participants: ", N_total, "\n")

cat("\n=== EDUCATION AUDIT ===\n")
participants %>%
  count(
    condition2,
    education_label,
    useNA = "ifany"
  ) %>%
  arrange(
    condition2,
    education_label
  ) %>%
  print(n = Inf)

cat("\n=== RAW KLASSETRIN AUDIT ===\n")
participants %>%
  count(
    klassetrin,
    klassetrin_short2,
    education_label,
    useNA = "ifany"
  ) %>%
  arrange(
    education_label,
    klassetrin
  ) %>%
  print(n = Inf)

# =============================================================================
# 3. TABLE 1: SAMPLE CHARACTERISTICS BY CONDITION
# =============================================================================

fmt_mean_sd <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  if (all(is.na(x))) {
    return("NA")
  }
  
  sprintf(
    "%.1f (%.1f)",
    mean(x, na.rm = TRUE),
    sd(x, na.rm = TRUE)
  )
}

fmt_median_sd <- function(median_x, sd_x) {
  median_x <- suppressWarnings(as.numeric(median_x))
  sd_x <- suppressWarnings(as.numeric(sd_x))
  
  if (all(is.na(median_x)) || all(is.na(sd_x))) {
    return("NA")
  }
  
  sprintf(
    "%.1f (%.1f)",
    median(median_x, na.rm = TRUE),
    mean(sd_x, na.rm = TRUE)
  )
}

fmt_pct <- function(x, n) {
  count <- sum(x, na.rm = TRUE)
  
  if (is.na(n) || n == 0) {
    return("0 (NA)")
  }
  
  sprintf(
    "%d (%.1f%%)",
    count,
    100 * count / n
  )
}

summarise_condition <- function(df, cond_label) {
  n <- nrow(df)
  
  tibble(
    condition = cond_label,
    n = n,
    
    # Demographics
    age_mean_sd = fmt_mean_sd(df$age),
    female_n_pct = fmt_pct(df$gender_label == "Female", n),
    male_n_pct = fmt_pct(df$gender_label == "Male", n),
    gender_missing_n_pct = fmt_pct(is.na(df$gender_label), n),
    
    # Education
    primary_n_pct = fmt_pct(df$education_label == "Primary Education", n),
    secondary_n_pct = fmt_pct(df$education_label == "Secondary Education", n),
    boarding_n_pct = fmt_pct(df$education_label == "Boarding School", n),
    other_missing_education_n_pct = fmt_pct(df$education_label == "Other / Missing", n),
    
    # Psychological scales, Survey 1
    addiction_index_mean_sd = fmt_mean_sd(df$addiction_index),
    self_control_index_mean_sd = fmt_mean_sd(df$self_control_index),
    fo_mo_mean_sd = fmt_mean_sd(df$fo_mo),
    trivsel_mean_sd = fmt_mean_sd(df$trivsel),
    socialconnection_mean_sd = fmt_mean_sd(df$socialconnection),
    
    # Baseline behaviour
    daily_time_min_mean_sd = fmt_mean_sd(df$total_time_minutes),
    daily_opens_median_sd = fmt_median_sd(
      df$daily_opens_median,
      df$sd_daily_opens
    ),
    session_length_min_mean_sd = fmt_mean_sd(df$mean_time_minutes)
  )
}

tbl_overall <- summarise_condition(
  participants,
  "Overall"
)

tbl_by_cond <- purrr::map_dfr(
  COND_LEVELS_ENG,
  ~ summarise_condition(
    participants %>% filter(condition2 == .x),
    .x
  )
)

table1_wide <- bind_rows(
  tbl_overall,
  tbl_by_cond
)


table1 <- table1_wide %>%
  mutate(
    across(
      -condition,
      as.character
    )
  ) %>%
  pivot_longer(
    cols = -condition,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(
      variable,
      n = "N",
      age_mean_sd = "Age, mean (SD)",
      female_n_pct = "Female, n (%)",
      male_n_pct = "Male, n (%)",
      gender_missing_n_pct = "Gender missing, n (%)",
      primary_n_pct = "Primary Education, n (%)",
      secondary_n_pct = "Secondary Education, n (%)",
      boarding_n_pct = "Boarding School, n (%)",
      other_missing_education_n_pct = "Other / Missing education, n (%)",
      addiction_index_mean_sd = "Addiction index, mean (SD)",
      self_control_index_mean_sd = "Self-control index, mean (SD)",
      fo_mo_mean_sd = "FOMO, mean (SD)",
      trivsel_mean_sd = "Well-being, mean (SD)",
      socialconnection_mean_sd = "Social connection, mean (SD)",
      daily_time_min_mean_sd = "Daily time baseline, mean (SD)",
      daily_opens_median_sd = "Daily opens baseline, median (SD)",
      session_length_min_mean_sd = "Session duration baseline, mean (SD)"
    ),
    variable = factor(
      variable,
      levels = c(
        "N",
        "Age, mean (SD)",
        "Female, n (%)",
        "Male, n (%)",
        "Gender missing, n (%)",
        "Primary Education, n (%)",
        "Secondary Education, n (%)",
        "Boarding School, n (%)",
        "Other / Missing education, n (%)",
        "Addiction index, mean (SD)",
        "Self-control index, mean (SD)",
        "FOMO, mean (SD)",
        "Well-being, mean (SD)",
        "Social connection, mean (SD)",
        "Daily time baseline, mean (SD)",
        "Daily opens baseline, median (SD)",
        "Session duration baseline, mean (SD)"
      )
    )
  ) %>%
  arrange(variable) %>%
  pivot_wider(
    names_from = condition,
    values_from = value
  )

save_model_table(
  table1,
  "table1_sample_characteristics",
  "tables/supplementary",
  caption = "Sample characteristics at baseline by experimental condition",
  label = "tab:table1_sample"
)

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    table1_wide,
    "tables/supplementary/table1_sample_characteristics.xlsx"
  )
}

cat("\n=== TABLE 1 OVERVIEW ===\n")
table1_wide %>%
  dplyr::select(
    condition,
    n,
    age_mean_sd,
    female_n_pct,
    male_n_pct,
    primary_n_pct,
    secondary_n_pct,
    boarding_n_pct,
    other_missing_education_n_pct,
    daily_time_min_mean_sd
  ) %>%
  print(n = Inf)

# Check that education categories sum to N
education_check <- table1_wide %>%
  transmute(
    condition,
    n,
    education_total =
      as.integer(str_extract(primary_n_pct, "^\\d+")) +
      as.integer(str_extract(secondary_n_pct, "^\\d+")) +
      as.integer(str_extract(boarding_n_pct, "^\\d+")) +
      as.integer(str_extract(other_missing_education_n_pct, "^\\d+")),
    matches_n = education_total == n
  )

cat("\n=== EDUCATION TOTAL CHECK ===\n")
print(education_check, n = Inf)

stopifnot(
  all(education_check$matches_n)
)

# =============================================================================
# 4. BALANCE TESTS
# =============================================================================

balance_continuous <- function(var, label, df = participants) {
  df_model <- df %>%
    filter(
      !is.na(condition2),
      !is.na(.data[[var]])
    )
  
  if (nrow(df_model) == 0 || n_distinct(df_model$condition2) < 2) {
    return(
      tibble(
        variable = label,
        test = "ANOVA",
        statistic = NA_real_,
        p_value = NA_real_,
        sig = NA
      )
    )
  }
  
  m <- aov(
    as.formula(paste(var, "~ condition2")),
    data = df_model
  )
  
  s <- summary(m)[[1]]
  
  tibble(
    variable = label,
    test = "ANOVA",
    statistic = round(s$`F value`[1], 3),
    p_value = round(s$`Pr(>F)`[1], 3),
    sig = s$`Pr(>F)`[1] < 0.05
  )
}

balance_categorical <- function(var, label, df = participants) {
  df_model <- df %>%
    mutate(
      value = .data[[var]]
    ) %>%
    filter(
      !is.na(condition2),
      !is.na(value)
    )
  
  if (nrow(df_model) == 0 || n_distinct(df_model$condition2) < 2 || n_distinct(df_model$value) < 2) {
    return(
      tibble(
        variable = label,
        test = "Chi-square",
        statistic = NA_real_,
        p_value = NA_real_,
        sig = NA
      )
    )
  }
  
  tbl <- table(
    df_model$value,
    df_model$condition2
  )
  
  ct <- chisq.test(
    tbl,
    simulate.p.value = TRUE
  )
  
  tibble(
    variable = label,
    test = "Chi-square",
    statistic = round(unname(ct$statistic), 3),
    p_value = round(ct$p.value, 3),
    sig = ct$p.value < 0.05
  )
}

balance_tests <- bind_rows(
  balance_continuous("age", "Age"),
  balance_continuous("addiction_index", "Addiction index"),
  balance_continuous("self_control_index", "Self-control index"),
  balance_continuous("fo_mo", "FOMO"),
  balance_continuous("trivsel", "Well-being"),
  balance_continuous("socialconnection", "Social connection"),
  balance_continuous("total_time_minutes", "Daily time baseline"),
  balance_continuous("daily_opens_median", "Daily opens baseline"),
  balance_continuous("mean_time_minutes", "Session duration baseline"),
  balance_categorical("gender_label", "Gender"),
  balance_categorical("education_label", "Education")
)

cat("\n=== BALANCE TESTS ===\n")
print(balance_tests, n = Inf)

save_model_table(
  balance_tests,
  "table1_balance_tests",
  "tables/supplementary",
  caption = "Baseline balance tests across experimental conditions",
  label = "tab:table1_balance"
)

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    balance_tests,
    "tables/supplementary/table1_balance_tests.xlsx"
  )
}

# =============================================================================
# 5. FIGURE: AGE DISTRIBUTION BY CONDITION
# =============================================================================

fig_age <- ggplot(
  participants %>%
    filter(
      !is.na(age),
      !is.na(condition2)
    ),
  aes(
    x = age,
    fill = condition2
  )
) +
  geom_histogram(
    binwidth = 1,
    color = "white",
    alpha = 0.85
  ) +
  facet_wrap(
    ~ condition2,
    ncol = 3
  ) +
  scale_fill_manual(
    values = COND_COLOURS,
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(13, 18, 1)
  ) +
  labs(
    x = "Age",
    y = "Count",
    subtitle = "Age distribution by experimental condition"
  ) +
  custom_general_theme_word() +
  theme(
    axis.title.y = element_text(
      size = 11,
      family = BODY_FONT
    ),
    panel.grid.major.y = element_line(
      color = "#cccccc",
      linetype = "dashed"
    ),
    panel.grid.major.x = element_blank()
  )

save_figure(
  fig_age,
  "sample_age_distribution",
  "figures/supplementary",
  width = 10,
  height = 4
)

# =============================================================================
# 6. FIGURE: BASELINE SOCIAL MEDIA USE BY CONDITION
# =============================================================================

fig_baseline_use <- participants %>%
  filter(
    !is.na(condition2)
  ) %>%
  pivot_longer(
    cols = c(
      total_time_minutes,
      daily_opens_median,
      mean_time_minutes
    ),
    names_to = "metric",
    values_to = "value"
  ) %>%
  filter(
    !is.na(value)
  ) %>%
  mutate(
    metric = recode(
      metric,
      total_time_minutes = "Daily time (min)",
      daily_opens_median = "Daily opens (median)",
      mean_time_minutes = "Session duration (min)"
    ),
    metric = factor(
      metric,
      levels = c(
        "Daily time (min)",
        "Daily opens (median)",
        "Session duration (min)"
      )
    )
  ) %>%
  ggplot(
    aes(
      x = condition2,
      y = value,
      fill = condition2
    )
  ) +
  geom_boxplot(
    alpha = 0.7,
    outlier.alpha = 0.3,
    outlier.size = 0.8,
    width = 0.55
  ) +
  facet_wrap(
    ~ metric,
    scales = "free_y",
    ncol = 3
  ) +
  scale_fill_manual(
    values = COND_COLOURS,
    guide = "none"
  ) +
  labs(
    x = "",
    y = "",
    subtitle = "Baseline use distribution by condition"
  ) +
  custom_general_theme_word() +
  theme(
    axis.text.x = element_text(
      size = 11,
      family = BODY_FONT,
      face = "bold"
    ),
    panel.grid.major.y = element_line(
      color = "#cccccc",
      linetype = "dashed"
    ),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(
      size = 11,
      family = BOLD_FONT
    )
  )

save_figure(
  fig_baseline_use,
  "sample_baseline_use_by_condition",
  "figures/supplementary",
  width = 10,
  height = 4
)

message("\n=== sample_characteristics complete ===")
