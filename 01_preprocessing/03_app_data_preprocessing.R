Sys.setenv(TZ = "Europe/Copenhagen")
# =============================================================================
# 03_app_data_preprocessing.R
#
# PURPOSE:
#   Read the raw merged smartphone app-logging data, attach participant
#   conditions, label study periods, run the exclusion pipeline, and save
#   the analysis-ready event-level dataset.
#
#   Also builds the combined covariates dataframe (demographics + survey
#   baseline scores + klassetrin) that all downstream scripts join against.
#
# INPUTS:
#   00_data/raw/merged_data_15_01_25.csv
#   data/survey/survey_data_clean_wide.csv
#   data/demographic/demographic_data.csv
#   data/survey/survey_data_clean_long.csv
#   data/survey/klassetrin_clean.csv
#
# OUTPUTS:
#   data/data_filt_step4_all_apps.csv     -- cleaned event-level dataset
#   data/experiment_list.xlsx             -- eligible participant IDs
#   data/covariates.csv                   -- one row per participant:
#                                            ID, cond, condition, age, gender,
#                                            region, klassetrin, klassetrin_short,
#                                            klassetrin_short2, addiction_index,
#                                            self_control_index, fo_mo, trivsel,
#                                            socialconnection
#
# REPORTS:
#   reports/03_app_preprocessing/exclusion_summary.csv
#   reports/03_app_preprocessing/participant_counts_by_condition.csv
# =============================================================================

library(tidyverse)
library(lubridate)
library(writexl)

dir.create("data",                         showWarnings = FALSE, recursive = TRUE)
dir.create("reports/03_app_preprocessing", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. TEST IDs TO REMOVE
# =============================================================================

TEST_IDS <- c(
  "study-denmark-pre-study-12345",
  "study-denmark-study-september-2024-",
  "study-denmark-pre-study-abc",
  "study-denmark-pre-study-R_2QuMViWhWtx9bkk",
  "study-denmark-study-september-2024-xyz",
  "study-denmark-pre-study-test123",
  "study-denmark-pre-study-xyz",
  "study-denmark-study-september-2024-Journalist Politiken ",
  "study-denmark-study-september-2024-Journalist Politiken",
  "study-denmark-study-september-2024-R_123acb",
  "study-denmark-study-september-2024-12345",
  "study-denmark-study-september-2024-123456",
  "study-denmark-study-september-2024-abc",
  "study-denmark-study-september-2024-abcd",
  "study-denmark-study-september-2024-qqqqq",
  "study-denmark-study-september-2024-R_hejmeddig",
  "study-denmark-study-september-2024-R_8AZeJ4VzQ5NZ225",
  "study-denmark-study-september-2024-R8AZeJ4VzQ5NZ225",
  "2LXYkvhlFotA3hu"
)

TARGET_APPS <- c(
  "instagram", "linkedin", "facebook", "beReal", "snapchat",
  "pinterest", "twitch", "twitter", "youtube", "reddit", "tikTok"
)

# =============================================================================
# 2. READ AND CLEAN RAW DATA
# =============================================================================

data_raw <- read_csv("00_data/raw/merged_data_15_01_25.csv", show_col_types = FALSE)
data_filt <- data_raw %>%
  distinct() %>%
  filter(!ID %in% TEST_IDS) %>%
  mutate(study_id = as.numeric(as.factor(ID)))

# =============================================================================
# 3. ATTACH CONDITION FROM SURVEY
# =============================================================================

survey_wide <- read_csv("data/survey/survey_data_clean_wide.csv",
                        show_col_types = FALSE) %>%
  rename(ID = response_id) %>%
  mutate(
    condition = case_when(
      cond %in% c(1, 2) ~ "control",
      cond %in% c(5, 6) ~ "intervention",
      cond %in% c(3, 4) ~ "default",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(ID, cond, condition)

data_filt <- data_filt %>%
  mutate(ID = str_remove(ID, "^study-denmark-study-september-2024-R_")) %>%
  left_join(survey_wide, by = "ID")

# =============================================================================
# 4. TIMESTAMPS → COPENHAGEN TIME
# =============================================================================

data_filt <- data_filt %>%
  mutate(
    time_utc = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"),
    time     = with_tz(time_utc, tzone = "Europe/Copenhagen"),
    date2    = as.Date(time, tz = "Europe/Copenhagen"),
    studyActivated = as.integer(studyActivated %in% c("1", "True")),
    week     = week(date2),
    day      = day(date2),
    week_day = paste(week, day, sep = "_")
  )

data_filt_sel <- data_filt %>%
  dplyr::select(
    ID, study_id, date, date2, studyActivated, app, timestamp, time,
    week, day, week_day, resolution, purpose, interventionType,
    interventionDuration, isReIntervention, cond, condition,
    timeIntervalUntilReIntervention
  )

# =============================================================================
# 5. STUDY PERIOD LABELS
# =============================================================================

# --- 5a. Activation times per participant ---
data_start <- data_filt_sel %>%
  filter(studyActivated == 1) %>%
  mutate(
    date_parsed        = parse_date_time(date, orders = "Ymd HMS", tz = "UTC"),
    time_at_activation = with_tz(date_parsed, "Europe/Copenhagen"),
    start_date_test    = as.Date(time_at_activation)
  )

n_activated <- n_distinct(data_start$ID)
message("Participants who activated the app: ", n_activated)

data_start_sel <- data_start %>%
  dplyr::select(ID, start_date_test, time_at_activation) %>%
  group_by(ID) %>%
  summarise(
    start_date_test    = last(start_date_test),
    time_at_activation = last(time_at_activation),
    .groups = "drop"
  ) %>%
  mutate(
    time_at_intervention = time_at_activation + seconds(14L * 86400L),
    time_at_outro        = time_at_activation + seconds(42L * 86400L),
    time_at_end          = time_at_activation + seconds(56L * 86400L)
  )

# --- 5b. Actual intervention start (first event with interventionType per ID) ---
data_filt_noact <- data_filt_sel %>% filter(studyActivated != 1)

intervention_start <- data_filt_noact %>%
  filter(!is.na(interventionType), interventionType != "") %>%
  group_by(ID) %>%
  summarise(
    intervention_start_v3 = as_datetime(first(timestamp),
                                        tz = "Europe/Copenhagen"),
    .groups = "drop"
  )

# --- 5c. Join and label periods ---
data_filt_1_1 <- data_filt_noact %>%
  left_join(data_start_sel, by = "ID") %>%
  left_join(intervention_start, by = "ID") %>%
  mutate(
    participation_day = as.numeric(
      difftime(date2, start_date_test, units = "days")
    ),
    period = case_when(
      time < time_at_activation ~
        "pre_study",
      
      time >= time_at_activation & time < time_at_intervention ~
        "baseline",
      
      time >= time_at_intervention & time < time_at_outro ~
        "intervention",
      
      time >= time_at_outro & time < time_at_end ~
        "outro",
      
      time >= time_at_end ~
        "post_study",
      
      TRUE ~
        NA_character_
    )
  )

# --- 5d. First intervention day per participant ---
first_intervention_id <- data_filt_1_1 %>%
  filter(
    !is.na(interventionType),
    interventionType != "",
    !is.na(condition),
    period == "intervention"
  ) %>%
  group_by(ID) %>%
  summarise(
    first_intervention_time = min(time),
    first_intervention_participation_day = min(participation_day),
    .groups = "drop"
  )

# =============================================================================
# 6. EXCLUSION PIPELINE  (all-apps version, matches original exactly)
# =============================================================================

count_ids <- function(df) n_distinct(df$ID)
initial_n <- count_ids(data_filt_1_1)

exclusion_summary <- tibble(
  step        = "0_activated",
  description = "Participants who activated the study app",
  n_removed   = NA_integer_,
  n_remaining = n_activated
)

# Step 1: Target apps + baseline/intervention only
data_step1 <- data_filt_1_1 %>%
  filter(app %in% TARGET_APPS,
         period %in% c("baseline", "intervention")) %>%
  left_join(first_intervention_id, by = "ID")

exclusion_summary <- add_row(exclusion_summary,
  step        = "1_app_and_period_filter",
  description = "Keep target apps, baseline + intervention only",
  n_removed   = n_activated - count_ids(data_step1),
  n_remaining = count_ids(data_step1)
)

# Step 2: Remove participants without any intervention period data
ids_no_int <- data_step1 %>%
  group_by(ID) %>%
  summarise(has_int = any(period == "intervention"), .groups = "drop") %>%
  filter(!has_int) %>% pull(ID)

data_step2 <- data_step1 %>% filter(!ID %in% ids_no_int)

exclusion_summary <- add_row(exclusion_summary,
  step        = "2_no_intervention_period",
  description = "Remove: no intervention period data",
  n_removed   = length(ids_no_int),
  n_remaining = count_ids(data_step2)
)

# Step 3: Remove participants where all interventionType values are missing
ids_no_type <- data_step2 %>%
  group_by(ID) %>%
  summarise(all_missing = all(is.na(interventionType) | interventionType == ""),
            .groups = "drop") %>%
  filter(all_missing) %>% pull(ID)

data_step3 <- data_step2 %>% filter(!ID %in% ids_no_type)

exclusion_summary <- add_row(exclusion_summary,
  step        = "3_missing_intervention_type",
  description = "Remove: all interventionType missing",
  n_removed   = length(ids_no_type),
  n_remaining = count_ids(data_step3)
)

# Step 4: Remove participants whose first intervention event was after day 24
ids_late <- data_step3 %>%
  group_by(ID) %>%
  summarise(first_day = first(first_intervention_participation_day),
            .groups = "drop") %>%
  filter(is.na(first_day) | first_day > 24) %>% pull(ID)

data_step4 <- data_step3 %>% filter(!ID %in% ids_late)

exclusion_summary <- add_row(exclusion_summary,
  step        = "4_first_intervention_after_day24",
  description = "Remove: first intervention event after day 24",
  n_removed   = length(ids_late),
  n_remaining = count_ids(data_step4)
)

# Step 5: Keep only ID×app combinations with openedApp in BOTH periods
valid_id_apps <- data_step4 %>%
  filter(resolution == "openedApp") %>%
  group_by(ID, app, period) %>%
  summarise(has_opened = TRUE, .groups = "drop") %>%
  group_by(ID, app) %>%
  summarise(n_periods = n_distinct(period), .groups = "drop") %>%
  filter(n_periods == 2)

data_filt_final <- data_step4 %>%
  semi_join(valid_id_apps, by = c("ID", "app"))

exclusion_summary <- add_row(exclusion_summary,
  step        = "5_opened_both_periods",
  description = "Keep only ID×app combos with openedApp in baseline AND intervention",
  n_removed   = count_ids(data_step4) - count_ids(data_filt_final),
  n_remaining = count_ids(data_filt_final)
)

print(exclusion_summary)

# =============================================================================
# 7. SAVE EVENT-LEVEL DATASET AND EXPERIMENT LIST
# =============================================================================

write_csv(data_filt_final, "data/data_filt_step4_all_apps.csv")
write_xlsx(data.frame(ID = unique(data_filt_final$ID)), "data/experiment_list.xlsx")

message("✓ Saved data/data_filt_step4_all_apps.csv: ",
        nrow(data_filt_final), " rows, ",
        count_ids(data_filt_final), " participants")

# =============================================================================
# 8. BUILD COMBINED COVARIATES DATAFRAME
#    One row per participant — used by ALL downstream analysis scripts
#    instead of joining three separate files every time.
# =============================================================================

demografi    <- read_csv("data/demographic/demographic_data.csv",
                         show_col_types = FALSE)
survey_long  <- read_csv("data/survey/survey_data_clean_long.csv",
                         show_col_types = FALSE) %>%
  filter(survey_nr == 1) %>%
  rename(ID = response_id) %>%
  dplyr::select(-cond)
klassetrin   <- read_csv("data/survey/klassetrin_clean.csv",
                         show_col_types = FALSE)

covariates <- survey_wide %>%                          # ID, cond, condition
  left_join(demografi,   by = "ID") %>%                # age, gender, region
  left_join(klassetrin,  by = "ID") %>%                # klassetrin, klassetrin_short
  left_join(                                           # survey scale scores
    survey_long %>%
      dplyr::select(ID, addiction_index, self_control_index,
                    fo_mo, trivsel, socialconnection, socialtforbundet,
                    retention_index, tilfredsmed_so_me, kontrolover_so_me, taenkerpa_so_me),
    by = "ID"
  ) %>%
  mutate(
    klassetrin_short2 = case_when(
      klassetrin %in% c("7. klasse","8. klasse","9. klasse","10. klasse") ~
        "Primary Education",
      klassetrin %in% c("1. år på ungdomsuddannelse",
                        "2. år på ungdomsuddannelse") ~
        "Secondary Education",
      klassetrin == "Efterskole" ~ "Boarding School",
      TRUE ~ "Others"
    )
  ) %>%
  filter(ID %in% unique(data_filt_final$ID))   # eligible participants only

write_csv(covariates, "data/covariates.csv")
message("✓ Saved data/covariates.csv: ", nrow(covariates), " participants")

# =============================================================================
# 9. REPORTS
# =============================================================================

write_csv(exclusion_summary,
          "reports/03_app_preprocessing/exclusion_summary.csv")

participant_counts <- data_filt_final %>%
  distinct(ID, condition) %>%
  count(condition, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1))

write_csv(participant_counts,
          "reports/03_app_preprocessing/participant_counts_by_condition.csv")

print(participant_counts)
message("✓ Reports saved to reports/03_app_preprocessing/")
