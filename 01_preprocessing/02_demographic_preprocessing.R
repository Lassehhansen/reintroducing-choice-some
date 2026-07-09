Sys.setenv(TZ = "Europe/Copenhagen")
# =============================================================================
# 02_demographic_preprocessing.R
#
# PURPOSE:
#   Combine demographics, survey scale scores, and klassetrin into one flat
#   participant-level CSV. This single file is what all analysis scripts load.
#   One row per participant, covering everything needed as a covariate.
#
# INPUTS:
#   00_data/survey_raw/Demografi_oversigt.xlsx
#   data/survey/survey_data_clean_long.csv     (from 01_survey_preprocessing.R)
#   data/survey/klassetrin_clean.csv           (from 01_survey_preprocessing.R)
#
# OUTPUT:
#   data/participants.csv
#     Columns: ID, age, gender, region,
#              klassetrin, klassetrin_short2,
#              addiction_index, self_control_index, retention_index,
#              fo_mo, trivsel, socialconnection, socialtforbundet,
#              tilfredsmed_so_me, forstyrretaf_so_me,
#              kontrolover_so_me, taenkerpa_so_me
#
# USAGE IN ANALYSIS SCRIPTS:
#   participants <- read_csv("data/participants.csv")
#   data <- data %>% left_join(participants, by = "ID")
# =============================================================================

library(tidyverse)
library(readxl)

dir.create("data",                showWarnings = FALSE, recursive = TRUE)
dir.create("reports/02_demographic", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. DEMOGRAPHICS
# =============================================================================

demografi_raw <- read_excel("00_data/survey_raw/Demografi_oversigt.xlsx")

demografi <- demografi_raw %>%
  mutate(ID = str_remove(ResponseId, "^R_")) %>%
  dplyr::select(ID, Deltageralder, Deltagerkøn, Deltagerregion) %>%
  rename(
    age    = Deltageralder,
    gender = Deltagerkøn,
    region = Deltagerregion
  ) %>%
  mutate(
    age    = as.numeric(age),
    gender = tolower(gender),
    region = as.character(region)
  )

message("Demographics loaded: ", nrow(demografi), " participants")

# =============================================================================
# 2. SURVEY SCALE SCORES  (baseline wave only)
# =============================================================================

survey_long <- read_csv("data/survey/survey_data_clean_long.csv",
                        show_col_types = FALSE)

survey_s1 <- survey_long %>%
  filter(survey_nr == 1) %>%
  rename(ID = response_id) %>%
  dplyr::select(
    ID,
    addiction_index,
    self_control_index,
    retention_index,
    fo_mo,
    trivsel,
    socialconnection,
    socialtforbundet,
    tilfredsmed_so_me,
    #forstyrretaf_so_me,
    kontrolover_so_me,
    taenkerpa_so_me
  )

message("Survey baseline scores loaded: ", nrow(survey_s1), " participants")

# =============================================================================
# 3. KLASSETRIN  (school grade from Survey3)
# =============================================================================

klassetrin_raw <- read_csv("data/survey/klassetrin_clean.csv",
                           show_col_types = FALSE)

klassetrin <- klassetrin_raw %>%
  dplyr::select(ID, klassetrin) %>%
  mutate(
    klassetrin = as.character(klassetrin),
    klassetrin_short2 = case_when(
      klassetrin %in% c("7. klasse", "8. klasse",
                        "9. klasse", "10. klasse")            ~ "Primary Education",
      klassetrin %in% c("1. år på ungdomsuddannelse",
                        "2. år på ungdomsuddannelse")         ~ "Secondary Education",
      klassetrin == "Efterskole"                               ~ "Boarding School",
      TRUE                                                     ~ "Others"
    )
  )

message("Klassetrin loaded: ", nrow(klassetrin), " participants")

# =============================================================================
# 4. JOIN INTO ONE FLAT FILE
# =============================================================================

participants <- demografi %>%
  left_join(survey_s1,   by = "ID") %>%
  left_join(klassetrin,  by = "ID")

message("participants.csv: ", nrow(participants), " rows, ",
        ncol(participants), " columns")
print(names(participants))

# =============================================================================
# 5. SAVE
# =============================================================================

write_csv(participants, "data/participants.csv")
message("✓ Saved: data/participants.csv")

# =============================================================================
# 6. SANITY CHECKS  (printed to console, not blocking)
# =============================================================================

cat("\n--- Missing values per column ---\n")
participants %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  print()

cat("\n--- Gender split ---\n")
print(table(participants$gender, useNA = "ifany"))

cat("\n--- Klassetrin split ---\n")
print(table(participants$klassetrin_short2, useNA = "ifany"))

cat("\n--- Age summary ---\n")
print(summary(participants$age))

cat("\n--- Addiction index summary ---\n")
print(summary(participants$addiction_index))

cat("\n--- Self-control index summary ---\n")
print(summary(participants$self_control_index))

# =============================================================================
# 7. REPORTS
# =============================================================================

demo_summary <- participants %>%
  summarise(
    n          = n(),
    mean_age   = round(mean(age,  na.rm = TRUE), 1),
    sd_age     = round(sd(age,    na.rm = TRUE), 1),
    min_age    = min(age,  na.rm = TRUE),
    max_age    = max(age,  na.rm = TRUE),
    n_male     = sum(gender == "mand",   na.rm = TRUE),
    n_female   = sum(gender == "kvinde", na.rm = TRUE),
    pct_female = round(100 * mean(gender == "kvinde", na.rm = TRUE), 1),
    mean_addiction_index    = round(mean(addiction_index,    na.rm = TRUE), 2),
    mean_self_control_index = round(mean(self_control_index, na.rm = TRUE), 2)
  )

write_csv(demo_summary, "reports/02_demographic/demographic_summary.csv")

region_split <- participants %>%
  count(region, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(pct = round(100 * n / sum(n), 1))

write_csv(region_split, "reports/02_demographic/region_split.csv")

print(demo_summary)
message("✓ Reports saved to reports/02_demographic/")
