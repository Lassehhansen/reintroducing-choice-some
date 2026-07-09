Sys.setenv(TZ = "Europe/Copenhagen")
# =============================================================================
# 01_survey_preprocessing.R
#
# PURPOSE:
#   Read the three raw Qualtrics survey exports (Survey1, Survey2, Survey3),
#   recode all Likert items, compute composite indices, run polychoric factor
#   analysis to produce factor scores, and save clean long/wide survey files.
#   Also extracts klassetrin (school grade) from Survey3.
#
# INPUTS  (from 00_data/survey_raw/):
#   Survey1.csv, Survey2.csv, Survey3.csv
#
# OUTPUTS (to data/survey/):
#   survey_data_clean_long.csv       -- one row per person per survey wave
#   survey_data_clean_wide.csv       -- one row per person, all waves wide
#   survey_factors_polychoric.csv    -- factor scores merged with long data
#   klassetrin_clean.csv             -- ID + klassetrin + klassetrin_short
#   survey_data_full.csv             -- all recoded items, all waves stacked
#
# REPORTS (to reports/01_survey/):
#   cronbachs_alpha.csv              -- alpha for each scale
#   scale_correlations.csv           -- polychoric correlation matrix (all items)
#   sample_sizes_by_wave.csv         -- N per survey wave
# =============================================================================

library(tidyverse)
library(janitor)
library(psych)

# ---- Directories ------------------------------------------------------------
dir.create("data/survey",       showWarnings = FALSE, recursive = TRUE)
dir.create("reports/01_survey", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. READ RAW SURVEYS
# =============================================================================

survey_data1 <- read_csv("00_data/survey_raw/Survey1.csv", show_col_types = FALSE)
survey_data2 <- read_csv("00_data/survey_raw/Survey2.csv", show_col_types = FALSE)
survey_data3 <- read_csv("00_data/survey_raw/Survey3.csv", show_col_types = FALSE)

survey_data1$survey_nr <- 1L
survey_data2$survey_nr <- 2L
survey_data3$survey_nr <- 3L

# ---- Fix ResponseId in Survey2 (uuid field used as primary ID) --------------
survey_data2 <- survey_data2 %>%
  mutate(ResponseId = ifelse(is.na(uuid), ID, uuid))

# Strip the "R_" prefix so IDs match the app data
survey_data1 <- survey_data1 %>%
  mutate(ResponseId = str_remove(ResponseId, "^R_"))
survey_data2 <- survey_data2 %>%
  mutate(ResponseId = str_remove(ResponseId, "^R_"))
survey_data3 <- survey_data3 %>%
  mutate(ResponseId = ifelse(is.na(uuid), ID, uuid),
         ResponseId = str_remove(ResponseId, "^R_"))

# Remove Qualtrics header rows (rows 1–2 are metadata in raw exports)
survey_data1 <- survey_data1[-c(1, 2), ]
survey_data2 <- survey_data2[-c(1, 2), ]
survey_data3 <- survey_data3[-c(1, 2), ]

# ---- Standardise column names -----------------------------------------------
survey_data1 <- clean_names(survey_data1)
survey_data2 <- clean_names(survey_data2)
survey_data3 <- clean_names(survey_data3)

# ---- Fix a small set of known Survey1 ResponseId errors ---------------------
survey_data1 <- survey_data1 %>%
  mutate(
    response_id = case_when(
      response_id == "R_8vih0pdpXh4TiRQ" ~ "R_2OiWtlYuclmzUkN",
      response_id == "R_85KoVVSfbvTlpoR" ~ "R_8MclGRBb0VqOxHP",
      response_id == "R_2jusHuUiU2OoGQN" ~ "R_8HUoupvd6Pu0wpO",
      response_id == "R_8TIIRkgsfuBjLxv" ~ "R_8KW72QeQ5BmoUJg",
      TRUE ~ response_id
    ),
    response_id = str_remove(response_id, "^R_")
  )

# ---- Rename satisfaction column to consistent name --------------------------
survey_data1 <- survey_data1 %>% rename(tilfredsmed_so_me = tilfredsmedsome)
survey_data3 <- survey_data3 %>% rename(tilfredsmed_so_me = tilfredsmedsome)

# =============================================================================
# 2. SELECT VARIABLES
# =============================================================================

# Carry the condition variable from Survey1 into later waves
merge_cond <- survey_data1 %>% dplyr::select(response_id, cond)

survey_data2 <- survey_data2 %>% left_join(merge_cond, by = "response_id")
survey_data3 <- survey_data3 %>% left_join(merge_cond, by = "response_id")

core_vars <- c(
  "response_id", "cond", "start_date", "end_date",
  "fo_mo", "socialconnection", "socialtforbundet",
  "survey_nr", "trivsel", "tilfredsmed_so_me",
  "svaertatlukke", "meretidendonsket", "fotrudttid",
  "forstyrretaf_so_me", "kontrolover_so_me", "taenkerpa_so_me"
)

survey_data1_sel <- survey_data1 %>%
  dplyr::select(all_of(core_vars), starts_with("afhaengighed_"), starts_with("selvkontrol_"))

survey_data2_sel <- survey_data2 %>%
  rename(socialtforbundet = socialforbundet) %>%
  dplyr::select(all_of(core_vars), starts_with("afhaengighed_"), starts_with("selvkontrol_"))

survey_data3_sel <- survey_data3 %>%
  dplyr::select(all_of(core_vars), starts_with("afhaengighed_"), starts_with("selvkontrol_"))

survey_data <- bind_rows(survey_data1_sel, survey_data2_sel, survey_data3_sel)

# =============================================================================
# 3. RECODE LIKERT ITEMS
# =============================================================================

freq_5 <- c(
  "Meget ofte eller altid" = 5,
  "Ofte" = 4,
  "Nogle gange" = 3,
  "Sjældent" = 2,
  "Meget sjældent eller aldrig" = 1
)

freq_5b <- c(
  "Meget ofte" = 5,
  "Ofte" = 4,
  "Nogle gange" = 3,
  "Sjældent" = 2,
  "Meget sjældent" = 1
)

satisfaction_4 <- c(
  "Meget tilfreds"  = 4,
  "Tilfreds"        = 3,
  "Utilfreds"       = 2,
  "Meget utilfreds" = 1
)

sc_reverse <- c(  # Items where high response = low self-control (reverse-coded)
  "Slet ikke" = 5, "Lidt" = 4, "Midt imellem" = 3, "Meget" = 2, "Rigtig meget" = 1
)

sc_forward <- c(  # Item 2 is forward-keyed
  "Slet ikke" = 1, "Lidt" = 2, "Midt imellem" = 3, "Meget" = 4, "Rigtig meget" = 5
)

survey_data <- survey_data %>%
  mutate(
    svaertatlukke      = freq_5[svaertatlukke],
    meretidendonsket   = freq_5[meretidendonsket],
    fotrudttid         = freq_5[fotrudttid],
    fo_mo              = freq_5[fo_mo],
    kontrolover_so_me  = freq_5[kontrolover_so_me],
    taenkerpa_so_me    = freq_5b[taenkerpa_so_me],
    forstyrretaf_so_me = freq_5b[forstyrretaf_so_me],
    tilfredsmed_so_me  = satisfaction_4[tilfredsmed_so_me],
    # Reverse-code kontrolover_so_me (high = low control)
    kontrolover_so_me  = 6L - kontrolover_so_me
  ) %>%
  mutate(across(starts_with("afhaengighed_"), ~ freq_5[.])) %>%
  mutate(
    selvkontrol_1 = sc_reverse[selvkontrol_1],
    selvkontrol_2 = sc_forward[selvkontrol_2],
    selvkontrol_3 = sc_reverse[selvkontrol_3],
    selvkontrol_4 = sc_reverse[selvkontrol_4],
    selvkontrol_5 = sc_reverse[selvkontrol_5]
  )

# =============================================================================
# 4. COMPOSITE INDICES
# =============================================================================

survey_data <- survey_data %>%
  mutate(
    retention_index    = rowMeans(pick(svaertatlukke, meretidendonsket, fotrudttid),
                                  na.rm = TRUE),
    self_control_index = rowMeans(pick(starts_with("selvkontrol_")), na.rm = TRUE),
    addiction_index    = rowMeans(pick(starts_with("afhaengighed_")), na.rm = TRUE),
    condition = case_when(
      cond %in% c(1, 2) ~ "control",
      cond %in% c(5, 6) ~ "intervention",
      cond %in% c(3, 4) ~ "default",
      TRUE ~ NA_character_
    )
  )

# =============================================================================
# 5. POLYCHORIC FACTOR ANALYSIS  (addiction + self-control items)
# =============================================================================

items_for_fa <- survey_data %>%
  dplyr::select(starts_with("afhaengighed_"), starts_with("selvkontrol_"))

poly_result <- psych::polychoric(items_for_fa)

fa_result <- fa(poly_result$rho, nfactors = 2, rotate = "varimax", fm = "ml")

scores <- factor.scores(items_for_fa, fa_result, method = "regression")

survey_factors <- tibble(
  addiction_factor     = scores$scores[, 1],
  self_control_factor  = scores$scores[, 2]
)

survey_data_with_factors <- cbind(survey_factors, survey_data)

# =============================================================================
# 6. WIDE FORMAT  (one row per participant, survey-2 and survey-3 values as
#    additional columns appended to survey-1 row)
# =============================================================================

survey_save <- survey_data %>%
  arrange(response_id, survey_nr) %>%
  group_by(response_id) %>%
  mutate(
    cond                        = first(cond),
    start_date_survey_2         = lead(start_date),
    end_date_survey_2           = lead(end_date),
    fomo_survey_2               = lead(fo_mo),
    trivsel_survey_2            = lead(trivsel, n = 1),
    socialconnection_survey_2   = lead(socialconnection, n = 1),
    tilfredsmed_so_me_survey_2  = lead(tilfredsmed_so_me, n = 1),
    kontrolover_so_me_survey_2  = lead(kontrolover_so_me),
    taenkerpa_so_me_survey_2    = lead(taenkerpa_so_me),
    retention_index_survey_2    = lead(retention_index),
    self_control_index_survey_2 = lead(self_control_index),
    addiction_index_survey_2    = lead(addiction_index),
    trivsel_survey_3            = lead(trivsel, n = 2),
    socialconnection_survey_3   = lead(socialconnection, n = 2),
    fomo_survey_3               = lead(fo_mo, n = 2),
    tilfredsmed_so_me_survey_3  = lead(tilfredsmed_so_me, n = 2),
    kontrolover_so_me_survey_3  = lead(kontrolover_so_me, n = 2),
    taenkerpa_so_me_survey_3    = lead(taenkerpa_so_me, n = 2),
    retention_index_survey_3    = lead(retention_index, n = 2),
    self_control_index_survey_3 = lead(self_control_index, n = 2),
    addiction_index_survey_3    = lead(addiction_index, n = 2),
    start_date_survey_3         = lead(start_date, n = 2),
    end_date_survey_3           = lead(end_date, n = 2)
  ) %>%
  filter(survey_nr == 1) %>%
  ungroup()

survey_data_wide <- survey_save %>%
  dplyr::select(
    response_id, cond,
    kontrolover_so_me, kontrolover_so_me_survey_2,
    taenkerpa_so_me,   taenkerpa_so_me_survey_2,
    addiction_index,   addiction_index_survey_2,   addiction_index_survey_3,
    retention_index,   retention_index_survey_2,   retention_index_survey_3,
    self_control_index, self_control_index_survey_2, self_control_index_survey_3,
    start_date, start_date_survey_2,
    end_date,   end_date_survey_2
  )

# =============================================================================
# 7. LONG FORMAT (one row per person per wave, for modelling)
# =============================================================================

survey_data_long <- survey_data %>%
  dplyr::select(
    response_id, cond, condition, survey_nr,
    kontrolover_so_me, taenkerpa_so_me,
    addiction_index, tilfredsmed_so_me,
    retention_index, self_control_index,
    trivsel, socialconnection, socialtforbundet, fo_mo
  )

# =============================================================================
# 8. KLASSETRIN (school grade) from Survey3
# =============================================================================

klassetrin_clean <- survey_data3 %>%
  dplyr::select(response_id, klassetrin) %>%
  mutate(
    klassetrin_short = case_when(
      klassetrin %in% c("7. klasse", "8. klasse", "9. klasse", "10. klasse") ~ "Udskoling",
      klassetrin %in% c(
        "1. år på ungdomsuddannelse",
        "2. år på ungdomsuddannelse"
      ) ~ "Ungdomsuddannelse",
      klassetrin == "Efterskole" ~ "Efterskole",
      TRUE ~ "Andet"
    )
  ) %>%
  rename(ID = response_id)

# =============================================================================
# 9. SAVE DATA FILES
# =============================================================================

write_csv(survey_data,              "data/survey/survey_data_full.csv")
write_csv(survey_data_wide,         "data/survey/survey_data_clean_wide.csv")
write_csv(survey_data_long,         "data/survey/survey_data_clean_long.csv")
write_csv(survey_data_with_factors, "data/survey/survey_factors_polychoric.csv")
write_csv(klassetrin_clean,         "data/survey/klassetrin_clean.csv")

message("✓ Survey data saved to data/survey/")

# =============================================================================
# 10. REPORTS
# =============================================================================

# --- Sample sizes by wave ---
sample_sizes <- survey_data %>%
  group_by(survey_nr) %>%
  summarise(
    n_total        = n(),
    n_with_cond    = sum(!is.na(cond)),
    n_control      = sum(condition == "control",      na.rm = TRUE),
    n_intervention = sum(condition == "intervention", na.rm = TRUE),
    n_default      = sum(condition == "default",      na.rm = TRUE),
    .groups = "drop"
  )
write_csv(sample_sizes, "reports/01_survey/sample_sizes_by_wave.csv")
print(sample_sizes)

# --- Cronbach's alpha ---
alpha_addiction    <- psych::alpha(items_for_fa %>% dplyr::select(starts_with("afhaengighed_")))
alpha_self_control <- psych::alpha(items_for_fa %>% dplyr::select(starts_with("selvkontrol_")))
alpha_retention    <- psych::alpha(
  survey_data %>% dplyr::select(svaertatlukke, meretidendonsket, fotrudttid)
)

cronbachs_alpha <- tibble(
  scale            = c("Addiction Index", "Self-Control Index", "Retention Index"),
  n_items          = c(6L, 5L, 3L),
  alpha_raw        = c(
    alpha_addiction$total$raw_alpha,
    alpha_self_control$total$raw_alpha,
    alpha_retention$total$raw_alpha
  ),
  alpha_std        = c(
    alpha_addiction$total$std.alpha,
    alpha_self_control$total$std.alpha,
    alpha_retention$total$std.alpha
  )
)
write_csv(cronbachs_alpha, "reports/01_survey/cronbachs_alpha.csv")
print(cronbachs_alpha)

# --- Polychoric correlation matrix (all items) ---
poly_corr_df <- as.data.frame(round(poly_result$rho, 3)) %>%
  rownames_to_column("item")
write_csv(poly_corr_df, "reports/01_survey/scale_correlations.csv")

message("✓ Reports saved to reports/01_survey/")
