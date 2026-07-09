# =============================================================================
# 02_analysis/app_specific/06d_app_specific_dismissal_probability.R
#
# PURPOSE:
#   Fit app-specific dismissal probability models during intervention.
#
# MODEL:
#   regret ~
#     condition +
#     ns(days_before_activation_model, df = 2):condition +
#     klassetrin_short2 + weekend + fall_break + region +
#     (1 | ID2)
#
# NOTE:
#   This is not a baseline/intervention ITS model. It is intervention-period
#   dismissal probability conditional on receiving an intervention.
# =============================================================================

Sys.setenv(TZ = "Europe/Copenhagen")

source("02_analysis/shared_utils.R")

make_output_dirs(
  "models/app_specific",
  "tables/app_specific",
  "figures/app_specific"
)

TARGET_APPS <- c("instagram", "tikTok", "snapchat", "youtube", "facebook")
COND_LEVELS_ENG <- c("Reflection", "Planning", "Waiting")

participants <- read_csv(
  "data/participants.csv",
  show_col_types = FALSE
) %>%
  dplyr::select(
    -any_of(c(
      "condition", "cond",
      "condition.x", "condition.y",
      "condition_x", "condition_y"
    ))
  )

regret_data_app <- read_csv(
  "data/regret_rate/intervention_app_for_regret.csv",
  show_col_types = FALSE
) %>%
  filter(
    !is.na(interventionType),
    interventionType != "",
    !is.na(condition),
    resolution %in% c("openedApp", "dismissedAppOpening"),
    app %in% TARGET_APPS
  ) %>%
  mutate(
    regret = as.integer(resolution == "dismissedAppOpening"),
    date = as.Date(date, tz = "Europe/Copenhagen"),
    day_of_week = weekdays(date),
    weekend = as.factor(as.integer(day_of_week %in% c("Saturday", "Sunday"))),
    fall_break = as.factor(as.integer(
      date >= as.Date("2024-10-14") &
        date <= as.Date("2024-10-18")
    )),
    christmas_break = as.factor(as.integer(
      date >= as.Date("2024-12-23") &
        date <= as.Date("2025-01-02")
    )),
    condition = factor(
      condition,
      levels = c("control", "intervention", "default")
    ),
    days_before_activation_num = as.numeric(days_before_activation_num)
  ) %>%
  left_join(participants, by = "ID") %>%
  add_klassetrin_short2() %>%
  mutate(
    ID2 = as.factor(as.numeric(as.factor(ID))),
    gender = factor(gender, levels = c("mand", "kvinde")),
    region = as.factor(region),
    klassetrin_short2 = factor(
      klassetrin_short2,
      levels = c(
        "Primary Education",
        "Secondary Education",
        "Boarding School",
        "Others"
      )
    ),
    app = factor(app, levels = TARGET_APPS)
  ) %>%
  drop_na(
    regret,
    condition,
    days_before_activation_num,
    ID2,
    weekend,
    fall_break,
    region,
    klassetrin_short2
  )

glm_regret_instagram <- glmer(
  regret ~
    condition +
    ns(days_before_activation_num, df = 2):condition +
    klassetrin_short2 +
    weekend + fall_break +
    region +
    (1 | ID2),
  family = binomial(),
  data = regret_data_app %>% filter(app == "instagram"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
)

glm_regret_tiktok <- glmer(
  regret ~
    condition +
    ns(days_before_activation_num, df = 2):condition +
    klassetrin_short2 +
    weekend + fall_break +
    region +
    (1 | ID2),
  family = binomial(),
  data = regret_data_app %>% filter(app == "tikTok"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
)

glm_regret_snapchat <- glmer(
  regret ~
    condition +
    ns(days_before_activation_num, df = 2):condition +
    klassetrin_short2 +
    weekend + fall_break +
    region +
    (1 | ID2),
  family = binomial(),
  data = regret_data_app %>% filter(app == "snapchat"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
)

glm_regret_youtube <- glmer(
  regret ~
    condition +
    ns(days_before_activation_num, df = 2):condition +
    klassetrin_short2 +
    weekend + fall_break +
    region +
    (1 | ID2),
  family = binomial(),
  data = regret_data_app %>% filter(app == "youtube"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
)

glm_regret_facebook <- glmer(
  regret ~
    condition +
    ns(days_before_activation_num, df = 2):condition +
    klassetrin_short2 +
    weekend + fall_break +
    region +
    (1 | ID2),
  family = binomial(),
  data = regret_data_app %>% filter(app == "facebook"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
)

saveRDS(glm_regret_instagram, "models/app_specific/glm_regret_instagram.rds")
saveRDS(glm_regret_tiktok,    "models/app_specific/glm_regret_tikTok.rds")
saveRDS(glm_regret_snapchat,  "models/app_specific/glm_regret_snapchat.rds")
saveRDS(glm_regret_youtube,   "models/app_specific/glm_regret_youtube.rds")
saveRDS(glm_regret_facebook,  "models/app_specific/glm_regret_facebook.rds")

get_regret_predictions <- function(model, app_label) {
  ggeffects::predict_response(
    model,
    terms = "condition",
    margin = "empirical",
    type = "response",
    ci_level = 0.95,
    interval = "confidence"
  ) %>%
    as.data.frame() %>%
    transmute(
      app = app_label,
      condition = as.character(x),
      condition2 = recode(
        condition,
        control = "Reflection",
        intervention = "Planning",
        default = "Waiting"
      ),
      prob = predicted,
      prob_low = conf.low,
      prob_high = conf.high,
      pct = 100 * prob,
      pct_low = 100 * prob_low,
      pct_high = 100 * prob_high
    )
}

dismissal_effects <- bind_rows(
  get_regret_predictions(glm_regret_instagram, "instagram"),
  get_regret_predictions(glm_regret_tiktok,    "tikTok"),
  get_regret_predictions(glm_regret_snapchat,  "snapchat"),
  get_regret_predictions(glm_regret_youtube,   "youtube"),
  get_regret_predictions(glm_regret_facebook,  "facebook")
) %>%
  mutate(
    app = factor(app, levels = TARGET_APPS),
    condition2 = factor(condition2, levels = COND_LEVELS_ENG)
  )

dismissal_effects_no_facebook <- dismissal_effects %>%
  filter(app != "facebook")

save_model_table(
  dismissal_effects,
  "dismissal_probability_by_app",
  "tables/app_specific",
  caption = "App-specific dismissal probability by condition",
  label = "tab:app_dismissal_probability"
)

write_csv(dismissal_effects, "tables/app_specific/dismissal_effects.csv")
write_csv(dismissal_effects_no_facebook, "tables/app_specific/dismissal_effects_no_facebook.csv")

regret_data_app %>%
  count(app, condition, regret) %>%
  arrange(app, condition, regret) %>%
  print(n = Inf)

message("06d_app_specific_dismissal_probability.R complete")