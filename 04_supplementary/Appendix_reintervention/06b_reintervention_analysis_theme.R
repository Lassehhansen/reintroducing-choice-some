# =============================================================================
# 02_analysis/appendix/06b_reintervention_analysis.R
#
# PURPOSE:
#   Produce appendix-ready descriptive tables and figures for the Planning
#   reintervention analysis.
#
# INPUTS:
#   data/regret_rate/reintervention_data_for_appendix.csv
#   data/regret_rate/reintervention_sessions_for_appendix.csv
#
# OUTPUTS:
#   tables/reintervention/reintervention_*.csv
#   tables/reintervention/reintervention_*.xlsx  when writexl is available
#   tables/reintervention/reintervention_*.tex
#   figures/reintervention/reintervention_*.png
#   figures/reintervention/reintervention_*.svg
# =============================================================================

Sys.setenv(TZ = "Europe/Copenhagen")

source("02_analysis/shared_utils.R")

library(tidyverse)
library(lubridate)
library(scales)

# ---- Directories ------------------------------------------------------------
make_output_dirs(
  "tables/reintervention",
  "figures/reintervention"
)

# ---- Helpers ----------------------------------------------------------------
escape_latex <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\textbackslash{}", x)
  x <- gsub("&", "\\&", x)
  x <- gsub("%", "\\%", x)
  x <- gsub("_", "\\_", x)
  x <- gsub("#", "\\#", x)
  x
}

write_table_outputs <- function(df, stem, caption = NULL, label = NULL) {
  csv_path <- file.path("tables/reintervention", paste0(stem, ".csv"))
  xlsx_path <- file.path("tables/reintervention", paste0(stem, ".xlsx"))
  tex_path <- file.path("tables/reintervention", paste0(stem, ".tex"))
  
  write_csv(df, csv_path)
  
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(df, xlsx_path)
  }
  
  tex_df <- df %>%
    mutate(across(where(is.numeric), ~ if_else(is.na(.x), NA_character_, format(round(.x, 3), nsmall = 3)))) %>%
    mutate(across(everything(), as.character))
  
  align <- paste0("l", paste(rep("r", max(ncol(tex_df) - 1, 0)), collapse = ""))
  names(tex_df) <- escape_latex(names(tex_df))
  tex_df <- tex_df %>%
    mutate(across(everything(), escape_latex))
  
  header <- paste(names(tex_df), collapse = " & ")
  rows <- apply(tex_df, 1, function(x) paste(if_else(is.na(x), "", x), collapse = " & "))
  
  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    if (!is.null(caption)) paste0("\\caption{", caption, "}") else NULL,
    if (!is.null(label)) paste0("\\label{", label, "}") else NULL,
    "\\scriptsize",
    "\\setlength{\\tabcolsep}{4pt}",
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(rows, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  
  writeLines(lines, tex_path)
}

format_reintervention_status <- function(x) {
  case_when(
    as.character(x) == "0" ~ "Initial planning prompt",
    as.character(x) == "1" ~ "Reintervention prompt",
    TRUE ~ NA_character_
  )
}

app_colors <- c(
  "TikTok" = "#9e0b1d",
  "Snapchat" = "#8dadbf",
  "Instagram" = "#a98467",
  "Facebook" = "#737373",
  "YouTube" = "#84a59d",
  "Other" = "#1c1c1c"
)

reintervention_colors <- c(
  "Initial planning prompt" = "#9e0b1d",
  "Reintervention prompt" = "#8dadbf"
)

# ---- Load data --------------------------------------------------------------
reintervention_data <- read_csv(
  "data/regret_rate/reintervention_data_for_appendix.csv",
  show_col_types = FALSE
)

reintervention_sessions <- read_csv(
  "data/regret_rate/reintervention_sessions_for_appendix.csv",
  show_col_types = FALSE
)

# ---- Basic summaries --------------------------------------------------------
analysis_audit <- tibble(
  metric = c(
    "Decision events",
    "Participants with decision events",
    "Reintervention decision events",
    "Initial planning decision events",
    "Matched planned sessions",
    "Participants with matched planned sessions"
  ),
  value = c(
    nrow(reintervention_data),
    n_distinct(reintervention_data$ID),
    sum(as.character(reintervention_data$isReIntervention) == "1", na.rm = TRUE),
    sum(as.character(reintervention_data$isReIntervention) == "0", na.rm = TRUE),
    nrow(reintervention_sessions),
    n_distinct(reintervention_sessions$ID)
  )
)

write_table_outputs(
  analysis_audit,
  "reintervention_analysis_audit",
  caption = "Sample sizes for the Planning reintervention appendix analysis.",
  label = "tab:reintervention_analysis_audit"
)

# ---- Planned duration distribution ------------------------------------------
duration_distribution <- reintervention_data %>%
  filter(!is.na(timeIntervalUntilReIntervention), timeIntervalUntilReIntervention > 0) %>%
  count(timeIntervalUntilReIntervention, name = "n") %>%
  mutate(
    planned_minutes = timeIntervalUntilReIntervention / 60,
    percent = 100 * n / sum(n)
  ) %>%
  arrange(planned_minutes)

write_table_outputs(
  duration_distribution,
  "reintervention_planned_duration_distribution",
  caption = "Distribution of planned session durations selected in the Planning condition.",
  label = "tab:reintervention_duration_distribution"
)

# ---- Top planned durations by app -------------------------------------------
app_counts <- reintervention_data %>%
  filter(!is.na(timeIntervalUntilReIntervention), timeIntervalUntilReIntervention > 0) %>%
  count(app2, name = "app_total")

top_durations_by_app <- reintervention_data %>%
  filter(!is.na(timeIntervalUntilReIntervention), timeIntervalUntilReIntervention > 0) %>%
  count(app2, timeIntervalUntilReIntervention, name = "n") %>%
  left_join(app_counts, by = "app2") %>%
  mutate(
    planned_minutes = timeIntervalUntilReIntervention / 60,
    percent_within_app = 100 * n / app_total
  ) %>%
  group_by(app2) %>%
  slice_max(order_by = percent_within_app, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(app2, desc(percent_within_app))

write_table_outputs(
  top_durations_by_app,
  "reintervention_top_planned_durations_by_app",
  caption = "Three most common planned session durations by app.",
  label = "tab:reintervention_top_durations_app"
)

# ---- Dismissal rates by initial prompt versus reintervention -----------------
dismissal_by_reintervention <- reintervention_data %>%
  filter(resolution %in% c("openedApp", "dismissedAppOpening")) %>%
  mutate(
    prompt_type = format_reintervention_status(isReIntervention)
  ) %>%
  count(prompt_type, resolution, name = "n") %>%
  pivot_wider(names_from = resolution, values_from = n, values_fill = 0) %>%
  mutate(
    total = openedApp + dismissedAppOpening,
    open_rate = openedApp / total,
    dismiss_rate = dismissedAppOpening / total,
    open_percent = 100 * open_rate,
    dismiss_percent = 100 * dismiss_rate
  ) %>%
  arrange(prompt_type)

write_table_outputs(
  dismissal_by_reintervention,
  "reintervention_dismissal_by_prompt_type",
  caption = "Opening and dismissal rates for initial Planning prompts and reintervention prompts.",
  label = "tab:reintervention_dismissal_prompt_type"
)

# ---- Dismissal rates by app and prompt type ----------------------------------
dismissal_by_app <- reintervention_data %>%
  filter(resolution %in% c("openedApp", "dismissedAppOpening")) %>%
  mutate(
    prompt_type = format_reintervention_status(isReIntervention)
  ) %>%
  count(app2, prompt_type, resolution, name = "n") %>%
  pivot_wider(names_from = resolution, values_from = n, values_fill = 0) %>%
  mutate(
    total = openedApp + dismissedAppOpening,
    open_rate = openedApp / total,
    dismiss_rate = dismissedAppOpening / total,
    open_percent = 100 * open_rate,
    dismiss_percent = 100 * dismiss_rate
  ) %>%
  arrange(app2, prompt_type)

write_table_outputs(
  dismissal_by_app,
  "reintervention_dismissal_by_app",
  caption = "Opening and dismissal rates by app and prompt type in the Planning condition.",
  label = "tab:reintervention_dismissal_app"
)

# ---- Planned time used -------------------------------------------------------
planned_time_used_overall <- reintervention_sessions %>%
  summarise(
    n_sessions = n(),
    n_participants = n_distinct(ID),
    mean_percent_used = 100 * mean(percentage_used, na.rm = TRUE),
    median_percent_used = 100 * median(percentage_used, na.rm = TRUE),
    mean_planned_minutes = mean(planned_minutes, na.rm = TRUE),
    median_planned_minutes = median(planned_minutes, na.rm = TRUE),
    mean_actual_minutes = mean(actual_minutes, na.rm = TRUE),
    median_actual_minutes = median(actual_minutes, na.rm = TRUE)
  )

write_table_outputs(
  planned_time_used_overall,
  "reintervention_planned_time_used_overall",
  caption = "Overall use of self-selected planned session time in the Planning condition.",
  label = "tab:reintervention_time_used_overall"
)

planned_time_used_by_prompt <- reintervention_sessions %>%
  mutate(
    prompt_type = format_reintervention_status(isReIntervention)
  ) %>%
  group_by(prompt_type) %>%
  summarise(
    n_sessions = n(),
    n_participants = n_distinct(ID),
    mean_percent_used = 100 * mean(percentage_used, na.rm = TRUE),
    median_percent_used = 100 * median(percentage_used, na.rm = TRUE),
    mean_planned_minutes = mean(planned_minutes, na.rm = TRUE),
    median_planned_minutes = median(planned_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(prompt_type)

write_table_outputs(
  planned_time_used_by_prompt,
  "reintervention_planned_time_used_by_prompt_type",
  caption = "Use of planned session time by initial Planning prompt and reintervention prompt.",
  label = "tab:reintervention_time_used_prompt_type"
)

planned_time_used_by_app <- reintervention_sessions %>%
  group_by(app2) %>%
  summarise(
    n_sessions = n(),
    n_participants = n_distinct(ID),
    mean_percent_used = 100 * mean(percentage_used, na.rm = TRUE),
    median_percent_used = 100 * median(percentage_used, na.rm = TRUE),
    mean_planned_minutes = mean(planned_minutes, na.rm = TRUE),
    median_planned_minutes = median(planned_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(app2)

write_table_outputs(
  planned_time_used_by_app,
  "reintervention_planned_time_used_by_app",
  caption = "Use of planned session time by app in the Planning condition.",
  label = "tab:reintervention_time_used_app"
)

planned_time_used_by_app_prompt <- reintervention_sessions %>%
  mutate(
    prompt_type = format_reintervention_status(isReIntervention)
  ) %>%
  group_by(app2, prompt_type) %>%
  summarise(
    n_sessions = n(),
    mean_percent_used = 100 * mean(percentage_used, na.rm = TRUE),
    median_percent_used = 100 * median(percentage_used, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(app2, prompt_type)

write_table_outputs(
  planned_time_used_by_app_prompt,
  "reintervention_planned_time_used_by_app_prompt_type",
  caption = "Use of planned session time by app and prompt type.",
  label = "tab:reintervention_time_used_app_prompt"
)

# ---- Optional logistic model -------------------------------------------------
# This model is descriptive and appendix-oriented: it asks whether dismissal
# probability differs between reintervention prompts and initial Planning prompts.
if (requireNamespace("lme4", quietly = TRUE) &&
    n_distinct(reintervention_data$ID) > 1 &&
    n_distinct(reintervention_data$app) > 1 &&
    n_distinct(reintervention_data$isReIntervention) > 1) {
  
  model_data <- reintervention_data %>%
    filter(resolution %in% c("openedApp", "dismissedAppOpening")) %>%
    mutate(
      regret = as.integer(resolution == "dismissedAppOpening"),
      isReIntervention = factor(isReIntervention, levels = c(0, 1)),
      app = as.factor(app),
      ID = as.factor(ID),
      weekend = as.factor(weekend),
      fall_break = as.factor(fall_break),
      region = as.factor(region),
      klassetrin_short2 = as.factor(klassetrin_short2)
    ) %>%
    drop_na(
      regret,
      isReIntervention,
      days_before_activation_num,
      weekend,
      fall_break,
      region,
      klassetrin_short2,
      ID,
      app
    )
  
  if (nrow(model_data) > 0 && n_distinct(model_data$regret) > 1) {
    dismissal_model <- lme4::glmer(
      regret ~
        isReIntervention +
        splines::ns(days_before_activation_num, df = 2) +
        weekend + fall_break + region + klassetrin_short2 +
        (1 | ID) +
        (1 | app),
      family = binomial(),
      data = model_data,
      control = lme4::glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 1e5)
      )
    )
    
    saveRDS(
      dismissal_model,
      "tables/reintervention/reintervention_dismissal_model.rds"
    )
    
    if (requireNamespace("broom.mixed", quietly = TRUE)) {
      dismissal_model_tidy <- broom.mixed::tidy(
        dismissal_model,
        effects = "fixed",
        conf.int = TRUE
      ) %>%
        mutate(
          odds_ratio = exp(estimate),
          odds_ratio_low = exp(conf.low),
          odds_ratio_high = exp(conf.high)
        )
      
      write_table_outputs(
        dismissal_model_tidy,
        "reintervention_dismissal_model_coefficients",
        caption = "Mixed-effects logistic model for dismissal probability in the Planning condition.",
        label = "tab:reintervention_dismissal_model"
      )
    }
  }
}

# ---- Figures ----------------------------------------------------------------

# Shared figure styling --------------------------------------------------------

REINTERVENTION_BAR_FILL <- "#8dadbf"

duration_breaks_plot <- unique(c(
  duration_min,
  5,
  10,
  15,
  20,
  25,
  30,
  35,
  40,
  duration_max
)) %>%
  sort()

reintervention_figure_theme <- custom_general_theme_word() +
  theme(
    axis.line = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    axis.ticks.y = element_line(color = "#000000"),
    axis.text.x = element_text(
      size = 12,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.text.y = element_text(
      size = 12,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.title.x = element_text(
      size = 12,
      family = BODY_FONT,
      color = "#000000",
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 12,
      family = BODY_FONT,
      color = "#000000",
      margin = margin(r = 10)
    ),
    panel.grid.major = element_line(
      color = "#000000",
      linetype = "dashed",
      linewidth = 0.15
    ),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_blank(),
    plot.margin = margin(t = 8, r = 12, b = 8, l = 6)
  )


# =============================================================================
# FIGURE S5: Planned duration distribution
# =============================================================================

duration_min <- min(duration_distribution$planned_minutes, na.rm = TRUE)
duration_max <- max(duration_distribution$planned_minutes, na.rm = TRUE)

p_duration <- duration_distribution %>%
  mutate(
    planned_minutes = as.numeric(planned_minutes)
  ) %>%
  ggplot(
    aes(
      x = planned_minutes,
      y = n
    )
  ) +
  geom_col(
    fill = REINTERVENTION_BAR_FILL,
    color = "#000000",
    width = 0.75,
    linewidth = 0.25
  ) +
  scale_x_continuous(
    breaks = duration_breaks_plot,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  coord_cartesian(
    xlim = c(duration_min - 0.5, duration_max + 0.5)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
    x = "Planned session duration (minutes)",
    y = "Number of planning prompts",
    title = ""
  ) +
  reintervention_figure_theme +
  theme(
    axis.line.x = element_line(colour = "black", linewidth = 0.3),
    panel.grid.major.x = element_blank()
  )

ggsave(
  "figures/reintervention/reintervention_planned_duration_distribution.png",
  p_duration,
  width = 8.5,
  height = 5.0,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/reintervention/reintervention_planned_duration_distribution.svg",
  p_duration,
  width = 8.5,
  height = 5.0,
  bg = "white"
)


# =============================================================================
# FIGURE S6: Planned time used by app
# =============================================================================

p_time_used_app <- planned_time_used_by_app %>%
  filter(!app2 %in% c("Facebook", "Other") ) %>% 
  mutate(
    app2 = fct_reorder(app2, mean_percent_used)
  ) %>%
  ggplot(
    aes(
      x = app2,
      y = mean_percent_used
    )
  ) +
  geom_col(
    fill = REINTERVENTION_BAR_FILL,
    color = "#000000",
    width = 0.72,
    linewidth = 0.25
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = c(0, 10, 20, 30, 40, 50),
    labels = label_percent(scale = 1),
    expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Mean percentage of planned time used",
    title = ""
  ) +
  reintervention_figure_theme +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(colour = "black")
  )

ggsave(
  "figures/reintervention/reintervention_time_used_by_app.png",
  p_time_used_app,
  width = 7.5,
  height = 4.8,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/reintervention/reintervention_time_used_by_app.svg",
  p_time_used_app,
  width = 7.5,
  height = 4.8,
  bg = "white"
)