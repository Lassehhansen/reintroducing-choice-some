Sys.setenv(TZ = "Europe/Copenhagen")

# =============================================================================
# 04_supplementary/appendix_A_baseline_descriptives/A_baseline_platform_use.R
#
# PURPOSE:
#   Produce Figures A1-A3 from Appendix A: descriptive summaries of
#   social media use during the two-week baseline period by platform.
#
#   Figure A1: Average daily time per platform
#   Figure A2: Median daily opens per platform
#   Figure A3: Mean session duration per platform
#
# INPUTS:
#   data/baseline_all_apps/session_time/time_per_day_app.csv
#   data/baseline_all_apps/session_time/time_per_day.csv
#   data/baseline_all_apps/session_time/time_per_day_avg_app.csv
#   data/baseline_all_apps/session_time/time_per_day_avg.csv
#   data/baseline_all_apps/opens/opens_per_day_app.csv
#   data/baseline_all_apps/opens/opens_per_day.csv
#
# OUTPUTS:
#   figures/supplementary/appendix_A1_daily_time_by_platform.{png,svg}
#   figures/supplementary/appendix_A2_daily_opens_by_platform.{png,svg}
#   figures/supplementary/appendix_A3_session_length_by_platform.{png,svg}
#   tables/supplementary/appendix_A_baseline_summary.{xlsx,tex}
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs(
  "figures/supplementary",
  "tables/supplementary"
)

# =============================================================================
# HELPERS
# =============================================================================

clean_app_name <- function(x) {
  recode(
    x,
    facebook = "Facebook",
    tikTok = "TikTok",
    snapchat = "Snapchat",
    instagram = "Instagram",
    youtube = "YouTube",
    linkedin = "LinkedIn",
    beReal = "BeReal",
    pinterest = "Pinterest",
    twitch = "Twitch",
    reddit = "Reddit",
    twitter = "Twitter",
    .default = x
  )
}

format_hours_minutes <- function(minutes) {
  h <- floor(minutes / 60)
  m <- round(minutes %% 60)
  glue::glue("{h}h {m}m")
}

# Exclude first baseline day and transition days.
EXCL_DAYS <- c(-14, 0, 1)

# =============================================================================
# 1. FIGURE A1: AVERAGE DAILY TIME PER PLATFORM
# =============================================================================

time_app <- read_csv(
  "data/baseline_all_apps/session_time/time_per_day_app.csv",
  show_col_types = FALSE
)

time_all <- read_csv(
  "data/baseline_all_apps/session_time/time_per_day.csv",
  show_col_types = FALSE
)

mean_all_minutes <- time_all %>%
  filter(
    !days_before_activation %in% EXCL_DAYS
  ) %>%
  group_by(ID) %>%
  summarise(
    mean_daily_time = mean(total_time_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    grand_mean = mean(mean_daily_time, na.rm = TRUE)
  ) %>%
  pull(grand_mean)

mean_time_label <- format_hours_minutes(mean_all_minutes)

app_sum_time_overall <- time_app %>%
  filter(
    !days_before_activation %in% EXCL_DAYS
  ) %>%
  group_by(app, ID) %>%
  summarise(
    participant_mean_daily_time = mean(total_time_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(app) %>%
  summarise(
    mean_total_time = mean(participant_mean_daily_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    app = clean_app_name(app),
    app = fct_reorder(app, mean_total_time)
  )

p_sum_mean_word <- ggplot(
  app_sum_time_overall,
  aes(
    x = app,
    y = mean_total_time
  )
) +
  geom_bar(
    stat = "identity",
    color = "black",
    width = 0.85,
    fill = "#8dadbf"
  ) +
  labs(
    title = "",
    subtitle = glue::glue("Average Daily Time Spent Across Social Media: {mean_time_label}"),
    x = "",
    y = "",
    fill = "App"
  ) +
  scale_y_continuous(
    limits = c(0, 130),
    breaks = c(0, 30, 60, 90, 120),
    labels = function(x) paste0(x, " Mins"),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    expand = c(0, 0)
  ) +
  coord_flip() +
  custom_general_theme_word() +
  theme(
    plot.margin = margin(t = 0, r = 30, b = -10, l = -10),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "black"),
    panel.grid.major.y = element_blank(),
    
    axis.title.x = element_text(
      size = 12,
      family = BODY_FONT,
      color = "black",
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 12,
      family = BODY_FONT,
      color = "black",
      margin = margin(t = 10)
    ),
    axis.text.x = element_text(
      size = 12,
      family = BODY_FONT,
      color = "black",
      margin = margin(t = 10)
    )
  )

save_figure(
  p_sum_mean_word,
  "appendix_A1_daily_time_by_platform",
  "figures/supplementary",
  width = 1200 / 96,
  height = 500 / 96
)

# =============================================================================
# 2. FIGURE A2: MEDIAN DAILY OPENS PER PLATFORM
# =============================================================================

opens_app <- read_csv(
  "data/baseline_all_apps/opens/opens_per_day_app.csv",
  show_col_types = FALSE
)

opens_all <- read_csv(
  "data/baseline_all_apps/opens/opens_per_day.csv",
  show_col_types = FALSE
)

median_all_opens <- opens_all %>%
  filter(
    !days_before_activation %in% EXCL_DAYS
  ) %>%
  group_by(ID) %>%
  summarise(
    participant_median_daily_opens = median(daily_opens, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    grand_median = median(participant_median_daily_opens, na.rm = TRUE)
  ) %>%
  pull(grand_median)

median_opens_label <- round(median_all_opens)

app_opens_overall <- opens_app %>%
  filter(
    !days_before_activation %in% EXCL_DAYS
  ) %>%
  group_by(app, ID) %>%
  summarise(
    participant_median_daily_opens = median(daily_opens, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(app) %>%
  summarise(
    median_opens = median(participant_median_daily_opens, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    app = clean_app_name(app),
    app = fct_reorder(app, median_opens)
  )

p_sum_mean_opens_word <- ggplot(
  app_opens_overall,
  aes(
    x = app,
    y = median_opens
  )
) +
  geom_bar(
    stat = "identity",
    color = "black",
    width = 0.9,
    fill = "#8dadbf"
  ) +
  labs(
    title = "",
    subtitle = glue::glue("Median Daily Opens Across Apps: {median_opens_label}"),
    x = "",
    y = ""
  ) +
  scale_y_continuous(
    limits = c(0, 25),
    breaks = c(0, 5, 10, 15, 20, 25),
    labels = function(x) paste0(x, " Opens"),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    expand = c(0, 0)
  ) +
  coord_flip() +
  custom_general_theme_word() +
  theme(
    plot.margin = margin(t = 0, r = 30, b = -10, l = -10),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "black"),
    panel.grid.major.y = element_blank(),
    
    axis.title.x = element_text(
      size = 12,
      family = BODY_FONT,
      color = "black",
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 12,
      family = BODY_FONT,
      color = "black",
      margin = margin(t = 10)
    ),
    axis.text.x = element_text(
      size = 12,
      family = BODY_FONT,
      color = "black",
      margin = margin(t = 10)
    )
  )
save_figure(
  p_sum_mean_opens_word,
  "appendix_A2_daily_opens_by_platform",
  "figures/supplementary",
  width = 1200 / 96,
  height = 500 / 96
)

# =============================================================================
# 3. FIGURE A3: MEAN SESSION DURATION PER PLATFORM
# =============================================================================

session_app <- read_csv(
  "data/baseline_all_apps/session_time/time_per_day_avg_app.csv",
  show_col_types = FALSE
)

session_all <- read_csv(
  "data/baseline_all_apps/session_time/time_per_day_avg.csv",
  show_col_types = FALSE
)

mean_session <- session_all %>%
  filter(
    !days_before_activation %in% EXCL_DAYS
  ) %>%
  group_by(ID) %>%
  summarise(
    participant_mean_session = mean(mean_time_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    grand_mean = mean(participant_mean_session, na.rm = TRUE)
  ) %>%
  pull(grand_mean)

mean_session_label <- glue::glue("{round(mean_session, 1)} min")

app_avg_session <- session_app %>%
  filter(
    !days_before_activation %in% EXCL_DAYS
  ) %>%
  group_by(app, ID) %>%
  summarise(
    participant_mean_session = mean(mean_time_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(app) %>%
  summarise(
    mean_sesh = mean(participant_mean_session, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    app = clean_app_name(app),
    app = fct_reorder(app, mean_sesh)
  )

p_session_mean_word <- ggplot(
  app_avg_session,
  aes(
    x = app,
    y = mean_sesh
  )
) +
  geom_bar(
    stat = "identity",
    color = "black",
    width = 0.9,
    fill = "#8dadbf"
  ) +
  labs(
    title = "",
    subtitle = glue::glue("Mean Session Length Across Apps: {mean_session_label}"),
    x = "",
    y = ""
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    expand = c(0, 0),
    labels = function(x) paste0(x, " min")
  ) +
  scale_x_discrete(
    expand = c(0, 0)
  ) +
  coord_flip() +
  custom_general_theme_word() +
  theme(
    plot.margin = margin(t = 0, r = 30, b = -10, l = -10),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "black"),
    panel.grid.major.y = element_blank(),
    
    axis.title.x = element_text(
      size = 12,
      family = BODY_FONT,
      color = "black",
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 12,
      family = BODY_FONT,
      color = "black",
      margin = margin(t = 10)
    ),
    axis.text.x = element_text(
      size = 12,
      family = BODY_FONT,
      color = "black",
      margin = margin(t = 10)
    )
  )

save_figure(
  p_session_mean_word,
  "appendix_A3_session_length_by_platform",
  "figures/supplementary",
  width = 1200 / 96,
  height = 500 / 96
)

# =============================================================================
# 4. SUMMARY TABLE
# =============================================================================

appendix_A_table <- app_sum_time_overall %>%
  rename(
    mean_daily_time_min = mean_total_time
  ) %>%
  full_join(
    app_opens_overall %>%
      rename(
        median_daily_opens = median_opens
      ),
    by = "app"
  ) %>%
  full_join(
    app_avg_session %>%
      rename(
        mean_session_min = mean_sesh
      ),
    by = "app"
  ) %>%
  arrange(
    desc(mean_daily_time_min)
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

save_model_table(
  appendix_A_table,
  "appendix_A_baseline_summary",
  "tables/supplementary",
  caption = "Appendix A: Baseline social media use by platform",
  label = "tab:appendix_A_baseline"
)

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    appendix_A_table,
    "tables/supplementary/appendix_A_baseline_summary.xlsx"
  )
}

cat("\n=== APPENDIX A SUMMARY ===\n")
print(appendix_A_table)

message("\n=== appendix_A_baseline_descriptives complete ===")