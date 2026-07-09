Sys.setenv(TZ = "Europe/Copenhagen")
# =============================================================================
# 04_supplementary/appendix_B_activity_patterns/B_activity_heatmap.R
#
# PURPOSE:
#   Produce Figure B4 from Appendix B: percentage of participants active on
#   any social media platform by hour and day of week during baseline.
#   Also computes the school-hours statistics reported in the Appendix B text.
#
# DATA PIPELINE:
#   Uses opens_per_hour.csv and baseline_opens_filt3.csv to build a
#   denominator of total possible user-hour observations, then computes
#   the proportion active at each hour × weekday cell.
#
# INPUTS:
#   data/baseline_all_apps/opens/opens_per_hour.csv
#   data/baseline/baseline_raw/baseline_opens_filt3.csv
#   data/survey/klassetrin_clean.csv
#
# OUTPUTS:
#   figures/supplementary/appendix_B4_activity_heatmap.{png,svg}
#   tables/supplementary/appendix_B_activity_by_hour.{xlsx,tex}
#   tables/supplementary/appendix_B_school_hours_stats.csv
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs("figures/supplementary", "tables/supplementary")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

opens_hour <- read_csv(
  "data/baseline_all_apps/opens/opens_per_hour.csv",
  show_col_types = FALSE
) %>%
  mutate(
    date       = as.Date(date, tz = "Europe/Copenhagen"),
    day_of_week = weekdays(date),
    # Standardised fall break: 2024-10-14
    fall_break = as.integer(
      date >= as.Date("2024-10-14") & date <= as.Date("2024-10-18")
    ),
    christmas_break = as.integer(
      date >= as.Date("2024-12-23") & date <= as.Date("2025-01-02")
    )
  )

klassetrin_clean <- read_csv("data/survey/klassetrin_clean.csv",
                              show_col_types = FALSE)

# Raw event-level data for building user activity bounds
opens_raw <- read_csv(
  "data/baseline/baseline_raw/baseline_opens_filt3.csv",
  show_col_types = FALSE
) %>%
  mutate(
    time = with_tz(time, tzone = "Europe/Copenhagen"),
    date = as.Date(time, tz  = "Europe/Copenhagen"),
    fall_break = as.integer(
      date >= as.Date("2024-10-14") & date <= as.Date("2024-10-18")
    ),
    christmas_break = as.integer(
      date >= as.Date("2024-12-23") & date <= as.Date("2025-01-02")
    )
  ) %>%
  left_join(klassetrin_clean, by = "ID") %>%
  filter(fall_break != 1, christmas_break != 1)

# =============================================================================
# 2. BUILD DENOMINATOR  (all user × hour slots between each user's first and last use)
# =============================================================================

user_bounds <- opens_raw %>%
  group_by(ID) %>%
  summarise(
    first_time = min(time, na.rm = TRUE),
    last_time  = max(time, na.rm = TRUE),
    .groups = "drop"
  )

user_slots <- purrr::map2_dfr(
  user_bounds$ID,
  seq_len(nrow(user_bounds)),
  function(id, i) {
    times <- seq(
      from = floor_date(user_bounds$first_time[i], "hour"),
      to   = floor_date(user_bounds$last_time[i],  "hour"),
      by   = "1 hour"
    )
    tibble(
      ID          = id,
      datetime    = times,
      hour        = hour(times),
      day_of_week = weekdays(as.Date(times))
    )
  }
)

slots_summary <- user_slots %>%
  group_by(day_of_week, hour) %>%
  summarise(n_possible = n(), .groups = "drop")

# =============================================================================
# 3. BUILD NUMERATOR  (observed active user-hours)
# =============================================================================

activity_events <- opens_hour %>%
  filter(daily_opens > 0, fall_break != 1, christmas_break != 1) %>%
  dplyr::select(ID, date, hour, day_of_week) %>%
  distinct() %>%
  group_by(day_of_week, hour) %>%
  summarise(n_active = n(), .groups = "drop")

# =============================================================================
# 4. COMPUTE % ACTIVE  (shift hours < 6 to next-day frame for continuity)
# =============================================================================

# Map for shifting late-night hours to the previous calendar day name
# (so 01:00 Monday appears at the end of Sunday's row)
prev_day <- c(
  Monday = "Sunday",  Tuesday = "Monday",  Wednesday = "Tuesday",
  Thursday = "Wednesday", Friday = "Thursday", Saturday = "Friday",
  Sunday = "Saturday"
)

pct_active <- slots_summary %>%
  left_join(activity_events, by = c("day_of_week","hour")) %>%
  mutate(
    n_active    = replace_na(n_active, 0),
    pct_active  = 100 * n_active / n_possible,
    hour_shifted = if_else(hour >= 6, hour, hour + 24L),
    day_adj = if_else(
      hour < 6,
      prev_day[day_of_week],
      day_of_week
    ),
    day_adj = factor(
      day_adj,
      levels = c("Monday","Tuesday","Wednesday",
                 "Thursday","Friday","Saturday","Sunday")
    )
  )

# =============================================================================
# 5. FIGURE B4  — Activity heatmap
# =============================================================================

fig_B4 <- ggplot(pct_active,
  aes(x = hour_shifted, y = day_adj, fill = pct_active)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("white","#8a97a9","#4f637e","#142f53","black"),
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = function(x) paste0(x, " %"),
    name   = "",
    guide  = guide_colorbar(barwidth = 15, barheight = 1)
  ) +
  scale_x_continuous(
    position = "top",
    expand   = c(0, 0.1),
    breaks   = c(6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 29),
    labels   = c("06:00","08:00","10:00","12:00",
                 "14:00","16:00","18:00","20:00",
                 "22:00","00:00","05:00")
  ) +
  facet_grid(day_adj ~ ., switch = "y", scales = "free") +
  labs(
    title = "Percentage of Active Users per Hour",
    x = "", y = "",
    fill  = "% Participants who used social media"
  ) +
  custom_general_theme_word() +
  theme(
    strip.text.y.left = element_text(size = 12, angle = 0,
                                     family = BODY_FONT, face = "bold"),
    axis.text.y       = element_blank(),
    axis.text.x       = element_text(size = 11, family = BODY_FONT,
                                     face = "bold"),
    axis.ticks.y      = element_blank(),
    strip.background  = element_rect(fill = NA, color = "#000000"),
    panel.background  = element_rect(fill = NA, color = "#000000"),
    legend.position   = "top",
    legend.text       = element_text(size = 11, family = BODY_FONT),
    panel.spacing     = unit(10, "pt"),
    panel.grid.major  = element_blank(),
    panel.border      = element_blank(),
    plot.margin       = margin(t = 5, r = 10, b = 5, l = 5)
  )

save_figure(fig_B4, "appendix_B4_activity_heatmap",
            "figures/supplementary",
            width = 1200/96, height = 600/96)

# =============================================================================
# 6. SCHOOL-HOURS STATISTICS  (reported in Appendix B text)
# =============================================================================

school_stats <- pct_active %>%
  filter(!day_of_week %in% c("Saturday","Sunday")) %>%
  mutate(
    time_block = case_when(
      hour >= 8  & hour < 14 ~ "School hours (08:00–14:00)",
      hour >= 14 & hour < 18 ~ "Afternoon (14:00–18:00)",
      hour >= 18 & hour < 24 ~ "Evening (18:00–24:00)",
      hour >= 6  & hour < 8  ~ "Morning (06:00–08:00)",
      TRUE                   ~ "Late night (00:00–06:00)"
    )
  ) %>%
  group_by(time_block) %>%
  summarise(
    mean_pct_active = round(mean(pct_active, na.rm = TRUE), 1),
    .groups = "drop"
  )

# Peak active hour
peak_hour <- pct_active %>%
  arrange(desc(pct_active)) %>%
  slice(1) %>%
  mutate(label = glue::glue(
    "{day_of_week} {hour}:00 — {round(pct_active, 1)}% active"
  ))

cat("\n=== APPENDIX B: SCHOOL-HOURS STATISTICS ===\n")
print(school_stats)
cat("\nPeak hour:", peak_hour$label, "\n")

write_csv(school_stats, "tables/supplementary/appendix_B_school_hours_stats.csv")

save_model_table(
  pct_active %>%
    dplyr::select(day_of_week, hour, n_possible, n_active, pct_active) %>%
    mutate(pct_active = round(pct_active, 1)),
  "appendix_B_activity_by_hour", "tables/supplementary",
  caption = "Appendix B: Percentage of participants active by hour and weekday (baseline)",
  label   = "tab:appendix_B_activity"
)

message("\n=== appendix_B_activity_heatmap complete ===")
