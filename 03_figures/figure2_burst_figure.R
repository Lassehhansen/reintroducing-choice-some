Sys.setenv(TZ = "Europe/Copenhagen")

# =============================================================================
# 03_figures/figure2_burst_figure.R
#
# PURPOSE:
#   Assemble Figure 2 burst visualization from current burst-analysis outputs.
#
# FIXES:
#   - Does not use rename_with() to rename rr/rr_low/rr_high into ratio columns,
#     because current burst_effects.csv can contain both rr and ratio columns.
#   - Coalesces compatible columns safely instead.
#   - Builds metric from metric if present, otherwise from model.
#   - Keeps the figure structure and visual styling from the uploaded script.
#   - Fixes the typo bg = "white"e.
# =============================================================================

source("02_analysis/shared_utils.R")

library(tidyverse)
library(scales)
library(patchwork)
library(showtext)
library(sysfonts)

make_output_dirs(
  "figures/burst",
  "tables/burst"
)

cond_colors <- c(
  "Reflection" = "#737373",
  "Planning" = "#9e0b1d",
  "Waiting" = "#8dadbf"
)

cond_levels <- c(
  "Reflection",
  "Planning",
  "Waiting"
)

# =============================================================================
# 1. LOAD HELPERS
# =============================================================================

read_existing_csv <- function(paths) {
  existing_path <- paths[file.exists(paths)][1]
  
  if (is.na(existing_path)) {
    stop(
      paste0(
        "None of these files exist:\n",
        paste(paths, collapse = "\n")
      ),
      call. = FALSE
    )
  }
  
  read_csv(existing_path, show_col_types = FALSE)
}

standardise_condition <- function(x) {
  case_when(
    as.character(x) == "control" ~ "Reflection",
    as.character(x) == "intervention" ~ "Planning",
    as.character(x) == "default" ~ "Waiting",
    TRUE ~ as.character(x)
  )
}

standardise_metric <- function(x) {
  case_when(
    as.character(x) == "burst_rate" ~ "Daily burst rate",
    as.character(x) == "multiapp_burst_count" ~ "Daily multi-app bursts",
    as.character(x) == "mean_burst_length" ~ "Mean burst length",
    as.character(x) == "Daily burst rate" ~ "Daily burst rate",
    as.character(x) == "Daily multi-app bursts" ~ "Daily multi-app bursts",
    as.character(x) == "Mean burst length" ~ "Mean burst length",
    TRUE ~ as.character(x)
  )
}

get_first_existing_numeric <- function(df, candidate_cols) {
  existing_cols <- intersect(candidate_cols, names(df))
  
  if (length(existing_cols) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  out <- suppressWarnings(as.numeric(df[[existing_cols[1]]]))
  
  if (length(existing_cols) > 1) {
    for (col_i in existing_cols[-1]) {
      out <- dplyr::coalesce(
        out,
        suppressWarnings(as.numeric(df[[col_i]]))
      )
    }
  }
  
  out
}

get_first_existing_character <- function(df, candidate_cols) {
  existing_cols <- intersect(candidate_cols, names(df))
  
  if (length(existing_cols) == 0) {
    return(rep(NA_character_, nrow(df)))
  }
  
  out <- as.character(df[[existing_cols[1]]])
  
  if (length(existing_cols) > 1) {
    for (col_i in existing_cols[-1]) {
      out <- dplyr::coalesce(
        out,
        as.character(df[[col_i]])
      )
    }
  }
  
  out
}

# =============================================================================
# 2. LOAD AND STANDARDISE BURST EFFECTS
# =============================================================================

effects_raw <- read_existing_csv(
  c(
    "tables/burst/burst_effects.csv",
    "analysis/burst/burst_effects.csv"
  )
)

effects_all <- effects_raw %>%
  mutate(
    model_raw = get_first_existing_character(
      .,
      c("model")
    ),
    metric_raw = get_first_existing_character(
      .,
      c("metric", "model")
    ),
    condition_raw = get_first_existing_character(
      .,
      c("condition", "condition2", "condition_label")
    ),
    ratio = get_first_existing_numeric(
      .,
      c("ratio", "rr")
    ),
    ratio_lo = get_first_existing_numeric(
      .,
      c("ratio_lo", "rr_low", "rr_lo")
    ),
    ratio_hi = get_first_existing_numeric(
      .,
      c("ratio_hi", "rr_high", "rr_hi")
    ),
    pct = get_first_existing_numeric(
      .,
      c("pct", "pct_change")
    ),
    pct_low = get_first_existing_numeric(
      .,
      c("pct_low", "pct_lo")
    ),
    pct_high = get_first_existing_numeric(
      .,
      c("pct_high", "pct_hi")
    ),
    p = get_first_existing_numeric(
      .,
      c("p", "p_value")
    ),
    metric = standardise_metric(metric_raw),
    condition = standardise_condition(condition_raw),
    condition = factor(
      condition,
      levels = cond_levels
    ),
    metric = factor(
      metric,
      levels = c(
        "Daily burst rate",
        "Daily multi-app bursts",
        "Mean burst length"
      )
    )
  ) %>%
  drop_na(
    metric,
    condition,
    ratio,
    ratio_lo,
    ratio_hi
  ) %>%
  dplyr::select(
    any_of(c(
      "model",
      "metric",
      "condition",
      "estimate_log",
      "se_log",
      "conf.low_log",
      "conf.high_log",
      "ratio",
      "ratio_lo",
      "ratio_hi",
      "pct",
      "pct_low",
      "pct_high",
      "z",
      "p"
    ))
  )

stopifnot(
  all(c("metric", "condition", "ratio", "ratio_lo", "ratio_hi") %in% names(effects_all)),
  all(!is.na(effects_all$metric)),
  all(!is.na(effects_all$condition)),
  all(effects_all$ratio_lo <= effects_all$ratio),
  all(effects_all$ratio <= effects_all$ratio_hi)
)

print(effects_all, n = Inf)

# =============================================================================
# 3. LOAD AND STANDARDISE HEATMAP DATA
# =============================================================================

standardise_heatmap_day_type <- function(x) {
  case_when(
    as.character(x) %in% c("Weekday", "weekday", "Monday-Thursday", "Mon-Thu") ~ "Weekday",
    as.character(x) %in% c(
      "Weekend",
      "weekend",
      "Weekend/Friday",
      "Friday/Weekend",
      "Friday-Sunday",
      "Fri-Sun"
    ) ~ "Weekend/Friday",
    TRUE ~ as.character(x)
  )
}

standardise_heatmap_data <- function(df, expected_day_type = NULL) {
  
  if ("condition_label" %in% names(df)) {
    condition_label_raw <- as.character(df$condition_label)
  } else if ("condition" %in% names(df)) {
    condition_label_raw <- as.character(df$condition)
  } else {
    stop(
      "Heatmap data must contain either `condition_label` or `condition`.",
      call. = FALSE
    )
  }
  
  if (!"day_type" %in% names(df)) {
    stop(
      "Heatmap data must contain `day_type`.",
      call. = FALSE
    )
  }
  
  if (!"hour" %in% names(df)) {
    stop(
      "Heatmap data must contain `hour`.",
      call. = FALSE
    )
  }
  
  if (!"median_pct_change" %in% names(df)) {
    stop(
      "Heatmap data must contain `median_pct_change`.",
      call. = FALSE
    )
  }
  
  if (!"n_users" %in% names(df)) {
    df <- df %>%
      mutate(
        n_users = NA_integer_
      )
  }
  
  df_out <- df %>%
    mutate(
      condition_label_raw = condition_label_raw,
      condition_label = standardise_condition(condition_label_raw),
      condition_label = factor(
        condition_label,
        levels = cond_levels
      ),
      day_type = standardise_heatmap_day_type(day_type),
      day_type = factor(
        day_type,
        levels = c("Weekday", "Weekend/Friday")
      ),
      hour = as.integer(hour),
      display_hour = if_else(
        hour < 5L,
        hour + 24L,
        hour
      ),
      median_pct_change = as.numeric(median_pct_change),
      n_users = as.integer(n_users)
    )
  
  if (!is.null(expected_day_type)) {
    expected_day_type <- standardise_heatmap_day_type(expected_day_type)
    
    df_out <- df_out %>%
      filter(
        as.character(day_type) == expected_day_type
      )
  }
  
  df_out %>%
    drop_na(
      condition_label,
      day_type,
      hour,
      display_hour
    ) %>%
    arrange(
      condition_label,
      display_hour
    )
}

heatmap_weekday <- read_existing_csv(
  c(
    "data/burst/data_heatmap_weekday.csv",
    "analysis/burst/data_heatmap_weekday.csv"
  )
) %>%
  standardise_heatmap_data(expected_day_type = "Weekday")

heatmap_weekend <- read_existing_csv(
  c(
    "data/burst/data_heatmap_weekend.csv",
    "analysis/burst/data_heatmap_weekend.csv"
  )
) %>%
  standardise_heatmap_data(expected_day_type = "Weekend/Friday")

stopifnot(
  all(c("condition_label", "day_type", "hour", "display_hour", "median_pct_change", "n_users") %in% names(heatmap_weekday)),
  all(c("condition_label", "day_type", "hour", "display_hour", "median_pct_change", "n_users") %in% names(heatmap_weekend)),
  nrow(heatmap_weekday) > 0,
  nrow(heatmap_weekend) > 0,
  all(as.character(heatmap_weekday$day_type) == "Weekday"),
  all(as.character(heatmap_weekend$day_type) == "Weekend/Friday")
)

message(
  "Heatmap weekday rows after standardisation: ",
  nrow(heatmap_weekday)
)

message(
  "Heatmap weekend/Friday rows after standardisation: ",
  nrow(heatmap_weekend)
)

print(
  heatmap_weekday %>%
    count(condition_label, day_type),
  n = Inf
)

print(
  heatmap_weekend %>%
    count(condition_label, day_type),
  n = Inf
)


# =============================================================================
# 4. DESCRIPTIVE TABLES FOR MANUSCRIPT SUPPORT
# =============================================================================

hour_label_from_display_hour <- function(display_hour) {
  case_when(
    display_hour == 24 ~ "12 am",
    display_hour == 25 ~ "1 am",
    display_hour == 26 ~ "2 am",
    display_hour == 0 ~ "12 am",
    display_hour < 12 ~ paste0(display_hour, " am"),
    display_hour == 12 ~ "12 pm",
    TRUE ~ paste0(display_hour - 12, " pm")
  )
}

hourly_change <- bind_rows(
  heatmap_weekday,
  heatmap_weekend
) %>%
  mutate(
    hour_label = hour_label_from_display_hour(display_hour),
    reliable = n_users >= 20,
    time_window = case_when(
      day_type == "Weekday" & display_hour >= 6 & display_hour < 9 ~ "Morning",
      day_type == "Weekday" & display_hour >= 9 & display_hour < 14 ~ "School hours",
      day_type == "Weekday" & display_hour >= 14 & display_hour < 18 ~ "After school",
      day_type == "Weekday" & display_hour >= 18 & display_hour < 23 ~ "Evening",
      day_type == "Weekday" & display_hour >= 23 ~ "Late night",
      day_type == "Weekday" & display_hour <= 2 ~ "Late night",
      day_type == "Weekend" & display_hour >= 6 & display_hour < 11 ~ "Morning",
      day_type == "Weekend" & display_hour >= 11 & display_hour < 17 ~ "Afternoon",
      day_type == "Weekend" & display_hour >= 17 & display_hour < 21 ~ "Evening",
      day_type == "Weekend" & display_hour >= 21 ~ "Night",
      day_type == "Weekend" & display_hour <= 2 ~ "Night",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(reliable)

hourly_results <- hourly_change %>%
  group_by(day_type, condition_label) %>%
  summarise(
    n_hours = n(),
    median_hourly_pct_change = median(median_pct_change, na.rm = TRUE),
    mean_hourly_pct_change = mean(median_pct_change, na.rm = TRUE),
    min_hourly_pct_change = min(median_pct_change, na.rm = TRUE),
    max_hourly_pct_change = max(median_pct_change, na.rm = TRUE),
    share_hours_reduced = mean(median_pct_change < 0, na.rm = TRUE),
    strongest_reduction_hour = hour_label[which.min(median_pct_change)],
    strongest_reduction_pct = min(median_pct_change, na.rm = TRUE),
    strongest_increase_hour = hour_label[which.max(median_pct_change)],
    strongest_increase_pct = max(median_pct_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(
        median_hourly_pct_change,
        mean_hourly_pct_change,
        min_hourly_pct_change,
        max_hourly_pct_change,
        strongest_reduction_pct,
        strongest_increase_pct
      ),
      ~ round(.x, 1)
    ),
    share_hours_reduced = round(100 * share_hours_reduced, 1)
  )

window_results <- hourly_change %>%
  filter(!is.na(time_window)) %>%
  mutate(
    time_window = case_when(
      day_type == "Weekday" ~ factor(
        time_window,
        levels = c(
          "Morning",
          "School hours",
          "After school",
          "Evening",
          "Late night"
        )
      ) %>% as.character(),
      day_type == "Weekend" ~ factor(
        time_window,
        levels = c(
          "Morning",
          "Afternoon",
          "Evening",
          "Night"
        )
      ) %>% as.character(),
      TRUE ~ as.character(time_window)
    )
  ) %>%
  group_by(day_type, condition_label, time_window) %>%
  summarise(
    n_hours = n(),
    median_pct_change = median(median_pct_change, na.rm = TRUE),
    mean_pct_change = mean(median_pct_change, na.rm = TRUE),
    min_pct_change = min(median_pct_change, na.rm = TRUE),
    max_pct_change = max(median_pct_change, na.rm = TRUE),
    median_n_users = median(n_users, na.rm = TRUE),
    min_n_users = min(n_users, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(
        median_pct_change,
        mean_pct_change,
        min_pct_change,
        max_pct_change
      ),
      ~ round(.x, 1)
    ),
    median_n_users = round(median_n_users, 0)
  )

strongest_windows <- window_results %>%
  group_by(day_type, condition_label) %>%
  slice_min(
    order_by = median_pct_change,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  transmute(
    day_type,
    condition_label,
    strongest_window = as.character(time_window),
    strongest_window_pct = median_pct_change
  )

write_csv(
  hourly_results,
  "tables/burst/burst_hourly_results.csv"
)

write_csv(
  window_results,
  "tables/burst/burst_window_results.csv"
)

write_csv(
  strongest_windows,
  "tables/burst/burst_strongest_windows.csv"
)

print(hourly_results, n = Inf)
print(window_results, n = Inf)
print(strongest_windows, n = Inf)

# =============================================================================
# 5. SHARED THEME
# =============================================================================

theme_figure <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      strip.background = element_rect(
        fill = "white",
        color = NA
      ),
      panel.grid.minor = element_blank(),
      axis.text = element_text(
        size = 10,
        family = BODY_FONT,
        color = "#000000"
      ),
      axis.title = element_text(
        size = 11,
        family = BODY_FONT,
        color = "#000000",
        face = "bold"
      ),
      strip.text = element_text(
        size = 11,
        family = BOLD_FONT,
        color = "#000000",
        face = "bold"
      ),
      plot.title = element_text(
        size = 11,
        family = BOLD_FONT,
        color = "#000000",
        hjust = 0.5,
        margin = margin(b = 4)
      ),
      plot.tag = element_text(
        size = 13,
        family = BODY_FONT,
        color = "#000000"
      ),
      plot.tag.position = c(0, 1),
      legend.position = "none",
      plot.margin = margin(
        t = 8,
        r = 10,
        b = 4,
        l = 10
      )
    )
}

# =============================================================================
# 6. PANEL A: BURST EFFECT FOREST PLOT
# =============================================================================

dodge_w <- 0.55

p_A <- effects_all %>%
  mutate(
    metric = factor(
      metric,
      levels = c(
        "Mean burst length",
        "Daily multi-app bursts",
        "Daily burst rate"
      )
    )
  ) %>%
  ggplot(
    aes(
      x = ratio,
      y = metric,
      color = condition
    )
  ) +
  geom_hline(
    yintercept = c(1.5, 2.5, 3.5),
    color = "#000000",
    linewidth = 0.05
  ) +
  geom_vline(
    xintercept = 1,
    linewidth = 0.6,
    color = "#000000"
  ) +
  geom_errorbarh(
    aes(
      xmin = ratio_lo,
      xmax = ratio_hi
    ),
    height = 0.3,
    linewidth = 0.5,
    position = position_dodge(width = dodge_w)
  ) +
  geom_point(
    size = 3.5,
    position = position_dodge(width = dodge_w)
  ) +
  scale_color_manual(
    values = cond_colors,
    name = NULL,
    guide = guide_legend(
      nrow = 1,
      override.aes = list(size = 4)
    )
  ) +
  scale_x_log10(
    limits = c(0.2, 4),
    breaks = c(0.2, 0.5, 1.0, 2.0, 4.0),
    labels = c("0.2", "0.5", "1.0", "2.0", "4.0"),
    expand = c(0, 0)
  ) +
  scale_y_discrete(
    expand = expansion(add = c(0.5, 0.5))
  ) +
  labs(
    x = "Rate ratio (log scale)",
    y = NULL,
    tag = "A"
  ) +
  theme_figure() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(
      color = "#000000",
      linewidth = 0.3
    ),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(
      size = 12,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.text.x = element_text(
      size = 12,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.title.x = element_text(
      size = 12,
      family = BODY_FONT,
      color = "#000000",
      face = "bold",
      margin = margin(t = 6)
    ),
    legend.position = "bottom",
    legend.text = element_text(
      size = 11,
      family = BODY_FONT,
      color = "#000000"
    ),
    legend.margin = margin(t = 4, b = 0),
    legend.key.size = unit(1.0, "lines"),
    strip.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(
      t = 12,
      r = 12,
      b = 20,
      l = 12
    )
  )

# =============================================================================
# 7. HEATMAP PANELS
# =============================================================================

hour_breaks <- c(
  6,
  8,
  10,
  12,
  14,
  16,
  18,
  20,
  22,
  24,
  26
)

hour_labels <- c(
  "6am",
  "8am",
  "10am",
  "12pm",
  "2pm",
  "4pm",
  "6pm",
  "8pm",
  "10pm",
  "12am",
  "2am"
)

windows_weekday <- tibble(
  xmin = c(5.5, 8.5, 13.5, 17.5, 22.5),
  xmax = c(8.5, 13.5, 17.5, 22.5, 26.5),
  label = c(
    "Morning",
    "School",
    "After school",
    "Evening",
    "Late night"
  ),
  x_lab = c(7.0, 11.0, 15.5, 20.0, 24.5)
)

windows_weekend <- tibble(
  xmin = c(5.5, 10.5, 16.5, 20.5),
  xmax = c(10.5, 16.5, 20.5, 26.5),
  label = c(
    "Morning",
    "Afternoon",
    "Evening",
    "Night"
  ),
  x_lab = c(8.0, 13.5, 18.5, 23.5)
)

make_heatmap <- function(data, windows, tag_label, title_label) {
  
  data <- data %>%
    mutate(
      condition_label = factor(
        condition_label,
        levels = cond_levels
      )
    )
  
  n_cond <- length(cond_levels)
  y_ann <- n_cond + 0.72
  x_max <- max(data$display_hour, na.rm = TRUE) + 0.5
  
  ggplot(
    data,
    aes(
      x = display_hour,
      y = condition_label,
      fill = median_pct_change
    )
  ) +
    geom_tile(
      color = "white",
      linewidth = 0.55
    ) +
    geom_tile(
      data = data %>%
        filter(is.na(median_pct_change)),
      aes(
        x = display_hour,
        y = condition_label
      ),
      fill = "#CCCCCC",
      color = "white",
      linewidth = 0.55,
      inherit.aes = FALSE
    ) +
    geom_rect(
      data = windows %>%
        filter(xmax <= x_max + 1),
      aes(
        xmin = xmin,
        xmax = pmin(xmax, x_max),
        ymin = 0.4,
        ymax = n_cond + 0.6
      ),
      fill = NA,
      color = "#888888",
      linetype = "dashed",
      linewidth = 0.35,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = windows %>%
        filter(x_lab <= x_max),
      aes(
        x = x_lab,
        y = y_ann,
        label = label
      ),
      size = 4,
      color = "#555555",
      fontface = "italic",
      family = BODY_FONT,
      vjust = 0,
      inherit.aes = FALSE
    ) +
    scale_fill_gradientn(
      colors = c(
        "#1a4573",
        "#2166AC",
        "#6BAED6",
        "#C6DBEF",
        "#FFFFFF",
        "#FCBBA1",
        "#FC4E2A",
        "#9e0b1d"
      ),
      values = rescale(
        c(
          -75,
          -50,
          -25,
          -5,
          0,
          5,
          15,
          25
        )
      ),
      limits = c(-75, 25),
      oob = squish,
      na.value = "#CCCCCC",
      name = "% Change in opens",
      breaks = c(-75, -50, -25, 0, 25),
      labels = c("\u221275%" ,"\u221250%", "\u221225%", "0%", "+25%"),
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(8, "cm"),
        barheight = unit(0.35, "cm"),
        ticks = TRUE,
        ticks.colour = "white",
        ticks.linewidth = 0.8,
        frame.colour = NA,
        label.theme = element_text(
          size = 10,
          family = BODY_FONT,
          color = "#000000",
          face = "bold"
        ),
        title.theme = element_text(
          size = 11,
          family = BOLD_FONT,
          color = "#000000",
          face = "bold",
          margin = margin(b = 4)
        )
      )
    ) +
    scale_x_continuous(
      breaks = hour_breaks,
      labels = hour_labels,
      limits = c(5.4, x_max),
      expand = c(0, 0)
    ) +
    scale_y_discrete(
      limits = cond_levels,
      expand = expansion(add = c(0.9, 1.2))
    ) +
    labs(
      title = title_label,
      x = "",
      y = NULL,
      tag = tag_label
    ) +
    theme_figure() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(
        size = 12,
        family = BODY_FONT,
        color = "#000000",
        face = "bold",
        angle = 0
      ),
      axis.text.y = element_text(
        size = 12,
        family = BODY_FONT,
        color = "#000000",
        face = "bold"
      ),
      axis.title.x = element_text(
        size = 12,
        family = BODY_FONT,
        color = "#000000",
        face = "bold",
        margin = margin(t = 6)
      ),
      axis.ticks.x = element_line(color = "#000000"),
      axis.line.x = element_line(
        color = "#000000",
        linewidth = 0.3
      ),
      axis.line.y = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(
        size = 11,
        family = BODY_FONT,
        color = "#000000",
        face = "bold"
      ),
      legend.margin = margin(t = 4, b = 2),
      plot.margin = margin(
        t = 20,
        r = 10,
        b = 4,
        l = 10
      )
    )
}

p_B <- make_heatmap(
  heatmap_weekday,
  windows_weekday,
  "B",
  "Weekday"
) +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 12,
      r = 12,
      b = 0,
      l = 12
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.title = element_text(
      size = 12,
      hjust = 0,
      family = BODY_FONT,
      color = "#000000",
      face = "bold"
    )
  )

p_C <- make_heatmap(
  heatmap_weekend,
  windows_weekend,
  "C",
  "Weekend"
) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(8, "cm"),
      barheight = unit(0.35, "cm"),
      ticks = TRUE,
      ticks.colour = "white",
      ticks.linewidth = 0.8,
      frame.colour = NA,
      label.theme = element_text(
        size = 10,
        family = BODY_FONT,
        color = "#000000",
        face = "bold"
      ),
      title.theme = element_text(
        size = 12,
        family = BODY_FONT,
        color = "#000000",
        face = "plain",
        margin = margin(b = 4)
      )
    )
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      size = 12,
      hjust = 0,
      family = BODY_FONT,
      color = "#000000",
      face = "bold"
    ),
    plot.margin = margin(
      t = 0,
      r = 12,
      b = 0,
      l = 12
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

# =============================================================================
# 8. ASSEMBLE AND SAVE
# =============================================================================

p_full <- p_A / p_B / p_C +
  plot_layout(
    heights = c(0.9, 1.1, 1.1),
    guides = "keep"
  ) &
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

p_full <- p_full +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.margin = margin(
        t = 12,
        r = 12,
        b = 12,
        l = 12
      )
    )
  )

p_full

save_figure(
  p_A,
  "figure2_panel_A_burst_effects",
  "figures/burst",
  width = 10,
  height = 3.6
)

save_figure(
  p_B,
  "figure2_panel_B_weekday_heatmap",
  "figures/burst",
  width = 10,
  height = 3.6
)

save_figure(
  p_C,
  "figure2_panel_C_weekend_heatmap",
  "figures/burst",
  width = 10,
  height = 3.6
)

save_figure(
  p_full,
  "figure2_burst",
  "figures/burst",
  width = 10,
  height = 11
)

ggsave(
  "figures/burst/figure2_burst.png",
  p_full,
  width = 10,
  height = 11,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/burst/figure2_burst.svg",
  p_full,
  width = 10,
  height = 11,
  bg = "white"
)

message("\n=== figure2_burst_figure.R complete ===")