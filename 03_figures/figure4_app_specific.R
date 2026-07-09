# =============================================================================
# 02_analysis/app_specific/07_figure4_app_specific_effects.R
#
# PURPOSE:
#   Build Figure 4 from the standardized app-specific model outputs.
#
# INPUTS:
#   tables/app_specific/app_changes_time.csv
#   tables/app_specific/app_changes_opens.csv
#   tables/app_specific/app_changes_session.csv
#   tables/app_specific/dismissal_effects.csv
#   OR
#   tables/app_specific/dismissal_effects_no_facebook.csv
#
# OUTPUTS:
#   figures/app_specific/figure4_app_specific_effects.png
#   figures/app_specific/figure4_app_specific_effects.svg
#   figures/app_specific/figure4_app_specific_effects.pdf
#
# Notes:
#   This follows the structure of the old plotting script, but reads from the
#   new standardized app-specific pipeline outputs. The old figure combined
#   total time, opens, session duration, and dismissal probability in one figure.
#   :contentReference[oaicite:0]{index=0}
# =============================================================================

Sys.setenv(TZ = "Europe/Copenhagen")

source("02_analysis/shared_utils.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
  library(showtext)
  library(sysfonts)
})

make_output_dirs(
  "figures/app_specific",
  "tables/app_specific"
)

# =============================================================================
# 1. GLOBAL SETTINGS
# =============================================================================

TARGET_APPS <- c(
  "instagram",
  "tikTok",
  "snapchat",
  "youtube",
  "facebook"
)

TARGET_APPS_NO_FACEBOOK <- c(
  "instagram",
  "tikTok",
  "snapchat",
  "youtube"
)

APP_LABELS <- c(
  "instagram" = "Instagram",
  "tikTok"    = "TikTok",
  "snapchat"  = "Snapchat",
  "youtube"   = "YouTube",
  "facebook"  = "Facebook"
)

APP_LEVELS_NICE <- c(
  "Instagram",
  "TikTok",
  "Snapchat",
  "YouTube",
  "Facebook"
)

APP_LEVELS_NICE_NO_FACEBOOK <- c(
  "Instagram",
  "TikTok",
  "Snapchat",
  "YouTube"
)

COND_LEVELS_ENG <- c(
  "Reflection",
  "Planning",
  "Waiting"
)

COND_COLOURS <- c(
  "Reflection" = "#737373",
  "Planning"   = "#9e0b1d",
  "Waiting"    = "#8dadbf"
)

# =============================================================================
# 2. LOAD MODEL EFFECT TABLES
# =============================================================================

time_change_app <- read_csv(
  "tables/app_specific/app_changes_time.csv",
  show_col_types = FALSE
) %>%
  mutate(
    facet_new = "Daily Activity"
  )

open_change_app <- read_csv(
  "tables/app_specific/app_changes_opens.csv",
  show_col_types = FALSE
) %>%
  mutate(
    facet_new = "Sessions"
  )

session_change_app <- read_csv(
  "tables/app_specific/app_changes_session.csv",
  show_col_types = FALSE
) %>%
  mutate(
    facet_new = "Session Length"
  )

dismissal_file <- if (file.exists("tables/app_specific/dismissal_effects_no_facebook.csv")) {
  "tables/app_specific/dismissal_effects_no_facebook.csv"
} else {
  "tables/app_specific/dismissal_effects.csv"
}

regret_df <- read_csv(
  dismissal_file,
  show_col_types = FALSE
)

# =============================================================================
# 3. CLEAN AND STACK RATE-RATIO OUTCOMES
# =============================================================================

together_plot_apps <- bind_rows(
  time_change_app,
  open_change_app,
  session_change_app
) %>%
  mutate(
    app_raw = as.character(app),
    app = recode(app_raw, !!!APP_LABELS),
    facet_new = factor(
      facet_new,
      levels = c(
        "Session Length",
        "Sessions",
        "Daily Activity"
      )
    ),
    condition2 = factor(
      condition2,
      levels = COND_LEVELS_ENG
    ),
    rr = case_when(
      "rr" %in% names(.) ~ as.numeric(rr),
      TRUE ~ exp(estimate_log)
    ),
    rr_low = case_when(
      "rr_low" %in% names(.) ~ as.numeric(rr_low),
      TRUE ~ exp(conf.low_log)
    ),
    rr_high = case_when(
      "rr_high" %in% names(.) ~ as.numeric(rr_high),
      TRUE ~ exp(conf.high_log)
    ),
    pct = 100 * (rr - 1),
    pct_low = 100 * (rr_low - 1),
    pct_high = 100 * (rr_high - 1)
  ) %>%
  filter(
    app %in% APP_LEVELS_NICE_NO_FACEBOOK
  ) %>%
  mutate(
    app = factor(
      app,
      levels = APP_LEVELS_NICE_NO_FACEBOOK
    )
  )

# =============================================================================
# 4. CLEAN DISMISSAL PROBABILITY OUTCOME
# =============================================================================

regret_df_clean <- regret_df %>%
  mutate(
    app_raw = as.character(app),
    app = recode(app_raw, !!!APP_LABELS),
    condition2 = factor(
      condition2,
      levels = COND_LEVELS_ENG
    ),
    pct = case_when(
      "pct" %in% names(.) ~ as.numeric(pct),
      "prob" %in% names(.) ~ 100 * as.numeric(prob),
      TRUE ~ NA_real_
    ),
    pct_low = case_when(
      "pct_low" %in% names(.) ~ as.numeric(pct_low),
      "prob_low" %in% names(.) ~ 100 * as.numeric(prob_low),
      TRUE ~ NA_real_
    ),
    pct_high = case_when(
      "pct_high" %in% names(.) ~ as.numeric(pct_high),
      "prob_high" %in% names(.) ~ 100 * as.numeric(prob_high),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    app %in% APP_LEVELS_NICE_NO_FACEBOOK
  ) %>%
  mutate(
    app = factor(
      app,
      levels = rev(APP_LEVELS_NICE_NO_FACEBOOK)
    )
  ) %>%
  drop_na(
    app,
    condition2,
    pct,
    pct_low,
    pct_high
  )

# =============================================================================
# 5. AUDIT TABLES
# =============================================================================

figure4_audit_rr <- together_plot_apps %>%
  count(
    facet_new,
    app,
    condition2,
    name = "n_estimates"
  )

figure4_audit_dismissal <- regret_df_clean %>%
  count(
    app,
    condition2,
    name = "n_estimates"
  )

print(figure4_audit_rr, n = Inf)
print(figure4_audit_dismissal, n = Inf)

write_csv(
  figure4_audit_rr,
  "tables/app_specific/figure4_rate_ratio_audit.csv"
)

write_csv(
  figure4_audit_dismissal,
  "tables/app_specific/figure4_dismissal_audit.csv"
)

# =============================================================================
# 6. PANEL A: RATE RATIOS FOR TIME, OPENS, SESSION DURATION
# =============================================================================

p_rate_ratio <- together_plot_apps %>%
  ggplot(
    aes(
      y = facet_new,
      x = rr,
      color = condition2,
      fill = condition2
    )
  ) +
  geom_vline(
    xintercept = 1,
    linewidth = 0.6,
    color = "#000000"
  ) +
  geom_hline(
    yintercept = c(1.5, 2.5, 3.5),
    linewidth = 0.05,
    color = "#000000"
  ) +
  geom_errorbarh(
    aes(
      xmin = rr_low,
      xmax = rr_high
    ),
    height = 0.5,
    linewidth = 0.75,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 3,
    shape = 21,
    stroke = 0.5,
    position = position_dodge(width = 0.55)
  ) +
  facet_wrap(
    ~ app,
    nrow = 2,
    scales = "free_x"
  ) +
  scale_x_log10(
    limits = c(0.2, 4),
    breaks = c(0.2, 0.5, 1, 2, 4),
    labels = c("0.2", "0.5", "1", "2", "4"),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    values = COND_COLOURS,
    drop = FALSE
  ) +
  scale_fill_manual(
    values = COND_COLOURS,
    drop = FALSE
  ) +
  labs(
    y = "",
    x = "Rate Ratio",
    color = "",
    fill = ""
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    strip.text = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.text.x = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.text.y = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.title.x = element_text(
      size = 14,
      family = BODY_FONT,
      color = "#000000"
    ),
    legend.text = element_text(
      size = 14,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.line.x = element_line(
      color = "#000000"
    ),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing.x = unit(1.2, "cm"),
    panel.spacing.y = unit(1, "cm"),
    plot.margin = margin(
      t = 10,
      r = 50,
      b = 10,
      l = 0
    ),
    panel.grid.minor = element_blank(),
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
    )
  )

# =============================================================================
# 7. PANEL B: DISMISSAL PROBABILITY
# =============================================================================

dismissal_xlim <- max(
  40,
  ceiling(max(regret_df_clean$pct_high, na.rm = TRUE) / 10) * 10
)

p_dismissal <- regret_df_clean %>%
  ggplot(
    aes(
      y = app,
      x = pct,
      color = condition2,
      fill = condition2
    )
  ) +
  geom_hline(
    yintercept = c(1.5, 2.5, 3.5, 4.5),
    linewidth = 0.05,
    color = "#000000"
  ) +
  geom_errorbarh(
    aes(
      xmin = pct_low,
      xmax = pct_high
    ),
    height = 0.5,
    linewidth = 0.75,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 3,
    shape = 21,
    stroke = 0.5,
    position = position_dodge(width = 0.55)
  ) +
  scale_fill_manual(
    values = COND_COLOURS,
    drop = FALSE
  ) +
  scale_color_manual(
    values = COND_COLOURS,
    drop = FALSE
  ) +
  scale_x_continuous(
    limits = c(0, dismissal_xlim),
    breaks = seq(0, dismissal_xlim, by = 10),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0),
    position = "bottom"
  ) +
  labs(
    x = "Dismissal probability",
    y = "",
    color = "",
    fill = ""
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.text.y = element_text(
      size = 13,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.title.x = element_text(
      size = 14,
      family = BODY_FONT,
      color = "#000000"
    ),
    legend.text = element_text(
      size = 12,
      family = BODY_FONT,
      color = "#000000"
    ),
    axis.title = element_text(
      size = 12,
      family = BODY_FONT
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(
      color = "#000000"
    ),
    axis.ticks.y = element_blank(),
    plot.margin = margin(
      t = 10,
      r = 45,
      b = 10,
      l = 35
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

# =============================================================================
# 8. COMBINE FIGURE
# =============================================================================

figure4_app_specific_effects <- p_rate_ratio /
  p_dismissal +
  plot_layout(
    heights = c(2.1, 1)
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(
      t = 5,
      r = 10,
      b = 5,
      l = 5
    )
  )

figure4_app_specific_effects

# =============================================================================
# 9. SAVE FIGURE
# =============================================================================

ggsave(
  filename = "figures/app_specific/figure4_app_specific_effects.png",
  plot = figure4_app_specific_effects,
  width = 12.5,
  height = 10.5,
  dpi = 300,
  units = "in",
  bg = "white"
)

ggsave(
  filename = "figures/app_specific/figure4_app_specific_effects.svg",
  plot = figure4_app_specific_effects,
  width = 12.5,
  height = 10.5,
  units = "in",
  bg = "white"
)

ggsave(
  filename = "figures/app_specific/figure4_app_specific_effects.pdf",
  plot = figure4_app_specific_effects,
  width = 12.5,
  height = 10.5,
  units = "in",
  bg = "white"
)

message("Figure 4 saved to figures/app_specific/")





# Optional companion plot for the difference-in-differences estimates, only if
# you have created `app_changes_opens_interpretable.csv` from the earlier
# interpretability extraction.

if (file.exists("tables/app_specific/app_changes_opens_interpretable.csv")) {
  
  app_changes_opens_interpretable <- read_csv(
    "tables/app_specific/app_changes_opens_interpretable.csv",
    show_col_types = FALSE
  ) %>%
    mutate(
      app = recode(as.character(app), !!!APP_LABELS),
      app = factor(app, levels = APP_LEVELS_NICE_NO_FACEBOOK),
      contrast = factor(
        contrast,
        levels = c(
          "Planning vs Reflection",
          "Waiting vs Reflection"
        )
      )
    ) %>%
    filter(
      app %in% APP_LEVELS_NICE_NO_FACEBOOK,
      estimand == "difference_in_differences"
    )
  
  p_opens_did <- app_changes_opens_interpretable %>%
    ggplot(
      aes(
        x = rr,
        y = app,
        xmin = rr_low,
        xmax = rr_high,
        color = contrast,
        fill = contrast
      )
    ) +
    geom_vline(
      xintercept = 1,
      linewidth = 0.6,
      color = "#000000"
    ) +
    geom_errorbarh(
      height = 0.5,
      linewidth = 0.75,
      position = position_dodge(width = 0.55)
    ) +
    geom_point(
      size = 3,
      shape = 21,
      stroke = 0.5,
      position = position_dodge(width = 0.55)
    ) +
    scale_x_log10(
      limits = c(0.2, 2),
      breaks = c(0.2, 0.5, 1, 2),
      labels = c("0.2", "0.5", "1", "2"),
      expand = c(0, 0)
    ) +
    scale_color_manual(
      values = c(
        "Planning vs Reflection" = "#9e0b1d",
        "Waiting vs Reflection" = "#8dadbf"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Planning vs Reflection" = "#9e0b1d",
        "Waiting vs Reflection" = "#8dadbf"
      )
    ) +
    labs(
      x = "Additional change relative to Reflection (Rate Ratio)",
      y = "",
      color = "",
      fill = ""
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(
        size = 13,
        family = BODY_FONT,
        color = "#000000"
      ),
      axis.text.y = element_text(
        size = 13,
        family = BODY_FONT,
        color = "#000000"
      ),
      axis.title.x = element_text(
        size = 14,
        family = BODY_FONT,
        color = "#000000"
      ),
      legend.text = element_text(
        size = 13,
        family = BODY_FONT,
        color = "#000000"
      ),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.background = element_rect(
        fill = "white",
        color = NA
      )
    )
  
  ggsave(
    filename = "figures/app_specific/figure4_supplement_opens_did.png",
    plot = p_opens_did,
    width = 7.5,
    height = 4.5,
    dpi = 300,
    units = "in",
    bg = "white"
  )
  
  ggsave(
    filename = "figures/app_specific/figure4_supplement_opens_did.svg",
    plot = p_opens_did,
    width = 7.5,
    height = 4.5,
    units = "in",
    bg = "white"
  )
}