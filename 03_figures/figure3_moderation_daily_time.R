# Using the uploaded visualization script as the visual template:
# :contentReference[oaicite:0]{index=0}

Sys.setenv(TZ = "Europe/Copenhagen")

# =============================================================================
# 03_figures/figure2_moderation_daily_time.R
#
# PURPOSE:
#   Assemble Figure 2 moderation visualization for daily time spent.
#
# VISUAL STYLE:
#   This keeps the original working visualization structure:
#     - separate p_effect_time / p_effect_addiction / p_effect_self_control objects
#     - x-axis limits c(0.2, 4)
#     - x-axis breaks c(0.2, 0.5, 1, 2, 4)
#     - legend only in the first panel
#     - same facet_wrap structure
#     - same margins, fonts, colors, point sizes, line widths
#
# INPUTS:
#   tables/moderation/time_mod_baseline_time_condition_effects.csv
#   tables/moderation/time_mod_addiction_condition_effects.csv
#   tables/moderation/time_mod_selfcontrol_condition_effects.csv
#
# OUTPUTS:
#   tables/main/vis_together_moderation.csv
#   figures/main/figure2_moderation.{png,svg}
# =============================================================================

source("02_analysis/shared_utils.R")

make_output_dirs("figures/main", "tables/main")

# =============================================================================
# 1. LOAD DELTA-METHOD MODERATION EFFECTS
# =============================================================================

baseline_time_effects <- read_csv(
  "tables/moderation/time_mod_baseline_time_condition_effects.csv",
  show_col_types = FALSE
) %>%
  mutate(
    facet_2 = "Daily activity (baseline)"
  )

addiction_effects <- read_csv(
  "tables/moderation/time_mod_addiction_condition_effects.csv",
  show_col_types = FALSE
) %>%
  mutate(
    facet_2 = "Social media addiction"
  )

selfcontrol_effects <- read_csv(
  "tables/moderation/time_mod_selfcontrol_condition_effects.csv",
  show_col_types = FALSE
) %>%
  mutate(
    facet_2 = "Self-control"
  )

together_for_plot <- bind_rows(
  baseline_time_effects,
  addiction_effects,
  selfcontrol_effects
) %>%
  mutate(
    condition2 = case_when(
      as.character(condition2) %in% c("Reflection", "Planning", "Waiting") ~ as.character(condition2),
      condition == "control" ~ "Reflection",
      condition == "intervention" ~ "Planning",
      condition == "default" ~ "Waiting",
      TRUE ~ as.character(condition2)
    ),
    condition2 = factor(
      condition2,
      levels = c("Reflection", "Planning", "Waiting")
    )
  ) %>%
  dplyr::select(
    condition2,
    condition,
    mod_level,
    mod_s,
    facet_2,
    rr,
    rr_low,
    rr_high,
    estimate_log,
    se_log,
    conf.low_log,
    conf.high_log,
    pct,
    pct_low,
    pct_high,
    z,
    p_value
  )

write_csv(
  together_for_plot,
  "tables/main/vis_together_moderation.csv"
)

save_model_table(
  together_for_plot,
  "vis_together_moderation",
  "tables/main",
  caption = "Moderation effect estimates used in Figure 2",
  label = "tab:vis_together_moderation"
)

# =============================================================================
# 2. VALIDATE FIGURE INPUT
# =============================================================================

validate_figure_mod_table <- function(df) {
  df %>%
    mutate(
      ci_excludes_1 = rr_low > 1 | rr_high < 1,
      p_sig = p_value < 0.05,
      mismatch = ci_excludes_1 != p_sig,
      bad_bounds = rr_low > rr | rr > rr_high,
      bad_log_bounds = conf.low_log > estimate_log | estimate_log > conf.high_log
    ) %>%
    filter(
      mismatch | bad_bounds | bad_log_bounds
    )
}

validation_time <- validate_figure_mod_table(together_for_plot)

print(validation_time, n = Inf)

write_csv(
  validation_time,
  "tables/main/vis_together_moderation_validation_flags.csv"
)

# =========================
# 3. TIME VERSION (Light/Average/Heavy Users)
# =========================

df_time <- together_for_plot %>%
  filter(str_detect(facet_2, regex("time|user|baseline|activity", ignore_case = TRUE))) %>%
  mutate(
    condition2 = factor(condition2, levels = c("Reflection", "Planning", "Waiting")),
    mod_level2 = case_when(
      mod_s == -1 ~ "Light",
      mod_s ==  0 ~ "Average",
      mod_s ==  1 ~ "Heavy",
      TRUE ~ as.character(mod_level)
    ),
    mod_level2 = factor(mod_level2, levels = c("Light", "Average", "Heavy"))
  )

p_effect_time <- ggplot(df_time, aes(x = rr, y = mod_level2, color = condition2)) +
  geom_errorbarh(
    aes(xmin = rr_low, xmax = rr_high),
    height = 0.3,
    linewidth = 0.5,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(size = 3.6, position = position_dodge(width = 0.55)) +
  facet_wrap(~facet_2, ncol = 1) +
  scale_color_manual(
    values = c(
      "Reflection" = "#737373",
      "Planning" = "#9e0b1d",
      "Waiting" = "#8dadbf"
    )
  ) +
  labs(y = "", x = "Rate Ratio", color = "") +
  scale_x_log10(
    limits = c(0.2, 4),
    breaks = c(0.2, 0.5, 1, 2, 4),
    expand = c(0, 0)
  ) +
  geom_vline(xintercept = 1, linewidth = 0.6) +
  geom_hline(
    yintercept = c(1.5, 2.5, 3.5, 4.5),
    linewidth = 0.05
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13, family = "FrutigerNextPro", color = "#000000"),
    strip.text = element_text(
      size = 13,
      family = "FrutigerNextProBold",
      color = "#000000",
      margin = margin(b = 10),
      hjust = 0.52
    ),
    axis.text.y = element_text(size = 13, family = "FrutigerNextPro", color = "#000000"),
    axis.text.x = element_text(size = 13, family = "FrutigerNextPro", color = "#000000"),
    axis.title.x = element_text(size = 14, family = "FrutigerNextPro", color = "#000000"),
    axis.title = element_text(size = 12, family = "FrutigerNextPro"),
    axis.line.x = element_line(color = "#000000", linetype = "solid", linewidth = 0.3),
    axis.line.y = element_blank(),
    plot.margin = margin(r = 40, l = 10, t = 10),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

p_effect_time

# =========================
# 4. ADDICTION VERSION (Low/Average/High Addiction)
# =========================

df_add <- together_for_plot %>%
  filter(str_detect(facet_2, regex("addict", ignore_case = TRUE))) %>%
  mutate(
    condition2 = factor(condition2, levels = c("Reflection", "Planning", "Waiting")),
    mod_level2 = case_when(
      mod_s == -1 ~ "Low",
      mod_s ==  0 ~ "Average",
      mod_s ==  1 ~ "High",
      TRUE ~ as.character(mod_level)
    ),
    mod_level2 = factor(mod_level2, levels = c("Low", "Average", "High"))
  )

p_effect_addiction <- ggplot(df_add, aes(x = rr, y = mod_level2, color = condition2)) +
  geom_errorbarh(
    aes(xmin = rr_low, xmax = rr_high),
    height = 0.3,
    linewidth = 0.5,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(size = 3.6, position = position_dodge(width = 0.55)) +
  facet_wrap(~facet_2, ncol = 1) +
  scale_color_manual(
    values = c(
      "Reflection" = "#737373",
      "Planning" = "#9e0b1d",
      "Waiting" = "#8dadbf"
    )
  ) +
  labs(y = "", x = "Rate Ratio", color = "") +
  scale_x_log10(
    limits = c(0.2, 4),
    breaks = c(0.2, 0.5, 1, 2, 4),
    expand = c(0, 0)
  ) +
  geom_vline(xintercept = 1, linewidth = 0.6) +
  geom_hline(
    yintercept = c(1.5, 2.5, 3.5, 4.5),
    linewidth = 0.05
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 13, family = "FrutigerNextPro", color = "#000000"),
    strip.text = element_text(
      size = 13,
      family = "FrutigerNextProBold",
      color = "#000000",
      margin = margin(b = 10),
      hjust = 0.52
    ),
    axis.text.y = element_text(size = 13, family = "FrutigerNextPro", color = "#000000"),
    axis.text.x = element_text(size = 13, family = "FrutigerNextPro", color = "#000000"),
    axis.title.x = element_text(size = 14, family = "FrutigerNextPro", color = "#000000"),
    axis.title = element_text(size = 12, family = "FrutigerNextPro"),
    axis.line.x = element_line(color = "#000000", linetype = "solid", linewidth = 0.3),
    axis.line.y = element_blank(),
    plot.margin = margin(r = -10, l = 10, t = 10),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

p_effect_addiction

# =========================
# 5. SELF-CONTROL VERSION
# =========================

df_sc <- together_for_plot %>%
  filter(str_detect(facet_2, regex("self", ignore_case = TRUE))) %>%
  mutate(
    condition2 = factor(condition2, levels = c("Reflection", "Planning", "Waiting")),
    mod_level2 = case_when(
      mod_s ==  1 ~ "High",
      mod_s ==  0 ~ "Average",
      mod_s == -1 ~ "Low",
      TRUE ~ as.character(mod_level)
    ),
    mod_level2 = factor(mod_level2, levels = c("High", "Average", "Low"))
  )

p_effect_self_control <- ggplot(df_sc, aes(x = rr, y = mod_level2, color = condition2)) +
  geom_errorbarh(
    aes(xmin = rr_low, xmax = rr_high),
    height = 0.3,
    linewidth = 0.5,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(size = 3.6, position = position_dodge(width = 0.55)) +
  facet_wrap(~facet_2, ncol = 1) +
  scale_color_manual(
    values = c(
      "Reflection" = "#737373",
      "Planning" = "#9e0b1d",
      "Waiting" = "#8dadbf"
    )
  ) +
  labs(y = "", x = "Rate Ratio", color = "") +
  scale_x_log10(
    limits = c(0.2, 4),
    breaks = c(0.2, 0.5, 1, 2, 4),
    expand = c(0, 0)
  ) +
  geom_vline(xintercept = 1, linewidth = 0.6) +
  geom_hline(
    yintercept = c(1.5, 2.5, 3.5, 4.5),
    linewidth = 0.05
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 13, family = "FrutigerNextPro", color = "#000000"),
    strip.text = element_text(
      size = 13,
      family = "FrutigerNextProBold",
      color = "#000000",
      margin = margin(b = 10),
      hjust = 0.52
    ),
    axis.text.y = element_text(size = 13, family = "FrutigerNextPro", color = "#000000"),
    axis.text.x = element_text(size = 13, family = "FrutigerNextPro", color = "#000000"),
    axis.title.x = element_text(size = 14, family = "FrutigerNextPro", color = "#000000"),
    axis.title = element_text(size = 12, family = "FrutigerNextPro"),
    axis.line.x = element_line(color = "#000000", linetype = "solid", linewidth = 0.3),
    axis.line.y = element_blank(),
    plot.margin = margin(r = 40, l = -10, t = 10),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

p_effect_self_control

# =========================
# 6. COMBINE AND SAVE
# =========================

p_bottom <- (p_effect_addiction | p_effect_self_control) +
  plot_layout(
    widths = c(1, 1)
  )

p_ABC <- p_effect_time /
  p_bottom +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A") +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "pt")
  )

p_ABC

ggsave(
  "figures/main/figure2_moderation.png",
  p_ABC,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/main/figure2_moderation.svg",
  p_ABC,
  width = 12,
  height = 8,
  bg = "white"
)

save_figure(
  p_effect_time,
  "figure2_panel_A_baseline_use",
  "figures/main"
)

save_figure(
  p_effect_addiction,
  "figure2_panel_B_addiction",
  "figures/main"
)

save_figure(
  p_effect_self_control,
  "figure2_panel_C_selfcontrol",
  "figures/main"
)

message("\n=== figure2_moderation_daily_time.R complete ===")