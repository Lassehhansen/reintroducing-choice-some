Sys.setenv(TZ = "Europe/Copenhagen")

# =============================================================================
# 04_supplementary/appendix_D_opens_session_trajectories/D_opens_session_its.R
#
# PURPOSE:
#   Produce appendix trajectory figures for:
#     1. Daily opens
#     2. Average session duration
#
#   These are styled to match Figure 1 Panel A from the main figure pipeline,
#   but each figure contains only the trajectory panel.
#
# INPUTS:
#   models/main/glm_opens_model.rds
#   models/main/glm_session_duration_model.rds
#
# OUTPUTS:
#   figures/supplementary/appendix_D5_opens_trajectory.{png,svg}
#   figures/supplementary/appendix_D6_session_trajectory.{png,svg}
#
# Adapted from uploaded appendix trajectory script. :contentReference[oaicite:0]{index=0}
# =============================================================================

source("02_analysis/shared_utils.R")

library(tidyverse)
library(lubridate)
library(ggeffects)
library(patchwork)

make_output_dirs(
  "figures/supplementary",
  "tables/supplementary"
)

# =============================================================================
# 1. SHARED HELPERS
# =============================================================================

standardise_prediction_df <- function(pred_obj) {
  as.data.frame(pred_obj) %>%
    as_tibble() %>%
    filter(
      (x <= 10 & group == "baseline") |
        (x > 10 & group == "intervention")
    ) %>%
    mutate(
      facet = factor(
        facet,
        levels = c("control", "intervention", "default")
      )
    )
}

make_trajectory_panel <- function(
    model,
    y_axis_labels,
    y_axis_breaks,
    y_axis_limits,
    y_axis_title = "",
    intervention_label_y,
    label_vjust = c(
      control = 0,
      intervention = -0.2,
      default = 0.35
    ),
    output_prediction_csv = NULL
) {
  
  pred <- predict_response(
    model,
    terms = c("time3[all]", "period2", "condition"),
    margin = "empirical",
    type = "response",
    ci_level = 0.95,
    interval = "prediction"
  )
  
  pred_df <- standardise_prediction_df(pred)
  
  if (!is.null(output_prediction_csv)) {
    write_csv(
      pred_df,
      output_prediction_csv
    )
  }
  
  ggplot(
    pred_df,
    aes(
      x = x,
      y = predicted,
      group = factor(facet)
    )
  ) +
    geom_rect(
      aes(
        xmin = 10,
        xmax = 11,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "#DADADA",
      alpha = 0.05
    ) +
    geom_segment(
      aes(
        x = 10,
        xend = 10,
        y = -Inf,
        yend = Inf
      ),
      linetype = "dashed",
      color = "#000000",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    geom_segment(
      aes(
        x = 11,
        xend = 11,
        y = -Inf,
        yend = Inf
      ),
      linetype = "dashed",
      color = "#000000",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    geom_point(
      aes(
        color = factor(facet)
      ),
      size = 1.5
    ) +
    geom_line(
      aes(
        color = factor(facet)
      ),
      linewidth = 0.8
    ) +
    geom_ribbon(
      aes(
        ymin = conf.low,
        ymax = conf.high,
        fill = factor(facet)
      ),
      alpha = 0.1
    ) +
    geom_text(
      data = pred_df %>%
        filter(
          x == max(x),
          facet == "control"
        ),
      aes(
        label = "Reflection",
        color = factor(facet)
      ),
      hjust = -0.2,
      vjust = label_vjust[["control"]],
      size = 4,
      show.legend = FALSE,
      family = BOLD_FONT,
      fontface = "bold"
    ) +
    geom_text(
      data = pred_df %>%
        filter(
          x == max(x),
          facet == "default"
        ),
      aes(
        label = "Waiting",
        color = factor(facet)
      ),
      hjust = -0.3,
      vjust = label_vjust[["default"]],
      size = 4,
      show.legend = FALSE,
      family = BOLD_FONT,
      fontface = "bold"
    ) +
    geom_text(
      data = pred_df %>%
        filter(
          x == max(x),
          facet == "intervention"
        ),
      aes(
        label = "Planning",
        color = factor(facet)
      ),
      hjust = -0.25,
      vjust = label_vjust[["intervention"]],
      size = 4,
      show.legend = FALSE,
      family = BOLD_FONT,
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = 10.5,
      y = intervention_label_y,
      label = "Interventions Start",
      color = "#000000",
      size = 4.4,
      family = BOLD_FONT,
      fontface = "bold"
    ) +
    scale_x_continuous(
      breaks = c(1, 11, 21, 31),
      labels = function(x) paste0("Day ", x + 4),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = y_axis_limits,
      breaks = y_axis_breaks,
      labels = y_axis_labels
    ) +
    labs(
      x = "",
      y = y_axis_title
    ) +
    scale_color_manual(
      values = c(
        "control" = "#737373",
        "intervention" = "#9e0b1d",
        "default" = "#8dadbf"
      ),
      breaks = c(
        "control",
        "intervention",
        "default"
      ),
      labels = c(
        "Reflection",
        "Planning",
        "Waiting"
      ),
      name = NULL
    ) +
    scale_shape_manual(
      values = c(
        "control" = 16,
        "intervention" = 16,
        "default" = 16
      ),
      breaks = c(
        "control",
        "intervention",
        "default"
      ),
      labels = c(
        "Reflection",
        "Planning",
        "Waiting"
      ),
      name = NULL
    ) +
    scale_fill_manual(
      values = c(
        "control" = "#737373",
        "intervention" = "#9e0b1d",
        "default" = "#8dadbf"
      ),
      breaks = c(
        "control",
        "intervention",
        "default"
      ),
      labels = c(
        "Reflection",
        "Planning",
        "Waiting"
      ),
      name = NULL
    ) +
    guides(
      color = guide_legend(
        override.aes = list(
          shape = 16,
          fill = NA
        )
      ),
      shape = "none",
      fill = "none"
    ) +
    coord_cartesian(
      xlim = c(0, 36),
      clip = "off"
    ) +
    custom_general_theme_word() +
    theme(
      plot.margin = margin(
        t = 10,
        r = 80,
        b = 0,
        l = -10
      ),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line(
        color = "#000000"
      ),
      axis.ticks.y = element_line(
        color = "#000000"
      ),
      axis.line.x = element_line(
        color = "#000000"
      ),
      axis.text.y = element_text(
        size = 13,
        family = BODY_FONT,
        color = "#000000"
      ),
      axis.text.x = element_text(
        size = 12,
        family = BODY_FONT,
        color = "#000000",
        margin = margin(
          t = 5,
          r = 0,
          b = -10,
          l = 0
        )
      ),
      axis.title.y = element_text(
        size = 13,
        family = BODY_FONT,
        color = "#000000",
        margin = margin(r = 8)
      ),
      panel.grid.major.y = element_line(
        color = "#000000",
        linewidth = 0.05
      ),
      legend.position = "bottom",
      legend.text = element_text(
        size = 14,
        family = BODY_FONT,
        color = "#000000"
      ),
      legend.box.margin = margin(
        t = -5,
        b = 15
      )
    )
}

# =============================================================================
# 2. LOAD MODELS
# =============================================================================

glm_opens_model <- readRDS(
  "models/main/glm_opens_model.rds"
)

glm_session_model <- readRDS(
  "models/main/glm_session_duration_model.rds"
)

# =============================================================================
# 3. DAILY OPENS TRAJECTORY
# =============================================================================

fig_D5_opens <- make_trajectory_panel(
  model = glm_opens_model,
  y_axis_limits = c(0, 95),
  y_axis_breaks = seq(0, 90, by = 30),
  y_axis_labels = function(x) paste0(x, " opens"),
  y_axis_title = "",
  intervention_label_y = 97,
  label_vjust = c(
    control = -0.15,
    intervention = -0.15,
    default = 0.35
  ),
  output_prediction_csv = "tables/supplementary/appendix_D5_opens_trajectory_predictions.csv"
)

fig_D5_opens

save_figure(
  fig_D5_opens,
  "appendix_D5_opens_trajectory",
  "figures/supplementary",
  width = 1200 / 96,
  height = 500 / 96
)

ggsave(
  "figures/supplementary/appendix_D5_opens_trajectory.png",
  fig_D5_opens,
  width = 1200 / 96,
  height = 500 / 96,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/supplementary/appendix_D5_opens_trajectory.svg",
  fig_D5_opens,
  width = 1200 / 96,
  height = 500 / 96,
  bg = "white"
)

# =============================================================================
# 4. AVERAGE SESSION DURATION TRAJECTORY
# =============================================================================

fig_D6_session <- make_trajectory_panel(
  model = glm_session_model,
  y_axis_limits = c(0, 12.5),
  y_axis_breaks = seq(0, 12, by = 3),
  y_axis_labels = function(x) paste0(x, " mins"),
  y_axis_title = "",
  intervention_label_y = 12.8,
  label_vjust = c(
    control = 0.15,
    intervention = -0.15,
    default = 0.35
  ),
  output_prediction_csv = "tables/supplementary/appendix_D6_session_trajectory_predictions.csv"
)

fig_D6_session

save_figure(
  fig_D6_session,
  "appendix_D6_session_trajectory",
  "figures/supplementary",
  width = 1200 / 96,
  height = 500 / 96
)

ggsave(
  "figures/supplementary/appendix_D6_session_trajectory.png",
  fig_D6_session,
  width = 1200 / 96,
  height = 500 / 96,
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/supplementary/appendix_D6_session_trajectory.svg",
  fig_D6_session,
  width = 1200 / 96,
  height = 500 / 96,
  bg = "white"
)

message("\n=== appendix_D_opens_session_trajectories.R complete ===")