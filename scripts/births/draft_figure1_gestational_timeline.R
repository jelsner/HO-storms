#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# Draft Figure 1 for the revised manuscript.
#
# The dates below are illustrative rather than drawn from a specific birth.
# They show how the counting-process analysis assigns risk time and changes
# exposure status at a residence-specific tropical-cyclone impact date.

output_dir <- file.path("figs", "births", "revised-manuscript")
output_png <- file.path(output_dir, "figure-1-gestational-risk-timeline.png")
output_pdf <- file.path(output_dir, "figure-1-gestational-risk-timeline.pdf")

days_per_week <- 7
entry_day <- 20 * days_per_week
impact_day <- 30 * days_per_week
vptb_cutoff_day <- 32 * days_per_week
ptb_cutoff_day <- 37 * days_per_week

# These endpoints reproduce the current time-to-event code. Because the
# intervals are represented as [start, end), days 0-3 end at impact + 4 and
# days 0-7 end at impact + 8.
short_window_end <- impact_day + 4
long_window_end <- impact_day + 8

risk_periods <- tibble(
  row_label = c("PTB risk period", "vPTB risk period"),
  y = c(6, 5),
  start_day = entry_day,
  end_day = c(ptb_cutoff_day, vptb_cutoff_day),
  interval_type = "At-risk period"
)

exposure_definitions <- tibble(
  row_label = c(
    "Persistent after impact",
    "Acute days 0-7",
    "Acute days 0-3"
  ),
  y = c(3, 2, 1),
  exposed_end_day = c(
    ptb_cutoff_day,
    long_window_end,
    short_window_end
  )
)

# Each exposure definition contributes unexposed person-time before impact,
# exposed person-time during its active window, and (for acute definitions)
# unexposed person-time after the window closes.
pre_impact_intervals <- exposure_definitions %>%
  transmute(
    row_label,
    y,
    start_day = entry_day,
    end_day = impact_day,
    interval_type = "Unexposed person-time"
  )

active_exposure_intervals <- exposure_definitions %>%
  transmute(
    row_label,
    y,
    start_day = impact_day,
    end_day = exposed_end_day,
    interval_type = "Exposed person-time"
  )

post_window_intervals <- exposure_definitions %>%
  transmute(
    row_label,
    y,
    start_day = exposed_end_day,
    end_day = ptb_cutoff_day,
    interval_type = "Unexposed person-time"
  ) %>%
  filter(end_day > start_day)

timeline_intervals <- bind_rows(
  risk_periods,
  pre_impact_intervals,
  active_exposure_intervals,
  post_window_intervals
) %>%
  mutate(
    interval_type = factor(
      interval_type,
      levels = c(
        "At-risk period",
        "Unexposed person-time",
        "Exposed person-time"
      )
    )
  )

row_lookup <- bind_rows(
  risk_periods %>% select(row_label, y),
  exposure_definitions %>% select(row_label, y)
) %>%
  distinct() %>%
  arrange(y)

milestones <- tibble(
  plot_week = c(20.15, 30.15, 32.15, 37.15),
  label_y = c(6.55, 6.68, 6.55, 6.55),
  label = c(
    "Follow-up begins\n20 weeks",
    "Storm impact\n30 weeks",
    "vPTB cutoff\n32 weeks",
    "PTB cutoff\n37 weeks"
  )
)

colors <- c(
  "At-risk period" = "#4B5563",
  "Unexposed person-time" = "#D1D5DB",
  "Exposed person-time" = "#2878B5"
)

p <- ggplot(timeline_intervals) +
  geom_segment(
    aes(
      x = start_day / days_per_week,
      xend = end_day / days_per_week,
      y = y,
      yend = y,
      color = interval_type
    ),
    linewidth = 7.5,
    lineend = "butt"
  ) +
  geom_vline(
    xintercept = entry_day / days_per_week,
    color = "#6B7280",
    linetype = "dashed",
    linewidth = 0.65
  ) +
  geom_vline(
    xintercept = impact_day / days_per_week,
    color = "#C44E52",
    linewidth = 0.9
  ) +
  geom_vline(
    xintercept = c(vptb_cutoff_day, ptb_cutoff_day) / days_per_week,
    color = "#6B7280",
    linetype = "dotted",
    linewidth = 0.65
  ) +
  geom_point(
    data = exposure_definitions,
    aes(x = impact_day / days_per_week, y = y),
    inherit.aes = FALSE,
    shape = 21,
    size = 3.2,
    stroke = 0.9,
    color = "#C44E52",
    fill = "white"
  ) +
  annotate(
    "segment",
    x = 19.2,
    xend = 37.2,
    y = 4,
    yend = 4,
    color = "#E5E7EB",
    linewidth = 0.7
  ) +
  geom_text(
    data = milestones,
    aes(x = plot_week, y = label_y, label = label),
    inherit.aes = FALSE,
    color = "#374151",
    size = 3.2,
    lineheight = 0.95,
    hjust = 0,
    vjust = 0
  ) +
  annotate(
    "text",
    x = impact_day / days_per_week + 0.25,
    y = 3.55,
    label = "Exposure switches on at local impact",
    hjust = 0,
    color = "#C44E52",
    size = 3.3,
    fontface = "bold"
  ) +
  scale_color_manual(values = colors, drop = FALSE) +
  scale_x_continuous(
    breaks = c(20, 25, 30, 32, 37),
    limits = c(18.5, 40),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = row_lookup$y,
    labels = row_lookup$row_label,
    limits = c(0.55, 7.25),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 5),
      title.position = "top"
    )
  ) +
  labs(
    title = "Gestational risk sets and time-varying storm exposure",
    subtitle = paste(
      "Illustrative pregnancy with residence-specific local impact at",
      "30 weeks of gestation"
    ),
    x = "Gestational age (weeks)",
    y = NULL,
    color = NULL,
    caption = paste0(
      "Pregnancies contribute unexposed person-time until local storm impact. ",
      "Storms on or after delivery do not assign exposure.\n",
      "Delivery before an outcome cutoff is an event; an ongoing pregnancy is ",
      "censored at the cutoff.\n",
      "The displayed acute intervals follow the current code: days 0-3 and ",
      "days 0-7 after impact, inclusive."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold", size = 17, color = "#111827"
    ),
    plot.subtitle = element_text(
      size = 11.5, color = "#374151", margin = margin(b = 20)
    ),
    plot.caption = element_text(
      hjust = 0, size = 9.2, color = "#4B5563",
      lineheight = 1.05, margin = margin(t = 14)
    ),
    axis.text.x = element_text(color = "#374151"),
    axis.text.y = element_text(color = "#111827", size = 10.5),
    axis.title.x = element_text(margin = margin(t = 9)),
    panel.grid.major.x = element_line(color = "#EEF0F2", linewidth = 0.45),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin = margin(t = 8),
    plot.margin = margin(14, 22, 12, 14)
  )

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  output_png,
  p,
  width = 11.5,
  height = 7.2,
  dpi = 300,
  bg = "white"
)

ggsave(
  output_pdf,
  p,
  width = 11.5,
  height = 7.2,
  device = grDevices::pdf,
  bg = "white",
  useDingbats = FALSE
)

message("Wrote: ", output_png)
message("Wrote: ", output_pdf)
