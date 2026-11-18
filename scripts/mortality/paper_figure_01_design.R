#!/usr/bin/env Rscript

source("scripts/mortality/paper_figure_helpers.R")
suppressPackageStartupMessages({
  library(patchwork)
  library(sf)
})

message("Building Figure 1: study design and event-relative periods")

geometry_path <- "data/mortality/derived/florida_panel_zcta_geometry_2020.rds"
if (!file.exists(geometry_path)) stop("Missing ZIP geometry: ", geometry_path)
zcta <- readRDS(geometry_path)
if (!inherits(zcta, "sf") || nrow(zcta) != 1046L) {
  stop("Expected an sf object with 1,046 Florida ZIP polygons.")
}
florida_outline <- st_sf(geometry = st_union(st_geometry(zcta)))

storm_path <- data.table(
  longitude = c(-85.15, -84.45, -83.45, -82.35),
  latitude = c(27.2, 28.0, 28.9, 29.8)
)

map_plot <- ggplot() +
  geom_sf(data = zcta, fill = "#E9F2F5", color = "white", linewidth = 0.05) +
  geom_sf(data = florida_outline, fill = NA, color = "#29566A", linewidth = 0.5) +
  geom_path(
    data = storm_path,
    aes(longitude, latitude),
    color = "#C44E52",
    linewidth = 0.8,
    linetype = "22",
    arrow = arrow(length = unit(2.5, "mm"), type = "closed")
  ) +
  geom_point(
    data = storm_path[2:3],
    aes(longitude, latitude),
    shape = 21,
    size = 2.4,
    stroke = 0.7,
    fill = "white",
    color = "#C44E52"
  ) +
  annotate(
    "label",
    x = -84.7,
    y = 30.45,
    label = "1,046 ZIPs\n1985-2022",
    family = "Helvetica",
    fontface = "bold",
    size = 3.5,
    fill = "white",
    color = "#173B4D",
    linewidth = 0.25
  ) +
  annotate(
    "text",
    x = -83.65,
    y = 26.0,
    label = "ZIP-specific day 0:\nfirst assigned date with\nmodeled winds >=34 kt",
    hjust = 0.5,
    family = "Helvetica",
    size = 3.0,
    color = "#6F3032"
  ) +
  coord_sf(xlim = c(-85.3, -80.0), ylim = c(24.3, 31.0), datum = NA) +
  labs(title = "A. Florida ZIP-day setting", subtitle = "Storm path is schematic") +
  theme_void(base_family = "Helvetica", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 8.5, color = "grey35")
  )

timeline <- data.table(
  xmin = c(-18, -14, -7, 0, 7, 14),
  xmax = c(-15, -8, -1, 6, 13, 17),
  ymin = 0,
  ymax = 1,
  period = c(
    "Reference days",
    "Early threat",
    "Imminent threat /\npreparation",
    "Impact and\nimmediate aftermath",
    "Early recovery",
    "Reference days"
  ),
  days = c("", "-14 to -8", "-7 to -1", "0 to +6", "+7 to +13", ""),
  fill = c("#D9D9D9", "#B3CDE3", "#80B1D3", "#FDB462", "#FB8072", "#D9D9D9")
)

timeline_plot <- ggplot(timeline) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
    color = "white",
    linewidth = 0.8
  ) +
  geom_text(
    data = timeline[days != ""],
    aes(x = (xmin + xmax) / 2, y = 0.66, label = period),
    family = "Helvetica",
    fontface = "bold",
    size = 3.0,
    lineheight = 0.95
  ) +
  geom_text(
    data = timeline[days != ""],
    aes(x = (xmin + xmax) / 2, y = 0.26, label = days),
    family = "Helvetica",
    size = 3.0
  ) +
  geom_text(
    data = timeline[days == ""],
    aes(x = (xmin + xmax) / 2, y = 0.5, label = period),
    family = "Helvetica",
    size = 2.7,
    angle = 90,
    color = "grey25"
  ) +
  geom_vline(xintercept = 0, linewidth = 0.75, color = "#8B3A3A") +
  annotate(
    "label",
    x = 0,
    y = 1.13,
    label = "ZIP-specific impact day",
    family = "Helvetica",
    fontface = "bold",
    size = 3.0,
    fill = "white",
    color = "#8B3A3A",
    linewidth = 0.2
  ) +
  scale_fill_identity() +
  scale_x_continuous(breaks = c(-14, -7, 0, 7, 14), labels = c("-14", "-7", "0", "+7", "+14")) +
  coord_cartesian(ylim = c(-0.05, 1.25), clip = "off") +
  labs(
    title = "B. Event-relative periods preserve ZIP-specific timing",
    subtitle = "Each colored period contains seven days; all retained non-event days form the reference",
    x = "Day relative to ZIP-specific impact",
    y = NULL
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 8.5, color = "grey35", margin = margin(b = 8)),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 8)
  )

workflow <- data.table(
  x = 1:5,
  label = c(
    "Daily ZIP-level\nwind and mortality",
    "Assign closest\nimpact date",
    "Create four\n7-day periods",
    "Fixed-effects\nPoisson models",
    "Timing, cause/age,\nand spatial results"
  ),
  fill = c("#E8F1F5", "#E8F1F5", "#E8F1F5", "#FFF1D6", "#F5E6F2")
)

workflow_plot <- ggplot(workflow) +
  geom_segment(
    data = data.table(x = 1:4, xend = 2:5),
    aes(x = x + 0.47, xend = xend - 0.47, y = 0.68, yend = 0.68),
    linewidth = 0.7,
    color = "grey35",
    arrow = arrow(length = unit(2.5, "mm"), type = "closed")
  ) +
  geom_rect(
    aes(xmin = x - 0.45, xmax = x + 0.45, ymin = 0.28, ymax = 1.08, fill = fill),
    color = "#4D4D4D",
    linewidth = 0.45
  ) +
  geom_text(
    aes(x = x, y = 0.68, label = label),
    family = "Helvetica",
    fontface = "bold",
    size = 3.3,
    lineheight = 1.0
  ) +
  annotate(
    "rect",
    xmin = 0.55,
    xmax = 5.45,
    ymin = -0.42,
    ymax = 0.04,
    fill = "grey96",
    color = "grey80"
  ) +
  annotate(
    "text",
    x = 3,
    y = -0.19,
    label = paste0(
      "Outcomes: all diseases + 10 cause categories     |     ",
      "Ages: all, <65, 65-74, 75+     |     ",
      "Adjustment: ZIP, ISO year-week, day-of-week, ZIP x COVID period"
    ),
    family = "Helvetica",
    size = 3.0,
    color = "grey20"
  ) +
  scale_fill_identity() +
  coord_cartesian(xlim = c(0.45, 5.55), ylim = c(-0.48, 1.2), clip = "off") +
  labs(title = "C. Analysis workflow and inferential hierarchy") +
  theme_void(base_family = "Helvetica", base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 6)))

figure1 <- ((map_plot | timeline_plot) + plot_layout(widths = c(0.38, 0.62))) /
  workflow_plot +
  plot_layout(heights = c(1.2, 0.8)) +
  plot_annotation(
    title = "Study design: mortality relative to ZIP-specific tropical-cyclone impact",
    subtitle = "Florida, 1985-2022; primary exposure threshold: sustained winds >=34 kt",
    caption = paste0(
      "A ZIP-date in overlapping event windows was assigned to the closest impact date; ",
      "an exact-distance tie was assigned to the pre-impact event."
    ),
    theme = theme(
      text = element_text(family = "Helvetica"),
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(size = 8, color = "grey30", hjust = 0)
    )
  )

save_paper_plot(figure1, "figure_01_study_design", 12, 8.7)

