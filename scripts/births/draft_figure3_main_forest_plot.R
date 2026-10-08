#!/usr/bin/env Rscript

# Draft Figure 3 for the revised birth-outcomes manuscript.
#
# Main time-to-event forest plot for preterm birth (PTB) and very preterm
# birth (vPTB). Within each exposure window, the overall >=34-kt estimate is
# followed by intensity-specific estimates from a model that jointly includes
# tropical-storm and hurricane exposure indicators.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

input_file <- "models/births/time-to-event/time-to-event-estimates.csv"
figure_dir <- "figs/births/revised-manuscript"
summary_dir <- "outputs/births"
output_png <- file.path(figure_dir, "figure-3-time-to-event-forest.png")
output_pdf <- file.path(figure_dir, "figure-3-time-to-event-forest.pdf")
summary_file <- file.path(summary_dir, "figure-3-time-to-event-estimates.csv")

# Quartz embeds the Unicode range and inequality symbols cleanly on macOS.
# Other platforms fall back to the standard PDF device.
publication_pdf <- function(filename, ...) {
  if (identical(unname(Sys.info()["sysname"]), "Darwin")) {
    grDevices::quartz(file = filename, type = "pdf", ...)
  } else {
    grDevices::pdf(file = filename, ...)
  }
}

if (!file.exists(input_file)) {
  stop("Missing time-to-event estimates: ", input_file)
}
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)

results <- fread(input_file)
required_columns <- c(
  "outcome", "structure", "acute_days", "family", "contrast",
  "HR", "conf_low", "conf_high", "p_value", "births", "events"
)
missing_columns <- setdiff(required_columns, names(results))
if (length(missing_columns)) {
  stop(
    "Time-to-event results lack required column(s): ",
    paste(missing_columns, collapse = ", ")
  )
}

# Keep the overall estimate and the two intensity-specific estimates. The
# direct hurricane-versus-tropical-storm contrast has a different reference
# group and is intentionally reserved for supplementary reporting.
plot_data <- results[
  outcome %chin% c("PTB", "vPTB") &
    (
      (family == "any" & contrast == "any_td") |
        (family == "intensity" & contrast %chin% c("ts_td", "hu_td"))
    )
]

plot_data[, window := fcase(
  structure == "acute" & acute_days == 3L, "Days 0\u20133",
  structure == "acute" & acute_days == 7L, "Days 0\u20137",
  structure == "cumulative", "Persistent after impact",
  default = NA_character_
)]
plot_data[, estimate_type := fcase(
  family == "any" & contrast == "any_td", "Any \u226534 kt",
  family == "intensity" & contrast == "ts_td", "Tropical storm (34\u201363 kt)",
  family == "intensity" & contrast == "hu_td", "Hurricane (\u226564 kt)",
  default = NA_character_
)]
plot_data <- plot_data[!is.na(window) & !is.na(estimate_type)]

window_y <- c(
  "Days 0\u20133" = 11,
  "Days 0\u20137" = 7,
  "Persistent after impact" = 3
)
type_offset <- c(
  "Any \u226534 kt" = 0,
  "Tropical storm (34\u201363 kt)" = -1,
  "Hurricane (\u226564 kt)" = -2
)
plot_data[, y := unname(window_y[window] + type_offset[estimate_type])]

outcome_labels <- c(
  PTB = "Preterm birth (PTB)",
  vPTB = "Very preterm birth (vPTB)"
)
plot_data[, outcome_panel := factor(
  unname(outcome_labels[outcome]),
  levels = unname(outcome_labels)
)]
plot_data[, estimate_type := factor(
  estimate_type,
  levels = names(type_offset)
)]
plot_data[, estimate_text := sprintf(
  "%.3f (%.3f, %.3f)", HR, conf_low, conf_high
)]

if (nrow(plot_data) != 18L ||
    anyDuplicated(plot_data[, .(outcome, window, estimate_type)]) ||
    anyNA(plot_data[, .(HR, conf_low, conf_high, y)])) {
  stop("Expected 18 unique and complete estimates for the main forest plot.")
}
if (!all(plot_data$conf_low <= plot_data$HR &
         plot_data$HR <= plot_data$conf_high)) {
  stop("One or more confidence intervals do not contain their point estimate.")
}

setcolorder(
  plot_data,
  c(
    "outcome", "window", "estimate_type", "HR", "conf_low", "conf_high",
    "p_value", "births", "events", "estimate_text", "structure",
    "acute_days", "family", "contrast", "y", "outcome_panel"
  )
)
setorder(plot_data, outcome, -y)
fwrite(plot_data, summary_file)

row_breaks <- c(11, 10, 9, 7, 6, 5, 3, 2, 1)
row_labels <- c(
  "Days 0\u20133: any \u226534 kt",
  "    Tropical storm (34\u201363 kt)",
  "    Hurricane (\u226564 kt)",
  "Days 0\u20137: any \u226534 kt",
  "    Tropical storm (34\u201363 kt)",
  "    Hurricane (\u226564 kt)",
  "Persistent after impact: any \u226534 kt",
  "    Tropical storm (34\u201363 kt)",
  "    Hurricane (\u226564 kt)"
)

estimate_colors <- c(
  "Any \u226534 kt" = "#172B4D",
  "Tropical storm (34\u201363 kt)" = "#2A9D8F",
  "Hurricane (\u226564 kt)" = "#E76F51"
)
estimate_shapes <- c(
  "Any \u226534 kt" = 16,
  "Tropical storm (34\u201363 kt)" = 17,
  "Hurricane (\u226564 kt)" = 15
)

event_summary <- unique(plot_data[, .(outcome, births, events)])
ptb_births <- event_summary[outcome == "PTB", births][1]
ptb_events <- event_summary[outcome == "PTB", events][1]
vptb_events <- event_summary[outcome == "vPTB", events][1]

forest_plot <- ggplot(plot_data, aes(y = y)) +
  geom_hline(
    yintercept = c(8, 4),
    color = "#D7DEE8",
    linewidth = 0.45
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "#59636F",
    linewidth = 0.65
  ) +
  geom_errorbar(
    aes(xmin = conf_low, xmax = conf_high, color = estimate_type),
    orientation = "y",
    width = 0.20,
    linewidth = 0.85
  ) +
  geom_point(
    aes(x = HR, color = estimate_type, shape = estimate_type),
    size = 3.0,
    stroke = 0.6
  ) +
  geom_text(
    aes(x = 1.285, label = estimate_text),
    hjust = 0,
    size = 3.25,
    color = "#1F2937"
  ) +
  annotate(
    "text",
    x = 1.285,
    y = 12.05,
    label = "HR (95% CI)",
    hjust = 0,
    fontface = "bold",
    size = 3.45,
    color = "#172B4D"
  ) +
  facet_wrap(vars(outcome_panel), nrow = 1) +
  scale_y_continuous(
    breaks = row_breaks,
    labels = row_labels,
    limits = c(0.4, 12.35),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_log10(
    breaks = c(0.70, 0.80, 0.90, 1.00, 1.10, 1.25),
    labels = label_number(accuracy = 0.01),
    limits = c(0.68, 1.54),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_color_manual(values = estimate_colors) +
  scale_shape_manual(values = estimate_shapes) +
  labs(
    title = "Tropical-cyclone exposure and preterm-birth hazard",
    subtitle = paste0(
      "Time-varying exposure after residence-specific local storm impact; ",
      "persistent exposure remains active through the end of follow-up"
    ),
    x = "Adjusted hazard ratio (log scale)",
    y = NULL,
    caption = paste0(
      "N = ", comma(ptb_births), " singleton births; PTB events = ",
      comma(ptb_events), "; vPTB events = ", comma(vptb_events), ". ",
      "Intensity indicators were entered jointly and are compared with no active exposure."
    )
  ) +
  guides(color = "none", shape = "none") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold", size = 16, color = "#172B4D", margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 10.5, color = "#44546A", margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 8.8, color = "#5B6573", hjust = 0, margin = margin(t = 10)
    ),
    strip.text = element_text(face = "bold", size = 12, color = "#172B4D"),
    strip.background = element_rect(fill = "#EEF3F8", color = NA),
    axis.text.y = element_text(size = 9.4, color = "#263238", hjust = 0),
    axis.text.x = element_text(size = 9.2, color = "#374151"),
    axis.title.x = element_text(size = 10.5, margin = margin(t = 8)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E4E9F0", linewidth = 0.4),
    panel.spacing.x = grid::unit(1.4, "lines"),
    plot.margin = margin(15, 18, 12, 18)
  )

ggsave(
  output_png,
  forest_plot,
  width = 13.5,
  height = 7.6,
  units = "in",
  dpi = 400,
  bg = "white"
)
ggsave(
  output_pdf,
  forest_plot,
  device = publication_pdf,
  width = 13.5,
  height = 7.6,
  units = "in",
  bg = "white"
)

message("Wrote: ", output_png)
message("Wrote: ", output_pdf)
message("Wrote: ", summary_file)
