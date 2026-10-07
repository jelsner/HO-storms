required_packages <- c("data.table", "ggplot2", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "))
}

library(data.table)
library(ggplot2)

conventional_file <- file.path(
  "models", "births", "conventional-window1",
  "corrected_model_estimates_pooled.csv"
)
survival_file <- file.path(
  "models", "births", "time-to-event",
  "time-to-event-estimates.csv"
)
output_dir <- file.path(
  "outputs", "births", "window1-vs-time-to-event"
)
figure_dir <- file.path(
  "figs", "births", "window1-vs-time-to-event"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(conventional_file), file.exists(survival_file))
conventional <- fread(conventional_file)
survival <- fread(survival_file)

conventional_plot <- conventional[
  outcome %in% c("PTB", "vPTB") & trimester %in% c("T1", "T2") &
    threshold_kt == 34L,
  .(
    outcome,
    model = paste0("Conventional ", trimester, " (±1 day)"),
    analysis = "Conventional trimester model",
    effect_measure = "Adjusted OR",
    effect_ratio = OR,
    conf_low,
    conf_high,
    sample_size = n,
    events
  )
]

survival_plot <- survival[
  outcome %in% c("PTB", "vPTB") & structure == "acute" &
    acute_days == 3L & family == "any" & contrast == "any_td",
  .(
    outcome,
    model = "Time-to-event (0-3 days)",
    analysis = "Time-to-event model",
    effect_measure = "Adjusted HR",
    effect_ratio = HR,
    conf_low,
    conf_high,
    sample_size = births,
    events
  )
]

plot_data <- rbindlist(list(conventional_plot, survival_plot), use.names = TRUE)
stopifnot(
  nrow(conventional_plot) == 4L,
  nrow(survival_plot) == 2L,
  !anyNA(plot_data[, .(effect_ratio, conf_low, conf_high)]),
  all(plot_data$conf_low <= plot_data$effect_ratio),
  all(plot_data$effect_ratio <= plot_data$conf_high)
)

plot_data[, outcome := factor(outcome, levels = c("PTB", "vPTB"))]
plot_data[, model := factor(
  model,
  levels = rev(c(
    "Conventional T1 (±1 day)",
    "Conventional T2 (±1 day)",
    "Time-to-event (0-3 days)"
  ))
)]
plot_data[, estimate_label := sprintf(
  "%s %.3f (%.3f-%.3f)",
  fifelse(effect_measure == "Adjusted OR", "aOR", "aHR"),
  effect_ratio, conf_low, conf_high
)]

fwrite(
  plot_data[order(outcome, model)],
  file.path(output_dir, "corrected-window1-vs-time-to-event-estimates.csv")
)

comparison_plot <- ggplot(
  plot_data,
  aes(
    x = effect_ratio,
    y = model,
    color = analysis,
    shape = analysis
  )
) +
  geom_vline(xintercept = 1, linetype = 2, linewidth = 0.5, color = "grey45") +
  geom_errorbar(
    aes(xmin = conf_low, xmax = conf_high),
    orientation = "y", width = 0.13, linewidth = 0.75
  ) +
  geom_point(size = 2.8) +
  geom_text(
    aes(x = 1.205, label = estimate_label),
    color = "grey20", hjust = 1, size = 3.35, show.legend = FALSE
  ) +
  facet_grid(outcome ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_log10(
    limits = c(0.70, 1.22),
    breaks = c(0.70, 0.80, 0.90, 1.00, 1.10, 1.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_color_manual(
    values = c(
      "Conventional trimester model" = "#2878B5",
      "Time-to-event model" = "#D95F45"
    )
  ) +
  scale_shape_manual(values = c(
    "Conventional trimester model" = 16,
    "Time-to-event model" = 18
  )) +
  labs(
    x = "Adjusted effect ratio (log scale)",
    y = NULL,
    color = NULL,
    shape = NULL,
    title = "Corrected ±1-day trimester models versus 0-3-day time-to-event models",
    subtitle = paste(
      "34 kt local wind-field exposure; points show adjusted effect ratios",
      "with 95% confidence intervals"
    ),
    caption = paste0(
      "Conventional estimates are trimester-specific odds ratios and retain unequal gestational exposure opportunity.\n",
      "Time-to-event estimates are hazard ratios from delayed-entry models with time-varying exposure.\n",
      "ORs and HRs are different estimands."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0, color = "grey35", size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    strip.placement = "outside",
    strip.text.y.left = element_text(face = "bold", angle = 0, size = 11),
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.text.y = element_text(color = "grey20"),
    plot.margin = margin(12, 18, 12, 12)
  )

png_file <- file.path(
  figure_dir, "corrected-window1-vs-time-to-event-forest-plot.png"
)
pdf_file <- file.path(
  figure_dir, "corrected-window1-vs-time-to-event-forest-plot.pdf"
)
ggsave(png_file, comparison_plot, width = 10.5, height = 6.7, dpi = 300)
ggsave(pdf_file, comparison_plot, width = 10.5, height = 6.7)

message("Saved: ", png_file)
message("Saved: ", pdf_file)
