#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

input_file <- file.path(
  "models", "births", "conventional-window7",
  "corrected_model_estimates_pooled.csv"
)
output_png <- file.path(
  "figs", "births", "corrected-window7",
  "corrected-window7-all-birth-outcomes.png"
)
output_pdf <- file.path(
  "figs", "births", "corrected-window7",
  "corrected-window7-all-birth-outcomes.pdf"
)

results <- fread(input_file)
results <- results[
  threshold_kt == 34L & outcome %chin% c("PTB", "vPTB", "LBW", "vLBW", "tLBW")
]

outcome_names <- c(
  PTB = "Preterm birth",
  vPTB = "Very preterm birth",
  LBW = "Low birthweight",
  vLBW = "Very low birthweight",
  tLBW = "Term low birthweight"
)

results[, label := sprintf(
  "%s  |  %s",
  unname(outcome_names[outcome]),
  sub("T", "Trimester ", trimester)
)]
results[, interpretation := fifelse(
  conf_low > 1, "Increased odds",
  fifelse(conf_high < 1, "Decreased odds", "Includes the null")
)]
results[, estimate_text := sprintf("%.3f  (%.3f-%.3f)", OR, conf_low, conf_high)]

display_order <- c(
  "Preterm birth  |  Trimester 1",
  "Preterm birth  |  Trimester 2",
  "Very preterm birth  |  Trimester 1",
  "Very preterm birth  |  Trimester 2",
  "Low birthweight  |  Trimester 1",
  "Low birthweight  |  Trimester 2",
  "Very low birthweight  |  Trimester 1",
  "Very low birthweight  |  Trimester 2",
  "Term low birthweight  |  Trimester 1",
  "Term low birthweight  |  Trimester 2",
  "Term low birthweight  |  Trimester 3"
)
results[, label := factor(label, levels = rev(display_order))]
results[, interpretation := factor(
  interpretation,
  levels = c("Increased odds", "Includes the null", "Decreased odds")
)]

palette <- c(
  "Increased odds" = "#C44E52",
  "Includes the null" = "#6B7280",
  "Decreased odds" = "#2878B5"
)

p <- ggplot(results, aes(x = OR, y = label, color = interpretation)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.65,
             color = "#4B5563") +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                orientation = "y", width = 0.18, linewidth = 0.8) +
  geom_point(size = 3.1) +
  annotate(
    "text", x = 1.075, y = length(display_order) + 0.62,
    label = "Adjusted OR (95% CI)", hjust = 0, fontface = "bold",
    size = 3.6, color = "#111827"
  ) +
  geom_text(
    aes(x = 1.075, label = estimate_text),
    hjust = 0, color = "#111827", size = 3.45
  ) +
  scale_color_manual(values = palette, drop = FALSE) +
  scale_x_log10(
    breaks = c(0.75, 0.80, 0.90, 1.00),
    labels = label_number(accuracy = 0.01),
    limits = c(0.70, 1.23),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Corrected ±7-day tropical-cyclone exposure and birth outcomes",
    subtitle = paste(
      "Primary 34-kt wind-field exposure; adjusted logistic models with",
      "conception year-month and county fixed effects"
    ),
    x = "Adjusted odds ratio (log scale)",
    y = NULL,
    color = NULL,
    caption = paste0(
      "Points are adjusted odds ratios; bars are 95% confidence intervals. ",
      "Standard errors are clustered by county.\n",
      "The symmetric ±7-day indicator covers the seven days before through ",
      "seven days after the residence-specific local impact date."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 17, color = "#111827"),
    plot.subtitle = element_text(size = 11.5, color = "#374151",
                                 margin = margin(b = 12)),
    plot.caption = element_text(hjust = 0, size = 9.3, color = "#4B5563",
                                margin = margin(t = 14)),
    axis.text.y = element_text(size = 10.7, color = "#111827"),
    axis.text.x = element_text(color = "#374151"),
    axis.title.x = element_text(margin = margin(t = 8)),
    panel.grid.major.y = element_line(color = "#E5E7EB"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin = margin(t = 5),
    plot.margin = margin(14, 20, 12, 14)
  )

dir.create(dirname(output_png), recursive = TRUE, showWarnings = FALSE)
ggsave(output_png, p, width = 12, height = 8.2, dpi = 300, bg = "white")
ggsave(output_pdf, p, width = 12, height = 8.2, device = grDevices::pdf,
       bg = "white", useDingbats = FALSE)

message("Wrote: ", output_png)
message("Wrote: ", output_pdf)
