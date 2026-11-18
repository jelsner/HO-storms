#!/usr/bin/env Rscript

source("scripts/mortality/paper_figure_helpers.R")

message("Building Figure 4: age-specific prioritized causes")
results <- read_paper_results()
priority_causes <- c(
  "Cardiovascular",
  "Respiratory",
  "Injuries",
  "Neuropsychiatric disorders"
)
plot_data <- results[
  cause %chin% priority_causes & age_group %chin% paper_age_specific_levels
]
if (nrow(plot_data) != 48L) stop("Figure 4 requires 48 prioritized age-specific rows.")

plot_data[, cause := factor(cause, levels = priority_causes)]
plot_data[, age_group := factor(age_group, levels = paper_age_specific_levels)]
plot_data[, period := factor(period, levels = rev(paper_period_levels))]

annotation_data <- unique(plot_data[, .(
  cause,
  period,
  age_heterogeneity_fdr_q_cause_specific
)])
if (nrow(annotation_data) != 16L) stop("Expected 16 cause-period age-test annotations.")
annotation_data[, age_q_label := format_q_value(age_heterogeneity_fdr_q_cause_specific)]

age_q_count <- uniqueN(
  annotation_data[age_heterogeneity_fdr_q_cause_specific < 0.05, paste(cause, period)]
)
age_subtitle <- if (age_q_count == 0L) {
  "No global age-heterogeneity test survived FDR correction across 28 retained cause-period tests"
} else {
  sprintf("%d global age-heterogeneity tests survived the prespecified FDR correction", age_q_count)
}

dodge <- position_dodge(width = 0.58)
figure4 <- ggplot(
  plot_data,
  aes(x = rate_ratio, y = period, color = age_group, shape = age_group)
) +
  geom_vline(xintercept = 1, color = "grey50", linewidth = 0.5, linetype = "22") +
  geom_errorbar(
    aes(xmin = ci_lower, xmax = ci_upper),
    orientation = "y",
    position = dodge,
    width = 0.16,
    linewidth = 0.65
  ) +
  geom_point(position = dodge, size = 2.8, stroke = 0.8) +
  geom_text(
    data = annotation_data,
    aes(x = 1.61, y = period, label = age_q_label),
    inherit.aes = FALSE,
    family = "Helvetica",
    size = 2.7,
    color = "grey25",
    hjust = 0
  ) +
  facet_wrap(~ cause, ncol = 2) +
  scale_color_manual(values = paper_age_colors) +
  scale_shape_manual(values = paper_age_shapes) +
  scale_x_log10(
    breaks = c(0.80, 1.00, 1.20, 1.40, 1.60),
    labels = label_number(accuracy = 0.01),
    limits = c(0.75, 1.78)
  ) +
  labs(
    title = "Age-specific associations for prioritized causes",
    subtitle = age_subtitle,
    x = "Rate ratio (log scale); labels at right are global age-heterogeneity q-values",
    y = NULL,
    color = "Age group",
    shape = "Age group",
    caption = paste0(
      "Points are rate ratios and horizontal lines are 95% confidence intervals from separate age-specific models. ",
      "Age q-values are from two-degree-of-freedom tests in jointly fitted stacked-age models."
    )
  ) +
  paper_theme(9.5) +
  theme(panel.spacing = unit(6, "mm"))

save_paper_plot(figure4, "figure_04_prioritized_age", 12, 9.2)

