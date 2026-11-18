#!/usr/bin/env Rscript

source("scripts/mortality/paper_figure_helpers.R")

message("Building Figure 3: all-age cause-specific mortality")
results <- read_paper_results()
plot_data <- results[cause != "All diseases" & age_group == "All ages"]
if (nrow(plot_data) != 28L) stop("Figure 3 requires 28 all-age cause-specific rows.")

plot_data[, cause_label := fcase(
  cause == "Neuropsychiatric disorders", "Neuropsychiatric\ndisorders",
  cause == "Endocrine, nutritional, and metabolic disorders", "Endocrine, nutritional,\nand metabolic disorders",
  cause == "Infectious diseases", "Infectious\ndiseases",
  cause == "Digestive diseases", "Digestive\ndiseases",
  cause == "Renal diseases", "Renal\ndiseases",
  default = cause
)]
cause_labels <- unique(plot_data[order(cause_order), cause_label])
plot_data[, cause_label := factor(cause_label, levels = cause_labels)]
plot_data[, period := factor(period, levels = rev(paper_period_levels))]
plot_data[, fdr_status := factor(
  fifelse(main_fdr_q_all_cause_specific < 0.05, "112-test q<0.05", "Not FDR-supported"),
  levels = c("112-test q<0.05", "Not FDR-supported")
)]

figure3 <- ggplot(plot_data, aes(x = rate_ratio, y = period, color = analysis_tier)) +
  geom_vline(xintercept = 1, color = "grey50", linewidth = 0.5, linetype = "22") +
  geom_errorbar(
    aes(xmin = ci_lower, xmax = ci_upper),
    orientation = "y",
    width = 0.18,
    linewidth = 0.65
  ) +
  geom_point(aes(shape = fdr_status), size = 2.9, stroke = 0.95) +
  facet_wrap(~ cause_label, ncol = 4) +
  scale_color_manual(values = paper_tier_colors) +
  scale_shape_manual(values = c("112-test q<0.05" = 19, "Not FDR-supported" = 1)) +
  scale_x_log10(
    breaks = c(0.90, 1.00, 1.10, 1.20, 1.30),
    labels = label_number(accuracy = 0.01),
    limits = c(0.86, 1.30)
  ) +
  labs(
    title = "Cause-specific mortality varied across the event-relative periods",
    subtitle = "All-age estimates for seven retained disease categories",
    x = "Rate ratio (log scale)",
    y = NULL,
    color = "Analysis family",
    shape = NULL,
    caption = paste0(
      "Points are rate ratios and horizontal lines are 95% confidence intervals. Filled points meet ",
      "the amended Benjamini-Hochberg q<0.05 criterion across all 112 cause-specific tests."
    )
  ) +
  paper_theme(8.8) +
  theme(
    panel.spacing = unit(4.5, "mm"),
    axis.text.y = element_text(size = 7.2),
    strip.text = element_text(size = 8.0)
  )

save_paper_plot(figure3, "figure_03_cause_specific", 12, 8.5)

