#!/usr/bin/env Rscript

source("scripts/mortality/paper_figure_helpers.R")

message("Building Figure 2: all-disease mortality timing")
results <- read_paper_results()
plot_data <- results[cause == "All diseases"]
if (nrow(plot_data) != 16L) stop("Figure 2 requires 16 all-disease rows.")

plot_data[, age_group := factor(age_group, levels = paper_age_levels)]
plot_data[, period := factor(period, levels = rev(paper_period_levels))]
plot_data[, fdr_status := factor(
  fifelse(main_fdr_q_within_analysis_tier < 0.05, "Primary-family q<0.05", "Not FDR-supported"),
  levels = c("Primary-family q<0.05", "Not FDR-supported")
)]

figure2 <- ggplot(plot_data, aes(x = rate_ratio, y = period)) +
  geom_vline(xintercept = 1, color = "grey45", linewidth = 0.55, linetype = "22") +
  geom_errorbar(
    aes(xmin = ci_lower, xmax = ci_upper),
    orientation = "y",
    width = 0.18,
    linewidth = 0.7,
    color = "#2D5368"
  ) +
  geom_point(
    aes(shape = fdr_status),
    size = 3.2,
    stroke = 1.0,
    color = "#173B4D"
  ) +
  facet_wrap(~ age_group, ncol = 2) +
  scale_shape_manual(values = c("Primary-family q<0.05" = 19, "Not FDR-supported" = 1)) +
  scale_x_log10(
    breaks = c(0.99, 1.00, 1.03, 1.06, 1.09, 1.12),
    labels = label_number(accuracy = 0.01),
    limits = c(0.985, 1.125)
  ) +
  labs(
    title = "All-disease mortality was elevated before, during, and after impact",
    subtitle = "Rate ratios for four event-relative periods compared with retained non-event ZIP-days",
    x = "Rate ratio (log scale)",
    y = NULL,
    shape = NULL,
    caption = paste0(
      "Points are rate ratios and horizontal lines are 95% confidence intervals. Filled points meet ",
      "the prespecified Benjamini-Hochberg q<0.05 criterion across the 16 all-disease tests."
    )
  ) +
  paper_theme(10) +
  theme(panel.spacing = unit(7, "mm"))

save_paper_plot(figure2, "figure_02_all_disease_timing", 10.5, 8.0)

