#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(knitr)
  library(scales)
})

source("scripts/mortality/sensitivity_model_helpers.R")

artifact_version <- "environmental_research_sensitivity_v1_2026-11-18"
figure_dir <- file.path("figs/mortality", artifact_version)
artifact_output_dir <- file.path("outputs/mortality", artifact_version)
table_dir <- file.path(artifact_output_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

daily_dir <- file.path(sensitivity_output_dir, "daily_event_time_34kt")
threshold_55_dir <- file.path(sensitivity_output_dir, "threshold_55kt")
threshold_64_dir <- file.path(sensitivity_output_dir, "threshold_64kt")
daily_path <- file.path(daily_dir, "daily_event_time_estimates_34kt.csv")
daily_spatial_path <- file.path(daily_dir, "daily_spatial_target_estimates_34kt.csv")
comparison_path <- file.path(daily_dir, "seven_day_comparison_estimates_34kt.csv")
threshold_55_path <- file.path(threshold_55_dir, "wind_threshold_estimates_55kt.csv")
threshold_64_path <- file.path(threshold_64_dir, "wind_threshold_estimates_64kt.csv")
daily_support_paths <- file.path(
  daily_dir,
  c(
    "daily_event_time_diagnostics_34kt.csv",
    "daily_event_time_counts_34kt.csv",
    "daily_event_time_qa_34kt.csv"
  )
)
threshold_55_support_paths <- file.path(
  threshold_55_dir,
  c(
    "wind_threshold_diagnostics_55kt.csv",
    "wind_threshold_counts_55kt.csv",
    "wind_threshold_qa_55kt.csv"
  )
)
threshold_64_support_paths <- file.path(
  threshold_64_dir,
  c(
    "wind_threshold_diagnostics_64kt.csv",
    "wind_threshold_counts_64kt.csv",
    "wind_threshold_qa_64kt.csv"
  )
)
master_path <- paste0(
  "outputs/mortality/paper_master_34kt/amended_v2_2026-11-18/",
  "master_results_34kt_amended_v2.csv"
)

required_paths <- c(
  daily_path,
  daily_spatial_path,
  comparison_path,
  threshold_55_path,
  threshold_64_path,
  daily_support_paths,
  threshold_55_support_paths,
  threshold_64_support_paths,
  master_path
)
if (any(!file.exists(required_paths))) {
  stop("Missing sensitivity inputs: ", paste(required_paths[!file.exists(required_paths)], collapse = ", "))
}

daily <- fread(daily_path)
daily_spatial <- fread(daily_spatial_path)
comparison <- fread(comparison_path)
threshold_55 <- fread(threshold_55_path)
threshold_64 <- fread(threshold_64_path)
master <- fread(master_path)

period_boundaries <- data.table(
  period = sensitivity_period_levels[-1L],
  x = c(-14L, -7L, 0L, 7L),
  xend = c(-8L, -1L, 6L, 13L)
)
age_labels <- c(
  "All ages" = "All ages",
  "<65" = "Younger than 65",
  "65-74" = "Ages 65-74",
  "75+" = "Ages 75+"
)

sensitivity_theme <- function(base_size = 10) {
  theme_minimal(base_family = "Helvetica", base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.4), margin = margin(b = 4)),
      plot.subtitle = element_text(margin = margin(b = 8)),
      plot.caption = element_text(size = rel(0.78), color = "grey30", hjust = 0),
      strip.text = element_text(face = "bold", lineheight = 1.0),
      strip.background = element_rect(fill = "grey94", color = "grey82"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

save_plot_pair <- function(plot, stem, width, height) {
  png_path <- file.path(figure_dir, paste0(stem, ".png"))
  pdf_path <- file.path(figure_dir, paste0(stem, ".pdf"))
  ggsave(png_path, plot, width = width, height = height, units = "in", dpi = 400, bg = "white")
  ggsave(
    pdf_path,
    plot,
    width = width,
    height = height,
    units = "in",
    device = "pdf",
    family = "Helvetica",
    bg = "white"
  )
  c(png_path, pdf_path)
}

message("Building Figure S1: daily all-disease event-time profiles")
daily_all <- daily[profile_type == "All-disease age profile"]
daily_all[, age_label := factor(age_labels[age_group], levels = unname(age_labels))]
comparison_all <- merge(
  comparison[comparison_type == "All-disease age profile"],
  period_boundaries,
  by = "period"
)
comparison_all[, age_label := factor(age_labels[age_group], levels = unname(age_labels))]

figure_s1 <- ggplot(daily_all, aes(x = rel_day, y = rate_ratio)) +
  geom_hline(yintercept = 1, color = "grey45", linewidth = 0.45, linetype = "22") +
  geom_ribbon(
    aes(ymin = ci_lower, ymax = ci_upper),
    fill = "#56B4E9",
    alpha = 0.20
  ) +
  geom_line(color = "#0072B2", linewidth = 0.65) +
  geom_point(color = "#0072B2", size = 1.25) +
  geom_segment(
    data = comparison_all,
    aes(x = x, xend = xend, y = rate_ratio, yend = rate_ratio),
    inherit.aes = FALSE,
    color = "#D55E00",
    linewidth = 1.25,
    lineend = "round"
  ) +
  facet_wrap(~ age_label, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = c(-14, -7, 0, 7, 13)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Daily all-disease mortality profiles around ZIP-specific impact",
    subtitle = "Daily estimates with corresponding seven-day rate ratios shown in orange",
    x = "Day relative to ZIP-specific impact",
    y = "Rate ratio",
    caption = paste0(
      "Daily and seven-day estimates use the same fixed-effects Poisson specification. ",
      "Shading denotes daily 95% confidence intervals; panels use age-specific y-axis ranges."
    )
  ) +
  sensitivity_theme(10)
figure_s1_files <- save_plot_pair(figure_s1, "figure_s01_daily_all_disease", 10.5, 7.5)

message("Building Figure S3: daily profiles for amended spatial targets")
daily_spatial[, target_label := sprintf(
  "%s, age %s\nTarget period: %s",
  cause,
  age_group,
  target_period
)]
target_levels <- unique(daily_spatial[order(target_order), target_label])
daily_spatial[, target_label := factor(target_label, levels = target_levels)]
target_segments <- unique(daily_spatial[, .(
  target_id,
  target_order,
  target_label,
  target_period,
  target_statewide_rate_ratio
)])
target_segments <- merge(
  target_segments,
  period_boundaries,
  by.x = "target_period",
  by.y = "period",
  all.x = TRUE,
  sort = FALSE
)
target_segments[, target_label := factor(target_label, levels = target_levels)]

figure_s3 <- ggplot(daily_spatial, aes(x = rel_day, y = rate_ratio)) +
  geom_hline(yintercept = 1, color = "grey45", linewidth = 0.4, linetype = "22") +
  geom_ribbon(
    aes(ymin = ci_lower, ymax = ci_upper),
    fill = "#8DA0CB",
    alpha = 0.20
  ) +
  geom_line(color = "#4C568A", linewidth = 0.55) +
  geom_point(color = "#4C568A", size = 0.9) +
  geom_segment(
    data = target_segments,
    aes(
      x = x,
      xend = xend,
      y = target_statewide_rate_ratio,
      yend = target_statewide_rate_ratio
    ),
    inherit.aes = FALSE,
    color = "#D55E00",
    linewidth = 1.15,
    lineend = "round"
  ) +
  facet_wrap(~ target_label, ncol = 4, scales = "free_y", drop = FALSE) +
  scale_x_continuous(breaks = c(-14, 0, 13)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "Daily event-time profiles for the eight spatial targets",
    subtitle = "Orange segments show the seven-day estimate that qualified each target",
    x = "Day relative to ZIP-specific impact",
    y = "Rate ratio",
    caption = paste0(
      "Repeated cause-age profiles appear for targets qualifying in more than one seven-day period. ",
      "Shading denotes daily 95% confidence intervals; panels use target-specific y-axis ranges."
    )
  ) +
  sensitivity_theme(8.5) +
  theme(axis.text.x = element_text(size = 7.5))
figure_s3_files <- save_plot_pair(figure_s3, "figure_s03_daily_spatial_targets", 14, 8.5)

message("Aligning 34-, 55-, and 64-kt four-period estimates")
threshold_34 <- master[, .(
  wind_threshold_kt = 34L,
  cause,
  age_group,
  period,
  cause_order,
  age_order,
  period_order,
  log_rate_ratio,
  standard_error,
  p_value,
  observations,
  rate_ratio,
  ci_lower,
  ci_upper,
  percent_change
)]
threshold_columns <- names(threshold_34)
threshold_combined <- rbindlist(list(
  threshold_34,
  threshold_55[, ..threshold_columns],
  threshold_64[, ..threshold_columns]
), use.names = TRUE)
setorder(threshold_combined, cause_order, age_order, period_order, wind_threshold_kt)

baseline <- threshold_34[, .(
  cause,
  age_group,
  period,
  baseline_34kt_log_rate_ratio = log_rate_ratio,
  baseline_34kt_rate_ratio = rate_ratio,
  baseline_34kt_ci_lower = ci_lower,
  baseline_34kt_ci_upper = ci_upper
)]
threshold_combined <- merge(
  threshold_combined,
  baseline,
  by = c("cause", "age_group", "period"),
  all.x = TRUE,
  sort = FALSE
)
threshold_combined[, `:=`(
  log_rate_ratio_difference_from_34kt =
    log_rate_ratio - baseline_34kt_log_rate_ratio,
  direction_agrees_with_34kt =
    sign(log_rate_ratio) == sign(baseline_34kt_log_rate_ratio),
  confidence_interval_overlaps_34kt =
    ci_lower <= baseline_34kt_ci_upper &
      ci_upper >= baseline_34kt_ci_lower,
  analysis_version = sensitivity_version
)]
setorder(threshold_combined, cause_order, age_order, period_order, wind_threshold_kt)

threshold_csv_path <- file.path(table_dir, "table_s06_wind_threshold_sensitivity.csv")
fwrite(threshold_combined, threshold_csv_path)

message("Building Figure S2: all-disease wind-threshold sensitivity")
threshold_plot_data <- threshold_combined[cause == "All diseases"]
threshold_plot_data[, `:=`(
  age_label = factor(age_labels[age_group], levels = unname(age_labels)),
  period = factor(period, levels = sensitivity_period_levels[-1L]),
  threshold_label = factor(
    paste0(wind_threshold_kt, " kt"),
    levels = c("34 kt", "55 kt", "64 kt")
  )
)]
threshold_colors <- c("34 kt" = "#0072B2", "55 kt" = "#E69F00", "64 kt" = "#CC79A7")

figure_s2 <- ggplot(
  threshold_plot_data,
  aes(
    x = period,
    y = rate_ratio,
    color = threshold_label,
    group = threshold_label
  )
) +
  geom_hline(yintercept = 1, color = "grey45", linewidth = 0.45, linetype = "22") +
  geom_line(position = position_dodge(width = 0.35), linewidth = 0.55) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.12,
    position = position_dodge(width = 0.35),
    linewidth = 0.45
  ) +
  geom_point(position = position_dodge(width = 0.35), size = 2.1) +
  facet_wrap(~ age_label, ncol = 2, scales = "free_y") +
  scale_color_manual(values = threshold_colors, name = "Wind threshold") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "All-disease associations across wind-exposure thresholds",
    subtitle = "Four event-relative periods at sustained winds of at least 34, 55, or 64 kt",
    x = NULL,
    y = "Rate ratio",
    caption = paste0(
      "Points are fixed-effects Poisson rate ratios; bars are 95% two-way cluster-robust ",
      "confidence intervals. Panels use age-specific y-axis ranges. Sensitivity analyses are ",
      "evaluated by direction, magnitude, and uncertainty."
    )
  ) +
  sensitivity_theme(10) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
figure_s2_files <- save_plot_pair(figure_s2, "figure_s02_wind_thresholds", 10.5, 7.5)

threshold_summary <- threshold_combined[wind_threshold_kt %in% c(55L, 64L), .(
  estimates = .N,
  direction_agreement_n = sum(direction_agrees_with_34kt),
  ci_overlap_n = sum(confidence_interval_overlaps_34kt),
  median_absolute_log_rr_difference = median(abs(log_rate_ratio_difference_from_34kt)),
  maximum_absolute_log_rr_difference = max(abs(log_rate_ratio_difference_from_34kt))
), by = wind_threshold_kt]

summary_md_path <- file.path(table_dir, "table_s06_wind_threshold_sensitivity_summary.md")
summary_display <- threshold_summary[, .(
  `Wind threshold` = paste0(wind_threshold_kt, " kt"),
  Estimates = estimates,
  `Direction agreeing with 34 kt` = paste0(direction_agreement_n, "/", estimates),
  `95% CI overlapping 34 kt` = paste0(ci_overlap_n, "/", estimates),
  `Median |Δ log(RR)|` = sprintf("%.3f", median_absolute_log_rr_difference),
  `Maximum |Δ log(RR)|` = sprintf("%.3f", maximum_absolute_log_rr_difference)
)]
writeLines(c(
  "# Table S6. Wind-threshold sensitivity summary",
  "",
  capture.output(kable(summary_display, format = "pipe", align = c("l", "r", "c", "c", "r", "r"))),
  "",
  paste0(
    "Notes: The complete machine-readable table contains 384 aligned estimates: eight retained ",
    "outcomes × four age groups × four periods × three wind thresholds. Threshold sensitivities ",
    "use the primary fixed-effects and two-way clustering specification. No new multiplicity ",
    "correction was prespecified for sensitivity estimates."
  )
), summary_md_path)

figure_files <- c(figure_s1_files, figure_s2_files, figure_s3_files)
qa <- data.table(
  check_name = c(
    "Six sensitivity figure files written",
    "Figure S1 has 112 daily estimates",
    "Figure S2 has 48 all-disease estimates",
    "Figure S3 has 224 target-day estimates",
    "Table S6 has 384 aligned estimates",
    "Each threshold has 128 estimates",
    "All estimate confidence intervals are ordered",
    "Threshold comparison fields are complete",
    "All 55- and 64-kt directions summarized"
  ),
  status = c(
    if (all(file.exists(figure_files))) "PASS" else "FAIL",
    if (nrow(daily_all) == 112L) "PASS" else "FAIL",
    if (nrow(threshold_plot_data) == 48L) "PASS" else "FAIL",
    if (nrow(daily_spatial) == 224L) "PASS" else "FAIL",
    if (nrow(threshold_combined) == 384L) "PASS" else "FAIL",
    if (all(threshold_combined[, .N, by = wind_threshold_kt]$N == 128L)) "PASS" else "FAIL",
    if (all(
      threshold_combined$ci_lower <= threshold_combined$rate_ratio &
        threshold_combined$rate_ratio <= threshold_combined$ci_upper
    )) "PASS" else "FAIL",
    if (!anyNA(threshold_combined[, .(
      log_rate_ratio_difference_from_34kt,
      direction_agrees_with_34kt,
      confidence_interval_overlaps_34kt
    )])) "PASS" else "FAIL",
    if (sum(threshold_summary$estimates) == 256L) "PASS" else "FAIL"
  ),
  observed = c(
    sum(file.exists(figure_files)),
    nrow(daily_all),
    nrow(threshold_plot_data),
    nrow(daily_spatial),
    nrow(threshold_combined),
    paste(threshold_combined[, .N, by = wind_threshold_kt]$N, collapse = ","),
    sum(
      threshold_combined$ci_lower <= threshold_combined$rate_ratio &
        threshold_combined$rate_ratio <= threshold_combined$ci_upper
    ),
    sum(complete.cases(threshold_combined[, .(
      log_rate_ratio_difference_from_34kt,
      direction_agrees_with_34kt,
      confidence_interval_overlaps_34kt
    )])),
    sum(threshold_summary$estimates)
  ),
  expected = c(6L, 112L, 48L, 224L, 384L, "128,128,128", 384L, 384L, 256L)
)
qa_path <- file.path(artifact_output_dir, "exposure_event_time_artifact_qa.csv")
fwrite(qa, qa_path)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more exposure/event-time artifact checks failed.")
}

caption_path <- file.path(table_dir, "sensitivity_figure_captions.md")
writeLines(c(
  "# Sensitivity figure captions",
  "",
  "## Figure S1",
  paste0(
    "Daily all-disease event-time profiles from day −14 through day +13 for all ages and ",
    "the three mutually exclusive age groups. Orange segments show the corresponding ",
    "seven-day rate ratios from the primary analysis."
  ),
  "",
  "## Figure S2",
  paste0(
    "All-disease four-period associations under sustained-wind exposure thresholds of at ",
    "least 34, 55, and 64 kt. All models use the same fixed-effects and clustering specification; ",
    "panels use age-specific y-axis ranges."
  ),
  "",
  "## Figure S3",
  paste0(
    "Daily event-time profiles for all eight amended spatial targets. Orange segments identify ",
    "the seven-day association that satisfied the prespecified spatial-eligibility rule."
  )
), caption_path)

manifest_targets <- c(
  figure_files,
  threshold_csv_path,
  summary_md_path,
  caption_path,
  qa_path,
  "scripts/mortality/sensitivity_model_helpers.R",
  "scripts/mortality/run_daily_event_time_sensitivity_34kt.R",
  "scripts/mortality/run_wind_threshold_sensitivity.R",
  "scripts/mortality/build_exposure_event_time_sensitivity_artifacts.R",
  required_paths
)
hash_output <- system2("shasum", c("-a", "256", manifest_targets), stdout = TRUE)
manifest_split <- tstrsplit(hash_output, "  ", fixed = TRUE)
manifest <- data.table(
  sha256 = manifest_split[[1L]],
  path = manifest_split[[2L]],
  bytes = as.numeric(file.info(manifest_split[[2L]])$size),
  artifact_version = artifact_version,
  source_commit = get_sensitivity_source_commit(),
  build_date = as.IDate("2026-11-18")
)
manifest_path <- file.path(artifact_output_dir, "exposure_event_time_artifact_manifest.csv")
fwrite(manifest, manifest_path)

cat("\nExposure and event-time sensitivity artifacts completed\n")
cat("Figure S1:", figure_s1_files[1L], "\n")
cat("Figure S2:", figure_s2_files[1L], "\n")
cat("Figure S3:", figure_s3_files[1L], "\n")
cat("Table S6:", threshold_csv_path, "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
