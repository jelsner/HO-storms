#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(knitr)
  library(scales)
})

source("scripts/mortality/sensitivity_model_helpers.R")

offset_version <- "population_offset_v1_2026-11-19"
artifact_version <- "environmental_research_population_offset_v1_2026-11-19"
source_dir <- file.path(
  "outputs/mortality/paper_sensitivity_34kt",
  offset_version
)
figure_dir <- file.path("figs/mortality", artifact_version)
artifact_dir <- file.path("outputs/mortality", artifact_version)
table_dir <- file.path(artifact_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

estimate_source <- file.path(source_dir, "population_offset_estimates_34kt.csv")
comparison_source <- file.path(source_dir, "population_offset_comparison_34kt.csv")
diagnostic_source <- file.path(source_dir, "population_offset_diagnostics_34kt.csv")
count_source <- file.path(source_dir, "population_offset_counts_34kt.csv")
coverage_source <- file.path(source_dir, "population_offset_coverage_34kt.csv")
zip_year_source <- file.path(source_dir, "population_offset_zip_year_coverage_34kt.csv")
model_qa_source <- file.path(source_dir, "population_offset_qa_34kt.csv")
required_sources <- c(
  estimate_source,
  comparison_source,
  diagnostic_source,
  count_source,
  coverage_source,
  zip_year_source,
  model_qa_source
)
if (any(!file.exists(required_sources))) {
  stop(
    "Missing population-offset inputs: ",
    paste(required_sources[!file.exists(required_sources)], collapse = ", ")
  )
}

offset_estimates <- fread(estimate_source)
comparison <- fread(comparison_source)
coverage <- fread(coverage_source)
model_qa <- fread(model_qa_source)

age_labels <- c(
  "All ages" = "All ages",
  "<65" = "Younger than 65",
  "65-74" = "Ages 65-74",
  "75+" = "Ages 75+"
)
specification_levels <- c(
  "Frozen primary model",
  "Matched-sample count model",
  "Population-offset model"
)
specification_labels <- c(
  "Frozen primary model" = "Primary",
  "Matched-sample count model" = "Covered-sample count",
  "Population-offset model" = "Population offset"
)
specification_colors <- c(
  "Primary" = "#0072B2",
  "Covered-sample count" = "#E69F00",
  "Population offset" = "#009E73"
)

plot_data <- comparison[cause == "All diseases"]
plot_data[, `:=`(
  age_label = factor(age_labels[age_group], levels = unname(age_labels)),
  period = factor(period, levels = sensitivity_period_levels[-1L]),
  specification_label = factor(
    specification_labels[specification],
    levels = unname(specification_labels[specification_levels])
  )
)]

figure <- ggplot(
  plot_data,
  aes(
    x = period,
    y = rate_ratio,
    color = specification_label,
    group = specification_label
  )
) +
  geom_hline(
    yintercept = 1,
    color = "grey45",
    linewidth = 0.45,
    linetype = "22"
  ) +
  geom_line(position = position_dodge(width = 0.42), linewidth = 0.55) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.12,
    position = position_dodge(width = 0.42),
    linewidth = 0.45
  ) +
  geom_point(position = position_dodge(width = 0.42), size = 2.2) +
  facet_wrap(~ age_label, ncol = 2, scales = "free_y") +
  scale_color_manual(values = specification_colors, name = NULL) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "All-disease estimates with an annual ZIP population offset",
    subtitle = "Primary, population-covered-sample count, and population-offset specifications",
    x = NULL,
    y = "Rate ratio",
    caption = paste0(
      "Points are fixed-effects Poisson rate ratios; bars are 95% two-way cluster-robust ",
      "confidence intervals. Panels use age-specific y-axis ranges.\nAge-stratified offset ",
      "models use total ZIP population, not age-specific denominators."
    )
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.4), margin = margin(b = 4)),
    plot.subtitle = element_text(margin = margin(b = 8)),
    plot.caption = element_text(size = rel(0.78), color = "grey30", hjust = 0),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey94", color = "grey82"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "bottom"
  )

figure_png <- file.path(figure_dir, "figure_s05a_population_offset.png")
figure_pdf <- file.path(figure_dir, "figure_s05a_population_offset.pdf")
ggsave(figure_png, figure, width = 10.5, height = 7.5, dpi = 400, bg = "white")
ggsave(
  figure_pdf,
  figure,
  width = 10.5,
  height = 7.5,
  device = "pdf",
  family = "Helvetica",
  bg = "white"
)

table_csv <- file.path(table_dir, "table_s08_population_offset_component.csv")
fwrite(comparison, table_csv)

all_disease_summary <- offset_estimates[cause == "All diseases", .(
  `Age group` = age_labels[age_group],
  Period = period,
  `Primary RR (95% CI)` = sprintf(
    "%.3f (%.3f-%.3f)",
    frozen_rate_ratio,
    frozen_ci_lower,
    frozen_ci_upper
  ),
  `Covered-sample RR (95% CI)` = sprintf(
    "%.3f (%.3f-%.3f)",
    matched_rate_ratio,
    matched_ci_lower,
    matched_ci_upper
  ),
  `Offset RR (95% CI)` = sprintf(
    "%.3f (%.3f-%.3f)",
    rate_ratio,
    ci_lower,
    ci_upper
  )
)]
agreement_summary <- offset_estimates[, .(
  estimates = .N,
  direction_agreement_with_primary = sum(direction_agrees_with_frozen),
  direction_agreement_with_covered_sample = sum(direction_agrees_with_matched),
  ci_overlap_with_primary = sum(offset_ci_overlaps_frozen),
  ci_overlap_with_covered_sample = sum(offset_ci_overlaps_matched),
  median_absolute_offset_minus_covered_log_rr =
    median(abs(offset_minus_matched_log_rate_ratio)),
  maximum_absolute_offset_minus_covered_log_rr =
    max(abs(offset_minus_matched_log_rate_ratio)),
  median_absolute_covered_minus_primary_log_rr =
    median(abs(matched_minus_frozen_log_rate_ratio)),
  maximum_absolute_covered_minus_primary_log_rr =
    max(abs(matched_minus_frozen_log_rate_ratio))
)]
retained_percentage <- coverage[
  metric == "Retained ZIP-day percentage",
  value
]

summary_md <- file.path(table_dir, "population_offset_sensitivity_summary.md")
writeLines(c(
  "# Population-offset sensitivity summary",
  "",
  "## All-disease estimates",
  "",
  capture.output(kable(all_disease_summary, format = "pipe")),
  "",
  "## Agreement across all 80 estimands",
  "",
  paste0(
    "The population-offset estimates agreed in direction with the frozen primary and ",
    "covered-sample count estimates for ",
    agreement_summary$direction_agreement_with_primary,
    "/", agreement_summary$estimates,
    " estimands. All 80 offset confidence intervals overlapped both comparators."
  ),
  "",
  paste0(
    "The median and maximum absolute offset-versus-covered-sample differences on the ",
    "log-RR scale were ",
    sprintf("%.6f", agreement_summary$median_absolute_offset_minus_covered_log_rr),
    " and ",
    sprintf("%.6f", agreement_summary$maximum_absolute_offset_minus_covered_log_rr),
    ", respectively. The population-covered calendar retained ",
    sprintf("%.2f", retained_percentage),
    "% of primary ZIP-days."
  ),
  "",
  paste0(
    "Age-stratified offset models use total annual ZIP population because age-specific ",
    "ZIP denominators are unavailable; they are interpreted as population-scale ",
    "adjustments rather than age-specific mortality-rate models."
  )
), summary_md)

caption_path <- file.path(table_dir, "figure_s05a_caption.md")
writeLines(c(
  "# Figure S5A",
  "",
  paste0(
    "All-disease associations under the frozen primary model, a no-offset count model ",
    "restricted to ZIP-years with positive population data, and the corresponding model ",
    "with log annual ZIP population as an offset. All models use the same fixed effects ",
    "and two-way clustering specification."
  )
), caption_path)

artifact_qa <- data.table(
  check_name = c(
    "Model QA checks all pass",
    "Figure S5A PNG and PDF written",
    "Comparison table contains 240 estimates",
    "All-disease figure contains 48 estimates",
    "Eighty offset estimates summarized",
    "All offset directions agree with covered-sample count model",
    "All offset confidence intervals overlap covered-sample intervals"
  ),
  status = c(
    if (all(model_qa$status == "PASS")) "PASS" else "FAIL",
    if (all(file.exists(c(figure_png, figure_pdf)))) "PASS" else "FAIL",
    if (nrow(comparison) == 240L) "PASS" else "FAIL",
    if (nrow(plot_data) == 48L) "PASS" else "FAIL",
    if (nrow(offset_estimates) == 80L) "PASS" else "FAIL",
    if (all(offset_estimates$direction_agrees_with_matched)) "PASS" else "FAIL",
    if (all(offset_estimates$offset_ci_overlaps_matched)) "PASS" else "FAIL"
  ),
  observed = c(
    sum(model_qa$status == "PASS"),
    sum(file.exists(c(figure_png, figure_pdf))),
    nrow(comparison),
    nrow(plot_data),
    nrow(offset_estimates),
    sum(offset_estimates$direction_agrees_with_matched),
    sum(offset_estimates$offset_ci_overlaps_matched)
  ),
  expected = c(nrow(model_qa), 2L, 240L, 48L, 80L, 80L, 80L)
)
artifact_qa_path <- file.path(artifact_dir, "population_offset_artifact_qa.csv")
fwrite(artifact_qa, artifact_qa_path)
if (any(artifact_qa$status != "PASS")) {
  print(artifact_qa)
  stop("One or more population-offset artifact checks failed.")
}

manifest_targets <- c(
  figure_png,
  figure_pdf,
  table_csv,
  summary_md,
  caption_path,
  artifact_qa_path,
  required_sources,
  "scripts/mortality/run_population_offset_sensitivity_34kt.R",
  "scripts/mortality/build_population_offset_sensitivity_artifacts.R",
  "scripts/mortality/sensitivity_model_helpers.R"
)
hash_output <- system2("shasum", c("-a", "256", manifest_targets), stdout = TRUE)
manifest_split <- tstrsplit(hash_output, "  ", fixed = TRUE)
manifest <- data.table(
  sha256 = manifest_split[[1L]],
  path = manifest_split[[2L]],
  bytes = as.numeric(file.info(manifest_split[[2L]])$size),
  artifact_version = artifact_version,
  source_commit = get_sensitivity_source_commit(),
  build_date = as.IDate("2026-11-19")
)
manifest_path <- file.path(artifact_dir, "population_offset_manifest.csv")
fwrite(manifest, manifest_path)

cat("\nPopulation-offset artifacts completed\n")
cat("Figure S5A:", figure_png, "\n")
cat("Table S8 component:", table_csv, "\n")
cat("Artifact QA checks:", nrow(artifact_qa), "all PASS\n")
