#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(knitr)
  library(scales)
})

source("scripts/mortality/sensitivity_model_helpers.R")

icd10_version <- "icd10_era_v1_2026-11-20"
overlap_version <- "overlap_exclusion_v1_2026-11-20"
artifact_version <- "environmental_research_icd10_overlap_v1_2026-11-20"
base_sensitivity_dir <- "outputs/mortality/paper_sensitivity_34kt"
icd10_source_dir <- file.path(base_sensitivity_dir, icd10_version)
overlap_source_dir <- file.path(base_sensitivity_dir, overlap_version)
figure_dir <- file.path("figs/mortality", artifact_version)
artifact_dir <- file.path("outputs/mortality", artifact_version)
table_dir <- file.path(artifact_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

icd10_estimate_source <- file.path(
  icd10_source_dir,
  "icd10_era_estimates_34kt.csv"
)
icd10_coverage_source <- file.path(
  icd10_source_dir,
  "icd10_era_coverage_34kt.csv"
)
icd10_qa_source <- file.path(
  icd10_source_dir,
  "icd10_era_qa_34kt.csv"
)
overlap_estimate_source <- file.path(
  overlap_source_dir,
  "overlap_excluded_estimates_34kt.csv"
)
overlap_coverage_source <- file.path(
  overlap_source_dir,
  "overlap_excluded_coverage_34kt.csv"
)
overlap_qa_source <- file.path(
  overlap_source_dir,
  "overlap_excluded_qa_34kt.csv"
)
required_sources <- c(
  icd10_estimate_source,
  icd10_coverage_source,
  icd10_qa_source,
  overlap_estimate_source,
  overlap_coverage_source,
  overlap_qa_source
)
if (any(!file.exists(required_sources))) {
  stop(
    "Missing ICD-10-era or overlap-exclusion inputs: ",
    paste(required_sources[!file.exists(required_sources)], collapse = ", ")
  )
}

icd10 <- fread(icd10_estimate_source)
icd10_coverage <- fread(icd10_coverage_source)
icd10_qa <- fread(icd10_qa_source)
overlap <- fread(overlap_estimate_source)
overlap_coverage <- fread(overlap_coverage_source)
overlap_qa <- fread(overlap_qa_source)

cause_labels <- c(
  "All diseases" = "All diseases",
  "Cardiovascular" = "Cardiovascular",
  "Respiratory" = "Respiratory",
  "Infectious diseases" = "Infectious diseases",
  "Injuries" = "Injuries",
  "Neuropsychiatric disorders" = "Neuropsychiatric",
  "Renal diseases" = "Renal diseases",
  "Neoplasms" = "Neoplasms"
)

build_comparison_data <- function(estimates, sensitivity_label) {
  estimates_all_age <- estimates[age_group == "All ages"]
  comparison <- rbindlist(list(
    estimates_all_age[, .(
      cause,
      cause_order,
      period,
      period_order,
      specification = "Frozen primary model",
      specification_order = 1L,
      rate_ratio = frozen_rate_ratio,
      ci_lower = frozen_ci_lower,
      ci_upper = frozen_ci_upper
    )],
    estimates_all_age[, .(
      cause,
      cause_order,
      period,
      period_order,
      specification = sensitivity_label,
      specification_order = 2L,
      rate_ratio,
      ci_lower,
      ci_upper
    )]
  ))
  comparison[, `:=`(
    cause_label = factor(
      cause_labels[cause],
      levels = cause_labels[
        unique(estimates_all_age[order(cause_order), cause])
      ]
    ),
    period = factor(period, levels = sensitivity_period_levels[-1L]),
    specification = factor(
      specification,
      levels = c("Frozen primary model", sensitivity_label)
    )
  )]
  setorder(comparison, cause_order, period_order, specification_order)
  comparison
}

sensitivity_theme <- function() {
  theme_minimal(base_family = "Helvetica", base_size = 9.5) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.4), margin = margin(b = 4)),
      plot.subtitle = element_text(margin = margin(b = 8)),
      plot.caption = element_text(size = rel(0.78), color = "grey30", hjust = 0),
      strip.text = element_text(face = "bold", lineheight = 1.0),
      strip.background = element_rect(fill = "grey94", color = "grey82"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "bottom"
    )
}

build_comparison_figure <- function(
  comparison,
  title,
  subtitle,
  sensitivity_label,
  sensitivity_color,
  caption
) {
  ggplot(
    comparison,
    aes(
      x = period,
      y = rate_ratio,
      color = specification,
      group = specification
    )
  ) +
    geom_hline(
      yintercept = 1,
      color = "grey45",
      linewidth = 0.4,
      linetype = "22"
    ) +
    geom_line(position = position_dodge(width = 0.32), linewidth = 0.5) +
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.1,
      position = position_dodge(width = 0.32),
      linewidth = 0.4
    ) +
    geom_point(position = position_dodge(width = 0.32), size = 1.9) +
    facet_wrap(~ cause_label, ncol = 4, scales = "free_y") +
    scale_color_manual(
      values = c(
        "Frozen primary model" = "#0072B2",
        setNames(sensitivity_color, sensitivity_label)
      ),
      labels = c(
        "Frozen primary model" = "Primary",
        setNames(sensitivity_label, sensitivity_label)
      ),
      name = NULL
    ) +
    scale_y_continuous(labels = number_format(accuracy = 0.01)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Rate ratio",
      caption = caption
    ) +
    sensitivity_theme()
}

icd10_comparison <- build_comparison_data(icd10, "ICD-10 era")
overlap_comparison <- build_comparison_data(overlap, "Overlap-excluded")

figure_s5e <- build_comparison_figure(
  icd10_comparison,
  "All-age cause-specific estimates in the ICD-10 era",
  "Full-period primary models compared with models restricted to 1999-2022",
  "ICD-10 era",
  "#CC79A7",
  paste0(
    "Points are rate ratios; bars are 95% two-way cluster-robust confidence ",
    "intervals.\nPanels use cause-specific y-axis ranges. Complete age-specific ",
    "estimates are reported in Table S8."
  )
)
figure_s5e_png <- file.path(figure_dir, "figure_s05e_icd10_era.png")
figure_s5e_pdf <- file.path(figure_dir, "figure_s05e_icd10_era.pdf")
ggsave(figure_s5e_png, figure_s5e, width = 13.5, height = 7.2, dpi = 400, bg = "white")
ggsave(
  figure_s5e_pdf,
  figure_s5e,
  width = 13.5,
  height = 7.2,
  device = "pdf",
  family = "Helvetica",
  bg = "white"
)

figure_s5f <- build_comparison_figure(
  overlap_comparison,
  "Overlap-excluded all-age estimates",
  "Primary closest-impact assignment compared with deletion of all 5,912 overlapping ZIP-dates",
  "Overlap-excluded",
  "#009E73",
  paste0(
    "Points are rate ratios; bars are 95% two-way cluster-robust confidence ",
    "intervals.\nPanels use outcome-specific y-axis ranges. Complete age-specific ",
    "estimates are reported in Table S8."
  )
)
figure_s5f_png <- file.path(figure_dir, "figure_s05f_overlap_exclusion.png")
figure_s5f_pdf <- file.path(figure_dir, "figure_s05f_overlap_exclusion.pdf")
ggsave(figure_s5f_png, figure_s5f, width = 13.5, height = 7.2, dpi = 400, bg = "white")
ggsave(
  figure_s5f_pdf,
  figure_s5f,
  width = 13.5,
  height = 7.2,
  device = "pdf",
  family = "Helvetica",
  bg = "white"
)

icd10_table <- file.path(table_dir, "table_s08_icd10_era_component.csv")
overlap_table <- file.path(table_dir, "table_s08_overlap_exclusion_component.csv")
fwrite(icd10, icd10_table)
fwrite(overlap, overlap_table)

summary_statistics <- rbindlist(list(
  icd10[, .(
    specification = "ICD-10 era",
    estimates = .N,
    direction_agreement = sum(direction_agrees_with_frozen),
    confidence_interval_overlap = sum(confidence_interval_overlaps_frozen),
    ci_exclusion_changes = sum(ci_exclusion_changed),
    median_absolute_delta_log_rr =
      median(abs(sensitivity_minus_frozen_log_rate_ratio)),
    maximum_absolute_delta_log_rr =
      max(abs(sensitivity_minus_frozen_log_rate_ratio))
  )],
  overlap[, .(
    specification = "Overlap-excluded",
    estimates = .N,
    direction_agreement = sum(direction_agrees_with_frozen),
    confidence_interval_overlap = sum(confidence_interval_overlaps_frozen),
    ci_exclusion_changes = sum(ci_exclusion_changed),
    median_absolute_delta_log_rr =
      median(abs(sensitivity_minus_frozen_log_rate_ratio)),
    maximum_absolute_delta_log_rr =
      max(abs(sensitivity_minus_frozen_log_rate_ratio))
  )]
))
summary_table <- summary_statistics[, .(
  Specification = specification,
  Estimates = estimates,
  `Direction agreement` = paste0(direction_agreement, "/", estimates),
  `95% CI overlap` = paste0(confidence_interval_overlap, "/", estimates),
  `CI-exclusion changes` = ci_exclusion_changes,
  `Median |delta log(RR)|` = sprintf("%.4f", median_absolute_delta_log_rr),
  `Maximum |delta log(RR)|` = sprintf("%.4f", maximum_absolute_delta_log_rr)
)]
summary_md <- file.path(table_dir, "icd10_overlap_sensitivity_summary.md")
writeLines(c(
  "# ICD-10-era and overlap-exclusion sensitivity summary",
  "",
  capture.output(kable(summary_table, format = "pipe")),
  "",
  paste0(
    "Notes: The ICD-10-era analysis includes all seven retained cause-specific ",
    "outcomes and four age groups from 1999 through 2022 (112 estimates), retaining ",
    sprintf("%.2f", icd10_coverage$retained_zip_day_percentage),
    "% of full-period ZIP-days. All 112 confidence intervals overlap their frozen ",
    "comparators. Eight of the nine all-age cause-period findings that met the primary ",
    "112-test FDR criterion retain ICD-10-era confidence intervals above 1; neoplasm ",
    "mortality during days +7 to +13 attenuates to RR 1.021 (95% CI 0.994-1.049)."
  ),
  "",
  paste0(
    "The overlap sensitivity removes all ",
    format(overlap_coverage$excluded_overlap_zip_dates, big.mark = ","),
    " ZIP-dates assigned to more than one event window before closest-impact ",
    "resolution, retaining ",
    sprintf("%.2f", overlap_coverage$retained_zip_day_percentage),
    "% of primary ZIP-days. All 128 confidence intervals overlap their primary ",
    "comparators; every primary all-age FDR-supported finding retains a confidence ",
    "interval above 1. No sensitivity-specific multiplicity adjustment was prespecified."
  )
), summary_md)

caption_path <- file.path(table_dir, "figure_s05e_s05f_captions.md")
writeLines(c(
  "# Figure S5E",
  "",
  paste0(
    "All-age cause-specific associations in the full 1985-2022 primary analysis ",
    "and after restriction to the ICD-10 era, 1999-2022. Complete age-specific ",
    "estimates appear in Table S8."
  ),
  "",
  "# Figure S5F",
  "",
  paste0(
    "All-age associations under the primary closest-impact assignment and after ",
    "excluding every ZIP-date that appeared in more than one event window before ",
    "assignment. The sensitivity excludes 5,912 ZIP-dates."
  )
), caption_path)

artifact_qa <- data.table(
  check_name = c(
    "ICD-10 model QA checks all pass",
    "Overlap-exclusion model QA checks all pass",
    "Four figure files written",
    "ICD-10 Table S8 component has 112 estimates",
    "Overlap Table S8 component has 128 estimates",
    "All sensitivity confidence intervals overlap primary",
    "Figure comparison rows complete"
  ),
  status = c(
    if (all(icd10_qa$status == "PASS")) "PASS" else "FAIL",
    if (all(overlap_qa$status == "PASS")) "PASS" else "FAIL",
    if (all(file.exists(c(
      figure_s5e_png,
      figure_s5e_pdf,
      figure_s5f_png,
      figure_s5f_pdf
    )))) "PASS" else "FAIL",
    if (nrow(icd10) == 112L) "PASS" else "FAIL",
    if (nrow(overlap) == 128L) "PASS" else "FAIL",
    if (all(c(
      icd10$confidence_interval_overlaps_frozen,
      overlap$confidence_interval_overlaps_frozen
    ))) "PASS" else "FAIL",
    if (nrow(icd10_comparison) == 56L && nrow(overlap_comparison) == 64L)
      "PASS" else "FAIL"
  ),
  observed = c(
    sum(icd10_qa$status == "PASS"),
    sum(overlap_qa$status == "PASS"),
    sum(file.exists(c(
      figure_s5e_png,
      figure_s5e_pdf,
      figure_s5f_png,
      figure_s5f_pdf
    ))),
    nrow(icd10),
    nrow(overlap),
    sum(c(
      icd10$confidence_interval_overlaps_frozen,
      overlap$confidence_interval_overlaps_frozen
    )),
    paste(nrow(icd10_comparison), nrow(overlap_comparison), sep = "/")
  ),
  expected = c(
    nrow(icd10_qa),
    nrow(overlap_qa),
    4L,
    112L,
    128L,
    240L,
    "56/64"
  )
)
artifact_qa_path <- file.path(
  artifact_dir,
  "icd10_overlap_sensitivity_artifact_qa.csv"
)
fwrite(artifact_qa, artifact_qa_path)
if (any(artifact_qa$status != "PASS")) {
  print(artifact_qa)
  stop("One or more ICD-10/overlap artifact checks failed.")
}

manifest_targets <- c(
  figure_s5e_png,
  figure_s5e_pdf,
  figure_s5f_png,
  figure_s5f_pdf,
  icd10_table,
  overlap_table,
  summary_md,
  caption_path,
  artifact_qa_path,
  required_sources,
  "scripts/mortality/run_icd10_overlap_sensitivities_34kt.R",
  "scripts/mortality/build_icd10_overlap_sensitivity_artifacts.R",
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
  build_date = as.IDate("2026-11-20")
)
manifest_path <- file.path(
  artifact_dir,
  "icd10_overlap_sensitivity_manifest.csv"
)
fwrite(manifest, manifest_path)

cat("\nICD-10-era and overlap-exclusion artifacts completed\n")
cat("Figure S5E:", figure_s5e_png, "\n")
cat("Figure S5F:", figure_s5f_png, "\n")
cat("ICD-10 Table S8 component:", icd10_table, "\n")
cat("Overlap Table S8 component:", overlap_table, "\n")
cat("Artifact QA checks:", nrow(artifact_qa), "all PASS\n")
