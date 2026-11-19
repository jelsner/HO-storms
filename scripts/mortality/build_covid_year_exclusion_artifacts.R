#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(knitr)
  library(scales)
})

source("scripts/mortality/sensitivity_model_helpers.R")

exclusion_version <- "covid_exclusion_v1_2026-11-19"
artifact_version <- "environmental_research_covid_exclusion_v1_2026-11-19"
source_dir <- file.path(
  "outputs/mortality/paper_sensitivity_34kt",
  exclusion_version
)
figure_dir <- file.path("figs/mortality", artifact_version)
artifact_dir <- file.path("outputs/mortality", artifact_version)
table_dir <- file.path(artifact_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

estimate_source <- file.path(source_dir, "covid_excluded_estimates_34kt.csv")
diagnostic_source <- file.path(source_dir, "covid_excluded_diagnostics_34kt.csv")
count_source <- file.path(source_dir, "covid_excluded_counts_34kt.csv")
coverage_source <- file.path(source_dir, "covid_excluded_coverage_34kt.csv")
model_qa_source <- file.path(source_dir, "covid_excluded_qa_34kt.csv")
required_sources <- c(
  estimate_source,
  diagnostic_source,
  count_source,
  coverage_source,
  model_qa_source
)
if (any(!file.exists(required_sources))) {
  stop(
    "Missing COVID-exclusion inputs: ",
    paste(required_sources[!file.exists(required_sources)], collapse = ", ")
  )
}

estimates <- fread(estimate_source)
coverage <- fread(coverage_source)
model_qa <- fread(model_qa_source)

age_labels <- c(
  "All ages" = "All ages",
  "<65" = "Younger than 65",
  "65-74" = "Ages 65-74",
  "75+" = "Ages 75+"
)
comparison <- rbindlist(list(
  estimates[, .(
    age_group,
    age_order,
    period,
    period_order,
    specification = "Frozen primary model",
    specification_order = 1L,
    rate_ratio = frozen_rate_ratio,
    ci_lower = frozen_ci_lower,
    ci_upper = frozen_ci_upper
  )],
  estimates[, .(
    age_group,
    age_order,
    period,
    period_order,
    specification = "Exclude 2020-2022",
    specification_order = 2L,
    rate_ratio,
    ci_lower,
    ci_upper
  )]
))
comparison[, `:=`(
  age_label = factor(age_labels[age_group], levels = unname(age_labels)),
  period = factor(period, levels = sensitivity_period_levels[-1L]),
  specification = factor(
    specification,
    levels = c("Frozen primary model", "Exclude 2020-2022")
  )
)]
setorder(comparison, age_order, period_order, specification_order)

figure <- ggplot(
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
    linewidth = 0.45,
    linetype = "22"
  ) +
  geom_line(position = position_dodge(width = 0.32), linewidth = 0.55) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.12,
    position = position_dodge(width = 0.32),
    linewidth = 0.45
  ) +
  geom_point(position = position_dodge(width = 0.32), size = 2.2) +
  facet_wrap(~ age_label, ncol = 2, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Frozen primary model" = "#0072B2",
      "Exclude 2020-2022" = "#CC79A7"
    ),
    labels = c(
      "Frozen primary model" = "Primary",
      "Exclude 2020-2022" = "Exclude 2020-2022"
    ),
    name = NULL
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "All-disease estimates after excluding 2020-2022",
    subtitle = "Primary and pre-2020 fixed-effects Poisson specifications",
    x = NULL,
    y = "Rate ratio",
    caption = paste0(
      "Points are rate ratios; bars are 95% two-way cluster-robust confidence intervals. ",
      "Panels use age-specific y-axis ranges.\nThe ZIP-by-COVID fixed effect is redundant ",
      "after excluding all COVID-period observations and is omitted from the restricted models."
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

figure_png <- file.path(figure_dir, "figure_s05b_exclude_covid_years.png")
figure_pdf <- file.path(figure_dir, "figure_s05b_exclude_covid_years.pdf")
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

table_csv <- file.path(table_dir, "table_s08_covid_exclusion_component.csv")
fwrite(estimates, table_csv)

summary_display <- estimates[, .(
  `Age group` = age_labels[age_group],
  Period = period,
  `Primary RR (95% CI)` = sprintf(
    "%.3f (%.3f-%.3f)",
    frozen_rate_ratio,
    frozen_ci_lower,
    frozen_ci_upper
  ),
  `Exclude 2020-2022 RR (95% CI)` = sprintf(
    "%.3f (%.3f-%.3f)",
    rate_ratio,
    ci_lower,
    ci_upper
  ),
  `Delta log(RR)` = sprintf("%+.4f", excluded_minus_frozen_log_rate_ratio),
  `CI-exclusion changed` = fifelse(ci_exclusion_changed, "Yes", "No")
)]
retained_percentage <- as.numeric(coverage[
  metric == "Retained ZIP-day percentage",
  value
])
retained_impact_dates <- coverage[
  metric == "COVID-excluded distinct impact dates",
  value
]
summary_md <- file.path(table_dir, "covid_exclusion_sensitivity_summary.md")
writeLines(c(
  "# COVID-year exclusion sensitivity summary",
  "",
  capture.output(kable(summary_display, format = "pipe")),
  "",
  paste0(
    "Notes: The sensitivity analysis excludes all observations from 2020 through 2022 and ",
    "retains ", retained_impact_dates, " of 77 distinct impact dates and ",
    sprintf("%.2f", retained_percentage), "% of primary ZIP-days. All 16 point estimates ",
    "retain the primary positive direction and all confidence intervals overlap the ",
    "corresponding primary intervals. Three borderline estimates change whether the 95% ",
    "confidence interval excludes 1. No sensitivity-specific multiplicity adjustment was ",
    "prespecified."
  )
), summary_md)

caption_path <- file.path(table_dir, "figure_s05b_caption.md")
writeLines(c(
  "# Figure S5B",
  "",
  paste0(
    "All-disease associations in the frozen primary analysis and after excluding every ",
    "observation from 2020 through 2022. The restricted analysis retains 67 distinct impact ",
    "dates and uses ZIP, ISO year-week, and day-of-week fixed effects with two-way clustering ",
    "by ZIP and ISO year-week."
  )
), caption_path)

artifact_qa <- data.table(
  check_name = c(
    "Model QA checks all pass",
    "Figure S5B PNG and PDF written",
    "Table component contains 16 estimates",
    "Figure contains 32 comparison estimates",
    "All directions agree with primary",
    "All confidence intervals overlap primary",
    "CI-exclusion changes accounted for"
  ),
  status = c(
    if (all(model_qa$status == "PASS")) "PASS" else "FAIL",
    if (all(file.exists(c(figure_png, figure_pdf)))) "PASS" else "FAIL",
    if (nrow(estimates) == 16L) "PASS" else "FAIL",
    if (nrow(comparison) == 32L) "PASS" else "FAIL",
    if (all(estimates$direction_agrees_with_frozen)) "PASS" else "FAIL",
    if (all(estimates$confidence_interval_overlaps_frozen)) "PASS" else "FAIL",
    if (sum(estimates$ci_exclusion_changed) == 3L) "PASS" else "FAIL"
  ),
  observed = c(
    sum(model_qa$status == "PASS"),
    sum(file.exists(c(figure_png, figure_pdf))),
    nrow(estimates),
    nrow(comparison),
    sum(estimates$direction_agrees_with_frozen),
    sum(estimates$confidence_interval_overlaps_frozen),
    sum(estimates$ci_exclusion_changed)
  ),
  expected = c(nrow(model_qa), 2L, 16L, 32L, 16L, 16L, 3L)
)
artifact_qa_path <- file.path(artifact_dir, "covid_exclusion_artifact_qa.csv")
fwrite(artifact_qa, artifact_qa_path)
if (any(artifact_qa$status != "PASS")) {
  print(artifact_qa)
  stop("One or more COVID-exclusion artifact checks failed.")
}

manifest_targets <- c(
  figure_png,
  figure_pdf,
  table_csv,
  summary_md,
  caption_path,
  artifact_qa_path,
  required_sources,
  "scripts/mortality/run_covid_year_exclusion_sensitivity_34kt.R",
  "scripts/mortality/build_covid_year_exclusion_artifacts.R",
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
manifest_path <- file.path(artifact_dir, "covid_exclusion_manifest.csv")
fwrite(manifest, manifest_path)

cat("\nCOVID-exclusion artifacts completed\n")
cat("Figure S5B:", figure_png, "\n")
cat("Table S8 component:", table_csv, "\n")
cat("CI-exclusion changes:", sum(estimates$ci_exclusion_changed), "\n")
cat("Artifact QA checks:", nrow(artifact_qa), "all PASS\n")
