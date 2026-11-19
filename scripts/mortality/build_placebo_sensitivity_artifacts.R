#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(knitr)
  library(scales)
})

source("scripts/mortality/sensitivity_model_helpers.R")

placebo_version <- "placebo_v1_2026-11-19"
artifact_version <- "environmental_research_placebo_v1_2026-11-19"
source_dir <- file.path(
  "outputs/mortality/paper_sensitivity_34kt",
  placebo_version
)
figure_dir <- file.path("figs/mortality", artifact_version)
artifact_dir <- file.path("outputs/mortality", artifact_version)
table_dir <- file.path(artifact_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

estimate_source <- file.path(source_dir, "placebo_estimates_34kt.csv")
diagnostic_source <- file.path(source_dir, "placebo_diagnostics_34kt.csv")
count_source <- file.path(source_dir, "placebo_counts_34kt.csv")
calendar_source <- file.path(source_dir, "placebo_calendar_diagnostics_34kt.csv")
model_qa_source <- file.path(source_dir, "placebo_qa_34kt.csv")
required_sources <- c(
  estimate_source,
  diagnostic_source,
  count_source,
  calendar_source,
  model_qa_source
)
if (any(!file.exists(required_sources))) {
  stop(
    "Missing placebo inputs: ",
    paste(required_sources[!file.exists(required_sources)], collapse = ", ")
  )
}

estimates <- fread(estimate_source)
calendar_diagnostics <- fread(calendar_source)
model_qa <- fread(model_qa_source)

age_labels <- c(
  "All ages" = "All ages",
  "<65" = "Younger than 65",
  "65-74" = "Ages 65-74",
  "75+" = "Ages 75+"
)
comparison <- rbindlist(list(
  unique(estimates[, .(
    age_group,
    age_order,
    period,
    period_order,
    specification = "Frozen primary model",
    specification_order = 1L,
    rate_ratio = frozen_rate_ratio,
    ci_lower = frozen_ci_lower,
    ci_upper = frozen_ci_upper
  )]),
  estimates[, .(
    age_group,
    age_order,
    period,
    period_order,
    specification = placebo_type,
    specification_order = placebo_order + 1L,
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
    levels = c(
      "Frozen primary model",
      "Temporal placebo",
      "Spatial placebo"
    )
  )
)]
setorder(comparison, age_order, period_order, specification_order)

figure <- ggplot(
  comparison,
  aes(
    x = period,
    y = rate_ratio,
    color = specification,
    shape = specification,
    group = specification
  )
) +
  geom_hline(
    yintercept = 1,
    color = "grey45",
    linewidth = 0.45,
    linetype = "22"
  ) +
  geom_line(position = position_dodge(width = 0.46), linewidth = 0.5) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.1,
    position = position_dodge(width = 0.46),
    linewidth = 0.4
  ) +
  geom_point(position = position_dodge(width = 0.46), size = 2.2) +
  facet_wrap(~ age_label, ncol = 2, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Frozen primary model" = "#0072B2",
      "Temporal placebo" = "#D55E00",
      "Spatial placebo" = "#009E73"
    ),
    labels = c(
      "Frozen primary model" = "Primary",
      "Temporal placebo" = "Temporal placebo",
      "Spatial placebo" = "Spatial placebo"
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      "Frozen primary model" = 16,
      "Temporal placebo" = 17,
      "Spatial placebo" = 15
    ),
    labels = c(
      "Frozen primary model" = "Primary",
      "Temporal placebo" = "Temporal placebo",
      "Spatial placebo" = "Spatial placebo"
    ),
    name = NULL
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "All-disease estimates under temporal and spatial placebos",
    subtitle = "Negative controls compared with the frozen 34-kt primary estimates",
    x = NULL,
    y = "Rate ratio",
    caption = paste0(
      "Points are rate ratios; bars are 95% two-way cluster-robust confidence intervals. ",
      "Panels use age-specific y-axis ranges.\nTemporal-placebo impact dates were ",
      "randomized within storm season and at least 50 days from the observed date; ",
      "the spatial placebo permuted labels among ZIPs within each date."
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

figure_png <- file.path(figure_dir, "figure_s05c_placebo_negative_controls.png")
figure_pdf <- file.path(figure_dir, "figure_s05c_placebo_negative_controls.pdf")
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

table_csv <- file.path(table_dir, "table_s08_placebo_component.csv")
fwrite(estimates, table_csv)

summary_display <- estimates[, .(
  `Placebo` = placebo_type,
  `Age group` = age_labels[age_group],
  Period = period,
  `Primary RR (95% CI)` = sprintf(
    "%.3f (%.3f-%.3f)",
    frozen_rate_ratio,
    frozen_ci_lower,
    frozen_ci_upper
  ),
  `Placebo RR (95% CI)` = sprintf(
    "%.3f (%.3f-%.3f)",
    rate_ratio,
    ci_lower,
    ci_upper
  ),
  `Delta log(RR)` = sprintf("%+.4f", placebo_minus_frozen_log_rate_ratio),
  `Placebo CI excludes 1` = fifelse(placebo_ci_excludes_null, "Yes", "No")
)]
null_counts <- estimates[, .(
  estimates = .N,
  ci_excludes_null = sum(placebo_ci_excludes_null),
  direction_agrees = sum(direction_agrees_with_frozen)
), by = placebo_type]
null_sentence <- paste(
  null_counts[, paste0(
    placebo_type,
    ": ", ci_excludes_null, " of ", estimates,
    " confidence intervals exclude 1 and ", direction_agrees, " of ", estimates,
    " point estimates share the primary direction"
  )],
  collapse = "; "
)
temporal_overlap_percentage <- calendar_diagnostics[
  placebo_type == "Temporal placebo",
  observed_window_overlap_percentage
]
temporal_near_observed_13 <- calendar_diagnostics[
  placebo_type == "Temporal placebo",
  events_with_observed_impact_within_13_days
]
summary_md <- file.path(table_dir, "placebo_sensitivity_summary.md")
writeLines(c(
  "# Placebo sensitivity summary",
  "",
  capture.output(kable(summary_display, format = "pipe")),
  "",
  paste0(
    "Notes: These are negative-control analyses using the same four age groups, four ",
    "event-relative periods, fixed effects, and two-way clustering as the primary ",
    "all-disease models. ", null_sentence, ". Placebo results are interpreted as ",
    "falsification evidence, not as alternative estimates of a storm effect. No ",
    "placebo-specific multiplicity adjustment was prespecified. The legacy temporal ",
    "calendar keeps each randomized date at least 50 days from its corresponding ",
    "observed storm, but not from every other observed storm: ",
    sprintf("%.2f", temporal_overlap_percentage), "% of its active ZIP-days overlap an ",
    "observed event window, and ", temporal_near_observed_13, " ZIP-specific placebo ",
    "events fall within 13 days of another observed impact. This partial overlap does ",
    "not by itself explain the broadly positive temporal-placebo pattern, which warrants ",
    "further temporal-control diagnostics."
  )
), summary_md)

caption_path <- file.path(table_dir, "figure_s05c_caption.md")
writeLines(c(
  "# Figure S5C",
  "",
  paste0(
    "Frozen primary all-disease associations and two negative-control analyses. ",
    "The temporal placebo uses storm-season impact dates randomized at least 50 days ",
    "from observed impacts (seed 20260207). The spatial placebo permutes exposure ",
    "labels across ZIP codes independently within each date while preserving the ",
    "date-specific exposure distribution (seed 20260224). All models include ZIP, ",
    "ISO year-week, day-of-week, and ZIP-by-COVID-period fixed effects, with two-way ",
    "clustering by ZIP and ISO year-week."
  )
), caption_path)

artifact_qa <- data.table(
  check_name = c(
    "Model QA checks all pass",
    "Figure S5C PNG and PDF written",
    "Table component contains 32 estimates",
    "Figure contains 48 comparison estimates",
    "Both placebo types represented",
    "Calendar diagnostics represented"
  ),
  status = c(
    if (all(model_qa$status == "PASS")) "PASS" else "FAIL",
    if (all(file.exists(c(figure_png, figure_pdf)))) "PASS" else "FAIL",
    if (nrow(estimates) == 32L) "PASS" else "FAIL",
    if (nrow(comparison) == 48L) "PASS" else "FAIL",
    if (uniqueN(estimates$placebo_type) == 2L) "PASS" else "FAIL",
    if (nrow(calendar_diagnostics) == 2L) "PASS" else "FAIL"
  ),
  observed = c(
    sum(model_qa$status == "PASS"),
    sum(file.exists(c(figure_png, figure_pdf))),
    nrow(estimates),
    nrow(comparison),
    uniqueN(estimates$placebo_type),
    nrow(calendar_diagnostics)
  ),
  expected = c(nrow(model_qa), 2L, 32L, 48L, 2L, 2L)
)
artifact_qa_path <- file.path(artifact_dir, "placebo_artifact_qa.csv")
fwrite(artifact_qa, artifact_qa_path)
if (any(artifact_qa$status != "PASS")) {
  print(artifact_qa)
  stop("One or more placebo artifact checks failed.")
}

manifest_targets <- c(
  figure_png,
  figure_pdf,
  table_csv,
  summary_md,
  caption_path,
  artifact_qa_path,
  required_sources,
  "scripts/mortality/run_placebo_sensitivity_34kt.R",
  "scripts/mortality/build_placebo_sensitivity_artifacts.R",
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
manifest_path <- file.path(artifact_dir, "placebo_manifest.csv")
fwrite(manifest, manifest_path)

cat("\nPlacebo artifacts completed\n")
cat("Figure S5C:", figure_png, "\n")
cat("Table S8 component:", table_csv, "\n")
cat(
  "Placebo CIs excluding 1:",
  estimates[, sum(placebo_ci_excludes_null)],
  "of", nrow(estimates), "\n"
)
cat("Artifact QA checks:", nrow(artifact_qa), "all PASS\n")
