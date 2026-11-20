#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

source("scripts/mortality/sensitivity_model_helpers.R")

artifact_version <- "environmental_research_figure_s05_table_s08_v1_2026-11-20"
figure_dir <- file.path("figs/mortality", artifact_version)
artifact_dir <- file.path("outputs/mortality", artifact_version)
table_dir <- file.path(artifact_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

component_scripts <- c(
  "scripts/mortality/build_population_offset_sensitivity_artifacts.R",
  "scripts/mortality/build_covid_year_exclusion_artifacts.R",
  "scripts/mortality/build_placebo_sensitivity_artifacts.R",
  "scripts/mortality/build_strict_temporal_permutation_artifacts.R",
  "scripts/mortality/build_icd10_overlap_sensitivity_artifacts.R"
)
component_tables <- c(
  population = "outputs/mortality/environmental_research_population_offset_v1_2026-11-19/tables/table_s08_population_offset_component.csv",
  covid = "outputs/mortality/environmental_research_covid_exclusion_v1_2026-11-19/tables/table_s08_covid_exclusion_component.csv",
  placebo = "outputs/mortality/environmental_research_placebo_v1_2026-11-19/tables/table_s08_placebo_component.csv",
  strict = "outputs/mortality/environmental_research_strict_placebo_v1_2026-11-19/tables/table_s08_strict_temporal_permutation_component.csv",
  icd10 = "outputs/mortality/environmental_research_icd10_overlap_v1_2026-11-20/tables/table_s08_icd10_era_component.csv",
  overlap = "outputs/mortality/environmental_research_icd10_overlap_v1_2026-11-20/tables/table_s08_overlap_exclusion_component.csv"
)
required_sources <- c(component_scripts, component_tables)
if (any(!file.exists(required_sources))) {
  stop(
    "Missing Figure S5/Table S8 inputs: ",
    paste(required_sources[!file.exists(required_sources)], collapse = ", ")
  )
}

load_component_environment <- function(path) {
  component_environment <- new.env(parent = globalenv())
  invisible(capture.output(sys.source(path, envir = component_environment)))
  component_environment
}

population_environment <- load_component_environment(component_scripts[1L])
covid_environment <- load_component_environment(component_scripts[2L])
placebo_environment <- load_component_environment(component_scripts[3L])
strict_environment <- load_component_environment(component_scripts[4L])
icd10_overlap_environment <- load_component_environment(component_scripts[5L])

tag_figure <- function(figure, panel_letter) {
  figure +
    labs(title = paste0(panel_letter, ". ", figure$labels$title)) +
    theme(plot.title = element_text(face = "bold"))
}

figures <- list(
  A = tag_figure(population_environment$figure, "A"),
  B = tag_figure(covid_environment$figure, "B"),
  C = tag_figure(placebo_environment$figure, "C"),
  D = tag_figure(strict_environment$figure, "D"),
  E = tag_figure(icd10_overlap_environment$figure_s5e, "E"),
  F = tag_figure(icd10_overlap_environment$figure_s5f, "F")
)

panel_dimensions <- data.table(
  panel = names(figures),
  width = c(10.5, 10.5, 10.5, 9.5, 13.5, 13.5),
  height = c(7.5, 7.5, 7.5, 7.2, 7.2, 7.2)
)
panel_paths <- character()
for (panel_letter in names(figures)) {
  panel_png <- file.path(
    figure_dir,
    paste0("figure_s05", tolower(panel_letter), "_final.png")
  )
  panel_paths <- c(panel_paths, panel_png)
  dimensions <- panel_dimensions[panel == panel_letter]
  ggsave(
    panel_png,
    figures[[panel_letter]],
    width = dimensions$width,
    height = dimensions$height,
    dpi = 400,
    bg = "white"
  )
}

figure_pdf <- file.path(figure_dir, "figure_s05_alternative_specifications.pdf")
pdf_assembler <- "scripts/mortality/assemble_figure_s05_pdf.py"
pdf_python <- Sys.getenv(
  "CODEX_WORKSPACE_PYTHON",
  unset = paste0(
    "/Users/jelsner/.cache/codex-runtimes/codex-primary-runtime/",
    "dependencies/python/bin/python3"
  )
)
pdf_status <- system2(
  pdf_python,
  c(pdf_assembler, figure_pdf, panel_paths)
)
if (!identical(pdf_status, 0L)) {
  stop("Figure S5 PDF assembly failed with status ", pdf_status)
}

contact_sheet <- wrap_plots(figures, ncol = 2) +
  plot_annotation(
    title = "Figure S5. Alternative-specification analyses",
    theme = theme(
      plot.title = element_text(
        family = "Helvetica",
        face = "bold",
        size = 20,
        margin = margin(b = 12)
      )
    )
  )
contact_sheet_png <- file.path(
  figure_dir,
  "figure_s05_alternative_specifications_contact_sheet.png"
)
ggsave(
  contact_sheet_png,
  contact_sheet,
  width = 22,
  height = 29,
  dpi = 160,
  bg = "white"
)

population <- fread(component_tables[["population"]])
covid <- fread(component_tables[["covid"]])
placebo <- fread(component_tables[["placebo"]])
strict <- fread(component_tables[["strict"]])
icd10 <- fread(component_tables[["icd10"]])
overlap <- fread(component_tables[["overlap"]])

population_primary <- population[
  specification == "Frozen primary model",
  .(
    cause,
    age_group,
    period,
    primary_log_rate_ratio = log_rate_ratio,
    primary_standard_error = standard_error,
    primary_p_value = p_value,
    primary_rate_ratio = rate_ratio,
    primary_ci_lower = ci_lower,
    primary_ci_upper = ci_upper
  )
]
population_alternative <- merge(
  population[specification != "Frozen primary model"],
  population_primary,
  by = c("cause", "age_group", "period"),
  all.x = TRUE,
  sort = FALSE
)
population_alternative[, `:=`(
  analysis = "Population-offset analysis",
  analysis_order = 1L,
  specification_order = fifelse(
    specification == "Matched-sample count model",
    1L,
    2L
  ),
  specification = fifelse(
    specification == "Matched-sample count model",
    "Population-covered sample count model",
    "Annual ZIP population-offset model"
  ),
  alternative_interval_type = "95% confidence interval",
  delta_log_rate_ratio = log_rate_ratio - primary_log_rate_ratio,
  direction_agrees_with_primary =
    sign(log_rate_ratio) == sign(primary_log_rate_ratio),
  interval_overlaps_primary =
    ci_lower <= primary_ci_upper & ci_upper >= primary_ci_lower,
  primary_ci_excludes_null =
    primary_ci_lower > 1 | primary_ci_upper < 1,
  alternative_interval_excludes_null = ci_lower > 1 | ci_upper < 1,
  empirical_p_upper = NA_real_,
  empirical_p_two_sided_centered = NA_real_,
  empirical_p_upper_max_t = NA_real_,
  global_empirical_p_max_t = NA_real_,
  observations = NA_real_,
  analysis_note = paste0(
    "Age-stratified offset models use total annual ZIP population; the covered-sample ",
    "count model separates population-data coverage from the offset itself."
  )
)]
population_alternative[, ci_exclusion_changed :=
  primary_ci_excludes_null != alternative_interval_excludes_null]

standard_component <- function(
  x,
  analysis_label,
  analysis_order_value,
  delta_column,
  overlap_column,
  alternative_excludes_column,
  note
) {
  output <- copy(x)
  if (!"cause_order" %in% names(output)) {
    output[, cause_order := 1L]
  }
  output[, `:=`(
    analysis = analysis_label,
    analysis_order = analysis_order_value,
    specification_order = seq_len(.N),
    alternative_interval_type = "95% confidence interval",
    primary_log_rate_ratio = frozen_log_rate_ratio,
    primary_standard_error = frozen_standard_error,
    primary_p_value = frozen_p_value,
    primary_rate_ratio = frozen_rate_ratio,
    primary_ci_lower = frozen_ci_lower,
    primary_ci_upper = frozen_ci_upper,
    delta_log_rate_ratio = get(delta_column),
    direction_agrees_with_primary = get("direction_agrees_with_frozen"),
    interval_overlaps_primary = get(overlap_column),
    primary_ci_excludes_null = get("frozen_ci_excludes_null"),
    alternative_interval_excludes_null = get(alternative_excludes_column),
    ci_exclusion_changed =
      get("frozen_ci_excludes_null") != get(alternative_excludes_column),
    empirical_p_upper = NA_real_,
    empirical_p_two_sided_centered = NA_real_,
    empirical_p_upper_max_t = NA_real_,
    global_empirical_p_max_t = NA_real_,
    analysis_note = note
  )]
  output
}

covid_standard <- standard_component(
  covid,
  "COVID-year exclusion",
  2L,
  "excluded_minus_frozen_log_rate_ratio",
  "confidence_interval_overlaps_frozen",
  "excluded_ci_excludes_null",
  "Excludes every observation from 2020 through 2022."
)
covid_standard[, specification_order := 1L]

placebo[, confidence_interval_overlaps_frozen :=
  ci_lower <= frozen_ci_upper & ci_upper >= frozen_ci_lower]
placebo_standard <- standard_component(
  placebo,
  "Legacy negative controls",
  3L,
  "placebo_minus_frozen_log_rate_ratio",
  "confidence_interval_overlaps_frozen",
  "placebo_ci_excludes_null",
  paste0(
    "Negative-control estimates are falsification analyses, not alternative storm-effect ",
    "estimates; the legacy temporal placebo may overlap other observed storm windows."
  )
)
placebo_standard[, specification_order := placebo_order]

icd10_standard <- standard_component(
  icd10,
  "ICD-10-era restriction",
  5L,
  "sensitivity_minus_frozen_log_rate_ratio",
  "confidence_interval_overlaps_frozen",
  "sensitivity_ci_excludes_null",
  "Cause-specific models restricted to 1999 through 2022."
)
icd10_standard[, specification_order := 1L]

overlap_standard <- standard_component(
  overlap,
  "Overlapping-window exclusion",
  6L,
  "sensitivity_minus_frozen_log_rate_ratio",
  "confidence_interval_overlaps_frozen",
  "sensitivity_ci_excludes_null",
  "Excludes all 5,912 ZIP-dates appearing in more than one event window."
)
overlap_standard[, specification_order := 1L]

strict_standard <- strict[, .(
  analysis = "Strict temporal permutation",
  analysis_order = 4L,
  specification = "Strict matched temporal permutation null",
  specification_order = 1L,
  cause = "All diseases",
  cause_order = 1L,
  age_group = "All ages",
  age_order = 1L,
  period,
  period_order,
  log_rate_ratio = log(null_rr_median),
  standard_error = NA_real_,
  p_value = NA_real_,
  rate_ratio = null_rr_median,
  ci_lower = null_rr_025,
  ci_upper = null_rr_975,
  alternative_interval_type = "Permutation 2.5th-97.5th percentiles",
  observations = NA_real_,
  primary_log_rate_ratio = observed_log_rate_ratio,
  primary_standard_error = NA_real_,
  primary_p_value = observed_p_value,
  primary_rate_ratio = observed_rate_ratio,
  primary_ci_lower = observed_ci_lower,
  primary_ci_upper = observed_ci_upper,
  delta_log_rate_ratio = log(null_rr_median) - observed_log_rate_ratio,
  direction_agrees_with_primary = NA,
  interval_overlaps_primary = NA,
  primary_ci_excludes_null =
    observed_ci_lower > 1 | observed_ci_upper < 1,
  alternative_interval_excludes_null = NA,
  ci_exclusion_changed = NA,
  empirical_p_upper,
  empirical_p_two_sided_centered,
  empirical_p_upper_max_t,
  global_empirical_p_max_t,
  wind_threshold_kt = 34L,
  analysis_version,
  source_commit,
  build_date,
  analysis_note = paste0(
    "Alternative RR and interval columns summarize the 199-permutation null distribution; ",
    "they are not a model confidence interval."
  )
)]

select_harmonized <- function(x) {
  x[, .(
    analysis_order,
    analysis,
    specification_order,
    specification,
    cause_order,
    cause,
    age_order,
    age_group,
    period_order,
    period,
    alternative_log_rate_ratio = log_rate_ratio,
    alternative_standard_error = standard_error,
    alternative_p_value = p_value,
    alternative_rate_ratio = rate_ratio,
    alternative_ci_lower = ci_lower,
    alternative_ci_upper = ci_upper,
    alternative_interval_type,
    primary_log_rate_ratio,
    primary_standard_error,
    primary_p_value,
    primary_rate_ratio,
    primary_ci_lower,
    primary_ci_upper,
    delta_log_rate_ratio,
    direction_agrees_with_primary,
    interval_overlaps_primary,
    primary_ci_excludes_null,
    alternative_interval_excludes_null,
    ci_exclusion_changed,
    empirical_p_upper,
    empirical_p_two_sided_centered,
    empirical_p_upper_max_t,
    global_empirical_p_max_t,
    observations,
    wind_threshold_kt,
    analysis_version,
    source_commit,
    build_date,
    analysis_note
  )]
}

table_s08 <- rbindlist(list(
  select_harmonized(population_alternative),
  select_harmonized(covid_standard),
  select_harmonized(placebo_standard),
  select_harmonized(strict_standard),
  select_harmonized(icd10_standard),
  select_harmonized(overlap_standard)
), use.names = TRUE, fill = TRUE)
setorder(
  table_s08,
  analysis_order,
  specification_order,
  cause_order,
  age_order,
  period_order
)

table_s08_csv <- file.path(
  table_dir,
  "table_s08_alternative_specification_estimates.csv"
)
fwrite(table_s08, table_s08_csv, na = "")

table_s08_summary <- table_s08[, .(
  rows = .N,
  causes = uniqueN(cause),
  age_groups = uniqueN(age_group),
  periods = uniqueN(period),
  direction_evaluable = sum(!is.na(direction_agrees_with_primary)),
  direction_agreement = sum(direction_agrees_with_primary, na.rm = TRUE),
  interval_overlap_evaluable = sum(!is.na(interval_overlaps_primary)),
  interval_overlap = sum(interval_overlaps_primary, na.rm = TRUE),
  ci_exclusion_changes = sum(ci_exclusion_changed, na.rm = TRUE)
), by = .(analysis_order, analysis, specification_order, specification)]
setorder(table_s08_summary, analysis_order, specification_order)
table_s08_summary_csv <- file.path(
  table_dir,
  "table_s08_specification_summary.csv"
)
fwrite(table_s08_summary, table_s08_summary_csv)

data_dictionary <- data.table(
  variable = names(table_s08),
  description = c(
    "Display order for sensitivity-analysis families.",
    "Sensitivity-analysis family.",
    "Display order within the analysis family.",
    "Alternative model or negative-control specification.",
    "Frozen cause ordering.",
    "Mortality outcome.",
    "Frozen age-group ordering.",
    "Age group.",
    "Event-period ordering.",
    "Seven-day event-relative period.",
    "Alternative or permutation-null log rate ratio.",
    "Alternative-model standard error; blank for permutation summaries.",
    "Alternative-model raw p-value; blank for permutation summaries.",
    "Alternative rate ratio or permutation-null median.",
    "Alternative 95% CI lower bound or permutation 2.5th percentile.",
    "Alternative 95% CI upper bound or permutation 97.5th percentile.",
    "Meaning of the alternative interval columns.",
    "Frozen primary log rate ratio.",
    "Frozen primary standard error where available.",
    "Frozen primary raw p-value.",
    "Frozen primary rate ratio.",
    "Frozen primary 95% CI lower bound.",
    "Frozen primary 95% CI upper bound.",
    "Alternative minus primary log rate ratio.",
    "Whether alternative and primary point estimates share direction; not applicable to permutation summaries.",
    "Whether alternative and primary 95% CIs overlap; not applicable to permutation summaries.",
    "Whether the primary 95% CI excludes 1.",
    "Whether the alternative 95% CI excludes 1; not applicable to permutation summaries.",
    "Whether CI exclusion of 1 differs from primary; not applicable to permutation summaries.",
    "One-sided empirical permutation p-value.",
    "Centered two-sided empirical permutation p-value.",
    "Period-specific max-T permutation p-value.",
    "Global four-period max-T permutation p-value.",
    "Model observations where reported by the component analysis.",
    "Wind threshold in knots.",
    "Component analysis version.",
    "Source Git commit recorded by the component analysis.",
    "Component build date.",
    "Specification-specific interpretation note."
  )
)
data_dictionary_csv <- file.path(table_dir, "table_s08_data_dictionary.csv")
fwrite(data_dictionary, data_dictionary_csv)

caption_path <- file.path(table_dir, "figure_s05_caption.md")
writeLines(c(
  "# Figure S5. Alternative-specification analyses",
  "",
  paste0(
    "Panel A compares the frozen primary all-disease estimates with a no-offset count ",
    "model restricted to ZIP-years with positive annual population and with the ",
    "corresponding population-offset model. Panel B excludes observations from 2020 ",
    "through 2022. Panel C shows the legacy temporal and spatial placebo analyses. ",
    "Panel D shows empirical null distributions from 199 strict temporal placebo ",
    "calendars; orange lines are the frozen primary estimates and dashed lines are ",
    "permutation medians. Panel E restricts cause-specific models to the ICD-10 era ",
    "(1999-2022). Panel F excludes the 5,912 ZIP-dates appearing in more than one event ",
    "window before closest-impact assignment. Points and bars in panels A-C, E, and F ",
    "are rate ratios and 95% two-way cluster-robust confidence intervals. Complete ",
    "estimates are reported in Table S8."
  )
), caption_path)

table_notes_path <- file.path(table_dir, "table_s08_notes.md")
writeLines(c(
  "# Table S8. Alternative-specification estimates",
  "",
  paste0(
    "The table contains 452 rows across eight alternative specifications. Primary ",
    "comparators are repeated on each row to support direct machine-readable ",
    "comparison. The strict temporal permutation rows summarize the median and 2.5th ",
    "to 97.5th percentiles of the 199-calendar null distribution; those bounds are not ",
    "model confidence intervals. No sensitivity-specific multiplicity adjustment was ",
    "prespecified."
  )
), table_notes_path)

qa <- data.table(
  check_name = c(
    "Six Figure S5 panels assembled",
    "Figure S5 PDF and contact sheet written",
    "Table S8 contains 452 rows",
    "Table S8 contains eight specifications",
    "Table S8 contains the 39 harmonized fields",
    "All ordinary alternative rows have primary comparators",
    "Strict permutation rows contain all four periods",
    "Table S8 ordering is complete"
  ),
  status = c(
    if (length(figures) == 6L && length(panel_paths) == 6L) "PASS" else "FAIL",
    if (all(file.exists(c(figure_pdf, contact_sheet_png)))) "PASS" else "FAIL",
    if (nrow(table_s08) == 452L) "PASS" else "FAIL",
    if (uniqueN(table_s08$specification) == 8L) "PASS" else "FAIL",
    if (ncol(table_s08) == 39L && nrow(data_dictionary) == 39L)
      "PASS" else "FAIL",
    if (all(!is.na(table_s08[
      analysis != "Strict temporal permutation",
      primary_rate_ratio
    ]))) "PASS" else "FAIL",
    if (nrow(table_s08[analysis == "Strict temporal permutation"]) == 4L)
      "PASS" else "FAIL",
    if (all(!is.na(table_s08[, .(
      analysis_order,
      specification_order,
      cause_order,
      age_order,
      period_order
    )]))) "PASS" else "FAIL"
  ),
  observed = c(
    length(figures),
    sum(file.exists(c(figure_pdf, contact_sheet_png))),
    nrow(table_s08),
    uniqueN(table_s08$specification),
    paste(ncol(table_s08), nrow(data_dictionary), sep = "/"),
    sum(!is.na(table_s08[
      analysis != "Strict temporal permutation",
      primary_rate_ratio
    ])),
    nrow(table_s08[analysis == "Strict temporal permutation"]),
    sum(complete.cases(table_s08[, .(
      analysis_order,
      specification_order,
      cause_order,
      age_order,
      period_order
    )]))
  ),
  expected = c(6L, 2L, 452L, 8L, "39/39", 448L, 4L, 452L)
)
qa_path <- file.path(artifact_dir, "figure_s05_table_s08_qa.csv")
fwrite(qa, qa_path)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more Figure S5/Table S8 assembly checks failed.")
}

manifest_targets <- c(
  figure_pdf,
  contact_sheet_png,
  panel_paths,
  table_s08_csv,
  table_s08_summary_csv,
  data_dictionary_csv,
  caption_path,
  table_notes_path,
  qa_path,
  required_sources,
  "scripts/mortality/assemble_figure_s05_table_s08.R",
  "scripts/mortality/assemble_figure_s05_pdf.py",
  "scripts/mortality/build_table_s08_workbook.mjs"
)
workbook_path <- file.path(
  table_dir,
  "table_s08_alternative_specification_estimates.xlsx"
)
if (file.exists(workbook_path)) {
  manifest_targets <- c(manifest_targets, workbook_path)
}
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
manifest_path <- file.path(artifact_dir, "figure_s05_table_s08_manifest.csv")
fwrite(manifest, manifest_path)

cat("\nFigure S5 and Table S8 assembled\n")
cat("Figure S5 PDF:", figure_pdf, "\n")
cat("Figure S5 contact sheet:", contact_sheet_png, "\n")
cat("Table S8 CSV:", table_s08_csv, "\n")
cat("Assembly QA checks:", nrow(qa), "all PASS\n")
