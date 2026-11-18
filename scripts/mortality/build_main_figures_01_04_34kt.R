#!/usr/bin/env Rscript

source("scripts/mortality/paper_figure_helpers.R")

figure_scripts <- c(
  "scripts/mortality/paper_figure_01_design.R",
  "scripts/mortality/paper_figure_02_all_disease_timing.R",
  "scripts/mortality/paper_figure_03_cause_specific.R",
  "scripts/mortality/paper_figure_04_prioritized_age.R"
)

for (figure_script in figure_scripts) {
  source(figure_script, local = new.env(parent = globalenv()))
}

results <- read_paper_results()
figure_files <- file.path(
  paper_figure_dir,
  c(
    "figure_01_study_design.png",
    "figure_01_study_design.pdf",
    "figure_02_all_disease_timing.png",
    "figure_02_all_disease_timing.pdf",
    "figure_03_cause_specific.png",
    "figure_03_cause_specific.pdf",
    "figure_04_prioritized_age.png",
    "figure_04_prioritized_age.pdf"
  )
)

figure2_data <- results[cause == "All diseases"]
figure3_data <- results[cause != "All diseases" & age_group == "All ages"]
figure4_data <- results[
  cause %chin% c("Cardiovascular", "Respiratory", "Injuries", "Neuropsychiatric disorders") &
    age_group %chin% paper_age_specific_levels
]

qa <- data.table(
  check_name = c(
    "Eight figure files written",
    "Figure 2 has 16 estimates",
    "Figure 3 has 28 estimates",
    "Figure 4 has 48 estimates",
    "Figure 2 primary-family FDR markers",
    "Figure 3 amended 112-test FDR markers",
    "Figure 4 has 16 age-q annotations",
    "Figure 4 has no FDR-supported age heterogeneity"
  ),
  status = c(
    if (all(file.exists(figure_files))) "PASS" else "FAIL",
    if (nrow(figure2_data) == 16L) "PASS" else "FAIL",
    if (nrow(figure3_data) == 28L) "PASS" else "FAIL",
    if (nrow(figure4_data) == 48L) "PASS" else "FAIL",
    if (sum(figure2_data$main_fdr_q_within_analysis_tier < 0.05) == 14L) "PASS" else "FAIL",
    if (sum(figure3_data$main_fdr_q_all_cause_specific < 0.05) == 9L) "PASS" else "FAIL",
    if (uniqueN(figure4_data[, .(cause, period)]) == 16L) "PASS" else "FAIL",
    if (!any(figure4_data$age_heterogeneity_fdr_q_cause_specific < 0.05)) "PASS" else "FAIL"
  ),
  observed = c(
    sum(file.exists(figure_files)),
    nrow(figure2_data),
    nrow(figure3_data),
    nrow(figure4_data),
    sum(figure2_data$main_fdr_q_within_analysis_tier < 0.05),
    sum(figure3_data$main_fdr_q_all_cause_specific < 0.05),
    uniqueN(figure4_data[, .(cause, period)]),
    sum(figure4_data$age_heterogeneity_fdr_q_cause_specific < 0.05)
  ),
  expected = c(8L, 16L, 28L, 48L, 14L, 9L, 16L, 0L)
)

qa_path <- file.path(paper_figure_output_dir, "main_figures_01_04_qa.csv")
fwrite(qa, qa_path)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more main-figure QA checks failed.")
}

captions <- c(
  "# Main figure captions",
  "",
  "## Figure 1",
  paste0(
    "Study design and event-relative periods. Florida ZIP-day mortality from 1985 through 2022 was aligned ",
    "to ZIP-specific tropical-cyclone impact dates defined at sustained winds of at least 34 kt. Four ",
    "seven-day periods preserved event-relative timing; all retained non-event ZIP-days formed the reference."
  ),
  "",
  "## Figure 2",
  paste0(
    "All-disease mortality relative to ZIP-specific impact. Points are fixed-effects Poisson rate ratios and ",
    "horizontal lines are 95% two-way cluster-robust confidence intervals. Filled points satisfy the ",
    "Benjamini-Hochberg q<0.05 criterion across the 16 primary tests."
  ),
  "",
  "## Figure 3",
  paste0(
    "All-age cause-specific mortality across the four event-relative periods for the seven outcomes retained ",
    "under amendment A01. Color distinguishes the ",
    "prioritized secondary and exploratory families. Filled points satisfy the conservative ",
    "Benjamini-Hochberg q<0.05 criterion across all 112 retained cause-specific tests."
  ),
  "",
  "## Figure 4",
  paste0(
    "Age-specific associations for the four prioritized causes. Points and confidence intervals are from ",
    "separate age-specific models; displayed q-values are from global two-degree-of-freedom age-heterogeneity ",
    "tests in jointly fitted stacked-age models with FDR correction across 28 retained cause-period tests."
  )
)
caption_path <- file.path(paper_figure_output_dir, "main_figures_01_04_captions.md")
writeLines(captions, caption_path)

manifest_targets <- c(
  figure_files,
  qa_path,
  caption_path,
  "scripts/mortality/paper_figure_helpers.R",
  figure_scripts,
  "scripts/mortality/build_main_figures_01_04_34kt.R",
  paper_results_path
)
manifest_hashes <- system2("shasum", c("-a", "256", manifest_targets), stdout = TRUE)
manifest_split <- tstrsplit(manifest_hashes, "  ", fixed = TRUE)
manifest <- data.table(
  sha256 = manifest_split[[1L]],
  path = manifest_split[[2L]],
  bytes = as.numeric(file.info(manifest_split[[2L]])$size),
  artifact_version = paper_figure_version,
  freeze_date = as.IDate("2026-11-18"),
  build_date = as.IDate("2026-11-18")
)
fwrite(manifest, file.path(paper_figure_output_dir, "main_figures_01_04_manifest.csv"))

cat("\nMain Figures 1-4 completed\n")
cat("Figure directory:", paper_figure_dir, "\n")
cat("QA checks:", nrow(qa), "all PASS\n")

