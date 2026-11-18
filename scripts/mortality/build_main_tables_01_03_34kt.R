#!/usr/bin/env Rscript

source("scripts/mortality/paper_table_helpers.R")

table_scripts <- c(
  "scripts/mortality/paper_table_01_population_counts.R",
  "scripts/mortality/paper_table_02_all_disease.R",
  "scripts/mortality/paper_table_03_prioritized_age.R"
)

for (table_script in table_scripts) {
  source(table_script, local = new.env(parent = globalenv()))
}

table_files <- file.path(
  paper_table_dir,
  c(
    "table_01a_study_population_exposure.csv",
    "table_01b_mortality_counts.csv",
    "table_01_study_population_exposure_mortality.md",
    "table_02_all_disease_timing.csv",
    "table_02_all_disease_timing.md",
    "table_03_prioritized_causes_age.csv",
    "table_03_prioritized_causes_age.md"
  )
)

table1a <- fread(table_files[1L])
table1b <- fread(table_files[2L])
table2 <- fread(table_files[4L])
table3 <- fread(table_files[6L])

metric_value <- function(metric) {
  table1a[characteristic == metric, numeric_value]
}

priority_q_columns <- c(
  "q112_all_ages",
  "q112_under65",
  "q112_65_74",
  "q112_75plus"
)
priority_q_values <- unlist(table3[, ..priority_q_columns], use.names = FALSE)

qa <- data.table(
  check_name = c(
    "Seven table files written",
    "Table 1 has 12 design metrics",
    "Table 1 has eight retained outcomes",
    "Table 1 age counts reconcile with all ages",
    "Table 1 has 1,046 ZIP codes",
    "Table 1 modeled ZIP-day count",
    "Table 1 event ZIP-day count",
    "Table 2 has 16 unique estimates",
    "Table 2 has 14 primary FDR-supported results",
    "Table 3 has 16 cause-period rows",
    "Table 3 contains 64 conservative q-values",
    "Table 3 has 12 conservative FDR-supported results",
    "Table 3 has no FDR-supported age heterogeneity"
  ),
  status = c(
    if (all(file.exists(table_files))) "PASS" else "FAIL",
    if (nrow(table1a) == 12L) "PASS" else "FAIL",
    if (nrow(table1b) == 8L) "PASS" else "FAIL",
    if (all(
      table1b$deaths_under65 + table1b$deaths_65_74 + table1b$deaths_75plus ==
        table1b$deaths_all_ages
    )) "PASS" else "FAIL",
    if (metric_value("Florida ZIP codes") == 1046L) "PASS" else "FAIL",
    if (metric_value("Modeled ZIP-days") == 8516532L) "PASS" else "FAIL",
    if (metric_value("Event-window ZIP-days") == 374580L) "PASS" else "FAIL",
    if (nrow(table2) == 16L && uniqueN(table2$result_id) == 16L) "PASS" else "FAIL",
    if (sum(table2$primary_fdr_q_16 < 0.05) == 14L) "PASS" else "FAIL",
    if (nrow(table3) == 16L) "PASS" else "FAIL",
    if (length(priority_q_values) == 64L && all(is.finite(priority_q_values))) "PASS" else "FAIL",
    if (sum(priority_q_values < 0.05) == 12L) "PASS" else "FAIL",
    if (!any(table3$age_heterogeneity_q_28 < 0.05)) "PASS" else "FAIL"
  ),
  observed = c(
    sum(file.exists(table_files)),
    nrow(table1a),
    nrow(table1b),
    sum(
      table1b$deaths_under65 + table1b$deaths_65_74 + table1b$deaths_75plus ==
        table1b$deaths_all_ages
    ),
    metric_value("Florida ZIP codes"),
    metric_value("Modeled ZIP-days"),
    metric_value("Event-window ZIP-days"),
    paste0(nrow(table2), " rows; ", uniqueN(table2$result_id), " IDs"),
    sum(table2$primary_fdr_q_16 < 0.05),
    nrow(table3),
    length(priority_q_values),
    sum(priority_q_values < 0.05),
    sum(table3$age_heterogeneity_q_28 < 0.05)
  ),
  expected = c(
    7L,
    12L,
    8L,
    8L,
    1046L,
    8516532L,
    374580L,
    "16 rows; 16 IDs",
    14L,
    16L,
    64L,
    12L,
    0L
  )
)

qa_path <- file.path(paper_table_output_dir, "main_tables_01_03_qa.csv")
fwrite(qa, qa_path)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more main-table QA checks failed.")
}

caption_path <- file.path(
  paper_table_dir,
  "main_tables_01_03_captions.md"
)
writeLines(c(
  "# Main table captions",
  "",
  "## Table 1",
  paste0(
    "Study population, tropical-cyclone exposure, and mortality counts in the Florida ZIP-day ",
    "panel, 1985–2022. Exposure is defined by ZIP-specific sustained winds of at least 34 kt."
  ),
  "",
  "## Table 2",
  paste0(
    "All-disease mortality associations across four event-relative periods and four age groups. ",
    "The 16 q-values constitute the prespecified primary testing family."
  ),
  "",
  "## Table 3",
  paste0(
    "Prioritized cause-specific mortality associations and global age-heterogeneity tests. ",
    "Estimate q-values use the conservative 112-test retained cause-specific family; age-test ",
    "q-values use the 28-test retained cause-period family."
  )
), caption_path)

manifest_targets <- c(
  table_files,
  qa_path,
  caption_path,
  "scripts/mortality/paper_table_helpers.R",
  table_scripts,
  "scripts/mortality/build_main_tables_01_03_34kt.R",
  paper_table_master_path,
  "data/mortality/processed/death-panels/daily_panel_34kt.csv",
  "outputs/mortality/mortality_counts_by_cause_age_1985_2022.csv",
  paste0(
    "outputs/mortality/cause_age_event_period_models/",
    "all_diseases_age_event_period_sample_counts_34kt.csv"
  )
)
manifest_hashes <- system2(
  "shasum",
  c("-a", "256", manifest_targets),
  stdout = TRUE
)
manifest_split <- tstrsplit(manifest_hashes, "  ", fixed = TRUE)
manifest <- data.table(
  sha256 = manifest_split[[1L]],
  path = manifest_split[[2L]],
  bytes = as.numeric(file.info(manifest_split[[2L]])$size),
  artifact_version = paper_table_version,
  source_commit = get_paper_table_source_commit(),
  build_date = as.IDate("2026-11-18")
)
manifest_path <- file.path(
  paper_table_output_dir,
  "main_tables_01_03_manifest.csv"
)
fwrite(manifest, manifest_path)

cat("\nMain Tables 1-3 completed\n")
cat("Table directory:", paper_table_dir, "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
