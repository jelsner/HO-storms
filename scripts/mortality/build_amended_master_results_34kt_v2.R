#!/usr/bin/env Rscript

# Outcome-blind protocol amendment requested by the PI on 2026-11-18:
# exclude digestive diseases, diabetes, and endocrine/nutritional/metabolic
# disorders, then recompute every affected Benjamini-Hochberg family.

suppressPackageStartupMessages(library(data.table))

amendment_date <- as.IDate("2026-11-18")
analysis_version <- "amended_v2_2026-11-18"
parent_analysis_version <- "frozen_v1_2026-11-17"
parent_dir <- file.path("outputs/mortality/paper_master_34kt", parent_analysis_version)
output_dir <- file.path("outputs/mortality/paper_master_34kt", analysis_version)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

parent_master_path <- file.path(
  parent_dir,
  "master_results_34kt_frozen_v1.csv"
)
parent_contrast_path <- file.path(
  parent_dir,
  "age_interaction_contrasts_34kt_frozen_v1.csv"
)
parent_diagnostic_path <- file.path(
  parent_dir,
  "model_diagnostics_34kt_frozen_v1.csv"
)

excluded_causes <- c(
  "Digestive diseases",
  "Diabetes",
  "Endocrine, nutritional, and metabolic disorders"
)
retained_causes <- c(
  "All diseases",
  "Cardiovascular",
  "Respiratory",
  "Infectious diseases",
  "Injuries",
  "Neuropsychiatric disorders",
  "Renal diseases",
  "Neoplasms"
)

required_inputs <- c(
  parent_master_path,
  parent_contrast_path,
  parent_diagnostic_path
)
if (any(!file.exists(required_inputs))) {
  stop("Missing parent freeze inputs: ", paste(required_inputs[!file.exists(required_inputs)], collapse = ", "))
}

get_source_commit <- function() {
  bundled_git <- paste0(
    "/Users/jelsner/.cache/codex-runtimes/codex-primary-runtime/",
    "dependencies/bin/fallback/git"
  )
  git <- if (file.exists(bundled_git)) bundled_git else Sys.which("git")
  result <- tryCatch(
    system2(git, c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) NA_character_
  )
  if (!length(result)) NA_character_ else result[1L]
}

message("Reading immutable frozen-v1 inputs")
parent_master <- fread(parent_master_path)
parent_contrasts <- fread(parent_contrast_path)
parent_diagnostics <- fread(parent_diagnostic_path)
amendment_source_commit <- get_source_commit()

if (nrow(parent_master) != 176L || uniqueN(parent_master$result_id) != 176L) {
  stop("Parent master does not contain the expected 176 unique results.")
}

message("Filtering the three PI-excluded categories")
master <- copy(parent_master[cause %chin% retained_causes])
contrasts <- copy(parent_contrasts[cause %chin% retained_causes])
diagnostics <- copy(parent_diagnostics[cause %chin% retained_causes])

v1_q_snapshot <- parent_master[cause %chin% retained_causes, .(
  result_id,
  v1_main_fdr_q_within_analysis_tier = main_fdr_q_within_analysis_tier,
  v1_main_fdr_q_all_cause_specific = main_fdr_q_all_cause_specific,
  v1_age_heterogeneity_fdr_q_cause_specific =
    age_heterogeneity_fdr_q_cause_specific
)]

message("Recomputing amended Benjamini-Hochberg families")
master[, main_fdr_q_within_analysis_tier :=
  p.adjust(p_value, method = "BH"),
  by = analysis_tier]
master[, main_fdr_q_all_cause_specific := NA_real_]
master[cause != "All diseases", main_fdr_q_all_cause_specific :=
  p.adjust(p_value, method = "BH")]

# Pairwise contrasts: 96 overall, 84 cause-specific, and 56 focused
# older-versus-under-65 comparisons.
contrasts[, c(
  "contrast_fdr_q_all_132",
  "contrast_fdr_q_all_cause_specific",
  "contrast_fdr_q_age_focus",
  "age_heterogeneity_fdr_q_all_44",
  "age_heterogeneity_fdr_q_cause_specific"
) := NULL]
contrasts[, contrast_fdr_q_all_96 := p.adjust(
  contrast_p_value,
  method = "BH"
)]
contrasts[, contrast_fdr_q_all_cause_specific := NA_real_]
contrasts[cause != "All diseases", contrast_fdr_q_all_cause_specific :=
  p.adjust(contrast_p_value, method = "BH")]
contrasts[, contrast_fdr_q_age_focus_56 := NA_real_]
contrasts[
  cause != "All diseases" & reference_group == "<65",
  contrast_fdr_q_age_focus_56 := p.adjust(contrast_p_value, method = "BH")
]

# Global age tests are represented once per cause-period before adjustment.
heterogeneity <- unique(contrasts[, .(
  cause,
  period,
  age_heterogeneity_chisq,
  age_heterogeneity_df,
  age_heterogeneity_p_value
)])
heterogeneity[, age_heterogeneity_fdr_q_all_32 := p.adjust(
  age_heterogeneity_p_value,
  method = "BH"
)]
heterogeneity[, age_heterogeneity_fdr_q_cause_specific := NA_real_]
heterogeneity[cause != "All diseases", age_heterogeneity_fdr_q_cause_specific :=
  p.adjust(age_heterogeneity_p_value, method = "BH")]

contrasts <- merge(
  contrasts,
  heterogeneity[, .(
    cause,
    period,
    age_heterogeneity_fdr_q_all_32,
    age_heterogeneity_fdr_q_cause_specific
  )],
  by = c("cause", "period"),
  all.x = TRUE,
  sort = FALSE
)

# Replace the count-specific v1 contrast and heterogeneity q-value fields in
# the master table with accurately named amended-family fields.
master[, c(
  "age_vs_under65_fdr_q_all_132",
  "age_vs_under65_fdr_q_all_cause_specific",
  "age_vs_under65_fdr_q_age_focus_80",
  "age_heterogeneity_fdr_q_all_44",
  "age_heterogeneity_fdr_q_cause_specific"
) := NULL]

older_q <- contrasts[
  reference_group == "<65" & comparison_group %chin% c("65-74", "75+"),
  .(
    cause,
    age_group = comparison_group,
    period,
    age_vs_under65_fdr_q_all_96 = contrast_fdr_q_all_96,
    age_vs_under65_fdr_q_all_cause_specific =
      contrast_fdr_q_all_cause_specific,
    age_vs_under65_fdr_q_age_focus_56 = contrast_fdr_q_age_focus_56
  )
]
master <- merge(
  master,
  older_q,
  by = c("cause", "age_group", "period"),
  all.x = TRUE,
  sort = FALSE
)
master <- merge(
  master,
  heterogeneity[, .(
    cause,
    period,
    age_heterogeneity_fdr_q_all_32,
    age_heterogeneity_fdr_q_cause_specific
  )],
  by = c("cause", "period"),
  all.x = TRUE,
  sort = FALSE
)

master[, `:=`(
  parent_analysis_version = parent_analysis_version,
  parent_freeze_date = as.IDate("2026-11-17"),
  analysis_version = "amended_v2_2026-11-18",
  freeze_date = amendment_date,
  amendment_id = "A01_PI_OUTCOME_SCOPE",
  amendment_date = amendment_date,
  amendment_source_commit = amendment_source_commit,
  multiplicity_scope = paste0(
    "16 primary; 64 prioritized; 48 exploratory; 112 all cause-specific; ",
    "32 all-outcome and 28 cause-specific global age tests; ",
    "96 all, 84 cause-specific, and 56 focused pairwise age contrasts"
  )
)]
contrasts[, `:=`(
  parent_analysis_version = parent_analysis_version,
  parent_freeze_date = as.IDate("2026-11-17"),
  analysis_version = "amended_v2_2026-11-18",
  freeze_date = amendment_date,
  amendment_id = "A01_PI_OUTCOME_SCOPE",
  amendment_date = amendment_date,
  amendment_source_commit = amendment_source_commit
)]
diagnostics[, `:=`(
  parent_analysis_version = parent_analysis_version,
  analysis_version = "amended_v2_2026-11-18",
  freeze_date = amendment_date,
  amendment_id = "A01_PI_OUTCOME_SCOPE"
)]

setorder(master, cause_order, age_order, period_order)
setorder(contrasts, cause_order, period_order, comparison_order)

message("Building old-versus-new q-value and spatial-target comparisons")
q_changes <- merge(
  master[, .(
    result_id,
    cause,
    age_group,
    period,
    analysis_tier,
    rate_ratio,
    ci_lower,
    ci_upper,
    event_deaths,
    amended_main_fdr_q_within_analysis_tier =
      main_fdr_q_within_analysis_tier,
    amended_main_fdr_q_all_cause_specific =
      main_fdr_q_all_cause_specific,
    amended_age_heterogeneity_fdr_q_cause_specific =
      age_heterogeneity_fdr_q_cause_specific
  )],
  v1_q_snapshot,
  by = "result_id",
  all.x = TRUE,
  sort = FALSE
)
q_changes[, `:=`(
  v1_all_cause_discovery =
    !is.na(v1_main_fdr_q_all_cause_specific) &
    v1_main_fdr_q_all_cause_specific < 0.05,
  amended_all_cause_discovery =
    !is.na(amended_main_fdr_q_all_cause_specific) &
    amended_main_fdr_q_all_cause_specific < 0.05
)]
q_changes[, discovery_change := fcase(
  !v1_all_cause_discovery & amended_all_cause_discovery, "Added under 112-test family",
  v1_all_cause_discovery & !amended_all_cause_discovery, "Removed under 112-test family",
  v1_all_cause_discovery & amended_all_cause_discovery, "Retained discovery",
  default = "Not FDR-supported"
)]
setorder(q_changes, cause, age_group, period)

spatial_eligibility <- q_changes[
  cause != "All diseases" & age_group %chin% c("65-74", "75+") &
    rate_ratio > 1 & ci_lower > 1 & event_deaths >= 500
]
spatial_eligibility[, `:=`(
  v1_spatial_eligible = v1_main_fdr_q_all_cause_specific < 0.05,
  amended_spatial_eligible = amended_main_fdr_q_all_cause_specific < 0.05
)]
spatial_eligibility <- spatial_eligibility[
  v1_spatial_eligible | amended_spatial_eligible
]
spatial_eligibility[, spatial_target_change := fcase(
  !v1_spatial_eligible & amended_spatial_eligible, "Added under amendment",
  v1_spatial_eligible & !amended_spatial_eligible, "Removed under amendment",
  default = "Retained target"
)]
setorder(spatial_eligibility, cause, age_group, period)

message("Running amended-freeze QA")
qa <- data.table(
  check_name = c(
    "Amended master row count",
    "Retained outcome count",
    "Excluded causes absent",
    "Primary family size",
    "Prioritized family size",
    "Exploratory family size",
    "All cause-specific family size",
    "Pairwise age-contrast family size",
    "Cause-specific pairwise family size",
    "Focused older-versus-younger family size",
    "Global age-test family size",
    "Cause-specific global age-test family size",
    "Point estimates unchanged from v1",
    "Primary q-values unchanged from v1",
    "Prioritized tier q-values unchanged from v1",
    "Amended 112-test discoveries",
    "Amended spatial targets",
    "Added spatial targets",
    "No FDR-supported global age heterogeneity",
    "All amended q-values in [0,1]"
  ),
  status = "PASS",
  observed = c(
    nrow(master),
    uniqueN(master$cause),
    sum(master$cause %chin% excluded_causes),
    nrow(master[analysis_tier == "Primary"]),
    nrow(master[analysis_tier == "Prioritized secondary"]),
    nrow(master[analysis_tier == "Exploratory"]),
    nrow(master[cause != "All diseases"]),
    nrow(contrasts),
    nrow(contrasts[cause != "All diseases"]),
    nrow(contrasts[cause != "All diseases" & reference_group == "<65"]),
    nrow(heterogeneity),
    nrow(heterogeneity[cause != "All diseases"]),
    as.integer(max(abs(
      master$rate_ratio -
        parent_master[match(master$result_id, result_id), rate_ratio]
    )) < 1e-12),
    as.integer(max(abs(
      master[analysis_tier == "Primary"]$main_fdr_q_within_analysis_tier -
        parent_master[
          match(master[analysis_tier == "Primary"]$result_id, result_id),
          main_fdr_q_within_analysis_tier
        ]
    )) < 1e-12),
    as.integer(max(abs(
      master[analysis_tier == "Prioritized secondary"]$main_fdr_q_within_analysis_tier -
        parent_master[
          match(master[analysis_tier == "Prioritized secondary"]$result_id, result_id),
          main_fdr_q_within_analysis_tier
        ]
    )) < 1e-12),
    sum(master$main_fdr_q_all_cause_specific < 0.05, na.rm = TRUE),
    sum(spatial_eligibility$amended_spatial_eligible),
    sum(spatial_eligibility$spatial_target_change == "Added under amendment"),
    sum(heterogeneity$age_heterogeneity_fdr_q_cause_specific < 0.05, na.rm = TRUE),
    {
      q_columns <- grep("fdr_q", names(master), value = TRUE)
      q_values <- unlist(master[, ..q_columns], use.names = FALSE)
      sum(q_values >= 0 & q_values <= 1, na.rm = TRUE)
    }
  ),
  expected = c(
    128L, 8L, 0L, 16L, 64L, 48L, 112L, 96L, 84L, 56L,
    32L, 28L, 1L, 1L, 1L, 18L, 8L, 3L, 0L,
    {
      q_columns <- grep("fdr_q", names(master), value = TRUE)
      sum(!is.na(unlist(master[, ..q_columns], use.names = FALSE)))
    }
  )
)
qa[, status := fifelse(
  as.character(observed) == as.character(expected),
  "PASS",
  "FAIL"
)]
if (any(qa$status != "PASS")) {
  print(qa[status != "PASS"])
  stop("One or more amended-freeze QA checks failed.")
}

audit <- data.table(
  item = c(
    "Analysis version",
    "Parent analysis version",
    "Amendment date",
    "Decision maker",
    "Outcome-blind status",
    "Excluded categories",
    "Retained cause-specific categories",
    "Unchanged families",
    "Recomputed main-effect families",
    "Recomputed age-test families",
    "Spatial eligibility consequence",
    "Generating script"
  ),
  value = c(
    analysis_version,
    parent_analysis_version,
    as.character(amendment_date),
    "Principal investigator",
    "PI had not seen the results when requesting the scope change; analyst had access to frozen v1",
    paste(excluded_causes, collapse = "; "),
    paste(retained_causes[-1L], collapse = "; "),
    "16-test all-disease primary family and 64-test prioritized family",
    "48-test exploratory family and 112-test all-cause-specific family",
    "32 all-outcome and 28 cause-specific global tests; 96 all, 84 cause-specific, and 56 focused pairwise contrasts",
    "Five frozen-v1 targets retained and three targets added; amended total = eight",
    "scripts/mortality/build_amended_master_results_34kt_v2.R"
  )
)

dictionary <- data.table(
  table_name = "master_results_34kt_amended_v2",
  column_name = names(master),
  storage_type = vapply(master, function(x) class(x)[1L], character(1L)),
  description = "Inherited unchanged from frozen v1 unless identified as an amendment or recomputed q-value field."
)
dictionary[column_name == "main_fdr_q_within_analysis_tier", description :=
  "BH q-value within the amended Primary (16), Prioritized (64), or Exploratory (48) family."]
dictionary[column_name == "main_fdr_q_all_cause_specific", description :=
  "BH q-value across the 112 retained cause-specific main-effect tests."]
dictionary[column_name == "age_heterogeneity_fdr_q_all_32", description :=
  "BH q-value across 32 retained all-outcome global age-heterogeneity tests."]
dictionary[column_name == "age_heterogeneity_fdr_q_cause_specific", description :=
  "BH q-value across 28 retained cause-specific global age-heterogeneity tests."]
dictionary[column_name == "age_vs_under65_fdr_q_all_96", description :=
  "BH q-value across all 96 retained pairwise age contrasts."]
dictionary[column_name == "age_vs_under65_fdr_q_all_cause_specific", description :=
  "BH q-value across 84 retained cause-specific pairwise age contrasts."]
dictionary[column_name == "age_vs_under65_fdr_q_age_focus_56", description :=
  "BH q-value across 56 retained cause-specific older-versus-under-65 contrasts."]

master_path <- file.path(output_dir, "master_results_34kt_amended_v2.csv")
contrast_path <- file.path(output_dir, "age_interaction_contrasts_34kt_amended_v2.csv")
diagnostic_path <- file.path(output_dir, "model_diagnostics_34kt_amended_v2.csv")
q_change_path <- file.path(output_dir, "q_value_changes_34kt_amended_v2.csv")
spatial_change_path <- file.path(output_dir, "spatial_target_changes_34kt_amended_v2.csv")
qa_path <- file.path(output_dir, "qa_checks_34kt_amended_v2.csv")
audit_path <- file.path(output_dir, "analysis_audit_34kt_amended_v2.csv")
dictionary_path <- file.path(output_dir, "data_dictionary_34kt_amended_v2.csv")

fwrite(master, master_path)
fwrite(contrasts, contrast_path)
fwrite(diagnostics, diagnostic_path)
fwrite(q_changes, q_change_path)
fwrite(spatial_eligibility, spatial_change_path)
fwrite(qa, qa_path)
fwrite(audit, audit_path)
fwrite(dictionary, dictionary_path)

manifest_targets <- c(
  master_path,
  contrast_path,
  diagnostic_path,
  q_change_path,
  spatial_change_path,
  qa_path,
  audit_path,
  dictionary_path,
  "scripts/mortality/build_amended_master_results_34kt_v2.R",
  parent_master_path,
  parent_contrast_path,
  parent_diagnostic_path
)
hash_output <- system2("shasum", c("-a", "256", manifest_targets), stdout = TRUE)
manifest_split <- tstrsplit(hash_output, "  ", fixed = TRUE)
manifest <- data.table(
  sha256 = manifest_split[[1L]],
  path = manifest_split[[2L]],
  bytes = as.numeric(file.info(manifest_split[[2L]])$size),
  analysis_version = analysis_version,
  amendment_date = amendment_date
)
fwrite(manifest, file.path(output_dir, "freeze_manifest_34kt_amended_v2.csv"))

cat("\nAmended master results completed\n")
cat("Directory:", output_dir, "\n")
cat("Master rows:", nrow(master), "\n")
cat("112-test discoveries:", sum(master$main_fdr_q_all_cause_specific < 0.05, na.rm = TRUE), "\n")
cat("Spatial targets:", sum(spatial_eligibility$amended_spatial_eligible), "\n")
cat("QA checks:", nrow(qa), "all PASS\n")

