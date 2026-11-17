#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(data.table))

wind_threshold <- 34L
freeze_date <- as.IDate("2026-11-17")
analysis_version <- "frozen_v1_2026-11-17"
source_commit <- "804766c19ff320481142629ee5a0e7dfa0b362a9"

model_output_dir <- "outputs/mortality/cause_age_event_period_models"
interaction_dir <- "outputs/mortality/paper_master_34kt/age_interactions"
freeze_dir <- file.path(
  "outputs/mortality/paper_master_34kt",
  analysis_version
)
dir.create(freeze_dir, recursive = TRUE, showWarnings = FALSE)

cause_order <- c(
  "All diseases",
  "Cardiovascular",
  "Respiratory",
  "Infectious diseases",
  "Injuries",
  "Neuropsychiatric disorders",
  "Renal diseases",
  "Digestive diseases",
  "Diabetes",
  "Endocrine, nutritional, and metabolic disorders",
  "Neoplasms"
)
age_order <- c("All ages", "<65", "65-74", "75+")
period_order <- c(
  "Days -14 to -8",
  "Days -7 to -1",
  "Days 0 to +6",
  "Days +7 to +13"
)
prioritized_causes <- c(
  "Cardiovascular",
  "Respiratory",
  "Injuries",
  "Neuropsychiatric disorders"
)

cause_slug <- function(x) {
  gsub("_+$", "", gsub("[^A-Za-z0-9]+", "_", tolower(x)))
}

read_cause_file <- function(cause_name, suffix) {
  path <- file.path(
    model_output_dir,
    paste0(cause_slug(cause_name), suffix)
  )
  result <- fread(path)
  result[, cause := cause_name]
  result
}

message("Reading the 11 cause-specific estimate and count files")
estimates <- rbindlist(lapply(
  cause_order,
  read_cause_file,
  suffix = "_age_event_period_estimates_34kt.csv"
), use.names = TRUE, fill = TRUE)

counts <- rbindlist(lapply(
  cause_order,
  read_cause_file,
  suffix = "_age_event_period_sample_counts_34kt.csv"
), use.names = TRUE, fill = TRUE)
setnames(counts, "event_period", "period")
counts <- counts[period %chin% period_order]
setnames(
  counts,
  c("zip_days", "deaths", "nonzero_zip_days"),
  c("event_zip_days", "event_deaths", "event_nonzero_zip_days")
)

joint_estimates <- fread(file.path(
  interaction_dir,
  "all_age_specific_estimates.csv"
))
contrasts <- fread(file.path(interaction_dir, "all_age_contrasts.csv"))
heterogeneity <- fread(file.path(
  interaction_dir,
  "all_age_heterogeneity_tests.csv"
))
diagnostics <- fread(file.path(
  interaction_dir,
  "all_interaction_diagnostics.csv"
))

message("Applying explicitly defined Benjamini-Hochberg FDR families")
estimates[, analysis_tier := fcase(
  cause == "All diseases", "Primary",
  cause %chin% prioritized_causes, "Prioritized secondary",
  default = "Exploratory"
)]
estimates[, main_fdr_q_within_analysis_tier :=
  p.adjust(p_value, method = "BH"),
  by = analysis_tier]
estimates[, main_fdr_q_all_cause_specific := NA_real_]
estimates[cause != "All diseases", main_fdr_q_all_cause_specific :=
  p.adjust(p_value, method = "BH")]

contrasts[, contrast_fdr_q_all_132 := p.adjust(
  contrast_p_value,
  method = "BH"
)]
contrasts[, contrast_fdr_q_all_cause_specific := NA_real_]
contrasts[cause != "All diseases", contrast_fdr_q_all_cause_specific :=
  p.adjust(contrast_p_value, method = "BH")]
contrasts[, contrast_fdr_q_age_focus := NA_real_]
contrasts[
  cause != "All diseases" & reference_group == "<65",
  contrast_fdr_q_age_focus := p.adjust(contrast_p_value, method = "BH")
]

heterogeneity[, age_heterogeneity_fdr_q_all_44 := p.adjust(
  age_heterogeneity_p_value,
  method = "BH"
)]
heterogeneity[, age_heterogeneity_fdr_q_cause_specific := NA_real_]
heterogeneity[cause != "All diseases",
  age_heterogeneity_fdr_q_cause_specific := p.adjust(
    age_heterogeneity_p_value,
    method = "BH"
  )]

message("Merging estimates, event-period counts, interactions, and diagnostics")
master <- merge(
  estimates,
  counts[, .(
    cause,
    group,
    period,
    event_zip_days,
    event_deaths,
    event_nonzero_zip_days
  )],
  by = c("cause", "group", "period"),
  all.x = TRUE,
  sort = FALSE
)

master <- merge(
  master,
  joint_estimates,
  by = c("cause", "group", "period"),
  all.x = TRUE,
  sort = FALSE
)

older_vs_under65 <- contrasts[
  reference_group == "<65" & comparison_group %chin% c("65-74", "75+")
]
setnames(older_vs_under65, "comparison_group", "group")
setnames(
  older_vs_under65,
  c(
    "contrast_log_rate_ratio",
    "contrast_standard_error",
    "contrast_ratio_of_rate_ratios",
    "contrast_ci_lower",
    "contrast_ci_upper",
    "contrast_z",
    "contrast_p_value",
    "contrast_fdr_q_all_132",
    "contrast_fdr_q_all_cause_specific",
    "contrast_fdr_q_age_focus"
  ),
  paste0(
    "age_vs_under65_",
    c(
      "log_ratio_of_rate_ratios",
      "standard_error",
      "ratio_of_rate_ratios",
      "ci_lower",
      "ci_upper",
      "z_value",
      "p_value",
      "fdr_q_all_132",
      "fdr_q_all_cause_specific",
      "fdr_q_age_focus_80"
    )
  )
)
older_vs_under65[, reference_group := NULL]

master <- merge(
  master,
  older_vs_under65,
  by = c("cause", "group", "period"),
  all.x = TRUE,
  sort = FALSE
)
master <- merge(
  master,
  heterogeneity,
  by = c("cause", "period"),
  all.x = TRUE,
  sort = FALSE
)

diagnostic_columns <- setdiff(names(diagnostics), "wind_threshold_kt")
setnames(
  diagnostics,
  diagnostic_columns[diagnostic_columns != "cause"],
  paste0("joint_model_", diagnostic_columns[diagnostic_columns != "cause"])
)
master <- merge(master, diagnostics, by = "cause", all.x = TRUE, sort = FALSE)

master[, `:=`(
  analysis_version = analysis_version,
  freeze_date = freeze_date,
  source_commit = source_commit,
  wind_threshold_kt = wind_threshold,
  cause_order = match(cause, cause_order),
  age_order = match(group, age_order),
  period_order = match(period, period_order),
  model_family = "Fixed-effects Poisson",
  model_formula_summary =
    "deaths ~ event_period | ZIP + ISO year-week + day-of-week + ZIP x COVID",
  cluster_summary = "Two-way clustered by ZIP and ISO year-week",
  reference_period = "All retained non-event days"
)]
master[, result_id := sprintf(
  "34KT-C%02d-A%02d-P%02d",
  cause_order,
  age_order,
  period_order
)]
master[, joint_point_estimate_match := fifelse(
  group == "All ages",
  NA,
  abs(log_rate_ratio - joint_log_rate_ratio) < 1e-10
)]
master[, estimate_is_finite :=
  is.finite(log_rate_ratio) & is.finite(standard_error) &
  is.finite(rate_ratio) & is.finite(ci_lower) & is.finite(ci_upper)]
master[, confidence_interval_ordered :=
  ci_lower <= rate_ratio & rate_ratio <= ci_upper]

setnames(master, "group", "age_group")
setcolorder(master, c(
  "analysis_version",
  "freeze_date",
  "source_commit",
  "wind_threshold_kt",
  "result_id",
  "analysis_tier",
  "cause",
  "cause_order",
  "age_group",
  "age_order",
  "period",
  "period_order",
  "model_family",
  "model_formula_summary",
  "cluster_summary",
  "reference_period",
  "log_rate_ratio",
  "standard_error",
  "p_value",
  "main_fdr_q_within_analysis_tier",
  "main_fdr_q_all_cause_specific",
  "rate_ratio",
  "ci_lower",
  "ci_upper",
  "percent_change",
  "observations",
  "event_zip_days",
  "event_deaths",
  "event_nonzero_zip_days",
  "joint_log_rate_ratio",
  "joint_standard_error",
  "joint_rate_ratio",
  "joint_ci_lower",
  "joint_ci_upper",
  "joint_point_estimate_match",
  "age_vs_under65_log_ratio_of_rate_ratios",
  "age_vs_under65_standard_error",
  "age_vs_under65_ratio_of_rate_ratios",
  "age_vs_under65_ci_lower",
  "age_vs_under65_ci_upper",
  "age_vs_under65_z_value",
  "age_vs_under65_p_value",
  "age_vs_under65_fdr_q_all_132",
  "age_vs_under65_fdr_q_all_cause_specific",
  "age_vs_under65_fdr_q_age_focus_80",
  "age_heterogeneity_chisq",
  "age_heterogeneity_df",
  "age_heterogeneity_p_value",
  "age_heterogeneity_fdr_q_all_44",
  "age_heterogeneity_fdr_q_cause_specific",
  grep("^joint_model_", names(master), value = TRUE),
  "estimate_is_finite",
  "confidence_interval_ordered"
))
setorder(master, cause_order, age_order, period_order)

message("Running freeze acceptance checks")
qa <- data.table(
  check_name = character(),
  status = character(),
  observed = character(),
  expected = character(),
  details = character()
)
add_check <- function(name, pass, observed, expected, details = "") {
  qa <<- rbind(
    qa,
    data.table(
      check_name = name,
      status = if (isTRUE(pass)) "PASS" else "FAIL",
      observed = as.character(observed),
      expected = as.character(expected),
      details = details
    )
  )
}

add_check("Master row count", nrow(master) == 176L, nrow(master), 176L)
add_check(
  "Unique result IDs",
  !anyDuplicated(master$result_id),
  uniqueN(master$result_id),
  176L
)
add_check("Cause count", uniqueN(master$cause) == 11L, uniqueN(master$cause), 11L)
add_check(
  "Age-group count",
  uniqueN(master$age_group) == 4L,
  uniqueN(master$age_group),
  4L
)
add_check(
  "Event-period count",
  uniqueN(master$period) == 4L,
  uniqueN(master$period),
  4L
)
add_check(
  "Finite main estimates",
  all(master$estimate_is_finite),
  sum(master$estimate_is_finite),
  176L
)
add_check(
  "Ordered main confidence intervals",
  all(master$confidence_interval_ordered),
  sum(master$confidence_interval_ordered),
  176L
)
add_check(
  "Joint age-estimate row count",
  nrow(joint_estimates) == 132L,
  nrow(joint_estimates),
  132L
)
add_check(
  "Pairwise age-contrast row count",
  nrow(contrasts) == 132L,
  nrow(contrasts),
  132L
)
add_check(
  "Global age-test row count",
  nrow(heterogeneity) == 44L,
  nrow(heterogeneity),
  44L
)
add_check(
  "Interaction diagnostic row count",
  nrow(diagnostics) == 11L,
  nrow(diagnostics),
  11L
)
add_check(
  "All joint models converged",
  all(diagnostics$joint_model_converged),
  sum(diagnostics$joint_model_converged),
  11L
)

age_master <- master[age_group != "All ages"]
max_beta_difference <- max(abs(
  age_master$log_rate_ratio - age_master$joint_log_rate_ratio
))
add_check(
  "Joint and separate point estimates reconcile",
  max_beta_difference < 1e-10,
  format(max_beta_difference, scientific = TRUE, digits = 4L),
  "< 1e-10",
  "All 132 age-specific log rate ratios"
)

count_reconciliation <- master[, .(
  all_age_deaths = event_deaths[age_group == "All ages"],
  summed_age_deaths = sum(event_deaths[age_group != "All ages"])
), by = .(cause, period)]
max_count_difference <- max(abs(
  count_reconciliation$all_age_deaths -
    count_reconciliation$summed_age_deaths
))
add_check(
  "All-age deaths equal age-stratum sum",
  max_count_difference == 0,
  max_count_difference,
  0,
  "Checked within all 44 cause-period cells"
)

q_columns <- grep("fdr_q", names(master), value = TRUE)
q_values <- unlist(master[, ..q_columns], use.names = FALSE)
q_values <- q_values[!is.na(q_values)]
add_check(
  "Master FDR q-values in [0,1]",
  all(q_values >= 0 & q_values <= 1),
  paste0("range ", paste(signif(range(q_values), 5L), collapse = " to ")),
  "0 to 1"
)

age_focus_n <- contrasts[
  cause != "All diseases" & reference_group == "<65",
  sum(!is.na(contrast_fdr_q_age_focus))
]
add_check(
  "Age-focused interaction FDR family size",
  age_focus_n == 80L,
  age_focus_n,
  80L,
  "10 causes x 4 periods x 2 older-vs-<65 contrasts"
)

if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more freeze acceptance checks failed.")
}

message("Building diagnostics, dictionary, and audit tables")
separate_observations <- dcast(
  estimates,
  cause ~ group,
  value.var = "observations",
  fun.aggregate = max
)
setnames(
  separate_observations,
  age_order,
  paste0(
    "separate_model_observations_",
    c("all_ages", "under65", "65_74", "75plus")
  )
)
diagnostic_output <- merge(
  diagnostics,
  separate_observations,
  by = "cause",
  all.x = TRUE,
  sort = FALSE
)
diagnostic_output[, `:=`(
  analysis_version = analysis_version,
  freeze_date = freeze_date,
  source_commit = source_commit
)]
diagnostic_output[, diagnostic_cause_order := match(cause, cause_order)]
setorder(diagnostic_output, diagnostic_cause_order)
diagnostic_output[, diagnostic_cause_order := NULL]
setcolorder(diagnostic_output, c(
  "analysis_version", "freeze_date", "source_commit", "cause",
  setdiff(names(diagnostic_output), c(
    "analysis_version", "freeze_date", "source_commit", "cause"
  ))
))

interaction_output <- merge(
  contrasts,
  heterogeneity,
  by = c("cause", "period"),
  all.x = TRUE,
  sort = FALSE
)
interaction_output[, `:=`(
  analysis_version = analysis_version,
  freeze_date = freeze_date,
  source_commit = source_commit,
  wind_threshold_kt = wind_threshold,
  cause_order = match(cause, cause_order),
  period_order = match(period, period_order),
  comparison_order = match(
    paste(comparison_group, reference_group, sep = " vs "),
    c("65-74 vs <65", "75+ vs <65", "75+ vs 65-74")
  )
)]
setorder(interaction_output, cause_order, period_order, comparison_order)
setcolorder(interaction_output, c(
  "analysis_version", "freeze_date", "source_commit", "wind_threshold_kt",
  "cause", "cause_order", "period", "period_order", "comparison_group",
  "reference_group", "comparison_order",
  setdiff(names(interaction_output), c(
    "analysis_version", "freeze_date", "source_commit", "wind_threshold_kt",
    "cause", "cause_order", "period", "period_order", "comparison_group",
    "reference_group", "comparison_order"
  ))
))

dictionary_description <- c(
  analysis_version = "Immutable analysis freeze identifier.",
  freeze_date = "Date on which this versioned result set was frozen.",
  source_commit = "Git commit at the time the freeze was built; uncommitted analysis scripts are documented in the audit table.",
  wind_threshold_kt = "Storm exposure threshold in knots; this freeze uses winds greater than or equal to 34 kt.",
  result_id = "Unique stable identifier for a cause-age-period estimate.",
  analysis_tier = "Multiplicity and interpretation tier: Primary, Prioritized secondary, or Exploratory.",
  cause = "Underlying-cause-of-death category.",
  cause_order = "Display order of the cause category.",
  age_group = "All ages or one of three mutually exclusive age strata.",
  age_order = "Display order of the age group.",
  period = "Seven-day event-relative period based on the ZIP-specific storm impact date.",
  period_order = "Chronological display order of the event-relative period.",
  model_family = "Statistical model family.",
  model_formula_summary = "Concise description of the fixed-effects model specification.",
  cluster_summary = "Concise description of the clustered variance estimator.",
  reference_period = "Reference days against which each event-period rate ratio is estimated.",
  log_rate_ratio = "Poisson coefficient on the log rate-ratio scale.",
  standard_error = "Two-way cluster-robust standard error of the log rate ratio.",
  p_value = "Two-sided, unadjusted p-value for the event-period coefficient.",
  main_fdr_q_within_analysis_tier = "Benjamini-Hochberg q-value across all main-effect tests within the stated analysis tier (16 primary, 64 prioritized secondary, or 96 exploratory tests).",
  main_fdr_q_all_cause_specific = "Benjamini-Hochberg q-value across all 160 cause-specific main-effect tests; not defined for the all-disease primary outcome.",
  rate_ratio = "Exponentiated event-period coefficient.",
  ci_lower = "Lower endpoint of the 95% confidence interval for the rate ratio.",
  ci_upper = "Upper endpoint of the 95% confidence interval for the rate ratio.",
  percent_change = "One hundred times the rate ratio minus one.",
  observations = "Number of ZIP-day observations retained by the separately fitted model for this cause and age group.",
  event_zip_days = "Number of ZIP-days assigned to this event-relative period.",
  event_deaths = "Deaths in this cause-age-period cell.",
  event_nonzero_zip_days = "Event-period ZIP-days with at least one death in this cause-age group.",
  joint_log_rate_ratio = "Age-specific log rate ratio from the joint stacked-age interaction model; undefined for All ages.",
  joint_standard_error = "Cluster-robust standard error from the joint stacked-age model.",
  joint_rate_ratio = "Exponentiated joint-model age-specific coefficient.",
  joint_ci_lower = "Lower 95% confidence bound from the joint stacked-age model.",
  joint_ci_upper = "Upper 95% confidence bound from the joint stacked-age model.",
  joint_point_estimate_match = "Whether the joint and separate model log rate ratios agree to within 1e-10.",
  age_vs_under65_log_ratio_of_rate_ratios = "Difference in log rate ratios for the listed older group versus under 65; defined for ages 65-74 and 75+.",
  age_vs_under65_standard_error = "Standard error of the older-versus-under-65 log ratio-of-rate-ratios contrast.",
  age_vs_under65_ratio_of_rate_ratios = "Exponentiated older-versus-under-65 contrast.",
  age_vs_under65_ci_lower = "Lower 95% confidence bound for the older-versus-under-65 ratio of rate ratios.",
  age_vs_under65_ci_upper = "Upper 95% confidence bound for the older-versus-under-65 ratio of rate ratios.",
  age_vs_under65_z_value = "Wald z statistic for the older-versus-under-65 contrast.",
  age_vs_under65_p_value = "Unadjusted two-sided p-value for the older-versus-under-65 contrast.",
  age_vs_under65_fdr_q_all_132 = "Benjamini-Hochberg q-value across all 132 pairwise age contrasts.",
  age_vs_under65_fdr_q_all_cause_specific = "Benjamini-Hochberg q-value across all 120 cause-specific pairwise age contrasts.",
  age_vs_under65_fdr_q_age_focus_80 = "Benjamini-Hochberg q-value across the focused family of 80 cause-specific older-versus-under-65 contrasts.",
  age_heterogeneity_chisq = "Two-degree-of-freedom Wald chi-square statistic testing equality of all three age-specific event-period effects.",
  age_heterogeneity_df = "Degrees of freedom for the global age-heterogeneity test.",
  age_heterogeneity_p_value = "Unadjusted p-value for the global age-heterogeneity test.",
  age_heterogeneity_fdr_q_all_44 = "Benjamini-Hochberg q-value across all 44 global age-heterogeneity tests.",
  age_heterogeneity_fdr_q_cause_specific = "Benjamini-Hochberg q-value across the 40 cause-specific global age-heterogeneity tests.",
  estimate_is_finite = "QA flag indicating that the main estimate, standard error, rate ratio, and confidence limits are finite.",
  confidence_interval_ordered = "QA flag indicating ci_lower <= rate_ratio <= ci_upper."
)

dictionary <- data.table(
  table_name = "master_results_34kt_frozen_v1",
  column_name = names(master),
  storage_type = vapply(master, function(x) class(x)[1L], character(1L)),
  description = unname(dictionary_description[names(master)])
)
dictionary[is.na(description) & grepl("^joint_model_", column_name),
  description := paste(
    "Joint stacked-age model diagnostic:",
    gsub("_", " ", sub("^joint_model_", "", column_name))
  )]
dictionary[is.na(description), description :=
  "Supporting analysis field; see the generating script for its derivation."]

audit <- data.table(
  item = c(
    "Analysis version",
    "Freeze date",
    "Wind threshold",
    "Source commit",
    "Main model specification",
    "Variance estimator",
    "Primary outcome family",
    "Prioritized secondary causes",
    "Exploratory causes",
    "Interaction model",
    "Main-effect FDR families",
    "Age-interaction FDR families",
    "Event-window assignment",
    "Seasonal restriction",
    "Generating scripts"
  ),
  value = c(
    analysis_version,
    as.character(freeze_date),
    ">=34 kt",
    source_commit,
    "Fixed-effects Poisson with ZIP, ISO year-week, day-of-week, and ZIP x COVID fixed effects",
    "Two-way clustered by ZIP and ISO year-week",
    "All diseases: 4 age groups x 4 periods (16 tests)",
    paste(prioritized_causes, collapse = "; "),
    paste(setdiff(cause_order[-1L], prioritized_causes), collapse = "; "),
    "Three mutually exclusive age panels stacked with age-specific fixed effects; covariance retained for pairwise and 2-df global Wald tests",
    "BH within the Primary (16), Prioritized secondary (64), and Exploratory (96) tiers; plus BH across all 160 cause-specific tests",
    "BH across all 132 pairwise contrasts; all 120 cause-specific contrasts; focused 80 cause-specific older-vs-<65 contrasts; all 44 and 40 cause-specific global tests",
    "ZIP-specific impact dates; overlapping windows assigned to the closest impact date, with pre-impact chosen on an exact tie",
    "May-November, plus dates required to complete the -14 through +13 event windows",
    "scripts/mortality/fit_cause_age_interaction_models_34kt.R; scripts/mortality/build_frozen_master_results_34kt.R"
  )
)

master_path <- file.path(freeze_dir, "master_results_34kt_frozen_v1.csv")
interaction_path <- file.path(
  freeze_dir,
  "age_interaction_contrasts_34kt_frozen_v1.csv"
)
diagnostic_path <- file.path(
  freeze_dir,
  "model_diagnostics_34kt_frozen_v1.csv"
)
dictionary_path <- file.path(
  freeze_dir,
  "data_dictionary_34kt_frozen_v1.csv"
)
qa_path <- file.path(freeze_dir, "qa_checks_34kt_frozen_v1.csv")
audit_path <- file.path(freeze_dir, "analysis_audit_34kt_frozen_v1.csv")

fwrite(master, master_path)
fwrite(interaction_output, interaction_path)
fwrite(diagnostic_output, diagnostic_path)
fwrite(dictionary, dictionary_path)
fwrite(qa, qa_path)
fwrite(audit, audit_path)

manifest_targets <- c(
  master_path,
  interaction_path,
  diagnostic_path,
  dictionary_path,
  qa_path,
  audit_path,
  "scripts/mortality/fit_cause_age_interaction_models_34kt.R",
  "scripts/mortality/build_frozen_master_results_34kt.R",
  "scripts/mortality/build_master_results_workbook.mjs"
)
hash_output <- system2("shasum", c("-a", "256", manifest_targets), stdout = TRUE)
manifest <- tstrsplit(hash_output, "  ", fixed = TRUE)
manifest <- data.table(
  sha256 = manifest[[1L]],
  path = manifest[[2L]],
  bytes = as.numeric(file.info(manifest[[2L]])$size),
  analysis_version = analysis_version,
  freeze_date = freeze_date
)
manifest_path <- file.path(freeze_dir, "freeze_manifest_34kt_frozen_v1.csv")
fwrite(manifest, manifest_path)

cat("\nFrozen master results completed\n")
cat("Directory:", freeze_dir, "\n")
cat("Master rows:", nrow(master), "\n")
cat("Interaction rows:", nrow(interaction_output), "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
