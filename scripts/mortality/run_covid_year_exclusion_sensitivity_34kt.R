#!/usr/bin/env Rscript

source("scripts/mortality/sensitivity_model_helpers.R")

wind_threshold <- 34L
exclusion_version <- "covid_exclusion_v1_2026-11-19"
exclusion_dir <- file.path(
  "outputs/mortality/paper_sensitivity_34kt",
  exclusion_version
)
intermediate_dir <- file.path(exclusion_dir, "intermediate")
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)

master_path <- paste0(
  "outputs/mortality/paper_master_34kt/amended_v2_2026-11-18/",
  "master_results_34kt_amended_v2.csv"
)
if (!file.exists(master_path)) stop("Missing amended master results.")

message("Building the 34-kt calendar and excluding 2020-2022")
calendar_result <- build_sensitivity_calendar(wind_threshold, daily = FALSE)
calendar_full <- calendar_result$data
calendar <- calendar_full[date < as.IDate("2020-01-01")]
calendar[, `:=`(
  zip = factor(as.character(zip), levels = sort(unique(as.character(zip)))),
  zip_covid = NULL
)]
setorder(calendar, zip, date)
if (anyDuplicated(calendar[, .(zip, date)])) {
  stop("COVID-excluded calendar is not unique by ZIP and date.")
}

coverage <- data.table(
  metric = c(
    "Full calendar ZIP-days",
    "COVID-excluded ZIP-days",
    "Retained ZIP-day percentage",
    "Full calendar ZIPs",
    "COVID-excluded ZIPs",
    "Full distinct impact dates",
    "COVID-excluded distinct impact dates",
    "First retained date",
    "Last retained date"
  ),
  value = c(
    as.character(nrow(calendar_full)),
    as.character(nrow(calendar)),
    sprintf("%.6f", 100 * nrow(calendar) / nrow(calendar_full)),
    as.character(uniqueN(calendar_full$zip)),
    as.character(uniqueN(calendar$zip)),
    as.character(uniqueN(calendar_full$impact_date, na.rm = TRUE)),
    as.character(uniqueN(calendar$impact_date, na.rm = TRUE)),
    as.character(min(calendar$date)),
    as.character(max(calendar$date))
  ),
  analysis_version = exclusion_version
)

estimate_paths <- file.path(
  intermediate_dir,
  paste0(
    "all_diseases_",
    sensitivity_slugify(sensitivity_age_levels),
    "_estimates.csv"
  )
)
diagnostic_paths <- file.path(
  intermediate_dir,
  paste0(
    "all_diseases_",
    sensitivity_slugify(sensitivity_age_levels),
    "_diagnostics.csv"
  )
)
count_paths <- file.path(
  intermediate_dir,
  paste0(
    "all_diseases_",
    sensitivity_slugify(sensitivity_age_levels),
    "_counts.csv"
  )
)
complete <- file.exists(estimate_paths) &
  file.exists(diagnostic_paths) &
  file.exists(count_paths)

progress <- txtProgressBar(min = 0L, max = length(sensitivity_age_levels), style = 3L)
if (!all(complete)) {
  message("Loading mortality records for four all-disease models")
  deaths <- load_sensitivity_deaths(
    wind_threshold,
    calendar_result$complete_window_dates
  )
  counts <- build_cause_daily_counts(deaths, "All diseases")
  fixest::setFixest_notes(FALSE)

  for (age_i in seq_along(sensitivity_age_levels)) {
    if (complete[age_i]) {
      setTxtProgressBar(progress, age_i)
      next
    }
    age_name <- sensitivity_age_levels[age_i]
    message("\n[", age_i, "/4] All diseases, age ", age_name)
    model_data <- build_group_model_data(counts, calendar, age_name)
    sample_counts <- model_data[, .(
      zip_days = .N,
      deaths = sum(deaths),
      nonzero_zip_days = sum(deaths > 0L)
    ), by = event_period]

    model <- fepois(
      deaths ~ i(event_period, ref = "None") |
        zip + yw + dow,
      data = model_data,
      cluster = ~ zip + yw,
      notes = FALSE
    )
    estimates <- extract_sensitivity_estimates(model, "event_period::")
    setnames(estimates, "term", "period")
    estimates[, `:=`(
      cause = "All diseases",
      age_group = age_name,
      cause_order = 1L,
      age_order = age_i,
      period_order = match(period, sensitivity_period_levels[-1L]),
      specification = "Exclude 2020-2022",
      wind_threshold_kt = wind_threshold,
      analysis_version = exclusion_version
    )]
    diagnostics <- data.table(
      cause = "All diseases",
      age_group = age_name,
      cause_order = 1L,
      age_order = age_i,
      specification = "Exclude 2020-2022",
      input_rows = nrow(model_data),
      model_observations = nobs(model),
      removed_observations = nrow(model_data) - nobs(model),
      model_converged = if (is.null(model$convStatus)) TRUE else isTRUE(model$convStatus),
      coefficient_count = length(coef(model)),
      log_likelihood = as.numeric(logLik(model)),
      pseudo_r2 = safe_sensitivity_fitstat(model, "pr2"),
      squared_correlation = safe_sensitivity_fitstat(model, "sq.cor"),
      zip_clusters = uniqueN(model_data$zip),
      year_week_clusters = uniqueN(model_data$yw),
      wind_threshold_kt = wind_threshold,
      analysis_version = exclusion_version
    )
    sample_counts[, `:=`(
      cause = "All diseases",
      age_group = age_name,
      cause_order = 1L,
      age_order = age_i,
      specification = "Exclude 2020-2022",
      period = as.character(event_period),
      period_order = match(as.character(event_period), sensitivity_period_levels),
      wind_threshold_kt = wind_threshold,
      analysis_version = exclusion_version
    )]

    fwrite(estimates, estimate_paths[age_i])
    fwrite(diagnostics, diagnostic_paths[age_i])
    fwrite(sample_counts, count_paths[age_i])
    setTxtProgressBar(progress, age_i)
    rm(model_data, model, estimates, diagnostics, sample_counts)
    gc()
  }
  rm(deaths, counts)
  gc()
} else {
  setTxtProgressBar(progress, length(sensitivity_age_levels))
  message("Resuming from four complete COVID-excluded checkpoints")
}
close(progress)

estimates <- rbindlist(lapply(estimate_paths, fread), use.names = TRUE)
diagnostics <- rbindlist(lapply(diagnostic_paths, fread), use.names = TRUE)
sample_counts <- rbindlist(lapply(count_paths, fread), use.names = TRUE)
setorder(estimates, age_order, period_order)
setorder(diagnostics, age_order)
setorder(sample_counts, age_order, period_order)

master <- fread(master_path)[cause == "All diseases", .(
  cause,
  age_group,
  period,
  frozen_log_rate_ratio = log_rate_ratio,
  frozen_standard_error = standard_error,
  frozen_p_value = p_value,
  frozen_rate_ratio = rate_ratio,
  frozen_ci_lower = ci_lower,
  frozen_ci_upper = ci_upper
)]
estimates <- merge(
  estimates,
  master,
  by = c("cause", "age_group", "period"),
  all.x = TRUE,
  sort = FALSE
)
estimates[, `:=`(
  excluded_minus_frozen_log_rate_ratio =
    log_rate_ratio - frozen_log_rate_ratio,
  direction_agrees_with_frozen =
    sign(log_rate_ratio) == sign(frozen_log_rate_ratio),
  confidence_interval_overlaps_frozen =
    ci_lower <= frozen_ci_upper & ci_upper >= frozen_ci_lower,
  frozen_ci_excludes_null =
    frozen_ci_lower > 1 | frozen_ci_upper < 1,
  excluded_ci_excludes_null =
    ci_lower > 1 | ci_upper < 1
)]
estimates[, ci_exclusion_changed :=
  frozen_ci_excludes_null != excluded_ci_excludes_null]
setorder(estimates, age_order, period_order)

estimate_path <- file.path(exclusion_dir, "covid_excluded_estimates_34kt.csv")
diagnostic_path <- file.path(exclusion_dir, "covid_excluded_diagnostics_34kt.csv")
count_path <- file.path(exclusion_dir, "covid_excluded_counts_34kt.csv")
coverage_path <- file.path(exclusion_dir, "covid_excluded_coverage_34kt.csv")
qa_path <- file.path(exclusion_dir, "covid_excluded_qa_34kt.csv")

qa <- data.table(
  check_name = c(
    "Four all-disease age models",
    "Four period estimates per model",
    "Sixteen primary estimates",
    "All models converged",
    "Finite ordered confidence intervals",
    "All retained observations precede 2020",
    "Frozen comparison fields complete"
  ),
  status = c(
    if (nrow(diagnostics) == 4L) "PASS" else "FAIL",
    if (all(estimates[, .N, by = age_group]$N == 4L)) "PASS" else "FAIL",
    if (nrow(estimates) == 16L) "PASS" else "FAIL",
    if (all(diagnostics$model_converged)) "PASS" else "FAIL",
    if (all(
      is.finite(estimates$rate_ratio) &
        estimates$ci_lower <= estimates$rate_ratio &
        estimates$rate_ratio <= estimates$ci_upper
    )) "PASS" else "FAIL",
    if (max(calendar$date) < as.IDate("2020-01-01")) "PASS" else "FAIL",
    if (!anyNA(estimates[, .(
      frozen_log_rate_ratio,
      frozen_rate_ratio,
      excluded_minus_frozen_log_rate_ratio,
      direction_agrees_with_frozen,
      confidence_interval_overlaps_frozen
    )])) "PASS" else "FAIL"
  ),
  observed = c(
    nrow(diagnostics),
    paste(unique(estimates[, .N, by = age_group]$N), collapse = ","),
    nrow(estimates),
    sum(diagnostics$model_converged),
    sum(
      is.finite(estimates$rate_ratio) &
        estimates$ci_lower <= estimates$rate_ratio &
        estimates$rate_ratio <= estimates$ci_upper
    ),
    as.character(max(calendar$date)),
    sum(complete.cases(estimates[, .(
      frozen_log_rate_ratio,
      frozen_rate_ratio,
      excluded_minus_frozen_log_rate_ratio,
      direction_agrees_with_frozen,
      confidence_interval_overlaps_frozen
    )]))
  ),
  expected = c(4L, 4L, 16L, 4L, 16L, "Before 2020-01-01", 16L)
)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more COVID-exclusion sensitivity checks failed.")
}

source_commit <- get_sensitivity_source_commit()
for (object_name in c(
  "estimates", "diagnostics", "sample_counts", "coverage"
)) {
  object <- get(object_name)
  object[, `:=`(
    source_commit = source_commit,
    build_date = as.IDate("2026-11-19")
  )]
  assign(object_name, object)
}

fwrite(estimates, estimate_path)
fwrite(diagnostics, diagnostic_path)
fwrite(sample_counts, count_path)
fwrite(coverage, coverage_path)
fwrite(qa, qa_path)

cat("\nCOVID-year exclusion sensitivity completed\n")
cat("Models:", nrow(diagnostics), "\n")
cat("Estimates:", nrow(estimates), "\n")
cat("Retained impact dates:", uniqueN(calendar$impact_date, na.rm = TRUE), "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
cat("Output directory:", exclusion_dir, "\n")
