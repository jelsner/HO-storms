#!/usr/bin/env Rscript

source("scripts/mortality/sensitivity_model_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
wind_threshold <- if (length(args)) suppressWarnings(as.integer(args[1L])) else NA_integer_
if (is.na(wind_threshold) || !wind_threshold %in% c(55L, 64L)) {
  stop("Supply one wind threshold: 55 or 64.")
}

threshold_dir <- file.path(
  sensitivity_output_dir,
  paste0("threshold_", wind_threshold, "kt")
)
intermediate_dir <- file.path(threshold_dir, "intermediate")
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)

message("Building the ", wind_threshold, "-kt event calendar")
calendar_result <- build_sensitivity_calendar(wind_threshold, daily = FALSE)
calendar <- calendar_result$data

cause_paths <- file.path(
  intermediate_dir,
  paste0(sensitivity_slugify(sensitivity_retained_causes), "_estimates.csv")
)
diagnostic_paths <- file.path(
  intermediate_dir,
  paste0(sensitivity_slugify(sensitivity_retained_causes), "_diagnostics.csv")
)
count_paths <- file.path(
  intermediate_dir,
  paste0(sensitivity_slugify(sensitivity_retained_causes), "_counts.csv")
)

complete_causes <- file.exists(cause_paths) &
  file.exists(diagnostic_paths) &
  file.exists(count_paths)

if (!all(complete_causes)) {
  message("Loading mortality records once for the ", wind_threshold, "-kt fits")
  deaths <- load_sensitivity_deaths(
    wind_threshold,
    calendar_result$complete_window_dates
  )
  message("Precomputing retained cause masks")
  cause_masks <- build_cause_masks(deaths$year, deaths$code)
  fixest::setFixest_notes(FALSE)

  for (cause_index in which(!complete_causes)) {
    cause_name <- sensitivity_retained_causes[cause_index]
    message(
      "[", cause_index, "/", length(sensitivity_retained_causes), "] ",
      cause_name
    )
    counts <- build_cause_daily_counts(
      deaths,
      cause_name,
      cause_masks[[cause_name]]
    )
    cause_estimates <- vector("list", length(sensitivity_age_levels))
    cause_diagnostics <- vector("list", length(sensitivity_age_levels))
    cause_counts <- vector("list", length(sensitivity_age_levels))

    for (age_index in seq_along(sensitivity_age_levels)) {
      age_group_name <- sensitivity_age_levels[age_index]
      message("  Fitting age group ", age_group_name)
      model_data <- build_group_model_data(counts, calendar, age_group_name)
      sample_counts <- model_data[, .(
        zip_days = .N,
        deaths = sum(deaths),
        nonzero_zip_days = sum(deaths > 0L)
      ), by = event_period]

      model <- fepois(
        deaths ~ i(event_period, ref = "None") |
          zip + yw + dow + zip_covid,
        data = model_data,
        cluster = ~ zip + yw,
        notes = FALSE
      )
      estimates <- extract_sensitivity_estimates(
        model,
        "event_period::"
      )
      setnames(estimates, "term", "period")
      estimates[, `:=`(
        wind_threshold_kt = wind_threshold,
        cause = cause_name,
        age_group = age_group_name,
        cause_order = cause_index,
        age_order = age_index,
        period_order = match(period, sensitivity_period_levels),
        analysis_version = sensitivity_version
      )]
      sample_counts[, `:=`(
        wind_threshold_kt = wind_threshold,
        cause = cause_name,
        age_group = age_group_name,
        cause_order = cause_index,
        age_order = age_index,
        period_order = match(as.character(event_period), sensitivity_period_levels),
        analysis_version = sensitivity_version
      )]
      diagnostics <- data.table(
        wind_threshold_kt = wind_threshold,
        cause = cause_name,
        age_group = age_group_name,
        cause_order = cause_index,
        age_order = age_index,
        model_observations = nobs(model),
        input_rows = nrow(model_data),
        removed_observations = nrow(model_data) - nobs(model),
        model_converged = if (is.null(model$convStatus)) TRUE else isTRUE(model$convStatus),
        log_likelihood = as.numeric(logLik(model)),
        pseudo_r2 = safe_sensitivity_fitstat(model, "pr2"),
        squared_correlation = safe_sensitivity_fitstat(model, "sq.cor"),
        coefficient_count = length(coef(model)),
        analysis_version = sensitivity_version
      )

      cause_estimates[[age_index]] <- estimates
      cause_diagnostics[[age_index]] <- diagnostics
      cause_counts[[age_index]] <- sample_counts
      rm(model_data, model, estimates, diagnostics, sample_counts)
      gc()
    }

    fwrite(rbindlist(cause_estimates), cause_paths[cause_index])
    fwrite(rbindlist(cause_diagnostics), diagnostic_paths[cause_index])
    fwrite(rbindlist(cause_counts), count_paths[cause_index])
    rm(counts, cause_estimates, cause_diagnostics, cause_counts)
    gc()
  }
  rm(deaths, cause_masks)
  gc()
} else {
  message("Resuming from all eight complete cause-level checkpoints")
}

estimates <- rbindlist(lapply(cause_paths, fread), use.names = TRUE)
diagnostics <- rbindlist(lapply(diagnostic_paths, fread), use.names = TRUE)
sample_counts <- rbindlist(lapply(count_paths, fread), use.names = TRUE)
setorder(estimates, cause_order, age_order, period_order)
setorder(diagnostics, cause_order, age_order)
setorder(sample_counts, cause_order, age_order, period_order)

estimates_path <- file.path(
  threshold_dir,
  paste0("wind_threshold_estimates_", wind_threshold, "kt.csv")
)
diagnostics_path <- file.path(
  threshold_dir,
  paste0("wind_threshold_diagnostics_", wind_threshold, "kt.csv")
)
counts_path <- file.path(
  threshold_dir,
  paste0("wind_threshold_counts_", wind_threshold, "kt.csv")
)
qa_path <- file.path(
  threshold_dir,
  paste0("wind_threshold_qa_", wind_threshold, "kt.csv")
)

qa <- data.table(
  check_name = c(
    "Eight retained outcomes",
    "Four age groups per outcome",
    "Four period estimates per outcome-age model",
    "All models converged",
    "Four coefficients per model",
    "Finite ordered estimates",
    "Complete count cells",
    "Calendar contains 1,046 ZIPs"
  ),
  status = c(
    if (uniqueN(estimates$cause) == 8L) "PASS" else "FAIL",
    if (uniqueN(estimates[, .(cause, age_group)]) == 32L) "PASS" else "FAIL",
    if (nrow(estimates) == 128L) "PASS" else "FAIL",
    if (all(diagnostics$model_converged)) "PASS" else "FAIL",
    if (all(diagnostics$coefficient_count == 4L)) "PASS" else "FAIL",
    if (all(
      is.finite(estimates$rate_ratio) &
        estimates$ci_lower <= estimates$rate_ratio &
        estimates$rate_ratio <= estimates$ci_upper
    )) "PASS" else "FAIL",
    if (nrow(sample_counts) == 160L) "PASS" else "FAIL",
    if (calendar_result$panel_zips == 1046L) "PASS" else "FAIL"
  ),
  observed = c(
    uniqueN(estimates$cause),
    uniqueN(estimates[, .(cause, age_group)]),
    nrow(estimates),
    sum(diagnostics$model_converged),
    paste(unique(diagnostics$coefficient_count), collapse = ","),
    sum(
      is.finite(estimates$rate_ratio) &
        estimates$ci_lower <= estimates$rate_ratio &
        estimates$rate_ratio <= estimates$ci_upper
    ),
    nrow(sample_counts),
    calendar_result$panel_zips
  ),
  expected = c(8L, 32L, 128L, 32L, 4L, 128L, 160L, 1046L)
)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more threshold-sensitivity checks failed.")
}

source_commit <- get_sensitivity_source_commit()
for (object_name in c("estimates", "diagnostics", "sample_counts")) {
  object <- get(object_name)
  object[, `:=`(
    source_commit = source_commit,
    build_date = as.IDate("2026-11-18")
  )]
  assign(object_name, object)
}

fwrite(estimates, estimates_path)
fwrite(diagnostics, diagnostics_path)
fwrite(sample_counts, counts_path)
fwrite(qa, qa_path)

cat("\nWind-threshold sensitivity completed\n")
cat("Threshold:", wind_threshold, "kt\n")
cat("Estimates:", nrow(estimates), "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
cat("Output directory:", threshold_dir, "\n")
