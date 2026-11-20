#!/usr/bin/env Rscript

source("scripts/mortality/sensitivity_model_helpers.R")

wind_threshold <- 34L
worker_count <- as.integer(Sys.getenv("ALT_SENSITIVITY_WORKERS", "2"))
build_date <- as.IDate("2026-11-20")
icd10_version <- "icd10_era_v1_2026-11-20"
overlap_version <- "overlap_exclusion_v1_2026-11-20"
base_output_dir <- "outputs/mortality/paper_sensitivity_34kt"
icd10_dir <- file.path(base_output_dir, icd10_version)
overlap_dir <- file.path(base_output_dir, overlap_version)
icd10_intermediate <- file.path(icd10_dir, "intermediate")
overlap_intermediate <- file.path(overlap_dir, "intermediate")
dir.create(icd10_intermediate, recursive = TRUE, showWarnings = FALSE)
dir.create(overlap_intermediate, recursive = TRUE, showWarnings = FALSE)

if (!is.finite(worker_count) || worker_count < 1L) {
  stop("ALT_SENSITIVITY_WORKERS must be at least 1.")
}

master_path <- paste0(
  "outputs/mortality/paper_master_34kt/amended_v2_2026-11-18/",
  "master_results_34kt_amended_v2.csv"
)
daily_panel_path <-
  "data/mortality/processed/death-panels/daily_panel_34kt.csv"
if (!file.exists(master_path) || !file.exists(daily_panel_path)) {
  stop("Missing amended master results or 34-kt daily panel.")
}

message("Building the primary and overlap-excluded 34-kt calendars")
calendar_result <- build_sensitivity_calendar(wind_threshold, daily = FALSE)
calendar_full <- calendar_result$data

calendar_source <- fread(
  daily_panel_path,
  select = c("zip", "date", "rel_day")
)
calendar_source[, `:=`(
  zip = as.character(zip),
  date = as.IDate(date),
  rel_day = suppressWarnings(as.integer(rel_day))
)]
zip_events <- unique(calendar_source[!is.na(rel_day), .(
  zip,
  impact_date = date - rel_day
)])
event_calendar_raw <- zip_events[, .(rel_day = -14L:13L),
  by = .(zip, impact_date)
]
event_calendar_raw[, date := impact_date + rel_day]
overlap_zip_dates <- event_calendar_raw[, .N, by = .(zip, date)][N > 1L]
if (!nrow(overlap_zip_dates)) {
  stop("No overlapping ZIP-dates were found in the 34-kt event calendar.")
}
overlap_unique_zip_dates <- nrow(overlap_zip_dates)
overlap_competing_assignments <- sum(overlap_zip_dates$N - 1L)

calendar_overlap_excluded <- calendar_full[
  !overlap_zip_dates,
  on = .(zip, date)
]
calendar_icd10 <- calendar_full[date >= as.IDate("1999-01-01")]
setorder(calendar_overlap_excluded, zip, date)
setorder(calendar_icd10, zip, date)
if (anyDuplicated(calendar_overlap_excluded[, .(zip, date)])) {
  stop("Overlap-excluded calendar is not unique by ZIP and date.")
}
if (calendar_overlap_excluded[
  overlap_zip_dates,
  on = .(zip, date),
  nomatch = 0L,
  .N
] != 0L) {
  stop("At least one overlapping ZIP-date remains after exclusion.")
}

coverage <- rbindlist(list(
  data.table(
    specification = "ICD-10 era",
    analysis_version = icd10_version,
    full_zip_days = nrow(calendar_full),
    retained_zip_days = nrow(calendar_icd10),
    retained_zip_day_percentage =
      100 * nrow(calendar_icd10) / nrow(calendar_full),
    retained_zips = uniqueN(calendar_icd10$zip),
    first_date = min(calendar_icd10$date),
    last_date = max(calendar_icd10$date),
    excluded_overlap_zip_dates = NA_integer_,
    competing_overlap_assignments = NA_integer_
  ),
  data.table(
    specification = "Overlap-excluded",
    analysis_version = overlap_version,
    full_zip_days = nrow(calendar_full),
    retained_zip_days = nrow(calendar_overlap_excluded),
    retained_zip_day_percentage =
      100 * nrow(calendar_overlap_excluded) / nrow(calendar_full),
    retained_zips = uniqueN(calendar_overlap_excluded$zip),
    first_date = min(calendar_overlap_excluded$date),
    last_date = max(calendar_overlap_excluded$date),
    excluded_overlap_zip_dates = overlap_unique_zip_dates,
    competing_overlap_assignments = overlap_competing_assignments
  )
))

message("Loading mortality records and precomputing cause masks")
deaths_full <- load_sensitivity_deaths(
  wind_threshold,
  calendar_result$complete_window_dates
)
deaths_full[, source_row_id := .I]
cause_masks_full <- build_cause_masks(deaths_full$year, deaths_full$code)
deaths_icd10 <- deaths_full[date >= as.IDate("1999-01-01")]
cause_masks_icd10 <- lapply(
  cause_masks_full,
  function(mask) mask[deaths_icd10$source_row_id]
)

icd10_causes <- sensitivity_retained_causes[
  sensitivity_retained_causes != "All diseases"
]
task_grid <- rbindlist(list(
  data.table(
    specification = "ICD-10 era",
    analysis_version = icd10_version,
    cause = icd10_causes,
    cause_order = match(icd10_causes, sensitivity_retained_causes),
    output_dir = icd10_intermediate
  ),
  data.table(
    specification = "Overlap-excluded",
    analysis_version = overlap_version,
    cause = sensitivity_retained_causes,
    cause_order = seq_along(sensitivity_retained_causes),
    output_dir = overlap_intermediate
  )
))
task_grid[, task_order := .I]
task_grid[, checkpoint_stem := paste0(
  ifelse(specification == "ICD-10 era", "icd10_", "overlap_"),
  sprintf("c%02d_", cause_order),
  sensitivity_slugify(cause)
)]
task_grid[, `:=`(
  estimate_path = file.path(output_dir, paste0(checkpoint_stem, "_estimates.csv")),
  diagnostic_path = file.path(output_dir, paste0(checkpoint_stem, "_diagnostics.csv")),
  count_path = file.path(output_dir, paste0(checkpoint_stem, "_counts.csv"))
)]
task_grid[, complete :=
  file.exists(estimate_path) &
    file.exists(diagnostic_path) &
    file.exists(count_path)]

run_task_group <- function(task_indices, worker_id) {
  suppressPackageStartupMessages({
    library(data.table)
    library(fixest)
  })
  setDTthreads(1L)
  setFixest_nthreads(1L)
  setFixest_notes(FALSE)

  for (task_position in seq_along(task_indices)) {
    task_i <- task_indices[task_position]
    task <- task_grid[task_i]
    if (task$specification == "ICD-10 era") {
      task_calendar <- calendar_icd10
      task_deaths <- deaths_icd10
      task_masks <- cause_masks_icd10
    } else {
      task_calendar <- calendar_overlap_excluded
      task_deaths <- deaths_full
      task_masks <- cause_masks_full
    }

    message(
      "Worker ", worker_id, ": ", task$specification,
      ", ", task$cause, " (", task_position, "/",
      length(task_indices), " tasks)"
    )
    counts <- build_cause_daily_counts(
      task_deaths,
      task$cause,
      task_masks[[task$cause]]
    )
    task_estimates <- vector("list", length(sensitivity_age_levels))
    task_diagnostics <- vector("list", length(sensitivity_age_levels))
    task_counts <- vector("list", length(sensitivity_age_levels))

    for (age_i in seq_along(sensitivity_age_levels)) {
      age_name <- sensitivity_age_levels[age_i]
      fit_start <- proc.time()[["elapsed"]]
      model_data <- build_group_model_data(
        counts,
        task_calendar,
        age_name
      )
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
        nthreads = 1L,
        notes = FALSE
      )
      elapsed_seconds <- proc.time()[["elapsed"]] - fit_start
      estimates <- extract_sensitivity_estimates(model, "event_period::")
      setnames(estimates, "term", "period")
      estimates[, `:=`(
        cause = task$cause,
        age_group = age_name,
        cause_order = task$cause_order,
        age_order = age_i,
        period_order = match(period, sensitivity_period_levels[-1L]),
        specification = task$specification,
        wind_threshold_kt = wind_threshold,
        analysis_version = task$analysis_version
      )]
      diagnostics <- data.table(
        cause = task$cause,
        age_group = age_name,
        cause_order = task$cause_order,
        age_order = age_i,
        specification = task$specification,
        input_rows = nrow(model_data),
        model_observations = nobs(model),
        removed_observations = nrow(model_data) - nobs(model),
        model_converged = if (is.null(model$convStatus)) TRUE else
          isTRUE(model$convStatus),
        coefficient_count = length(coef(model)),
        log_likelihood = as.numeric(logLik(model)),
        pseudo_r2 = safe_sensitivity_fitstat(model, "pr2"),
        squared_correlation = safe_sensitivity_fitstat(model, "sq.cor"),
        zip_clusters = uniqueN(model_data$zip),
        year_week_clusters = uniqueN(model_data$yw),
        elapsed_seconds = elapsed_seconds,
        wind_threshold_kt = wind_threshold,
        analysis_version = task$analysis_version
      )
      sample_counts[, `:=`(
        cause = task$cause,
        age_group = age_name,
        cause_order = task$cause_order,
        age_order = age_i,
        period = as.character(event_period),
        period_order = match(
          as.character(event_period),
          sensitivity_period_levels
        ),
        specification = task$specification,
        wind_threshold_kt = wind_threshold,
        analysis_version = task$analysis_version
      )]

      task_estimates[[age_i]] <- estimates
      task_diagnostics[[age_i]] <- diagnostics
      task_counts[[age_i]] <- sample_counts
      rm(model_data, model, estimates, diagnostics, sample_counts)
      gc(FALSE)
    }

    fwrite(rbindlist(task_estimates), task$estimate_path)
    fwrite(rbindlist(task_diagnostics), task$diagnostic_path)
    fwrite(rbindlist(task_counts), task$count_path)
    message(
      "Worker ", worker_id, ": completed ",
      task$specification, ", ", task$cause
    )
    rm(counts, task_estimates, task_diagnostics, task_counts)
    gc(FALSE)
  }
  TRUE
}

pending <- which(!task_grid$complete)
if (length(pending)) {
  workers_used <- min(worker_count, length(pending))
  pending_groups <- split(
    pending,
    rep(seq_len(workers_used), length.out = length(pending))
  )
  message(
    "Fitting ", length(pending), " cause-specification tasks with ",
    workers_used, " worker(s)"
  )
  if (workers_used == 1L) {
    run_task_group(pending_groups[[1L]], 1L)
  } else {
    worker_results <- parallel::mclapply(
      seq_along(pending_groups),
      function(worker_i) {
        run_task_group(pending_groups[[worker_i]], worker_i)
      },
      mc.cores = workers_used,
      mc.preschedule = TRUE
    )
    if (!all(vapply(worker_results, isTRUE, logical(1L)))) {
      stop("At least one alternative-sensitivity worker failed.")
    }
  }
} else {
  message("Resuming from all 15 complete cause-specification checkpoints")
}

if (!all(
  file.exists(task_grid$estimate_path) &
    file.exists(task_grid$diagnostic_path) &
    file.exists(task_grid$count_path)
)) {
  stop("Not all alternative-sensitivity checkpoints are complete.")
}

master <- fread(master_path)
source_commit <- get_sensitivity_source_commit()

finalize_specification <- function(
  specification_name,
  version_name,
  output_dir,
  expected_causes,
  expected_estimates,
  expected_models,
  expected_count_rows
) {
  rows <- task_grid[specification == specification_name]
  estimates <- rbindlist(lapply(rows$estimate_path, fread), use.names = TRUE)
  diagnostics <- rbindlist(lapply(rows$diagnostic_path, fread), use.names = TRUE)
  sample_counts <- rbindlist(lapply(rows$count_path, fread), use.names = TRUE)
  setorder(estimates, cause_order, age_order, period_order)
  setorder(diagnostics, cause_order, age_order)
  setorder(sample_counts, cause_order, age_order, period_order)

  frozen <- master[cause %in% expected_causes, .(
    cause,
    age_group,
    period,
    frozen_log_rate_ratio = log_rate_ratio,
    frozen_standard_error = standard_error,
    frozen_p_value = p_value,
    frozen_rate_ratio = rate_ratio,
    frozen_ci_lower = ci_lower,
    frozen_ci_upper = ci_upper,
    frozen_q_value = fifelse(
      cause == "All diseases",
      main_fdr_q_within_analysis_tier,
      main_fdr_q_all_cause_specific
    )
  )]
  estimates <- merge(
    estimates,
    frozen,
    by = c("cause", "age_group", "period"),
    all.x = TRUE,
    sort = FALSE
  )
  estimates[, `:=`(
    sensitivity_minus_frozen_log_rate_ratio =
      log_rate_ratio - frozen_log_rate_ratio,
    direction_agrees_with_frozen =
      sign(log_rate_ratio) == sign(frozen_log_rate_ratio),
    confidence_interval_overlaps_frozen =
      ci_lower <= frozen_ci_upper & ci_upper >= frozen_ci_lower,
    frozen_ci_excludes_null =
      frozen_ci_lower > 1 | frozen_ci_upper < 1,
    sensitivity_ci_excludes_null =
      ci_lower > 1 | ci_upper < 1
  )]
  estimates[, ci_exclusion_changed :=
    frozen_ci_excludes_null != sensitivity_ci_excludes_null]
  setorder(estimates, cause_order, age_order, period_order)

  spec_coverage <- coverage[specification == specification_name]
  qa <- data.table(
    check_name = c(
      "Expected retained outcomes",
      "Four age groups per outcome",
      "Four estimates per outcome-age model",
      "Expected sensitivity estimates",
      "Expected model count",
      "All models converged",
      "Four coefficients per model",
      "Finite ordered confidence intervals",
      "Complete count rows",
      "Frozen comparison fields complete"
    ),
    status = c(
      if (uniqueN(estimates$cause) == length(expected_causes))
        "PASS" else "FAIL",
      if (uniqueN(estimates[, .(cause, age_group)]) == expected_models)
        "PASS" else "FAIL",
      if (all(estimates[, .N, by = .(cause, age_group)]$N == 4L))
        "PASS" else "FAIL",
      if (nrow(estimates) == expected_estimates) "PASS" else "FAIL",
      if (nrow(diagnostics) == expected_models) "PASS" else "FAIL",
      if (all(diagnostics$model_converged)) "PASS" else "FAIL",
      if (all(diagnostics$coefficient_count == 4L)) "PASS" else "FAIL",
      if (all(
        is.finite(estimates$rate_ratio) &
          estimates$ci_lower <= estimates$rate_ratio &
          estimates$rate_ratio <= estimates$ci_upper
      )) "PASS" else "FAIL",
      if (nrow(sample_counts) == expected_count_rows) "PASS" else "FAIL",
      if (!anyNA(estimates[, .(
        frozen_log_rate_ratio,
        frozen_rate_ratio,
        sensitivity_minus_frozen_log_rate_ratio,
        direction_agrees_with_frozen,
        confidence_interval_overlaps_frozen
      )])) "PASS" else "FAIL"
    ),
    observed = c(
      uniqueN(estimates$cause),
      uniqueN(estimates[, .(cause, age_group)]),
      paste(unique(estimates[, .N, by = .(cause, age_group)]$N), collapse = ","),
      nrow(estimates),
      nrow(diagnostics),
      sum(diagnostics$model_converged),
      paste(unique(diagnostics$coefficient_count), collapse = ","),
      sum(
        is.finite(estimates$rate_ratio) &
          estimates$ci_lower <= estimates$rate_ratio &
          estimates$rate_ratio <= estimates$ci_upper
      ),
      nrow(sample_counts),
      sum(complete.cases(estimates[, .(
        frozen_log_rate_ratio,
        frozen_rate_ratio,
        sensitivity_minus_frozen_log_rate_ratio,
        direction_agrees_with_frozen,
        confidence_interval_overlaps_frozen
      )]))
    ),
    expected = c(
      length(expected_causes),
      expected_models,
      4L,
      expected_estimates,
      expected_models,
      expected_models,
      4L,
      expected_estimates,
      expected_count_rows,
      expected_estimates
    )
  )
  if (any(qa$status != "PASS")) {
    print(qa)
    stop("One or more ", specification_name, " checks failed.")
  }

  for (object_name in c(
    "estimates",
    "diagnostics",
    "sample_counts",
    "spec_coverage"
  )) {
    object <- get(object_name)
    object[, `:=`(
      source_commit = source_commit,
      build_date = build_date
    )]
    assign(object_name, object)
  }
  file_stem <- if (specification_name == "ICD-10 era") {
    "icd10_era"
  } else {
    "overlap_excluded"
  }
  fwrite(estimates, file.path(output_dir, paste0(file_stem, "_estimates_34kt.csv")))
  fwrite(diagnostics, file.path(output_dir, paste0(file_stem, "_diagnostics_34kt.csv")))
  fwrite(sample_counts, file.path(output_dir, paste0(file_stem, "_counts_34kt.csv")))
  fwrite(spec_coverage, file.path(output_dir, paste0(file_stem, "_coverage_34kt.csv")))
  fwrite(qa, file.path(output_dir, paste0(file_stem, "_qa_34kt.csv")))

  list(
    estimates = estimates,
    diagnostics = diagnostics,
    sample_counts = sample_counts,
    coverage = spec_coverage,
    qa = qa
  )
}

icd10_results <- finalize_specification(
  "ICD-10 era",
  icd10_version,
  icd10_dir,
  icd10_causes,
  112L,
  28L,
  140L
)
overlap_results <- finalize_specification(
  "Overlap-excluded",
  overlap_version,
  overlap_dir,
  sensitivity_retained_causes,
  128L,
  32L,
  160L
)

cat("\nICD-10-era and overlap-exclusion sensitivities completed\n")
cat("ICD-10 models:", nrow(icd10_results$diagnostics), "\n")
cat("ICD-10 estimates:", nrow(icd10_results$estimates), "\n")
cat("Overlap-excluded models:", nrow(overlap_results$diagnostics), "\n")
cat("Overlap-excluded estimates:", nrow(overlap_results$estimates), "\n")
cat("Excluded overlapping ZIP-dates:", overlap_unique_zip_dates, "\n")
cat("All model QA checks PASS\n")
