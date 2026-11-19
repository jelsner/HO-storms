#!/usr/bin/env Rscript

source("scripts/mortality/sensitivity_model_helpers.R")

wind_threshold <- 34L
influence_version <- "event_influence_v1_2026-11-19"
influence_dir <- file.path(
  "outputs/mortality/paper_sensitivity_34kt",
  influence_version,
  "leave_one_impact_date_out_34kt"
)
intermediate_dir <- file.path(influence_dir, "intermediate")
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)

master_path <- paste0(
  "outputs/mortality/paper_master_34kt/amended_v2_2026-11-18/",
  "master_results_34kt_amended_v2.csv"
)
if (!file.exists(master_path)) stop("Missing amended master results.")
master <- fread(master_path)

profile_specs <- data.table(
  profile_type = "All-disease age profile",
  profile_id = paste0("LOIDO-ALL-A", sprintf("%02d", seq_along(sensitivity_age_levels))),
  profile_order = seq_along(sensitivity_age_levels),
  cause = "All diseases",
  age_group = sensitivity_age_levels
)

message("Building the 34-kt four-period calendar with assigned impact dates")
calendar_result <- build_sensitivity_calendar(wind_threshold, daily = FALSE)
calendar <- calendar_result$data
impact_dates <- sort(unique(calendar$impact_date[!is.na(calendar$impact_date)]))
if (length(impact_dates) != 77L) {
  stop("Expected 77 distinct assigned impact dates; found ", length(impact_dates), ".")
}

fit_grid <- CJ(
  profile_index = seq_len(nrow(profile_specs)),
  date_index = seq_along(impact_dates)
)
fit_grid <- merge(
  fit_grid,
  profile_specs[, .(profile_index = .I, profile_id)],
  by = "profile_index",
  sort = FALSE
)
fit_grid[, omitted_impact_date := impact_dates[date_index]]
fit_grid[, checkpoint_stem := paste0(
  sensitivity_slugify(profile_id),
  "_omit_",
  format(as.Date(omitted_impact_date), "%Y%m%d")
)]
fit_grid[, `:=`(
  estimate_path = file.path(intermediate_dir, paste0(checkpoint_stem, "_estimates.csv")),
  diagnostic_path = file.path(intermediate_dir, paste0(checkpoint_stem, "_diagnostics.csv"))
)]
checkpoint_is_valid <- function(estimate_path, diagnostic_path) {
  if (!file.exists(estimate_path) || !file.exists(diagnostic_path)) return(FALSE)
  estimate_fields <- names(fread(estimate_path, nrows = 0L))
  diagnostic_fields <- names(fread(diagnostic_path, nrows = 0L))
  all(c(
    "period", "log_rate_ratio", "rate_ratio", "profile_id",
    "omitted_impact_date", "analysis_version"
  ) %in% estimate_fields) &&
    all(c(
      "profile_id", "omitted_impact_date", "warm_start_used",
      "elapsed_seconds", "analysis_version"
    ) %in% diagnostic_fields)
}
fit_grid[, complete := mapply(
  checkpoint_is_valid,
  estimate_path,
  diagnostic_path
)]

progress <- txtProgressBar(min = 0L, max = nrow(fit_grid), style = 3L)
completed_count <- 0L
if (!all(fit_grid$complete)) {
  message("Loading mortality records once for exact all-disease refits")
  deaths <- load_sensitivity_deaths(
    wind_threshold,
    calendar_result$complete_window_dates
  )
  counts <- build_cause_daily_counts(deaths, "All diseases")
  fixest::setFixest_notes(FALSE)
  fixest::setFixest_nthreads(4L)

  for (profile_i in seq_len(nrow(profile_specs))) {
    spec <- profile_specs[profile_i]
    profile_rows <- fit_grid[fit_grid$profile_index == profile_i]
    if (all(profile_rows$complete)) {
      completed_count <- completed_count + nrow(profile_rows)
      setTxtProgressBar(progress, completed_count)
      next
    }

    message(
      "\n[", profile_i, "/", nrow(profile_specs), "] ",
      spec$cause, ", age ", spec$age_group,
      ": fitting full model for warm starts"
    )
    model_data <- build_group_model_data(counts, calendar, spec$age_group)
    full_model <- fepois(
      deaths ~ i(event_period, ref = "None") |
        zip + yw + dow + zip_covid,
      data = model_data,
      notes = FALSE
    )
    full_linear_predictor <- predict(
      full_model,
      newdata = model_data,
      type = "link"
    )
    full_sample_keep <- rep(TRUE, nrow(model_data))
    if (length(full_model$obs_selection$obsRemoved)) {
      full_sample_keep[abs(full_model$obs_selection$obsRemoved)] <- FALSE
    }
    baseline_start <- unname(coef(full_model))
    if (length(baseline_start) != 4L || any(!is.finite(baseline_start))) {
      stop("Could not construct four starting coefficients for ", spec$profile_id)
    }

    for (date_i in seq_along(impact_dates)) {
      grid_row <- fit_grid[
        fit_grid$profile_index == profile_i &
          fit_grid$date_index == date_i
      ]
      if (grid_row$complete) {
        completed_count <- completed_count + 1L
        setTxtProgressBar(progress, completed_count)
        next
      }

      omitted_date <- impact_dates[date_i]
      keep <- is.na(model_data$impact_date) | model_data$impact_date != omitted_date
      omitted_rows <- !keep
      fit_start <- proc.time()[["elapsed"]]
      warm_start_used <- TRUE
      warm_error <- NA_character_
      coefficients <- tryCatch(
        fepois(
          deaths ~ i(event_period, ref = "None") |
            zip + yw + dow + zip_covid,
          data = model_data,
          subset = keep,
          etastart = full_linear_predictor[keep & full_sample_keep],
          only.coef = TRUE,
          notes = FALSE
        ),
        error = function(error) {
          warm_error <<- conditionMessage(error)
          warm_start_used <<- FALSE
          fepois(
            deaths ~ i(event_period, ref = "None") |
              zip + yw + dow + zip_covid,
            data = model_data,
            subset = keep,
            only.coef = TRUE,
            notes = FALSE
          )
        }
      )
      elapsed_seconds <- proc.time()[["elapsed"]] - fit_start
      if (length(coefficients) != 4L || any(!is.finite(coefficients))) {
        stop(
          "Non-finite or incomplete coefficients for ", spec$profile_id,
          " omitting ", omitted_date
        )
      }

      estimates <- data.table(
        period = sub("^event_period::", "", names(coefficients)),
        log_rate_ratio = unname(coefficients)
      )
      estimates[, `:=`(
        rate_ratio = exp(log_rate_ratio),
        percent_change = 100 * (exp(log_rate_ratio) - 1),
        profile_type = spec$profile_type,
        profile_id = spec$profile_id,
        profile_order = spec$profile_order,
        cause = spec$cause,
        age_group = spec$age_group,
        period_order = match(period, sensitivity_period_levels[-1L]),
        omitted_impact_date = omitted_date,
        wind_threshold_kt = wind_threshold,
        analysis_version = influence_version
      )]
      diagnostics <- data.table(
        profile_type = spec$profile_type,
        profile_id = spec$profile_id,
        profile_order = spec$profile_order,
        cause = spec$cause,
        age_group = spec$age_group,
        omitted_impact_date = omitted_date,
        omitted_event_zip_days = sum(omitted_rows),
        omitted_event_zips = uniqueN(model_data$zip[omitted_rows]),
        omitted_deaths = sum(model_data$deaths[omitted_rows]),
        input_rows = nrow(model_data),
        retained_input_rows = sum(keep),
        full_model_observations = nobs(full_model),
        coefficient_count = length(coefficients),
        coefficients_finite = all(is.finite(coefficients)),
        warm_start_used = warm_start_used,
        warm_start_error = warm_error,
        elapsed_seconds = elapsed_seconds,
        wind_threshold_kt = wind_threshold,
        analysis_version = influence_version
      )

      fwrite(estimates, grid_row$estimate_path)
      fwrite(diagnostics, grid_row$diagnostic_path)
      completed_count <- completed_count + 1L
      setTxtProgressBar(progress, completed_count)
      rm(coefficients, estimates, diagnostics, keep, omitted_rows)
    }
    rm(
      model_data,
      full_model,
      full_linear_predictor,
      full_sample_keep,
      baseline_start
    )
    gc()
  }
  rm(deaths, counts)
  gc()
} else {
  completed_count <- nrow(fit_grid)
  setTxtProgressBar(progress, completed_count)
  message("Resuming from 308 complete refit checkpoints")
}
close(progress)

fit_grid[, complete := mapply(
  checkpoint_is_valid,
  estimate_path,
  diagnostic_path
)]
if (!all(fit_grid$complete)) stop("One or more refit checkpoints are missing.")

raw_estimates <- rbindlist(lapply(fit_grid$estimate_path, fread), use.names = TRUE)
diagnostics <- rbindlist(lapply(fit_grid$diagnostic_path, fread), use.names = TRUE)
raw_estimates[, period_order := match(period, sensitivity_period_levels[-1L])]
setorder(raw_estimates, profile_order, omitted_impact_date, period_order)
setorder(diagnostics, profile_order, omitted_impact_date)

raw_estimates <- merge(
  raw_estimates,
  diagnostics[, .(
    profile_id,
    omitted_impact_date,
    omitted_event_zip_days,
    omitted_event_zips,
    omitted_deaths,
    warm_start_used,
    elapsed_seconds
  )],
  by = c("profile_id", "omitted_impact_date"),
  all.x = TRUE,
  sort = FALSE
)

jackknife <- merge(
  raw_estimates,
  master[cause == "All diseases", .(
    cause,
    age_group,
    period,
    result_id,
    age_order,
    period_order,
    full_log_rate_ratio = log_rate_ratio,
    full_standard_error = standard_error,
    full_p_value = p_value,
    full_rate_ratio = rate_ratio,
    full_ci_lower = ci_lower,
    full_ci_upper = ci_upper
  )],
  by = c("cause", "age_group", "period", "period_order"),
  all.x = TRUE,
  sort = FALSE
)
jackknife[, `:=`(
  estimand_type = "All-disease age-period",
  estimand_id = result_id,
  estimand_order = (age_order - 1L) * 4L + period_order,
  delta_log_rate_ratio = log_rate_ratio - full_log_rate_ratio,
  absolute_delta_log_rate_ratio = abs(log_rate_ratio - full_log_rate_ratio),
  direction_reversal = sign(log_rate_ratio) != sign(full_log_rate_ratio)
)]
jackknife[, influence_rank := frank(
  -absolute_delta_log_rate_ratio,
  ties.method = "first"
), by = estimand_id]
setnames(
  jackknife,
  c("log_rate_ratio", "rate_ratio", "percent_change"),
  c(
    "jackknife_log_rate_ratio",
    "jackknife_rate_ratio",
    "jackknife_percent_change"
  )
)
setorder(jackknife, estimand_order, omitted_impact_date)

summary_results <- jackknife[, {
  influential_row <- .SD[which.max(absolute_delta_log_rate_ratio)]
  reversal_dates <- omitted_impact_date[direction_reversal]
  .(
    full_log_rate_ratio = unique(full_log_rate_ratio),
    full_rate_ratio = unique(full_rate_ratio),
    full_ci_lower = unique(full_ci_lower),
    full_ci_upper = unique(full_ci_upper),
    jackknife_min_rate_ratio = min(jackknife_rate_ratio),
    jackknife_max_rate_ratio = max(jackknife_rate_ratio),
    jackknife_median_rate_ratio = median(jackknife_rate_ratio),
    maximum_absolute_delta_log_rate_ratio = max(absolute_delta_log_rate_ratio),
    most_influential_impact_date = influential_row$omitted_impact_date,
    most_influential_rate_ratio = influential_row$jackknife_rate_ratio,
    most_influential_delta_log_rate_ratio = influential_row$delta_log_rate_ratio,
    direction_reversal_count = sum(direction_reversal),
    direction_reversal_dates = if (length(reversal_dates)) {
      paste(format(as.Date(reversal_dates), "%Y-%m-%d"), collapse = "; ")
    } else ""
  )
}, by = .(
  estimand_type,
  estimand_id,
  estimand_order,
  cause,
  age_group,
  age_order,
  period,
  period_order
)]
setorder(summary_results, estimand_order)

raw_path <- file.path(influence_dir, "leave_one_impact_date_out_model_estimates_34kt.csv")
estimate_path <- file.path(influence_dir, "leave_one_impact_date_out_estimates_34kt.csv")
summary_path <- file.path(influence_dir, "leave_one_impact_date_out_summary_34kt.csv")
diagnostic_path <- file.path(influence_dir, "leave_one_impact_date_out_diagnostics_34kt.csv")
date_path <- file.path(influence_dir, "leave_one_impact_date_out_dates_34kt.csv")
qa_path <- file.path(influence_dir, "leave_one_impact_date_out_qa_34kt.csv")

date_inventory <- calendar[!is.na(impact_date), .(
  assigned_event_zip_days = .N,
  assigned_zips = uniqueN(zip),
  first_assigned_date = min(date),
  last_assigned_date = max(date)
), by = impact_date][order(impact_date)]
date_inventory[, `:=`(
  wind_threshold_kt = wind_threshold,
  analysis_version = influence_version
)]

qa <- data.table(
  check_name = c(
    "Seventy-seven distinct impact dates",
    "Four all-disease age profiles",
    "Three hundred eight exact point-estimate refits",
    "Four finite coefficients per refit",
    "One thousand two hundred thirty-two reported estimates",
    "Sixteen estimands per omitted date",
    "Sixteen all-disease summary rows",
    "Finite positive reported rate ratios",
    "Unique estimand-date rows",
    "Every refit omits assigned event ZIP-days"
  ),
  status = c(
    if (length(impact_dates) == 77L) "PASS" else "FAIL",
    if (uniqueN(raw_estimates$profile_id) == 4L) "PASS" else "FAIL",
    if (nrow(diagnostics) == 308L) "PASS" else "FAIL",
    if (all(
      diagnostics$coefficient_count == 4L & diagnostics$coefficients_finite
    )) "PASS" else "FAIL",
    if (nrow(jackknife) == 1232L) "PASS" else "FAIL",
    if (all(jackknife[, .N, by = omitted_impact_date]$N == 16L)) "PASS" else "FAIL",
    if (nrow(summary_results) == 16L) "PASS" else "FAIL",
    if (all(is.finite(jackknife$jackknife_rate_ratio) & jackknife$jackknife_rate_ratio > 0)) "PASS" else "FAIL",
    if (!anyDuplicated(jackknife[, .(estimand_id, omitted_impact_date)])) "PASS" else "FAIL",
    if (all(diagnostics$omitted_event_zip_days > 0L)) "PASS" else "FAIL"
  ),
  observed = c(
    length(impact_dates),
    uniqueN(raw_estimates$profile_id),
    nrow(diagnostics),
    sum(diagnostics$coefficient_count == 4L & diagnostics$coefficients_finite),
    nrow(jackknife),
    paste(unique(jackknife[, .N, by = omitted_impact_date]$N), collapse = ","),
    nrow(summary_results),
    sum(is.finite(jackknife$jackknife_rate_ratio) & jackknife$jackknife_rate_ratio > 0),
    uniqueN(jackknife, by = c("estimand_id", "omitted_impact_date")),
    sum(diagnostics$omitted_event_zip_days > 0L)
  ),
  expected = c(77L, 4L, 308L, 308L, 1232L, 16L, 16L, 1232L, 1232L, 308L)
)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more leave-one-impact-date-out checks failed.")
}

source_commit <- get_sensitivity_source_commit()
for (object_name in c(
  "raw_estimates", "jackknife", "summary_results", "diagnostics",
  "date_inventory"
)) {
  object <- get(object_name)
  object[, `:=`(
    source_commit = source_commit,
    build_date = as.IDate("2026-11-19")
  )]
  assign(object_name, object)
}

fwrite(raw_estimates, raw_path)
fwrite(jackknife, estimate_path)
fwrite(summary_results, summary_path)
fwrite(diagnostics, diagnostic_path)
fwrite(date_inventory, date_path)
fwrite(qa, qa_path)

cat("\nLeave-one-impact-date-out analysis completed\n")
cat("Exact point-estimate refits:", nrow(diagnostics), "\n")
cat("Reported estimand-date estimates:", nrow(jackknife), "\n")
cat("Direction reversals:", sum(jackknife$direction_reversal), "\n")
cat("Warm-start fallbacks:", sum(!diagnostics$warm_start_used), "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
cat("Output directory:", influence_dir, "\n")
