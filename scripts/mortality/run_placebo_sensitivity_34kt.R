#!/usr/bin/env Rscript

source("scripts/mortality/sensitivity_model_helpers.R")

wind_threshold <- 34L
placebo_version <- "placebo_v1_2026-11-19"
placebo_dir <- file.path(
  "outputs/mortality/paper_sensitivity_34kt",
  placebo_version
)
intermediate_dir <- file.path(placebo_dir, "intermediate")
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)

master_path <- paste0(
  "outputs/mortality/paper_master_34kt/amended_v2_2026-11-18/",
  "master_results_34kt_amended_v2.csv"
)
temporal_source_path <- paste0(
  "data/mortality/processed/death-panels/",
  "daily_panel_34kt_PLACEBO.csv"
)
if (!file.exists(master_path)) stop("Missing amended master results.")
if (!file.exists(temporal_source_path)) stop("Missing temporal placebo calendar.")

build_calendar_from_events <- function(calendar_source, zip_events) {
  event_calendar_raw <- zip_events[, .(rel_day = -14L:13L),
    by = .(zip, impact_date)
  ]
  event_calendar_raw[, date := impact_date + rel_day]
  overlap_zip_dates <- event_calendar_raw[, .N, by = .(zip, date)][N > 1L]
  overlap_assignments <- if (nrow(overlap_zip_dates)) {
    sum(overlap_zip_dates$N - 1L)
  } else {
    0L
  }
  event_calendar <- event_calendar_raw[
    order(zip, date, abs(rel_day), rel_day, impact_date)
  ][, .SD[1L], by = .(zip, date)]
  event_calendar[, event_period := period_from_rel_day(rel_day)]
  complete_window_dates <- unique(event_calendar$date)

  calendar <- unique(calendar_source[, .(zip, date)])
  calendar[, month := month(as.Date(date))]
  calendar <- calendar[month %in% 5:11 | date %in% complete_window_dates]
  calendar[, month := NULL]
  calendar <- event_calendar[
    calendar,
    on = .(zip, date),
    .(
      zip = i.zip,
      date = i.date,
      event_period = x.event_period,
      impact_date = x.impact_date
    )
  ]
  calendar[is.na(event_period), event_period := "None"]
  calendar[, event_period := factor(
    event_period,
    levels = sensitivity_period_levels
  )]

  all_panel_zips <- sort(unique(calendar$zip))
  calendar[, `:=`(
    zip = factor(zip, levels = all_panel_zips),
    yw = year(as.Date(date)) * 100L + isoweek(as.Date(date)),
    dow = as.integer(strftime(as.Date(date), "%u")),
    covid = as.integer(
      date >= as.IDate("2020-01-01") & date <= as.IDate("2022-12-31")
    )
  )]
  calendar[, zip_covid := interaction(zip, covid, drop = TRUE)]
  stopifnot(!anyDuplicated(calendar[, .(zip, date)]))

  list(
    data = calendar,
    complete_window_dates = complete_window_dates,
    zip_events = nrow(zip_events),
    distinct_impact_dates = uniqueN(zip_events$impact_date),
    overlap_assignments = overlap_assignments,
    panel_zips = length(all_panel_zips)
  )
}

message("Building the observed and temporal-placebo 34-kt calendars")
observed_result <- build_sensitivity_calendar(wind_threshold, daily = FALSE)
observed_calendar <- observed_result$data

temporal_source <- fread(
  temporal_source_path,
  select = c("zip", "date", "rel_day")
)
temporal_source[, `:=`(
  zip = as.character(zip),
  date = as.IDate(date),
  rel_day = suppressWarnings(as.integer(rel_day))
)]
temporal_events <- unique(temporal_source[!is.na(rel_day), .(
  zip,
  impact_date = date - rel_day
)])
temporal_result <- build_calendar_from_events(temporal_source, temporal_events)
temporal_calendar <- temporal_result$data

observed_events <- unique(observed_calendar[!is.na(impact_date), .(
  zip = as.character(zip),
  impact_date
)])
temporal_events_for_distance <- copy(temporal_events)
setkey(observed_events, zip, impact_date)
setkey(temporal_events_for_distance, zip, impact_date)
nearest_observed <- observed_events[
  temporal_events_for_distance,
  on = .(zip, impact_date),
  roll = "nearest",
  .(
    zip = i.zip,
    placebo_impact_date = i.impact_date,
    nearest_observed_impact_date = x.impact_date
  )
]
nearest_observed[, nearest_observed_gap_days := abs(as.integer(
  placebo_impact_date - nearest_observed_impact_date
))]
temporal_active <- temporal_calendar[
  as.character(event_period) != "None",
  .(zip = as.character(zip), date)
]
observed_active <- observed_calendar[
  as.character(event_period) != "None",
  .(zip = as.character(zip), date)
]
temporal_observed_overlap <- merge(
  temporal_active,
  observed_active,
  by = c("zip", "date")
)
temporal_overlap_zip_days <- nrow(temporal_observed_overlap)
temporal_overlap_percentage <-
  100 * temporal_overlap_zip_days / nrow(temporal_active)
temporal_near_observed_13 <- nearest_observed[
  nearest_observed_gap_days <= 13L,
  .N
]
temporal_near_observed_27 <- nearest_observed[
  nearest_observed_gap_days <= 27L,
  .N
]
rm(temporal_source, temporal_events)
gc()

message("Building the fixed within-date spatial-permutation calendar")
spatial_seed <- 20260224L
spatial_calendar <- copy(observed_calendar)
set.seed(spatial_seed)
spatial_calendar[, event_period_permuted := sample(as.character(event_period)),
  by = date
]
spatial_calendar[, event_period := factor(
  event_period_permuted,
  levels = sensitivity_period_levels
)]
spatial_calendar[, `:=`(
  event_period_permuted = NULL,
  impact_date = as.IDate(NA)
)]

observed_distribution <- observed_calendar[, .N,
  by = .(date, event_period = as.character(event_period))
]
spatial_distribution <- spatial_calendar[, .N,
  by = .(date, event_period = as.character(event_period))
]
setorder(observed_distribution, date, event_period)
setorder(spatial_distribution, date, event_period)
spatial_distribution_preserved <- identical(
  observed_distribution,
  spatial_distribution
)
rm(observed_distribution, spatial_distribution)
gc()

calendar_diagnostics <- rbindlist(list(
  data.table(
    placebo_type = "Temporal placebo",
    source = temporal_source_path,
    seed = 20260207L,
    zip_days = nrow(temporal_calendar),
    panel_zips = temporal_result$panel_zips,
    zip_specific_events = temporal_result$zip_events,
    distinct_impact_dates = temporal_result$distinct_impact_dates,
    overlap_assignments_resolved = temporal_result$overlap_assignments,
    active_window_zip_days = nrow(temporal_active),
    overlapping_observed_window_zip_days = temporal_overlap_zip_days,
    observed_window_overlap_percentage = temporal_overlap_percentage,
    events_with_observed_impact_within_13_days = temporal_near_observed_13,
    events_with_observed_impact_within_27_days = temporal_near_observed_27,
    event_period_distribution_preserved_within_date = NA
  ),
  data.table(
    placebo_type = "Spatial placebo",
    source = "Observed 34-kt calendar permuted within date",
    seed = spatial_seed,
    zip_days = nrow(spatial_calendar),
    panel_zips = uniqueN(spatial_calendar$zip),
    zip_specific_events = observed_result$zip_events,
    distinct_impact_dates = observed_result$distinct_impact_dates,
    overlap_assignments_resolved = observed_result$overlap_assignments,
    active_window_zip_days = spatial_calendar[
      as.character(event_period) != "None",
      .N
    ],
    overlapping_observed_window_zip_days = NA_integer_,
    observed_window_overlap_percentage = NA_real_,
    events_with_observed_impact_within_13_days = NA_integer_,
    events_with_observed_impact_within_27_days = NA_integer_,
    event_period_distribution_preserved_within_date =
      spatial_distribution_preserved
  )
))
calendar_diagnostics[, analysis_version := placebo_version]

calendar_list <- list(
  "Temporal placebo" = temporal_calendar,
  "Spatial placebo" = spatial_calendar
)
placebo_types <- names(calendar_list)
model_grid <- CJ(
  placebo_type = placebo_types,
  age_group = sensitivity_age_levels,
  unique = TRUE
)
model_grid[, `:=`(
  placebo_order = match(placebo_type, placebo_types),
  age_order = match(age_group, sensitivity_age_levels)
)]
setorder(model_grid, placebo_order, age_order)

estimate_paths <- file.path(
  intermediate_dir,
  paste0(
    sensitivity_slugify(model_grid$placebo_type),
    "_all_diseases_",
    sensitivity_slugify(model_grid$age_group),
    "_estimates.csv"
  )
)
diagnostic_paths <- sub("_estimates\\.csv$", "_diagnostics.csv", estimate_paths)
count_paths <- sub("_estimates\\.csv$", "_counts.csv", estimate_paths)
complete <- file.exists(estimate_paths) &
  file.exists(diagnostic_paths) &
  file.exists(count_paths)

progress <- txtProgressBar(min = 0L, max = nrow(model_grid), style = 3L)
if (!all(complete)) {
  message("Loading mortality records for eight all-disease placebo models")
  complete_window_dates <- unique(c(
    observed_result$complete_window_dates,
    temporal_result$complete_window_dates
  ))
  deaths <- load_sensitivity_deaths(wind_threshold, complete_window_dates)
  counts <- build_cause_daily_counts(deaths, "All diseases")
  fixest::setFixest_notes(FALSE)

  for (model_i in seq_len(nrow(model_grid))) {
    if (complete[model_i]) {
      setTxtProgressBar(progress, model_i)
      next
    }
    placebo_name <- model_grid$placebo_type[model_i]
    age_name <- model_grid$age_group[model_i]
    calendar <- calendar_list[[placebo_name]]
    message(
      "\n[", model_i, "/", nrow(model_grid), "] ",
      placebo_name, ", all diseases, age ", age_name
    )
    model_data <- build_group_model_data(counts, calendar, age_name)
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
    estimates <- extract_sensitivity_estimates(model, "event_period::")
    setnames(estimates, "term", "period")
    estimates[, `:=`(
      cause = "All diseases",
      age_group = age_name,
      placebo_type = placebo_name,
      specification = placebo_name,
      placebo_order = model_grid$placebo_order[model_i],
      age_order = model_grid$age_order[model_i],
      period_order = match(period, sensitivity_period_levels[-1L]),
      wind_threshold_kt = wind_threshold,
      analysis_version = placebo_version
    )]
    diagnostics <- data.table(
      cause = "All diseases",
      age_group = age_name,
      placebo_type = placebo_name,
      specification = placebo_name,
      placebo_order = model_grid$placebo_order[model_i],
      age_order = model_grid$age_order[model_i],
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
      wind_threshold_kt = wind_threshold,
      analysis_version = placebo_version
    )
    sample_counts[, `:=`(
      cause = "All diseases",
      age_group = age_name,
      placebo_type = placebo_name,
      specification = placebo_name,
      placebo_order = model_grid$placebo_order[model_i],
      age_order = model_grid$age_order[model_i],
      period = as.character(event_period),
      period_order = match(
        as.character(event_period),
        sensitivity_period_levels
      ),
      wind_threshold_kt = wind_threshold,
      analysis_version = placebo_version
    )]

    fwrite(estimates, estimate_paths[model_i])
    fwrite(diagnostics, diagnostic_paths[model_i])
    fwrite(sample_counts, count_paths[model_i])
    setTxtProgressBar(progress, model_i)
    rm(model_data, model, estimates, diagnostics, sample_counts)
    gc()
  }
  rm(deaths, counts)
  gc()
} else {
  setTxtProgressBar(progress, nrow(model_grid))
  message("Resuming from eight complete placebo-model checkpoints")
}
close(progress)

estimates <- rbindlist(lapply(estimate_paths, fread), use.names = TRUE)
diagnostics <- rbindlist(lapply(diagnostic_paths, fread), use.names = TRUE)
sample_counts <- rbindlist(lapply(count_paths, fread), use.names = TRUE)
setorder(estimates, placebo_order, age_order, period_order)
setorder(diagnostics, placebo_order, age_order)
setorder(sample_counts, placebo_order, age_order, period_order)

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
  placebo_minus_frozen_log_rate_ratio =
    log_rate_ratio - frozen_log_rate_ratio,
  direction_agrees_with_frozen =
    sign(log_rate_ratio) == sign(frozen_log_rate_ratio),
  confidence_interval_overlaps_frozen =
    ci_lower <= frozen_ci_upper & ci_upper >= frozen_ci_lower,
  frozen_ci_excludes_null =
    frozen_ci_lower > 1 | frozen_ci_upper < 1,
  placebo_ci_excludes_null = ci_lower > 1 | ci_upper < 1
)]
setorder(estimates, placebo_order, age_order, period_order)

estimate_path <- file.path(placebo_dir, "placebo_estimates_34kt.csv")
diagnostic_path <- file.path(placebo_dir, "placebo_diagnostics_34kt.csv")
count_path <- file.path(placebo_dir, "placebo_counts_34kt.csv")
calendar_path <- file.path(placebo_dir, "placebo_calendar_diagnostics_34kt.csv")
qa_path <- file.path(placebo_dir, "placebo_qa_34kt.csv")

qa <- data.table(
  check_name = c(
    "Eight all-disease placebo models",
    "Four period estimates per model",
    "Thirty-two placebo estimates",
    "All models converged",
    "Finite ordered confidence intervals",
    "Spatial distribution preserved within every date",
    "Frozen comparison fields complete",
    "Both placebo specifications represented"
  ),
  status = c(
    if (nrow(diagnostics) == 8L) "PASS" else "FAIL",
    if (all(estimates[, .N, by = .(placebo_type, age_group)]$N == 4L))
      "PASS" else "FAIL",
    if (nrow(estimates) == 32L) "PASS" else "FAIL",
    if (all(diagnostics$model_converged)) "PASS" else "FAIL",
    if (all(
      is.finite(estimates$rate_ratio) &
        estimates$ci_lower <= estimates$rate_ratio &
        estimates$rate_ratio <= estimates$ci_upper
    )) "PASS" else "FAIL",
    if (spatial_distribution_preserved) "PASS" else "FAIL",
    if (!anyNA(estimates[, .(
      frozen_log_rate_ratio,
      frozen_rate_ratio,
      placebo_minus_frozen_log_rate_ratio,
      direction_agrees_with_frozen,
      confidence_interval_overlaps_frozen
    )])) "PASS" else "FAIL",
    if (setequal(unique(estimates$placebo_type), placebo_types))
      "PASS" else "FAIL"
  ),
  observed = c(
    nrow(diagnostics),
    paste(unique(estimates[, .N, by = .(placebo_type, age_group)]$N),
      collapse = ","
    ),
    nrow(estimates),
    sum(diagnostics$model_converged),
    sum(
      is.finite(estimates$rate_ratio) &
        estimates$ci_lower <= estimates$rate_ratio &
        estimates$rate_ratio <= estimates$ci_upper
    ),
    as.character(spatial_distribution_preserved),
    sum(complete.cases(estimates[, .(
      frozen_log_rate_ratio,
      frozen_rate_ratio,
      placebo_minus_frozen_log_rate_ratio,
      direction_agrees_with_frozen,
      confidence_interval_overlaps_frozen
    )])),
    paste(sort(unique(estimates$placebo_type)), collapse = "; ")
  ),
  expected = c(
    8L,
    4L,
    32L,
    8L,
    32L,
    "TRUE",
    32L,
    paste(sort(placebo_types), collapse = "; ")
  )
)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more placebo sensitivity checks failed.")
}

source_commit <- get_sensitivity_source_commit()
for (object_name in c(
  "estimates",
  "diagnostics",
  "sample_counts",
  "calendar_diagnostics"
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
fwrite(calendar_diagnostics, calendar_path)
fwrite(qa, qa_path)

cat("\nPlacebo sensitivity completed\n")
cat("Models:", nrow(diagnostics), "\n")
cat("Estimates:", nrow(estimates), "\n")
cat(
  "Placebo CIs excluding 1:",
  estimates[, sum(placebo_ci_excludes_null)],
  "of", nrow(estimates), "\n"
)
cat("QA checks:", nrow(qa), "all PASS\n")
cat("Output directory:", placebo_dir, "\n")
