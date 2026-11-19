#!/usr/bin/env Rscript

source("scripts/mortality/sensitivity_model_helpers.R")

wind_threshold <- 34L
permutation_count <- as.integer(Sys.getenv("STRICT_PLACEBO_B", "199"))
worker_count <- as.integer(Sys.getenv("STRICT_PLACEBO_WORKERS", "2"))
base_seed <- 20261119L
minimum_gap_days <- 28L
permutation_version <- "strict_temporal_permutation_v1_2026-11-19"
permutation_dir <- file.path(
  "outputs/mortality/paper_sensitivity_34kt",
  permutation_version
)
checkpoint_dir <- file.path(permutation_dir, "checkpoints")
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

if (!is.finite(permutation_count) || permutation_count < 19L) {
  stop("STRICT_PLACEBO_B must be at least 19.")
}
if (!is.finite(worker_count) || worker_count < 1L) {
  stop("STRICT_PLACEBO_WORKERS must be at least 1.")
}

master_path <- paste0(
  "outputs/mortality/paper_master_34kt/amended_v2_2026-11-18/",
  "master_results_34kt_amended_v2.csv"
)
daily_panel_path <- paste0(
  "data/mortality/processed/death-panels/daily_panel_34kt.csv"
)
if (!file.exists(master_path)) stop("Missing amended master results.")
if (!file.exists(daily_panel_path)) stop("Missing observed 34-kt daily panel.")

message("Reading observed ZIP-specific impact dates")
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
  observed_impact_date = date - rel_day
)])
event_dates <- sort(unique(zip_events$observed_impact_date))
if (length(event_dates) != 77L) {
  stop("Expected 77 distinct observed impact dates; found ", length(event_dates), ".")
}

candidate_list <- vector("list", length(event_dates))
candidate_diagnostics <- vector("list", length(event_dates))
for (event_i in seq_along(event_dates)) {
  observed_date <- event_dates[event_i]
  footprint_zips <- zip_events[
    observed_impact_date == observed_date,
    zip
  ]
  relevant_real_dates <- unique(zip_events[
    zip %in% footprint_zips,
    observed_impact_date
  ])
  event_year <- year(as.Date(observed_date))
  candidate_dates <- as.IDate(seq(
    as.Date(sprintf("%d-05-15", event_year)),
    as.Date(sprintf("%d-11-17", event_year)),
    by = "day"
  ))
  observed_weekday <- wday(as.Date(observed_date), week_start = 1L)
  candidate_dates <- candidate_dates[
    wday(as.Date(candidate_dates), week_start = 1L) == observed_weekday
  ]
  candidate_is_valid <- vapply(
    candidate_dates,
    function(candidate_date) {
      all(abs(as.integer(candidate_date - relevant_real_dates)) >=
        minimum_gap_days)
    },
    logical(1L)
  )
  candidate_dates <- candidate_dates[candidate_is_valid]
  if (!length(candidate_dates)) {
    stop("No valid strict-placebo date for observed event ", observed_date, ".")
  }
  candidate_list[[event_i]] <- candidate_dates
  candidate_diagnostics[[event_i]] <- data.table(
    event_order = event_i,
    observed_impact_date = observed_date,
    event_year = event_year,
    weekday = weekdays(as.Date(observed_date)),
    footprint_zips = length(footprint_zips),
    relevant_real_impact_dates = length(relevant_real_dates),
    valid_candidate_dates = length(candidate_dates),
    earliest_candidate_date = min(candidate_dates),
    latest_candidate_date = max(candidate_dates),
    minimum_gap_days = minimum_gap_days
  )
}
candidate_diagnostics <- rbindlist(candidate_diagnostics)
if (any(candidate_diagnostics$valid_candidate_dates < 1L)) {
  stop("At least one observed event has no valid strict placebo date.")
}

message("Building the constant May-November estimation panel")
observed_result <- build_sensitivity_calendar(wind_threshold, daily = FALSE)
calendar <- observed_result$data[
  month(as.Date(date)) %in% 5:11
]
calendar[, event_period := factor("None", levels = sensitivity_period_levels)]
setorder(calendar, zip, date)

deaths <- load_sensitivity_deaths(
  wind_threshold,
  observed_result$complete_window_dates
)
counts <- build_cause_daily_counts(deaths, "All diseases")
model_data <- build_group_model_data(counts, calendar, "All ages")
model_data[, placebo_period_code := 0L]
model_data[, c("event_period", "impact_date", "covid") := NULL]
setkey(model_data, zip, date)
rm(deaths, counts, calendar, calendar_source)
gc()

observed_active <- observed_result$data[
  as.character(event_period) != "None",
  .(zip, date)
]
setkey(observed_active, zip, date)

estimate_paths <- file.path(
  checkpoint_dir,
  sprintf("permutation_%03d_estimates.csv", seq_len(permutation_count))
)
diagnostic_paths <- file.path(
  checkpoint_dir,
  sprintf("permutation_%03d_diagnostics.csv", seq_len(permutation_count))
)
assignment_paths <- file.path(
  checkpoint_dir,
  sprintf("permutation_%03d_assignments.csv", seq_len(permutation_count))
)
complete <- file.exists(estimate_paths) &
  file.exists(diagnostic_paths) &
  file.exists(assignment_paths)
pending <- which(!complete)

run_permutation_group <- function(permutation_indices, worker_id) {
  suppressPackageStartupMessages({
    library(data.table)
    library(fixest)
  })
  setDTthreads(1L)
  setFixest_nthreads(1L)
  worker_data <- model_data

  for (position_i in seq_along(permutation_indices)) {
    permutation_i <- permutation_indices[position_i]
    set.seed(base_seed + permutation_i)
    placebo_dates <- as.IDate(vapply(
      candidate_list,
      function(candidate_dates) {
        as.integer(sample(candidate_dates, size = 1L))
      },
      integer(1L)
    ))
    assignments <- data.table(
      permutation = permutation_i,
      event_order = seq_along(event_dates),
      observed_impact_date = event_dates,
      placebo_impact_date = placebo_dates,
      valid_candidate_dates = vapply(candidate_list, length, integer(1L)),
      seed = base_seed + permutation_i
    )
    placebo_zip_events <- merge(
      zip_events,
      assignments[, .(observed_impact_date, placebo_impact_date)],
      by = "observed_impact_date",
      all.x = TRUE,
      sort = FALSE
    )
    expanded <- placebo_zip_events[, .(rel_day = -14L:13L),
      by = .(zip, observed_impact_date, placebo_impact_date)
    ]
    expanded[, date := placebo_impact_date + rel_day]
    raw_assignments <- nrow(expanded)
    labels <- expanded[
      order(zip, date, abs(rel_day), rel_day, placebo_impact_date)
    ][, .SD[1L], by = .(zip, date)]
    labels[, placebo_period_code := fcase(
      rel_day >= -14L & rel_day <= -8L, 1L,
      rel_day >= -7L & rel_day <= -1L, 2L,
      rel_day >= 0L & rel_day <= 6L, 3L,
      rel_day >= 7L & rel_day <= 13L, 4L,
      default = NA_integer_
    )]
    labels[, zip := factor(zip, levels = levels(worker_data$zip))]
    actual_overlap_zip_days <- observed_active[
      labels,
      on = .(zip, date),
      nomatch = 0L,
      .N
    ]
    if (actual_overlap_zip_days != 0L) {
      stop(
        "Strict permutation ", permutation_i,
        " overlaps ", actual_overlap_zip_days, " observed window ZIP-days."
      )
    }

    worker_data[
      labels,
      on = .(zip, date),
      placebo_period_code := i.placebo_period_code
    ]
    beta <- fepois(
      deaths ~ i(placebo_period_code, ref = 0L) |
        zip + yw + dow + zip_covid,
      data = worker_data,
      nthreads = 1L,
      lean = TRUE,
      only.coef = TRUE,
      warn = FALSE,
      notes = FALSE
    )
    worker_data[
      labels,
      on = .(zip, date),
      placebo_period_code := 0L
    ]

    coefficient_names <- paste0("placebo_period_code::", 1:4)
    if (!all(coefficient_names %in% names(beta))) {
      stop("Missing coefficient in strict permutation ", permutation_i, ".")
    }
    permutation_estimates <- data.table(
      permutation = permutation_i,
      period = sensitivity_period_levels[-1L],
      period_order = 1:4,
      log_rate_ratio = unname(beta[coefficient_names]),
      rate_ratio = exp(unname(beta[coefficient_names])),
      seed = base_seed + permutation_i,
      analysis_version = permutation_version
    )
    permutation_diagnostics <- data.table(
      permutation = permutation_i,
      seed = base_seed + permutation_i,
      observed_events = length(event_dates),
      placebo_zip_specific_events = nrow(placebo_zip_events),
      raw_window_assignments = raw_assignments,
      resolved_window_zip_days = nrow(labels),
      overlapping_placebo_assignments_resolved =
        raw_assignments - nrow(labels),
      observed_window_overlap_zip_days = actual_overlap_zip_days,
      distinct_placebo_impact_dates = uniqueN(placebo_dates),
      finite_coefficients = all(is.finite(beta[coefficient_names])),
      analysis_version = permutation_version
    )
    fwrite(permutation_estimates, estimate_paths[permutation_i])
    fwrite(permutation_diagnostics, diagnostic_paths[permutation_i])
    fwrite(assignments, assignment_paths[permutation_i])

    if (
      position_i == 1L || position_i == length(permutation_indices) ||
        position_i %% 5L == 0L
    ) {
      message(
        "Worker ", worker_id, ": completed permutation ",
        permutation_i, " (", position_i, "/",
        length(permutation_indices), " assigned)"
      )
    }
    rm(
      assignments,
      placebo_zip_events,
      expanded,
      labels,
      beta,
      permutation_estimates,
      permutation_diagnostics
    )
    gc(FALSE)
  }
  TRUE
}

if (length(pending)) {
  workers_used <- min(worker_count, length(pending))
  pending_groups <- split(
    pending,
    rep(seq_len(workers_used), length.out = length(pending))
  )
  message(
    "Fitting ", length(pending), " pending strict temporal permutations with ",
    workers_used, " worker(s)"
  )
  if (workers_used == 1L) {
    run_permutation_group(pending_groups[[1L]], 1L)
  } else {
    worker_results <- parallel::mclapply(
      seq_along(pending_groups),
      function(worker_i) {
        run_permutation_group(pending_groups[[worker_i]], worker_i)
      },
      mc.cores = workers_used,
      mc.preschedule = TRUE
    )
    if (!all(vapply(worker_results, isTRUE, logical(1L)))) {
      stop("At least one strict-permutation worker failed.")
    }
  }
} else {
  message("Resuming from ", permutation_count, " complete permutation checkpoints")
}

if (!all(
  file.exists(estimate_paths) &
    file.exists(diagnostic_paths) &
    file.exists(assignment_paths)
)) {
  stop("Not all strict temporal permutation checkpoints are complete.")
}

permutation_estimates <- rbindlist(lapply(estimate_paths, fread))
permutation_diagnostics <- rbindlist(lapply(diagnostic_paths, fread))
permutation_assignments <- rbindlist(lapply(assignment_paths, fread))
setorder(permutation_estimates, permutation, period_order)
setorder(permutation_diagnostics, permutation)
setorder(permutation_assignments, permutation, event_order)

observed <- fread(master_path)[
  cause == "All diseases" & age_group == "All ages",
  .(
    period,
    period_order,
    observed_log_rate_ratio = log_rate_ratio,
    observed_rate_ratio = rate_ratio,
    observed_ci_lower = ci_lower,
    observed_ci_upper = ci_upper,
    observed_p_value = p_value
  )
]
setorder(observed, period_order)

null_summary <- permutation_estimates[, .(
  permutation_count = .N,
  null_log_rr_mean = mean(log_rate_ratio),
  null_log_rr_median = median(log_rate_ratio),
  null_log_rr_sd = sd(log_rate_ratio),
  null_rr_median = median(rate_ratio),
  null_rr_025 = quantile(rate_ratio, 0.025),
  null_rr_975 = quantile(rate_ratio, 0.975)
), by = .(period, period_order)]
summary_results <- merge(
  observed,
  null_summary,
  by = c("period", "period_order"),
  all.x = TRUE,
  sort = FALSE
)
empirical_p_upper <- vapply(
  seq_len(nrow(summary_results)),
  function(result_i) {
    period_name <- summary_results$period[result_i]
    observed_value <- summary_results$observed_log_rate_ratio[result_i]
    null_values <- permutation_estimates$log_rate_ratio[
      permutation_estimates$period == period_name
    ]
    (1 + sum(null_values >= observed_value)) / (permutation_count + 1)
  },
  numeric(1L)
)
empirical_p_two_sided_centered <- vapply(
  seq_len(nrow(summary_results)),
  function(result_i) {
    period_name <- summary_results$period[result_i]
    observed_value <- summary_results$observed_log_rate_ratio[result_i]
    null_values <- permutation_estimates$log_rate_ratio[
      permutation_estimates$period == period_name
    ]
    null_center <- median(null_values)
    (1 + sum(
      abs(null_values - null_center) >= abs(observed_value - null_center)
    )) / (permutation_count + 1)
  },
  numeric(1L)
)
summary_results[, `:=`(
  empirical_p_upper = empirical_p_upper,
  empirical_p_two_sided_centered = empirical_p_two_sided_centered
)]

null_location <- summary_results$null_log_rr_median
null_scale <- summary_results$null_log_rr_sd
observed_standardized <-
  (summary_results$observed_log_rate_ratio - null_location) / null_scale
permutation_matrix <- dcast(
  permutation_estimates,
  permutation ~ period_order,
  value.var = "log_rate_ratio"
)
permutation_values <- as.matrix(permutation_matrix[, -"permutation"])
permutation_standardized <- sweep(
  sweep(permutation_values, 2L, null_location, "-"),
  2L,
  null_scale,
  "/"
)
permutation_max_standardized <- apply(permutation_standardized, 1L, max)
permutation_count_scalar <- permutation_count
empirical_p_upper_max_t <- vapply(
  observed_standardized,
  function(observed_z) {
    (1 + sum(permutation_max_standardized >= observed_z)) /
      (permutation_count_scalar + 1)
  },
  numeric(1L)
)
summary_results[, empirical_p_upper_max_t := empirical_p_upper_max_t]
global_observed_max_standardized <- max(observed_standardized)
global_empirical_p_max_t <-
  (1 + sum(
    permutation_max_standardized >= global_observed_max_standardized
  )) / (permutation_count + 1)
summary_results[, `:=`(
  observed_standardized_against_null = observed_standardized,
  global_observed_max_standardized = global_observed_max_standardized,
  global_empirical_p_max_t = global_empirical_p_max_t,
  minimum_gap_days = minimum_gap_days,
  base_seed = base_seed,
  analysis_version = permutation_version
)]
setorder(summary_results, period_order)

qa <- data.table(
  check_name = c(
    "Expected observed impact dates",
    "Every event has a valid candidate date",
    "Expected completed permutations",
    "Four estimates per permutation",
    "All permutation coefficients finite",
    "No placebo window overlaps an observed window",
    "Every assignment preserves event year",
    "Every assignment preserves weekday",
    "Every placebo window remains within May-November",
    "Empirical p-values in valid range"
  ),
  status = c(
    if (length(event_dates) == 77L) "PASS" else "FAIL",
    if (all(candidate_diagnostics$valid_candidate_dates >= 1L))
      "PASS" else "FAIL",
    if (uniqueN(permutation_estimates$permutation) == permutation_count)
      "PASS" else "FAIL",
    if (all(permutation_estimates[, .N, by = permutation]$N == 4L))
      "PASS" else "FAIL",
    if (all(is.finite(permutation_estimates$log_rate_ratio)))
      "PASS" else "FAIL",
    if (all(permutation_diagnostics$observed_window_overlap_zip_days == 0L))
      "PASS" else "FAIL",
    if (all(
      year(as.Date(permutation_assignments$observed_impact_date)) ==
        year(as.Date(permutation_assignments$placebo_impact_date))
    )) "PASS" else "FAIL",
    if (all(
      wday(as.Date(permutation_assignments$observed_impact_date), week_start = 1L) ==
        wday(as.Date(permutation_assignments$placebo_impact_date), week_start = 1L)
    )) "PASS" else "FAIL",
    if (all(
      month(as.Date(permutation_assignments$placebo_impact_date - 14L)) >= 5L &
        month(as.Date(permutation_assignments$placebo_impact_date + 13L)) <= 11L
    )) "PASS" else "FAIL",
    if (all(
      summary_results$empirical_p_upper >= 0 &
        summary_results$empirical_p_upper <= 1 &
        summary_results$empirical_p_upper_max_t >= 0 &
        summary_results$empirical_p_upper_max_t <= 1
    )) "PASS" else "FAIL"
  ),
  observed = c(
    length(event_dates),
    min(candidate_diagnostics$valid_candidate_dates),
    uniqueN(permutation_estimates$permutation),
    paste(unique(permutation_estimates[, .N, by = permutation]$N), collapse = ","),
    sum(is.finite(permutation_estimates$log_rate_ratio)),
    sum(permutation_diagnostics$observed_window_overlap_zip_days),
    sum(
      year(as.Date(permutation_assignments$observed_impact_date)) ==
        year(as.Date(permutation_assignments$placebo_impact_date))
    ),
    sum(
      wday(as.Date(permutation_assignments$observed_impact_date), week_start = 1L) ==
        wday(as.Date(permutation_assignments$placebo_impact_date), week_start = 1L)
    ),
    sum(
      month(as.Date(permutation_assignments$placebo_impact_date - 14L)) >= 5L &
        month(as.Date(permutation_assignments$placebo_impact_date + 13L)) <= 11L
    ),
    sum(
      summary_results$empirical_p_upper >= 0 &
        summary_results$empirical_p_upper <= 1 &
        summary_results$empirical_p_upper_max_t >= 0 &
        summary_results$empirical_p_upper_max_t <= 1
    )
  ),
  expected = c(
    77L,
    ">=1",
    permutation_count,
    4L,
    4L * permutation_count,
    0L,
    nrow(permutation_assignments),
    nrow(permutation_assignments),
    nrow(permutation_assignments),
    4L
  )
)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more strict temporal permutation checks failed.")
}

source_commit <- get_sensitivity_source_commit()
for (object_name in c(
  "candidate_diagnostics",
  "permutation_estimates",
  "permutation_diagnostics",
  "permutation_assignments",
  "summary_results"
)) {
  object <- get(object_name)
  object[, `:=`(
    source_commit = source_commit,
    build_date = as.IDate("2026-11-19")
  )]
  assign(object_name, object)
}

fwrite(
  candidate_diagnostics,
  file.path(permutation_dir, "strict_temporal_candidate_diagnostics_34kt.csv")
)
fwrite(
  permutation_estimates,
  file.path(permutation_dir, "strict_temporal_permutation_estimates_34kt.csv")
)
fwrite(
  permutation_diagnostics,
  file.path(permutation_dir, "strict_temporal_permutation_diagnostics_34kt.csv")
)
fwrite(
  permutation_assignments,
  file.path(permutation_dir, "strict_temporal_permutation_assignments_34kt.csv")
)
fwrite(
  summary_results,
  file.path(permutation_dir, "strict_temporal_permutation_summary_34kt.csv")
)
fwrite(
  qa,
  file.path(permutation_dir, "strict_temporal_permutation_qa_34kt.csv")
)

cat("\nStrict temporal permutation analysis completed\n")
cat("Permutations:", permutation_count, "\n")
cat("Minimum gap from every relevant observed impact:", minimum_gap_days, "days\n")
cat("Global four-period max-T empirical p:", global_empirical_p_max_t, "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
cat("Output directory:", permutation_dir, "\n")
