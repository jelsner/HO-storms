#!/usr/bin/env Rscript

source("scripts/mortality/sensitivity_model_helpers.R")

wind_threshold <- 34L
spatial_target_path <- paste0(
  "outputs/mortality/paper_spatial_34kt/spatial_frozen_v2_2026-11-18/",
  "spatial_targets_34kt_frozen_v2.csv"
)
master_path <- paste0(
  "outputs/mortality/paper_master_34kt/amended_v2_2026-11-18/",
  "master_results_34kt_amended_v2.csv"
)
if (!file.exists(spatial_target_path) || !file.exists(master_path)) {
  stop("Missing amended spatial targets or master results.")
}

daily_dir <- file.path(sensitivity_output_dir, "daily_event_time_34kt")
intermediate_dir <- file.path(daily_dir, "intermediate")
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)

targets <- fread(spatial_target_path)
setorder(targets, target_order)
if (nrow(targets) != 8L) {
  stop("Expected eight amended spatial targets.")
}

all_disease_specs <- data.table(
  profile_type = "All-disease age profile",
  profile_id = paste0("DAILY-ALL-A", sprintf("%02d", seq_along(sensitivity_age_levels))),
  cause = "All diseases",
  age_group = sensitivity_age_levels,
  profile_order = seq_along(sensitivity_age_levels)
)
spatial_specs <- unique(targets[, .(cause, age_group)])
spatial_specs[, `:=`(
  profile_type = "Spatial-target cause-age profile",
  profile_id = paste0("DAILY-SPATIAL-", sprintf("%02d", .I)),
  profile_order = length(sensitivity_age_levels) + .I
)]
setcolorder(
  spatial_specs,
  c("profile_type", "profile_id", "cause", "age_group", "profile_order")
)
profile_specs <- rbind(all_disease_specs, spatial_specs)
if (nrow(profile_specs) != 10L) {
  stop("Expected four all-disease and six unique spatial-target profiles.")
}

estimate_paths <- file.path(
  intermediate_dir,
  paste0(sensitivity_slugify(profile_specs$profile_id), "_estimates.csv")
)
diagnostic_paths <- file.path(
  intermediate_dir,
  paste0(sensitivity_slugify(profile_specs$profile_id), "_diagnostics.csv")
)
count_paths <- file.path(
  intermediate_dir,
  paste0(sensitivity_slugify(profile_specs$profile_id), "_counts.csv")
)
complete_profiles <- file.exists(estimate_paths) &
  file.exists(diagnostic_paths) &
  file.exists(count_paths)

message("Building the 34-kt daily event-time calendar")
calendar_result <- build_sensitivity_calendar(wind_threshold, daily = TRUE)
calendar <- calendar_result$data

if (!all(complete_profiles)) {
  message("Loading mortality records once for daily event-time fits")
  deaths <- load_sensitivity_deaths(
    wind_threshold,
    calendar_result$complete_window_dates
  )
  message("Precomputing retained cause masks")
  cause_masks <- build_cause_masks(deaths$year, deaths$code)
  fixest::setFixest_notes(FALSE)

  for (cause_name in unique(profile_specs[!complete_profiles, cause])) {
    message("Preparing daily counts for ", cause_name)
    counts <- build_cause_daily_counts(
      deaths,
      cause_name,
      cause_masks[[cause_name]]
    )
    cause_indices <- which(
      profile_specs$cause == cause_name & !complete_profiles
    )

    for (profile_index in cause_indices) {
      spec <- profile_specs[profile_index]
      message(
        "[", profile_index, "/", nrow(profile_specs), "] ",
        spec$cause, ", age ", spec$age_group
      )
      model_data <- build_group_model_data(
        counts,
        calendar,
        spec$age_group
      )
      sample_counts <- model_data[, .(
        zip_days = .N,
        deaths = sum(deaths),
        nonzero_zip_days = sum(deaths > 0L)
      ), by = rel_day_factor]

      model <- fepois(
        deaths ~ i(rel_day_factor, ref = "None") |
          zip + yw + dow + zip_covid,
        data = model_data,
        cluster = ~ zip + yw,
        notes = FALSE
      )
      estimates <- extract_sensitivity_estimates(
        model,
        "rel_day_factor::"
      )
      setnames(estimates, "term", "rel_day")
      estimates[, rel_day := as.integer(rel_day)]
      estimates[, `:=`(
        profile_type = spec$profile_type,
        profile_id = spec$profile_id,
        profile_order = spec$profile_order,
        cause = spec$cause,
        age_group = spec$age_group,
        wind_threshold_kt = wind_threshold,
        analysis_version = sensitivity_version
      )]
      sample_counts[, `:=`(
        profile_type = spec$profile_type,
        profile_id = spec$profile_id,
        profile_order = spec$profile_order,
        cause = spec$cause,
        age_group = spec$age_group,
        wind_threshold_kt = wind_threshold,
        rel_day = suppressWarnings(as.integer(as.character(rel_day_factor))),
        analysis_version = sensitivity_version
      )]
      diagnostics <- data.table(
        profile_type = spec$profile_type,
        profile_id = spec$profile_id,
        profile_order = spec$profile_order,
        cause = spec$cause,
        age_group = spec$age_group,
        wind_threshold_kt = wind_threshold,
        input_rows = nrow(model_data),
        model_observations = nobs(model),
        removed_observations = nrow(model_data) - nobs(model),
        model_converged = if (is.null(model$convStatus)) TRUE else isTRUE(model$convStatus),
        coefficient_count = length(coef(model)),
        log_likelihood = as.numeric(logLik(model)),
        pseudo_r2 = safe_sensitivity_fitstat(model, "pr2"),
        squared_correlation = safe_sensitivity_fitstat(model, "sq.cor"),
        analysis_version = sensitivity_version
      )

      fwrite(estimates, estimate_paths[profile_index])
      fwrite(diagnostics, diagnostic_paths[profile_index])
      fwrite(sample_counts, count_paths[profile_index])
      rm(model_data, model, estimates, diagnostics, sample_counts)
      gc()
    }
    rm(counts)
    gc()
  }
  rm(deaths, cause_masks)
  gc()
} else {
  message("Resuming from ten complete daily-profile checkpoints")
}

estimates <- rbindlist(lapply(estimate_paths, fread), use.names = TRUE)
diagnostics <- rbindlist(lapply(diagnostic_paths, fread), use.names = TRUE)
sample_counts <- rbindlist(lapply(count_paths, fread), use.names = TRUE)
setorder(estimates, profile_order, rel_day)
setorder(diagnostics, profile_order)
setorder(sample_counts, profile_order, rel_day)

spatial_estimates <- merge(
  estimates[profile_type == "Spatial-target cause-age profile"],
  targets[, .(
    target_id,
    target_order,
    cause,
    age_group,
    target_period = period,
    target_statewide_rate_ratio = pooled_rate_ratio
  )],
  by = c("cause", "age_group"),
  allow.cartesian = TRUE,
  all.x = TRUE,
  sort = FALSE
)
setorder(spatial_estimates, target_order, rel_day)

master <- fread(master_path)
seven_day_comparison <- rbindlist(list(
  master[cause == "All diseases", .(
    comparison_type = "All-disease age profile",
    target_id = NA_character_,
    target_order = NA_integer_,
    cause,
    age_group,
    period,
    rate_ratio,
    ci_lower,
    ci_upper
  )],
  targets[, .(
    comparison_type = "Spatial target",
    target_id,
    target_order,
    cause,
    age_group,
    period,
    rate_ratio = pooled_rate_ratio,
    ci_lower = frozen_ci_lower,
    ci_upper = frozen_ci_upper
  )]
), use.names = TRUE)
seven_day_comparison[, analysis_version := sensitivity_version]

estimate_path <- file.path(daily_dir, "daily_event_time_estimates_34kt.csv")
spatial_path <- file.path(daily_dir, "daily_spatial_target_estimates_34kt.csv")
comparison_path <- file.path(daily_dir, "seven_day_comparison_estimates_34kt.csv")
diagnostic_path <- file.path(daily_dir, "daily_event_time_diagnostics_34kt.csv")
count_path <- file.path(daily_dir, "daily_event_time_counts_34kt.csv")
qa_path <- file.path(daily_dir, "daily_event_time_qa_34kt.csv")

qa <- data.table(
  check_name = c(
    "Ten unique daily profiles",
    "Twenty-eight daily estimates per profile",
    "Four all-disease profiles",
    "Eight expanded spatial targets",
    "All daily models converged",
    "Twenty-eight coefficients per model",
    "Finite ordered estimates",
    "Sixteen all-disease seven-day comparisons",
    "Eight spatial-target seven-day comparisons"
  ),
  status = c(
    if (uniqueN(estimates$profile_id) == 10L) "PASS" else "FAIL",
    if (nrow(estimates) == 280L) "PASS" else "FAIL",
    if (uniqueN(estimates[profile_type == "All-disease age profile", profile_id]) == 4L) "PASS" else "FAIL",
    if (uniqueN(spatial_estimates$target_id) == 8L && nrow(spatial_estimates) == 224L) "PASS" else "FAIL",
    if (all(diagnostics$model_converged)) "PASS" else "FAIL",
    if (all(diagnostics$coefficient_count == 28L)) "PASS" else "FAIL",
    if (all(
      is.finite(estimates$rate_ratio) &
        estimates$ci_lower <= estimates$rate_ratio &
        estimates$rate_ratio <= estimates$ci_upper
    )) "PASS" else "FAIL",
    if (nrow(seven_day_comparison[comparison_type == "All-disease age profile"]) == 16L) "PASS" else "FAIL",
    if (nrow(seven_day_comparison[comparison_type == "Spatial target"]) == 8L) "PASS" else "FAIL"
  ),
  observed = c(
    uniqueN(estimates$profile_id),
    nrow(estimates),
    uniqueN(estimates[profile_type == "All-disease age profile", profile_id]),
    paste0(uniqueN(spatial_estimates$target_id), " targets; ", nrow(spatial_estimates), " rows"),
    sum(diagnostics$model_converged),
    paste(unique(diagnostics$coefficient_count), collapse = ","),
    sum(
      is.finite(estimates$rate_ratio) &
        estimates$ci_lower <= estimates$rate_ratio &
        estimates$rate_ratio <= estimates$ci_upper
    ),
    nrow(seven_day_comparison[comparison_type == "All-disease age profile"]),
    nrow(seven_day_comparison[comparison_type == "Spatial target"])
  ),
  expected = c(10L, 280L, 4L, "8 targets; 224 rows", 10L, 28L, 280L, 16L, 8L)
)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more daily event-time checks failed.")
}

source_commit <- get_sensitivity_source_commit()
for (object_name in c(
  "estimates", "spatial_estimates", "seven_day_comparison",
  "diagnostics", "sample_counts"
)) {
  object <- get(object_name)
  object[, `:=`(
    source_commit = source_commit,
    build_date = as.IDate("2026-11-18")
  )]
  assign(object_name, object)
}

fwrite(estimates, estimate_path)
fwrite(spatial_estimates, spatial_path)
fwrite(seven_day_comparison, comparison_path)
fwrite(diagnostics, diagnostic_path)
fwrite(sample_counts, count_path)
fwrite(qa, qa_path)

cat("\nDaily event-time sensitivity completed\n")
cat("Unique fitted profiles:", uniqueN(estimates$profile_id), "\n")
cat("Daily estimates:", nrow(estimates), "\n")
cat("Expanded spatial-target rows:", nrow(spatial_estimates), "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
cat("Output directory:", daily_dir, "\n")
