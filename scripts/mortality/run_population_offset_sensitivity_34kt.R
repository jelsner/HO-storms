#!/usr/bin/env Rscript

source("scripts/mortality/sensitivity_model_helpers.R")

wind_threshold <- 34L
offset_version <- "population_offset_v1_2026-11-19"
offset_dir <- file.path(
  "outputs/mortality/paper_sensitivity_34kt",
  offset_version
)
intermediate_dir <- file.path(offset_dir, "intermediate")
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)

population_path <-
  "data/mortality/population/florida_zip_population_1985_2022.rds"
master_path <- paste0(
  "outputs/mortality/paper_master_34kt/amended_v2_2026-11-18/",
  "master_results_34kt_amended_v2.csv"
)
if (!file.exists(population_path) || !file.exists(master_path)) {
  stop("Missing annual ZIP population data or amended master results.")
}

offset_causes <- c(
  "All diseases",
  "Cardiovascular",
  "Respiratory",
  "Injuries",
  "Neuropsychiatric disorders"
)
specifications <- c(
  "Matched-sample count model",
  "Population-offset model"
)

message("Building the 34-kt calendar")
calendar_result <- build_sensitivity_calendar(wind_threshold, daily = FALSE)
calendar_full <- calendar_result$data
calendar_full[, `:=`(
  zip = as.character(zip),
  year = lubridate::year(as.Date(date))
)]

population <- as.data.table(readRDS(population_path))[, .(
  year = as.integer(year),
  zip = as.character(zip_code),
  population = as.numeric(population)
)]
if (anyDuplicated(population[, .(zip, year)])) {
  stop("Population data are not unique by ZIP and year.")
}

zip_year_coverage <- unique(calendar_full[, .(zip, year)])
zip_year_coverage <- merge(
  zip_year_coverage,
  population,
  by = c("zip", "year"),
  all.x = TRUE,
  sort = FALSE
)
zip_year_coverage[, positive_population :=
  is.finite(population) & population > 0]

calendar <- merge(
  calendar_full,
  population,
  by = c("zip", "year"),
  all.x = TRUE,
  sort = FALSE
)
calendar[, positive_population :=
  is.finite(population) & population > 0]
calendar <- calendar[positive_population == TRUE]
calendar[, `:=`(
  log_population = log(population),
  zip = factor(zip, levels = sort(unique(zip)))
)]
calendar[, zip_covid := interaction(zip, covid, drop = TRUE)]
setorder(calendar, zip, date)
if (anyDuplicated(calendar[, .(zip, date)])) {
  stop("Population-covered calendar is not unique by ZIP and date.")
}

coverage_summary <- data.table(
  metric = c(
    "Full calendar ZIP-years",
    "Positive-population ZIP-years",
    "Missing/nonpositive ZIP-years",
    "Full calendar ZIPs",
    "Population-covered ZIPs",
    "ZIPs ever missing population",
    "Full calendar ZIP-days",
    "Population-covered ZIP-days",
    "Retained ZIP-day percentage"
  ),
  value = c(
    nrow(zip_year_coverage),
    sum(zip_year_coverage$positive_population),
    sum(!zip_year_coverage$positive_population),
    uniqueN(calendar_full$zip),
    uniqueN(calendar$zip),
    zip_year_coverage[, any(!positive_population), by = zip][V1 == TRUE, .N],
    nrow(calendar_full),
    nrow(calendar),
    100 * nrow(calendar) / nrow(calendar_full)
  ),
  analysis_version = offset_version
)

model_specs <- CJ(
  cause_order = seq_along(offset_causes),
  age_order = seq_along(sensitivity_age_levels),
  specification_order = seq_along(specifications)
)
model_specs[, `:=`(
  cause = offset_causes[cause_order],
  age_group = sensitivity_age_levels[age_order],
  specification = specifications[specification_order]
)]
model_specs[, model_id := paste0(
  "PO-",
  sprintf("C%02d", cause_order),
  "-",
  sprintf("A%02d", age_order),
  "-S",
  sprintf("%02d", specification_order)
)]
model_specs[, checkpoint_stem := sensitivity_slugify(model_id)]
model_specs[, `:=`(
  estimate_path = file.path(
    intermediate_dir,
    paste0(checkpoint_stem, "_estimates.csv")
  ),
  diagnostic_path = file.path(
    intermediate_dir,
    paste0(checkpoint_stem, "_diagnostics.csv")
  ),
  count_path = file.path(
    intermediate_dir,
    paste0(checkpoint_stem, "_counts.csv")
  )
)]
model_specs[, complete :=
  file.exists(estimate_path) &
    file.exists(diagnostic_path) &
    file.exists(count_path)]

progress <- txtProgressBar(min = 0L, max = nrow(model_specs), style = 3L)
completed_count <- 0L
if (!all(model_specs$complete)) {
  message("Loading mortality records once for population-offset fits")
  deaths <- load_sensitivity_deaths(
    wind_threshold,
    calendar_result$complete_window_dates
  )
  message("Precomputing prioritized cause masks")
  cause_masks <- build_cause_masks(deaths$year, deaths$code)
  fixest::setFixest_notes(FALSE)

  for (cause_i in seq_along(offset_causes)) {
    cause_name <- offset_causes[cause_i]
    cause_rows <- model_specs[
      model_specs$cause_order == cause_i & !model_specs$complete
    ]
    if (!nrow(cause_rows)) {
      completed_count <- completed_count + 8L
      setTxtProgressBar(progress, completed_count)
      next
    }

    message("\nPreparing counts for ", cause_name)
    counts <- build_cause_daily_counts(
      deaths,
      cause_name,
      cause_masks[[cause_name]]
    )
    for (age_i in seq_along(sensitivity_age_levels)) {
      age_name <- sensitivity_age_levels[age_i]
      age_rows <- model_specs[
        model_specs$cause_order == cause_i &
          model_specs$age_order == age_i
      ]
      if (all(age_rows$complete)) {
        completed_count <- completed_count + 2L
        setTxtProgressBar(progress, completed_count)
        next
      }

      message("  Age group ", age_name)
      model_data <- build_group_model_data(counts, calendar, age_name)
      sample_counts <- model_data[, .(
        zip_days = .N,
        deaths = sum(deaths),
        nonzero_zip_days = sum(deaths > 0L),
        population_min = min(population),
        population_median = median(population),
        population_max = max(population)
      ), by = event_period]

      for (spec_i in seq_along(specifications)) {
        row <- model_specs[
          model_specs$cause_order == cause_i &
            model_specs$age_order == age_i &
            model_specs$specification_order == spec_i
        ]
        if (row$complete) {
          completed_count <- completed_count + 1L
          setTxtProgressBar(progress, completed_count)
          next
        }

        fit_start <- proc.time()[["elapsed"]]
        if (specifications[spec_i] == "Population-offset model") {
          model <- fepois(
            deaths ~ i(event_period, ref = "None") |
              zip + yw + dow + zip_covid,
            data = model_data,
            offset = ~ log_population,
            cluster = ~ zip + yw,
            notes = FALSE
          )
        } else {
          model <- fepois(
            deaths ~ i(event_period, ref = "None") |
              zip + yw + dow + zip_covid,
            data = model_data,
            cluster = ~ zip + yw,
            notes = FALSE
          )
        }
        elapsed_seconds <- proc.time()[["elapsed"]] - fit_start
        estimates <- extract_sensitivity_estimates(model, "event_period::")
        setnames(estimates, "term", "period")
        estimates[, `:=`(
          model_id = row$model_id,
          cause = cause_name,
          age_group = age_name,
          cause_order = cause_i,
          age_order = age_i,
          period_order = match(period, sensitivity_period_levels[-1L]),
          specification = specifications[spec_i],
          specification_order = spec_i,
          wind_threshold_kt = wind_threshold,
          analysis_version = offset_version
        )]
        diagnostics <- data.table(
          model_id = row$model_id,
          cause = cause_name,
          age_group = age_name,
          cause_order = cause_i,
          age_order = age_i,
          specification = specifications[spec_i],
          specification_order = spec_i,
          input_rows = nrow(model_data),
          model_observations = nobs(model),
          removed_observations = nrow(model_data) - nobs(model),
          model_converged = if (is.null(model$convStatus)) TRUE else isTRUE(model$convStatus),
          coefficient_count = length(coef(model)),
          log_likelihood = as.numeric(logLik(model)),
          pseudo_r2 = safe_sensitivity_fitstat(model, "pr2"),
          squared_correlation = safe_sensitivity_fitstat(model, "sq.cor"),
          elapsed_seconds = elapsed_seconds,
          wind_threshold_kt = wind_threshold,
          analysis_version = offset_version
        )
        counts_output <- copy(sample_counts)
        counts_output[, `:=`(
          model_id = row$model_id,
          cause = cause_name,
          age_group = age_name,
          cause_order = cause_i,
          age_order = age_i,
          specification = specifications[spec_i],
          specification_order = spec_i,
          period = as.character(event_period),
          period_order = match(
            as.character(event_period),
            sensitivity_period_levels
          ),
          wind_threshold_kt = wind_threshold,
          analysis_version = offset_version
        )]

        fwrite(estimates, row$estimate_path)
        fwrite(diagnostics, row$diagnostic_path)
        fwrite(counts_output, row$count_path)
        completed_count <- completed_count + 1L
        setTxtProgressBar(progress, completed_count)
        rm(model, estimates, diagnostics, counts_output)
      }
      rm(model_data, sample_counts)
      gc()
    }
    rm(counts)
    gc()
  }
  rm(deaths, cause_masks)
  gc()
} else {
  completed_count <- nrow(model_specs)
  setTxtProgressBar(progress, completed_count)
  message("Resuming from 40 complete population-sensitivity checkpoints")
}
close(progress)

model_specs[, complete :=
  file.exists(estimate_path) &
    file.exists(diagnostic_path) &
    file.exists(count_path)]
if (!all(model_specs$complete)) stop("Population-sensitivity checkpoints are missing.")

estimates <- rbindlist(lapply(model_specs$estimate_path, fread), use.names = TRUE)
diagnostics <- rbindlist(lapply(model_specs$diagnostic_path, fread), use.names = TRUE)
sample_counts <- rbindlist(lapply(model_specs$count_path, fread), use.names = TRUE)
setorder(estimates, cause_order, age_order, period_order, specification_order)
setorder(diagnostics, cause_order, age_order, specification_order)
setorder(sample_counts, cause_order, age_order, specification_order, period_order)

master <- fread(master_path)[
  cause %in% offset_causes,
  .(
    cause,
    age_group,
    period,
    cause_order_master = cause_order,
    age_order_master = age_order,
    period_order_master = period_order,
    frozen_log_rate_ratio = log_rate_ratio,
    frozen_standard_error = standard_error,
    frozen_p_value = p_value,
    frozen_rate_ratio = rate_ratio,
    frozen_ci_lower = ci_lower,
    frozen_ci_upper = ci_upper
  )
]

offset_estimates <- merge(
  estimates[specification == "Population-offset model"],
  estimates[specification == "Matched-sample count model", .(
    cause,
    age_group,
    period,
    matched_log_rate_ratio = log_rate_ratio,
    matched_standard_error = standard_error,
    matched_p_value = p_value,
    matched_rate_ratio = rate_ratio,
    matched_ci_lower = ci_lower,
    matched_ci_upper = ci_upper
  )],
  by = c("cause", "age_group", "period"),
  all.x = TRUE,
  sort = FALSE
)
offset_estimates <- merge(
  offset_estimates,
  master,
  by = c("cause", "age_group", "period"),
  all.x = TRUE,
  sort = FALSE
)
offset_estimates[, `:=`(
  offset_minus_matched_log_rate_ratio =
    log_rate_ratio - matched_log_rate_ratio,
  offset_minus_frozen_log_rate_ratio =
    log_rate_ratio - frozen_log_rate_ratio,
  matched_minus_frozen_log_rate_ratio =
    matched_log_rate_ratio - frozen_log_rate_ratio,
  direction_agrees_with_matched =
    sign(log_rate_ratio) == sign(matched_log_rate_ratio),
  direction_agrees_with_frozen =
    sign(log_rate_ratio) == sign(frozen_log_rate_ratio),
  offset_ci_overlaps_matched =
    ci_lower <= matched_ci_upper & ci_upper >= matched_ci_lower,
  offset_ci_overlaps_frozen =
    ci_lower <= frozen_ci_upper & ci_upper >= frozen_ci_lower
)]
setorder(offset_estimates, cause_order, age_order, period_order)

comparison_long <- rbindlist(list(
  master[, .(
    cause,
    age_group,
    period,
    specification = "Frozen primary model",
    specification_order = 1L,
    log_rate_ratio = frozen_log_rate_ratio,
    standard_error = frozen_standard_error,
    p_value = frozen_p_value,
    rate_ratio = frozen_rate_ratio,
    ci_lower = frozen_ci_lower,
    ci_upper = frozen_ci_upper
  )],
  estimates[specification == "Matched-sample count model", .(
    cause,
    age_group,
    period,
    specification,
    specification_order = 2L,
    log_rate_ratio,
    standard_error,
    p_value,
    rate_ratio,
    ci_lower,
    ci_upper
  )],
  estimates[specification == "Population-offset model", .(
    cause,
    age_group,
    period,
    specification,
    specification_order = 3L,
    log_rate_ratio,
    standard_error,
    p_value,
    rate_ratio,
    ci_lower,
    ci_upper
  )]
), use.names = TRUE)
comparison_long[, `:=`(
  cause_order = match(cause, offset_causes),
  age_order = match(age_group, sensitivity_age_levels),
  period_order = match(period, sensitivity_period_levels[-1L]),
  wind_threshold_kt = wind_threshold,
  analysis_version = offset_version
)]
setorder(comparison_long, cause_order, age_order, period_order, specification_order)

estimate_path <- file.path(offset_dir, "population_offset_estimates_34kt.csv")
comparison_path <- file.path(offset_dir, "population_offset_comparison_34kt.csv")
diagnostic_path <- file.path(offset_dir, "population_offset_diagnostics_34kt.csv")
count_path <- file.path(offset_dir, "population_offset_counts_34kt.csv")
coverage_path <- file.path(offset_dir, "population_offset_coverage_34kt.csv")
zip_year_path <- file.path(offset_dir, "population_offset_zip_year_coverage_34kt.csv")
qa_path <- file.path(offset_dir, "population_offset_qa_34kt.csv")

qa <- data.table(
  check_name = c(
    "Five prespecified outcomes",
    "Four age groups per outcome",
    "Two matched-sample specifications",
    "Forty fitted models",
    "Four period estimates per model",
    "Eighty population-offset estimates",
    "Two hundred forty comparison estimates",
    "All fitted models converged",
    "Finite ordered confidence intervals",
    "All modeled ZIP-years have positive population",
    "Offset comparison fields complete"
  ),
  status = c(
    if (uniqueN(estimates$cause) == 5L) "PASS" else "FAIL",
    if (all(estimates[, uniqueN(age_group), by = cause]$V1 == 4L)) "PASS" else "FAIL",
    if (uniqueN(estimates$specification) == 2L) "PASS" else "FAIL",
    if (nrow(diagnostics) == 40L) "PASS" else "FAIL",
    if (all(estimates[, .N, by = model_id]$N == 4L)) "PASS" else "FAIL",
    if (nrow(offset_estimates) == 80L) "PASS" else "FAIL",
    if (nrow(comparison_long) == 240L) "PASS" else "FAIL",
    if (all(diagnostics$model_converged)) "PASS" else "FAIL",
    if (all(
      is.finite(estimates$rate_ratio) &
        estimates$ci_lower <= estimates$rate_ratio &
        estimates$rate_ratio <= estimates$ci_upper
    )) "PASS" else "FAIL",
    if (all(calendar$population > 0 & is.finite(calendar$population))) "PASS" else "FAIL",
    if (!anyNA(offset_estimates[, .(
      matched_log_rate_ratio,
      frozen_log_rate_ratio,
      offset_minus_matched_log_rate_ratio,
      direction_agrees_with_matched,
      offset_ci_overlaps_matched
    )])) "PASS" else "FAIL"
  ),
  observed = c(
    uniqueN(estimates$cause),
    paste(unique(estimates[, uniqueN(age_group), by = cause]$V1), collapse = ","),
    uniqueN(estimates$specification),
    nrow(diagnostics),
    paste(unique(estimates[, .N, by = model_id]$N), collapse = ","),
    nrow(offset_estimates),
    nrow(comparison_long),
    sum(diagnostics$model_converged),
    sum(
      is.finite(estimates$rate_ratio) &
        estimates$ci_lower <= estimates$rate_ratio &
        estimates$rate_ratio <= estimates$ci_upper
    ),
    sum(calendar$population > 0 & is.finite(calendar$population)),
    sum(complete.cases(offset_estimates[, .(
      matched_log_rate_ratio,
      frozen_log_rate_ratio,
      offset_minus_matched_log_rate_ratio,
      direction_agrees_with_matched,
      offset_ci_overlaps_matched
    )]))
  ),
  expected = c(
    5L, 4L, 2L, 40L, 4L, 80L, 240L, 40L, 160L,
    nrow(calendar), 80L
  )
)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more population-offset sensitivity checks failed.")
}

source_commit <- get_sensitivity_source_commit()
for (object_name in c(
  "offset_estimates", "comparison_long", "diagnostics", "sample_counts",
  "coverage_summary", "zip_year_coverage"
)) {
  object <- get(object_name)
  object[, `:=`(
    source_commit = source_commit,
    build_date = as.IDate("2026-11-19")
  )]
  assign(object_name, object)
}

fwrite(offset_estimates, estimate_path)
fwrite(comparison_long, comparison_path)
fwrite(diagnostics, diagnostic_path)
fwrite(sample_counts, count_path)
fwrite(coverage_summary, coverage_path)
fwrite(zip_year_coverage, zip_year_path)
fwrite(qa, qa_path)

cat("\nPopulation-offset sensitivity completed\n")
cat("Models:", nrow(diagnostics), "\n")
cat("Population-offset estimates:", nrow(offset_estimates), "\n")
cat("Positive-population ZIP-years:", sum(zip_year_coverage$positive_population), "of", nrow(zip_year_coverage), "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
cat("Output directory:", offset_dir, "\n")
