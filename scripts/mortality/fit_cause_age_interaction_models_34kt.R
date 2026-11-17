#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(lubridate)
})

source("scripts/mortality/cause_age_count_helpers.R")

wind_threshold <- 34L
period_levels <- c(
  "None",
  "Days -14 to -8",
  "Days -7 to -1",
  "Days 0 to +6",
  "Days +7 to +13"
)
age_groups <- c("<65", "65-74", "75+")
output_dir <- "outputs/mortality/paper_master_34kt/age_interactions"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

requested_causes <- commandArgs(trailingOnly = TRUE)
available_causes <- names(build_cause_masks(integer(), character()))
causes_to_fit <- if (length(requested_causes)) {
  unknown <- setdiff(requested_causes, available_causes)
  if (length(unknown)) {
    stop("Unknown cause(s): ", paste(unknown, collapse = ", "))
  }
  requested_causes
} else {
  available_causes
}

cause_slug <- function(x) {
  gsub("_+$", "", gsub("[^A-Za-z0-9]+", "_", tolower(x)))
}

period_from_rel_day <- function(rel_day) {
  fcase(
    rel_day >= -14L & rel_day <= -8L, "Days -14 to -8",
    rel_day >= -7L & rel_day <= -1L, "Days -7 to -1",
    rel_day >= 0L & rel_day <= 6L, "Days 0 to +6",
    rel_day >= 7L & rel_day <= 13L, "Days +7 to +13",
    default = NA_character_
  )
}

safe_fitstat <- function(model, statistic) {
  result <- tryCatch(
    fitstat(model, statistic, simplify = TRUE),
    error = function(e) NA_real_
  )
  if (!length(result)) NA_real_ else as.numeric(result[1L])
}

message("Building the common 34-kt event calendar")
calendar_source <- fread(
  sprintf(
    "data/mortality/processed/death-panels/daily_panel_%dkt.csv",
    wind_threshold
  ),
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
event_calendar <- zip_events[, .(rel_day = -14L:13L), by = .(zip, impact_date)]
event_calendar[, `:=`(
  date = impact_date + rel_day,
  event_period = period_from_rel_day(rel_day)
)]
event_calendar <- event_calendar[
  order(zip, date, abs(rel_day), rel_day, impact_date)
][, .SD[1L], by = .(zip, date)]

complete_window_dates <- unique(event_calendar$date)
calendar <- calendar_source[, .(zip, date)]
calendar[, month := month(as.Date(date))]
calendar <- calendar[month %in% 5:11 | date %in% complete_window_dates]
calendar[, month := NULL]
calendar <- event_calendar[
  calendar,
  on = .(zip, date),
  .(zip = i.zip, date = i.date, event_period = x.event_period)
]
calendar[is.na(event_period), event_period := "None"]
calendar[, event_period := factor(event_period, levels = period_levels)]

zip_levels <- sort(unique(calendar$zip))
yw_values <- year(as.Date(calendar$date)) * 100L + isoweek(as.Date(calendar$date))
yw_levels <- sort(unique(yw_values))
calendar[, `:=`(
  zip_id = match(zip, zip_levels),
  yw_id = match(yw_values, yw_levels),
  dow = as.integer(strftime(as.Date(date), "%u")),
  covid = as.integer(
    date >= as.IDate("2020-01-01") & date <= as.IDate("2022-12-31")
  )
)]
stopifnot(!anyDuplicated(calendar[, .(zip, date)]))

rm(calendar_source, event_calendar, yw_values)
gc()

message("Reading deaths and underlying-cause codes")
deaths_sf <- readRDS(
  sprintf(
    "data/mortality/processed/spatial-results/deaths_with_zip_%dkt.rds",
    wind_threshold
  )
)
deaths <- as.data.table(deaths_sf)[, .(
  Death_ID,
  zip = as.character(zip),
  date = as.IDate(Death_Date),
  age = suppressWarnings(as.integer(age))
)]
rm(deaths_sf)
gc()

deaths <- deaths[
  age >= 0L & age <= 120L &
    (month(as.Date(date)) %in% 5:11 | date %in% complete_window_dates) &
    !is.na(zip)
]
deaths[, `:=`(
  year = year(as.Date(date)),
  join_year = as.character(year(as.Date(date))),
  age_group = as.character(make_age_group(age))
)]

cause_lookup <- as.data.table(readRDS(
  "data/mortality/derived/mortality_underlying_cause_lookup_1985_2022.rds"
))[, .(
  Death_ID,
  join_year = as.character(join_year),
  code = as.character(code)
)]
if (anyDuplicated(cause_lookup[, .(Death_ID, join_year)])) {
  stop("Underlying-cause lookup is not unique by Death_ID and year.")
}
deaths <- merge(
  deaths,
  cause_lookup,
  by = c("Death_ID", "join_year"),
  all.x = TRUE,
  sort = FALSE
)
rm(cause_lookup)
gc()

cause_masks <- build_cause_masks(deaths$year, deaths$code)
n_zip <- length(zip_levels)
n_yw <- length(yw_levels)

build_stacked_model_data <- function(daily_counts) {
  parts <- vector("list", length(age_groups))
  for (age_index in seq_along(age_groups)) {
    age_name <- age_groups[age_index]
    counts_one_age <- daily_counts[
      age_group == age_name,
      .(zip, date, deaths)
    ]
    part <- counts_one_age[calendar, on = .(zip, date)]
    part[is.na(deaths), deaths := 0L]
    part[, `:=`(
      age_group = factor(age_name, levels = age_groups),
      zip_age = zip_id + (age_index - 1L) * n_zip,
      yw_age = yw_id + (age_index - 1L) * n_yw,
      dow_age = dow + (age_index - 1L) * 7L,
      zip_covid_age = zip_id + covid * n_zip +
        (age_index - 1L) * 2L * n_zip
    )]
    parts[[age_index]] <- part[, .(
      deaths,
      event_period,
      age_group,
      zip_id,
      yw_id,
      zip_age,
      yw_age,
      dow_age,
      zip_covid_age
    )]
  }
  rbindlist(parts, use.names = TRUE)
}

extract_interaction_results <- function(model, cause_name) {
  beta <- coef(model)
  variance <- vcov(model)
  standard_error <- sqrt(diag(variance))
  result <- vector("list", 0L)
  contrasts <- vector("list", 0L)
  global_tests <- vector("list", 0L)

  for (period_name in period_levels[-1L]) {
    term_names <- paste0(
      "event_period::", period_name, ":age_group::", age_groups
    )
    if (!all(term_names %in% names(beta))) {
      stop(
        "Missing age-specific coefficient(s) for ", cause_name,
        ", ", period_name
      )
    }

    period_beta <- unname(beta[term_names])
    period_vcov <- variance[term_names, term_names, drop = FALSE]
    period_se <- sqrt(diag(period_vcov))
    result[[length(result) + 1L]] <- data.table(
      cause = cause_name,
      group = age_groups,
      period = period_name,
      joint_log_rate_ratio = period_beta,
      joint_standard_error = period_se,
      joint_rate_ratio = exp(period_beta),
      joint_ci_lower = exp(period_beta - 1.96 * period_se),
      joint_ci_upper = exp(period_beta + 1.96 * period_se)
    )

    contrast_pairs <- list(
      c("65-74", "<65"),
      c("75+", "<65"),
      c("75+", "65-74")
    )
    for (pair in contrast_pairs) {
      comparison_index <- match(pair[1L], age_groups)
      reference_index <- match(pair[2L], age_groups)
      contrast_vector <- numeric(length(age_groups))
      contrast_vector[comparison_index] <- 1
      contrast_vector[reference_index] <- -1
      difference <- sum(contrast_vector * period_beta)
      difference_variance <- as.numeric(
        t(contrast_vector) %*% period_vcov %*% contrast_vector
      )
      difference_se <- sqrt(difference_variance)
      z_value <- difference / difference_se
      contrasts[[length(contrasts) + 1L]] <- data.table(
        cause = cause_name,
        period = period_name,
        comparison_group = pair[1L],
        reference_group = pair[2L],
        contrast_log_rate_ratio = difference,
        contrast_standard_error = difference_se,
        contrast_ratio_of_rate_ratios = exp(difference),
        contrast_ci_lower = exp(difference - 1.96 * difference_se),
        contrast_ci_upper = exp(difference + 1.96 * difference_se),
        contrast_z = z_value,
        contrast_p_value = 2 * pnorm(-abs(z_value))
      )
    }

    contrast_matrix <- rbind(
      c(-1, 1, 0),
      c(-1, 0, 1)
    )
    difference_vector <- as.numeric(contrast_matrix %*% period_beta)
    difference_vcov <- contrast_matrix %*% period_vcov %*%
      t(contrast_matrix)
    test_statistic <- as.numeric(
      t(difference_vector) %*% solve(difference_vcov, difference_vector)
    )
    global_tests[[length(global_tests) + 1L]] <- data.table(
      cause = cause_name,
      period = period_name,
      age_heterogeneity_chisq = test_statistic,
      age_heterogeneity_df = 2L,
      age_heterogeneity_p_value = pchisq(
        test_statistic,
        df = 2L,
        lower.tail = FALSE
      )
    )
  }

  list(
    estimates = rbindlist(result),
    contrasts = rbindlist(contrasts),
    global_tests = rbindlist(global_tests)
  )
}

all_interaction_results <- vector("list", length(causes_to_fit))
all_contrast_results <- vector("list", length(causes_to_fit))
all_global_results <- vector("list", length(causes_to_fit))
all_diagnostics <- vector("list", length(causes_to_fit))

for (cause_index in seq_along(causes_to_fit)) {
  cause_name <- causes_to_fit[cause_index]
  slug <- cause_slug(cause_name)
  message(
    "[", cause_index, "/", length(causes_to_fit), "] Fitting ", cause_name
  )

  mask <- cause_masks[[cause_name]]
  deaths_for_cause <- deaths[which(!is.na(mask) & mask)]
  daily_counts <- deaths_for_cause[, .(
    deaths = .N
  ), by = .(zip, date, age_group)]
  total_rows <- 3L * nrow(calendar)
  model_data <- build_stacked_model_data(daily_counts)
  stopifnot(nrow(model_data) == total_rows)

  model <- fepois(
    deaths ~ i(event_period, age_group, ref = "None") |
      zip_age + yw_age + dow_age + zip_covid_age,
    data = model_data,
    cluster = ~ zip_id + yw_id,
    notes = TRUE
  )

  extracted <- extract_interaction_results(model, cause_name)
  diagnostics <- data.table(
    cause = cause_name,
    wind_threshold_kt = wind_threshold,
    total_stacked_rows = total_rows,
    model_observations = nobs(model),
    removed_observations = total_rows - nobs(model),
    zip_clusters = uniqueN(model_data$zip_id),
    year_week_clusters = uniqueN(model_data$yw_id),
    log_likelihood = as.numeric(logLik(model)),
    pseudo_r2 = safe_fitstat(model, "pr2"),
    squared_correlation = safe_fitstat(model, "sq.cor"),
    converged = if (is.null(model$convStatus)) TRUE else isTRUE(model$convStatus),
    coefficient_count = length(coef(model))
  )

  fwrite(
    extracted$estimates,
    file.path(output_dir, paste0(slug, "_age_specific_estimates.csv"))
  )
  fwrite(
    extracted$contrasts,
    file.path(output_dir, paste0(slug, "_age_contrasts.csv"))
  )
  fwrite(
    extracted$global_tests,
    file.path(output_dir, paste0(slug, "_age_heterogeneity_tests.csv"))
  )
  fwrite(
    diagnostics,
    file.path(output_dir, paste0(slug, "_interaction_diagnostics.csv"))
  )

  all_interaction_results[[cause_index]] <- extracted$estimates
  all_contrast_results[[cause_index]] <- extracted$contrasts
  all_global_results[[cause_index]] <- extracted$global_tests
  all_diagnostics[[cause_index]] <- diagnostics

  rm(
    deaths_for_cause, daily_counts, model_data, model, extracted,
    diagnostics, mask
  )
  gc()
}

fwrite(
  rbindlist(all_interaction_results),
  file.path(output_dir, "all_age_specific_estimates.csv")
)
fwrite(
  rbindlist(all_contrast_results),
  file.path(output_dir, "all_age_contrasts.csv")
)
fwrite(
  rbindlist(all_global_results),
  file.path(output_dir, "all_age_heterogeneity_tests.csv")
)
fwrite(
  rbindlist(all_diagnostics),
  file.path(output_dir, "all_interaction_diagnostics.csv")
)

cat("\nCompleted joint age-interaction models\n")
cat("Causes:", paste(causes_to_fit, collapse = "; "), "\n")
cat("Output directory:", output_dir, "\n")
