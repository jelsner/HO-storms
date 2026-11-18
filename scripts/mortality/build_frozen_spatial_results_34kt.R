#!/usr/bin/env Rscript

# Build and freeze the ZIP-level spatial analyses prespecified in the
# Environmental Research analysis-and-reporting plan. The default reproduces
# the five-target v1 freeze; set HO_SPATIAL_RELEASE=v2 to build the amended
# eight-target family from the amended master results. Each target uses the
# pooled cause-age-period fixed-effects Poisson model to obtain counterfactual
# expected deaths, gamma-Poisson empirical-Bayes smoothing of ZIP observed /
# expected ratios, and global/local Moran statistics under queen contiguity.

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(lubridate)
  library(Matrix)
  library(sf)
})

source("scripts/mortality/cause_age_count_helpers.R")

wind_threshold <- 34L
spatial_release <- Sys.getenv("HO_SPATIAL_RELEASE", unset = "v1")
if (!spatial_release %chin% c("v1", "v2")) {
  stop("HO_SPATIAL_RELEASE must be either 'v1' or 'v2'.")
}
is_amended_v2 <- identical(spatial_release, "v2")
freeze_date <- as.IDate(if (is_amended_v2) "2026-11-18" else "2026-11-17")
analysis_version <- if (is_amended_v2) {
  "spatial_frozen_v2_2026-11-18"
} else {
  "spatial_frozen_v1_2026-11-17"
}
release_label <- if (is_amended_v2) "frozen_v2" else "frozen_v1"
expected_target_count <- if (is_amended_v2) 8L else 5L
global_q_column <- if (is_amended_v2) {
  "global_moran_fdr_q_eight_maps"
} else {
  "global_moran_fdr_q_five_maps"
}
nsim <- 9999L
random_seed <- 20260916L

period_levels <- c(
  "None",
  "Days -14 to -8",
  "Days -7 to -1",
  "Days 0 to +6",
  "Days +7 to +13"
)

primary_master_path <- if (is_amended_v2) {
  paste0(
    "outputs/mortality/paper_master_34kt/amended_v2_2026-11-18/",
    "master_results_34kt_amended_v2.csv"
  )
} else {
  paste0(
    "outputs/mortality/paper_master_34kt/frozen_v1_2026-11-17/",
    "master_results_34kt_frozen_v1.csv"
  )
}
daily_panel_path <- sprintf(
  "data/mortality/processed/death-panels/daily_panel_%dkt.csv",
  wind_threshold
)
deaths_path <- sprintf(
  "data/mortality/processed/spatial-results/deaths_with_zip_%dkt.rds",
  wind_threshold
)
cause_lookup_path <-
  "data/mortality/derived/mortality_underlying_cause_lookup_1985_2022.rds"
geometry_cache_path <-
  "data/mortality/derived/florida_panel_zcta_geometry_2020.rds"
legacy_geometry_path <- paste0(
  "outputs/mortality/spatial_cardiovascular_75plus_34kt/",
  "cardiovascular_75plus_impact_zip_eb_34kt.gpkg"
)
legacy_eb_path <- paste0(
  "outputs/mortality/spatial_cardiovascular_75plus_34kt/",
  "cardiovascular_75plus_impact_zip_eb_34kt.csv"
)
legacy_global_moran_path <- paste0(
  "outputs/mortality/spatial_cardiovascular_75plus_34kt/",
  "cardiovascular_75plus_impact_global_moran_34kt.csv"
)

output_dir <- file.path(
  "outputs/mortality/paper_spatial_34kt",
  analysis_version
)
intermediate_dir <- file.path(output_dir, "intermediate")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)

target_path <- file.path(
  output_dir,
  paste0("spatial_targets_34kt_", release_label, ".csv")
)
zip_path <- file.path(
  output_dir,
  paste0("spatial_zip_results_34kt_", release_label, ".csv")
)
global_path <- file.path(
  output_dir,
  paste0("spatial_global_moran_34kt_", release_label, ".csv")
)
diagnostic_path <- file.path(
  output_dir,
  paste0("spatial_model_diagnostics_34kt_", release_label, ".csv")
)
gpkg_path <- file.path(
  output_dir,
  paste0("spatial_results_34kt_", release_label, ".gpkg")
)
dictionary_path <- file.path(
  output_dir,
  paste0("spatial_data_dictionary_34kt_", release_label, ".csv")
)
qa_path <- file.path(
  output_dir,
  paste0("spatial_qa_checks_34kt_", release_label, ".csv")
)
manifest_path <- file.path(
  output_dir,
  paste0("spatial_freeze_manifest_34kt_", release_label, ".csv")
)

required_paths <- c(
  primary_master_path,
  daily_panel_path,
  deaths_path,
  cause_lookup_path
)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths)) {
  stop("Missing required inputs: ", paste(missing_paths, collapse = ", "))
}

get_source_commit <- function() {
  bundled_git <- paste0(
    "/Users/jelsner/.cache/codex-runtimes/codex-primary-runtime/",
    "dependencies/bin/fallback/git"
  )
  git <- if (file.exists(bundled_git)) bundled_git else Sys.which("git")
  if (!nzchar(git)) return(NA_character_)
  result <- tryCatch(
    system2(git, c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) NA_character_
  )
  if (!length(result)) NA_character_ else result[1L]
}

source_commit <- get_source_commit()

period_from_rel_day <- function(rel_day) {
  fcase(
    rel_day >= -14L & rel_day <= -8L, "Days -14 to -8",
    rel_day >= -7L & rel_day <= -1L, "Days -7 to -1",
    rel_day >= 0L & rel_day <= 6L, "Days 0 to +6",
    rel_day >= 7L & rel_day <= 13L, "Days +7 to +13",
    default = NA_character_
  )
}

slugify <- function(x) {
  gsub("_+$", "", gsub("[^A-Za-z0-9]+", "_", tolower(x)))
}

safe_fitstat <- function(model, statistic) {
  result <- tryCatch(
    fitstat(model, statistic, simplify = TRUE),
    error = function(e) NA_real_
  )
  if (!length(result)) NA_real_ else as.numeric(result[1L])
}

fit_gamma_prior <- function(observed, expected) {
  marginal_nll <- function(log_parameters) {
    shape <- exp(log_parameters[1L])
    rate <- exp(log_parameters[2L])
    -sum(
      lgamma(observed + shape) - lgamma(shape) - lgamma(observed + 1) +
        shape * log(rate) + observed * log(expected) -
        (observed + shape) * log(expected + rate)
    )
  }

  mean_ratio <- sum(observed) / sum(expected)
  raw_ratio <- observed / expected
  between_variance <- max(
    stats::var(raw_ratio) - mean(mean_ratio / expected),
    0.01^2
  )
  initial_shape <- mean_ratio^2 / between_variance
  initial_rate <- mean_ratio / between_variance

  fit <- optim(
    par = log(c(initial_shape, initial_rate)),
    fn = marginal_nll,
    method = "L-BFGS-B",
    lower = log(c(1e-6, 1e-6)),
    upper = log(c(1e8, 1e8))
  )
  if (fit$convergence != 0L || !is.finite(fit$value)) {
    stop("Gamma-Poisson EB prior estimation failed: ", fit$message)
  }
  c(shape = exp(fit$par[1L]), rate = exp(fit$par[2L]))
}

load_panel_geometry <- function(panel_zips) {
  geometry_source <- NA_character_

  if (file.exists(geometry_cache_path)) {
    geometry <- readRDS(geometry_cache_path)
    geometry_source <- geometry_cache_path
  } else if (file.exists(legacy_geometry_path)) {
    geometry <- st_read(legacy_geometry_path, quiet = TRUE)
    geometry <- geometry[, "zip", drop = FALSE]
    dir.create(dirname(geometry_cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(geometry, geometry_cache_path)
    geometry_source <- legacy_geometry_path
  } else {
    if (!requireNamespace("tigris", quietly = TRUE)) {
      stop(
        "No geometry cache or legacy geometry file was found, and package ",
        "'tigris' is unavailable."
      )
    }
    options(tigris_use_cache = TRUE)
    geometry <- tigris::zctas(cb = TRUE, year = 2020, progress_bar = FALSE)
    zip_column <- if ("ZCTA5CE20" %in% names(geometry)) {
      "ZCTA5CE20"
    } else if ("GEOID20" %in% names(geometry)) {
      "GEOID20"
    } else {
      stop("Could not identify a ZIP column in the 2020 ZCTA data.")
    }
    geometry$zip <- as.character(geometry[[zip_column]])
    geometry <- geometry[geometry$zip %in% panel_zips, "zip", drop = FALSE]
    dir.create(dirname(geometry_cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(geometry, geometry_cache_path)
    geometry_source <- "tigris::zctas(cb=TRUE, year=2020)"
  }

  geometry$zip <- as.character(geometry$zip)
  geometry <- geometry[geometry$zip %in% panel_zips, "zip", drop = FALSE]
  geometry <- geometry[!duplicated(geometry$zip), ]
  if (any(st_is_empty(geometry))) {
    stop("The panel geometry contains empty features.")
  }

  missing_geometry <- setdiff(panel_zips, geometry$zip)
  if (length(missing_geometry)) {
    stop(
      "Geometry is missing for ", length(missing_geometry), " panel ZIPs: ",
      paste(head(missing_geometry, 20L), collapse = ", ")
    )
  }
  geometry <- geometry[match(panel_zips, geometry$zip), ]
  list(data = geometry, source = geometry_source)
}

calculate_moran <- function(map_data, target_id, target_order) {
  analysis_map <- map_data[
    is.finite(map_data$rate_ratio_eb) & map_data$rate_ratio_eb > 0,
  ]
  if (nrow(analysis_map) < 3L) {
    stop("Fewer than three ZIPs are estimable for ", target_id)
  }

  analysis_projected <- st_transform(st_make_valid(analysis_map), 5070)
  neighbors <- st_touches(analysis_projected, sparse = TRUE)
  neighbor_count <- lengths(neighbors)

  row_index <- rep(seq_len(nrow(analysis_projected)), neighbor_count)
  column_index <- unlist(neighbors, use.names = FALSE)
  if (!length(column_index)) {
    stop("No queen-contiguous ZIP pairs were found for ", target_id)
  }

  weights <- sparseMatrix(
    i = row_index,
    j = column_index,
    x = 1 / neighbor_count[row_index],
    dims = c(nrow(analysis_projected), nrow(analysis_projected))
  )
  non_island <- neighbor_count > 0L
  s0 <- sum(weights)

  log_rr <- log(analysis_projected$rate_ratio_eb)
  centered <- log_rr - mean(log_rr)
  n_zips <- length(centered)
  m2 <- sum(centered^2) / n_zips
  if (!is.finite(m2) || m2 <= 0) {
    stop("No finite spatial variance remains for ", target_id)
  }

  spatial_lag <- as.numeric(weights %*% centered)
  local_i <- centered * spatial_lag / m2
  local_i[!non_island] <- NA_real_
  global_i <- (n_zips / s0) *
    sum(centered * spatial_lag) /
    sum(centered^2)
  expected_global_i <- -1 / (n_zips - 1)

  message("Running ", format(nsim, big.mark = ","), " Moran permutations")
  set.seed(random_seed)
  permuted_global_i <- numeric(nsim)
  local_extreme_count <- integer(n_zips)
  progress <- txtProgressBar(min = 0L, max = nsim, style = 3L)
  progress_step <- max(1L, floor(nsim / 100L))

  for (simulation in seq_len(nsim)) {
    permuted <- sample(centered, replace = FALSE)
    permuted_lag <- as.numeric(weights %*% permuted)
    permuted_local_i <- permuted * permuted_lag / m2
    permuted_global_i[simulation] <- (n_zips / s0) *
      sum(permuted * permuted_lag) /
      sum(permuted^2)
    extreme <- abs(permuted_local_i) >= abs(local_i)
    extreme[!non_island | is.na(extreme)] <- FALSE
    local_extreme_count <- local_extreme_count + as.integer(extreme)

    if (simulation %% progress_step == 0L || simulation == nsim) {
      setTxtProgressBar(progress, simulation)
    }
  }
  close(progress)

  global_p_two_sided <- (
    1 + sum(
      abs(permuted_global_i - expected_global_i) >=
        abs(global_i - expected_global_i)
    )
  ) / (nsim + 1)
  global_z <- (
    global_i - mean(permuted_global_i)
  ) / stats::sd(permuted_global_i)

  local_p <- (local_extreme_count + 1) / (nsim + 1)
  local_p[!non_island] <- NA_real_
  local_q <- rep(NA_real_, n_zips)
  local_q[non_island] <- p.adjust(local_p[non_island], method = "BH")

  lisa_cluster <- rep("Not significant", n_zips)
  significant <- non_island & !is.na(local_q) & local_q < 0.05
  lisa_cluster[significant & centered > 0 & spatial_lag > 0] <- "High-High"
  lisa_cluster[significant & centered < 0 & spatial_lag < 0] <- "Low-Low"
  lisa_cluster[significant & centered > 0 & spatial_lag < 0] <- "High-Low"
  lisa_cluster[significant & centered < 0 & spatial_lag > 0] <- "Low-High"
  lisa_cluster[!non_island] <- "Island"

  local <- data.table(
    target_id = target_id,
    target_order = target_order,
    zip = as.character(analysis_projected$zip),
    centered_log_rr = centered,
    neighbor_count = neighbor_count,
    spatial_lag_centered_log_rr = spatial_lag,
    local_morans_i = local_i,
    local_permutation_p_value = local_p,
    local_fdr_q_value = local_q,
    lisa_cluster = lisa_cluster
  )

  global <- data.table(
    target_id = target_id,
    target_order = target_order,
    contiguity = "queen",
    weight_style = "row-standardized",
    zips_analyzed = n_zips,
    non_island_zips = sum(non_island),
    island_zips = sum(!non_island),
    undirected_neighbor_links = sum(neighbor_count) / 2,
    mean_neighbors = mean(neighbor_count),
    morans_i = global_i,
    expected_i = expected_global_i,
    permutation_mean_i = mean(permuted_global_i),
    permutation_sd_i = stats::sd(permuted_global_i),
    permutation_z = global_z,
    permutation_p_two_sided = global_p_two_sided,
    permutations = nsim,
    random_seed = random_seed
  )

  list(local = local, global = global)
}

message("Selecting the locked spatial targets from the primary freeze")
master <- fread(primary_master_path)
targets <- master[
  cause != "All diseases" &
    age_group %chin% c("65-74", "75+") &
    rate_ratio > 1 &
    ci_lower > 1 &
    main_fdr_q_all_cause_specific < 0.05 &
    event_deaths >= 500L
]

expected_target_keys <- c(
  "Cardiovascular|65-74|Days 0 to +6",
  "Cardiovascular|75+|Days 0 to +6",
  if (is_amended_v2) "Cardiovascular|75+|Days +7 to +13",
  "Respiratory|75+|Days +7 to +13",
  if (is_amended_v2) "Injuries|75+|Days 0 to +6",
  "Neuropsychiatric disorders|75+|Days 0 to +6",
  if (is_amended_v2) "Neuropsychiatric disorders|75+|Days +7 to +13",
  "Neoplasms|75+|Days -7 to -1"
)
target_keys <- paste(targets$cause, targets$age_group, targets$period, sep = "|")
if (
  nrow(targets) != expected_target_count ||
    !setequal(target_keys, expected_target_keys)
) {
  stop(
    "The frozen eligibility rule did not return the locked targets. Got: ",
    paste(target_keys, collapse = "; ")
  )
}

targets[, target_order := match(target_keys, expected_target_keys)]
targets[, `:=`(
  target_id = paste0("SP-", result_id),
  target_slug = slugify(paste(cause, age_group, period, sep = "_"))
)]
setorder(targets, target_order)
targets <- targets[, .(
  target_id,
  target_slug,
  target_order,
  result_id,
  cause,
  age_group,
  period,
  frozen_log_rate_ratio = log_rate_ratio,
  frozen_rate_ratio = rate_ratio,
  frozen_ci_lower = ci_lower,
  frozen_ci_upper = ci_upper,
  frozen_p_value = p_value,
  frozen_fdr_q_all_cause_specific = main_fdr_q_all_cause_specific,
  frozen_event_deaths = event_deaths,
  frozen_event_zip_days = event_zip_days,
  frozen_event_nonzero_zip_days = event_nonzero_zip_days
)]

message("Building the common 34-kt event calendar")
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
event_calendar_raw <- zip_events[, .(
  rel_day = -14L:13L
), by = .(zip, impact_date)]
event_calendar_raw[, `:=`(
  date = impact_date + rel_day,
  event_period = period_from_rel_day(rel_day)
)]
overlap_zip_dates <- event_calendar_raw[, .N, by = .(zip, date)][N > 1L]
overlap_assignments <- if (nrow(overlap_zip_dates)) {
  sum(overlap_zip_dates$N - 1L)
} else {
  0L
}
event_calendar <- event_calendar_raw[
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

rm(
  calendar_source, event_calendar_raw, event_calendar, overlap_zip_dates,
  zip_events
)
gc()

message("Loading the common 2020 ZCTA geometry")
geometry_loaded <- load_panel_geometry(all_panel_zips)
panel_geometry <- geometry_loaded$data
geometry_source <- geometry_loaded$source
rm(geometry_loaded)

all_zip_results <- vector("list", nrow(targets))
all_global_results <- vector("list", nrow(targets))
all_diagnostics <- vector("list", nrow(targets))
all_spatial_layers <- vector("list", nrow(targets))

intermediate_paths <- rbindlist(lapply(seq_len(nrow(targets)), function(index) {
  slug <- targets$target_slug[index]
  data.table(
    target_index = index,
    zip_path = file.path(intermediate_dir, paste0(slug, "_zip_results.csv")),
    global_path = file.path(intermediate_dir, paste0(slug, "_global_moran.csv")),
    diagnostic_path = file.path(intermediate_dir, paste0(slug, "_diagnostics.csv"))
  )
}))
resume_from_intermediates <- all(file.exists(unlist(
  intermediate_paths[, .(zip_path, global_path, diagnostic_path)]
)))

if (resume_from_intermediates) {
  message(
    "Resuming from ", expected_target_count,
    " complete per-target intermediate result sets"
  )
  for (target_index in seq_len(nrow(targets))) {
    zip_results <- fread(intermediate_paths$zip_path[target_index])
    zip_results[, zip := as.character(zip)]
    global_results <- fread(intermediate_paths$global_path[target_index])
    diagnostics <- fread(intermediate_paths$diagnostic_path[target_index])
    map_output <- merge(
      panel_geometry,
      as.data.frame(zip_results),
      by = "zip",
      all.x = TRUE,
      sort = FALSE
    )
    map_output <- st_as_sf(map_output)

    all_zip_results[[target_index]] <- zip_results
    all_global_results[[target_index]] <- global_results
    all_diagnostics[[target_index]] <- diagnostics
    all_spatial_layers[[target_index]] <- map_output
  }
} else {
message("Reading mortality records and attaching cause codes")
deaths_sf <- readRDS(deaths_path)
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

cause_lookup <- as.data.table(readRDS(cause_lookup_path))[, .(
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

all_cause_masks <- build_cause_masks(deaths$year, deaths$code)
target_causes <- unique(targets$cause)
cause_masks <- all_cause_masks[target_causes]
rm(all_cause_masks)
gc()

fixest::setFixest_notes(FALSE)

for (target_index in seq_len(nrow(targets))) {
  target <- targets[target_index]
  target_id <- target$target_id
  target_slug <- target$target_slug
  target_period <- target$period
  target_age <- target$age_group
  target_cause <- target$cause

  message(
    "[", target_index, "/", nrow(targets), "] ", target_cause,
    ", age ", target_age, ", ", target_period
  )

  cause_mask <- cause_masks[[target_cause]]
  keep <- !is.na(cause_mask) & cause_mask & deaths$age_group == target_age
  deaths_target <- deaths[which(keep)]
  daily_counts <- deaths_target[, .(deaths = .N), by = .(zip, date)]
  daily_counts[, zip := factor(zip, levels = all_panel_zips)]

  model_data <- daily_counts[calendar, on = .(zip, date)]
  model_data[is.na(deaths), deaths := 0L]

  model <- fepois(
    deaths ~ i(event_period, ref = "None") |
      zip + yw + dow + zip_covid,
    data = model_data,
    notes = FALSE
  )
  target_term <- paste0("event_period::", target_period)
  if (!target_term %in% names(coef(model))) {
    stop("The target coefficient is not estimable for ", target_id)
  }
  pooled_beta <- unname(coef(model)[[target_term]])
  point_difference <- pooled_beta - target$frozen_log_rate_ratio
  if (abs(point_difference) >= 1e-10) {
    stop(
      "Pooled coefficient does not reconcile with the primary freeze for ",
      target_id, ": difference = ", format(point_difference, scientific = TRUE)
    )
  }

  target_data <- model_data[as.character(event_period) == target_period]
  counterfactual <- copy(target_data)
  counterfactual[, event_period := factor("None", levels = period_levels)]
  target_data[, expected_deaths := predict(
    model,
    newdata = counterfactual,
    type = "response"
  )]

  zip_counts <- target_data[
    is.finite(expected_deaths) & expected_deaths > 0,
    .(
      observed_deaths = sum(deaths),
      expected_deaths = sum(expected_deaths),
      target_days = .N
    ),
    by = .(zip = as.character(zip))
  ]
  zip_counts[, `:=`(
    rate_ratio_raw = observed_deaths / expected_deaths,
    status = "estimated"
  )]

  zip_results <- merge(
    data.table(zip = all_panel_zips),
    zip_counts,
    by = "zip",
    all.x = TRUE,
    sort = TRUE
  )
  zip_results[is.na(status), status := "counterfactual_not_estimable"]

  estimated <- zip_results[status == "estimated" & expected_deaths > 0]
  if (nrow(estimated) < 10L) {
    stop("Fewer than ten ZIP counts are estimable for ", target_id)
  }

  prior <- fit_gamma_prior(
    estimated$observed_deaths,
    estimated$expected_deaths
  )
  prior_shape <- unname(prior[["shape"]])
  prior_rate <- unname(prior[["rate"]])
  prior_mean <- prior_shape / prior_rate
  prior_sd <- sqrt(prior_shape / prior_rate^2)

  estimated[, `:=`(
    posterior_shape = prior_shape + observed_deaths,
    posterior_rate = prior_rate + expected_deaths,
    reliability = expected_deaths / (prior_rate + expected_deaths)
  )]
  estimated[, `:=`(
    rate_ratio_eb = posterior_shape / posterior_rate,
    ci_lower_eb = qgamma(0.025, posterior_shape, rate = posterior_rate),
    ci_upper_eb = qgamma(0.975, posterior_shape, rate = posterior_rate),
    probability_rr_gt_1 = 1 - pgamma(
      1,
      posterior_shape,
      rate = posterior_rate
    ),
    beta_eb = log(posterior_shape / posterior_rate)
  )]

  eb_columns <- c(
    "posterior_shape",
    "posterior_rate",
    "reliability",
    "rate_ratio_eb",
    "ci_lower_eb",
    "ci_upper_eb",
    "probability_rr_gt_1",
    "beta_eb"
  )
  zip_results[estimated, on = "zip", (eb_columns) := mget(paste0("i.", eb_columns))]
  zip_results[, `:=`(
    target_id = target_id,
    target_slug = target_slug,
    target_order = target$target_order,
    cause = target_cause,
    age_group = target_age,
    period = target_period
  )]

  map_data <- merge(
    panel_geometry,
    as.data.frame(zip_results),
    by = "zip",
    all.x = TRUE,
    sort = FALSE
  )
  map_data <- st_as_sf(map_data)

  moran <- calculate_moran(map_data, target_id, target$target_order)
  zip_results <- merge(
    zip_results,
    moran$local,
    by = c("target_id", "target_order", "zip"),
    all.x = TRUE,
    sort = FALSE
  )
  zip_results[status != "estimated", lisa_cluster := "Not estimable"]

  aggregate_observed <- sum(estimated$observed_deaths)
  aggregate_expected <- sum(estimated$expected_deaths)
  aggregate_rr <- aggregate_observed / aggregate_expected
  diagnostics <- data.table(
    target_id = target_id,
    target_order = target$target_order,
    cause = target_cause,
    age_group = target_age,
    period = target_period,
    total_panel_zips = length(all_panel_zips),
    geometry_zips = nrow(panel_geometry),
    estimable_zips = nrow(estimated),
    total_panel_rows = nrow(model_data),
    model_observations = nobs(model),
    removed_observations = nrow(model_data) - nobs(model),
    coefficient_count = length(coef(model)),
    model_converged = if (is.null(model$convStatus)) TRUE else isTRUE(model$convStatus),
    log_likelihood = as.numeric(logLik(model)),
    pseudo_r2 = safe_fitstat(model, "pr2"),
    squared_correlation = safe_fitstat(model, "sq.cor"),
    pooled_log_rate_ratio = pooled_beta,
    pooled_rate_ratio = exp(pooled_beta),
    frozen_log_rate_ratio = target$frozen_log_rate_ratio,
    point_estimate_difference = point_difference,
    aggregate_observed_deaths = aggregate_observed,
    aggregate_expected_deaths = aggregate_expected,
    aggregate_standardized_rate_ratio = aggregate_rr,
    pooled_vs_aggregate_rr_difference = exp(pooled_beta) - aggregate_rr,
    eb_prior_shape = prior_shape,
    eb_prior_rate = prior_rate,
    eb_prior_mean_rr = prior_mean,
    eb_prior_sd_rr = prior_sd,
    geometry_source = geometry_source,
    overlap_assignments_resolved = overlap_assignments
  )

  map_output <- merge(
    panel_geometry,
    as.data.frame(zip_results),
    by = "zip",
    all.x = TRUE,
    sort = FALSE
  )
  map_output <- st_as_sf(map_output)

  fwrite(
    zip_results,
    file.path(intermediate_dir, paste0(target_slug, "_zip_results.csv"))
  )
  fwrite(
    moran$global,
    file.path(intermediate_dir, paste0(target_slug, "_global_moran.csv"))
  )
  fwrite(
    diagnostics,
    file.path(intermediate_dir, paste0(target_slug, "_diagnostics.csv"))
  )

  all_zip_results[[target_index]] <- zip_results
  all_global_results[[target_index]] <- moran$global
  all_diagnostics[[target_index]] <- diagnostics
  all_spatial_layers[[target_index]] <- map_output

  rm(
    cause_mask, keep, deaths_target, daily_counts, model_data, model,
    target_data, counterfactual, zip_counts, estimated, prior, map_data,
    moran, map_output, diagnostics, zip_results
  )
  gc()
}
}

message(
  "Combining targets and applying the ", expected_target_count,
  "-map global Moran FDR"
)
zip_output <- rbindlist(all_zip_results, use.names = TRUE, fill = TRUE)
global_output <- rbindlist(all_global_results, use.names = TRUE, fill = TRUE)
diagnostic_output <- rbindlist(all_diagnostics, use.names = TRUE, fill = TRUE)
spatial_output <- do.call(rbind, all_spatial_layers)

global_output[, global_moran_fdr_q_spatial_family := p.adjust(
  permutation_p_two_sided,
  method = "BH"
)]
global_output[, (global_q_column) := global_moran_fdr_q_spatial_family]
global_output[, spatial_concentration_conclusion := fifelse(
  morans_i > 0 & global_moran_fdr_q_spatial_family < 0.05,
  "Evidence of global spatial concentration",
  "No evidence of global spatial concentration"
)]

targets_output <- merge(
  targets,
  diagnostic_output,
  by = c("target_id", "target_order", "cause", "age_group", "period"),
  all.x = TRUE,
  sort = FALSE
)
targets_output <- merge(
  targets_output,
  global_output,
  by = c("target_id", "target_order"),
  all.x = TRUE,
  sort = FALSE
)
setorder(targets_output, target_order)

global_merge <- global_output[, c(
  "target_id",
  "morans_i",
  "permutation_p_two_sided",
  "global_moran_fdr_q_spatial_family",
  global_q_column,
  "spatial_concentration_conclusion"
), with = FALSE]
setnames(
  global_merge,
  c("morans_i", "permutation_p_two_sided"),
  c("global_morans_i", "global_moran_p_value")
)

zip_output <- merge(
  zip_output,
  global_merge,
  by = "target_id",
  all.x = TRUE,
  sort = FALSE
)
setorder(zip_output, target_order, zip)

spatial_output <- merge(
  spatial_output,
  global_merge,
  by = "target_id",
  all.x = TRUE,
  sort = FALSE
)
spatial_output <- st_as_sf(spatial_output)

for (object_name in c(
  "targets_output", "zip_output", "global_output", "diagnostic_output"
)) {
  object <- get(object_name)
  object[, `:=`(
    analysis_version = analysis_version,
    freeze_date = freeze_date,
    source_commit = source_commit,
    wind_threshold_kt = wind_threshold
  )]
  assign(object_name, object)
}
spatial_output$analysis_version <- analysis_version
spatial_output$freeze_date <- as.Date(freeze_date)
spatial_output$source_commit <- source_commit
spatial_output$wind_threshold_kt <- wind_threshold

message("Running spatial-freeze acceptance checks")
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

add_check(
  "Locked target count",
  nrow(targets_output) == expected_target_count,
  nrow(targets_output),
  expected_target_count
)
add_check(
  "Unique target IDs",
  uniqueN(targets_output$target_id) == expected_target_count,
  uniqueN(targets_output$target_id),
  expected_target_count
)
add_check(
  "ZIP rows per target",
  all(zip_output[, .N, by = target_id]$N == length(all_panel_zips)),
  paste(unique(zip_output[, .N, by = target_id]$N), collapse = ","),
  length(all_panel_zips)
)
add_check(
  "Complete panel geometry",
  nrow(panel_geometry) == length(all_panel_zips),
  nrow(panel_geometry),
  length(all_panel_zips)
)
add_check(
  "All pooled models converged",
  all(diagnostic_output$model_converged),
  sum(diagnostic_output$model_converged),
  expected_target_count
)
max_point_difference <- max(abs(diagnostic_output$point_estimate_difference))
add_check(
  "Pooled point estimates reconcile with primary freeze",
  max_point_difference < 1e-10,
  format(max_point_difference, scientific = TRUE, digits = 4L),
  "< 1e-10"
)
max_aggregate_difference <- max(abs(
  diagnostic_output$pooled_vs_aggregate_rr_difference
))
add_check(
  "Aggregate observed-expected ratios reconcile with pooled RRs",
  max_aggregate_difference < 1e-7,
  format(max_aggregate_difference, scientific = TRUE, digits = 4L),
  "< 1e-7",
  "Secondary score-equation diagnostic; locked coefficients use the stricter 1e-10 check."
)
add_check(
  "At least ten estimable ZIPs per target",
  all(diagnostic_output$estimable_zips >= 10L),
  min(diagnostic_output$estimable_zips),
  ">= 10"
)

estimated_zip <- zip_output[status == "estimated"]
add_check(
  "Finite positive EB rate ratios",
  all(is.finite(estimated_zip$rate_ratio_eb) & estimated_zip$rate_ratio_eb > 0),
  sum(is.finite(estimated_zip$rate_ratio_eb) & estimated_zip$rate_ratio_eb > 0),
  nrow(estimated_zip)
)
add_check(
  "Ordered EB confidence intervals",
  all(
    estimated_zip$ci_lower_eb <= estimated_zip$rate_ratio_eb &
      estimated_zip$rate_ratio_eb <= estimated_zip$ci_upper_eb
  ),
  sum(
    estimated_zip$ci_lower_eb <= estimated_zip$rate_ratio_eb &
      estimated_zip$rate_ratio_eb <= estimated_zip$ci_upper_eb
  ),
  nrow(estimated_zip)
)
add_check(
  "EB reliability in [0,1]",
  all(estimated_zip$reliability >= 0 & estimated_zip$reliability <= 1),
  paste(signif(range(estimated_zip$reliability), 5L), collapse = " to "),
  "0 to 1"
)
add_check(
  "Posterior probabilities in [0,1]",
  all(
    estimated_zip$probability_rr_gt_1 >= 0 &
      estimated_zip$probability_rr_gt_1 <= 1
  ),
  paste(signif(range(estimated_zip$probability_rr_gt_1), 5L), collapse = " to "),
  "0 to 1"
)
add_check(
  paste(expected_target_count, "global Moran tests"),
  nrow(global_output) == expected_target_count,
  nrow(global_output),
  expected_target_count
)
add_check(
  "Global Moran permutation count",
  all(global_output$permutations == nsim),
  paste(unique(global_output$permutations), collapse = ","),
  nsim
)
add_check(
  "Global Moran p and q values in [0,1]",
  all(
    global_output$permutation_p_two_sided >= 0 &
      global_output$permutation_p_two_sided <= 1 &
      global_output$global_moran_fdr_q_spatial_family >= 0 &
      global_output$global_moran_fdr_q_spatial_family <= 1
  ),
  paste0(
    "p ", paste(signif(range(global_output$permutation_p_two_sided), 4L), collapse = " to "),
    "; q ", paste(signif(range(global_output$global_moran_fdr_q_spatial_family), 4L), collapse = " to ")
  ),
  "0 to 1"
)
local_q <- estimated_zip$local_fdr_q_value
local_q <- local_q[!is.na(local_q)]
add_check(
  "Local Moran q values in [0,1]",
  all(local_q >= 0 & local_q <= 1),
  paste(signif(range(local_q), 4L), collapse = " to "),
  "0 to 1"
)

if (file.exists(legacy_eb_path)) {
  legacy_eb <- fread(legacy_eb_path)
  legacy_eb[, zip := sprintf("%05d", as.integer(zip))]
  calibration <- zip_output[
    cause == "Cardiovascular" & age_group == "75+" & period == "Days 0 to +6" &
      status == "estimated",
    .(zip, new_rate_ratio_eb = rate_ratio_eb)
  ]
  calibration <- merge(calibration, legacy_eb[, .(
    zip,
    legacy_rate_ratio_eb = rate_ratio_eb
  )], by = "zip")
  calibration_difference <- max(abs(
    calibration$new_rate_ratio_eb - calibration$legacy_rate_ratio_eb
  ))
  add_check(
    "Cardiovascular 75+ EB calibration",
    nrow(calibration) == nrow(legacy_eb) & calibration_difference < 1e-10,
    paste0(
      nrow(calibration), " ZIPs; max difference ",
      format(calibration_difference, scientific = TRUE, digits = 4L)
    ),
    paste0(nrow(legacy_eb), " ZIPs; difference < 1e-10")
  )
}

if (file.exists(legacy_global_moran_path)) {
  legacy_global <- fread(legacy_global_moran_path)
  new_global <- global_output[
    target_id == "SP-34KT-C02-A04-P03",
    morans_i
  ]
  global_difference <- abs(new_global - legacy_global$morans_i[1L])
  add_check(
    "Cardiovascular 75+ global Moran calibration",
    length(new_global) == 1L && global_difference < 1e-10,
    format(global_difference, scientific = TRUE, digits = 4L),
    "< 1e-10"
  )
}

if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more spatial-freeze acceptance checks failed.")
}

description_map <- c(
  analysis_version = "Immutable spatial-analysis freeze identifier.",
  freeze_date = "Date on which the spatial result set was frozen.",
  source_commit = "Git commit at the time the freeze was built.",
  wind_threshold_kt = "Tropical-cyclone wind exposure threshold in knots.",
  target_id = "Stable identifier linking the spatial analysis to a frozen statewide result.",
  target_order = "Locked display and processing order of the spatial targets.",
  target_slug = "Filesystem-safe target label.",
  result_id = "Identifier of the source row in the frozen master results.",
  cause = "Underlying-cause-of-death category.",
  age_group = "Mutually exclusive older-adult age group.",
  period = "Seven-day event-relative period.",
  zip = "Five-character ZIP/ZCTA identifier stored as text.",
  status = "Whether the ZIP counterfactual expected count was estimable.",
  observed_deaths = "Observed deaths in the target period and ZIP.",
  expected_deaths = "Counterfactual deaths predicted with the target event indicator set to the reference level.",
  target_days = "Target-period ZIP-days contributing to the ZIP aggregate.",
  rate_ratio_raw = "Observed divided by expected deaths before smoothing.",
  posterior_shape = "Gamma posterior shape parameter.",
  posterior_rate = "Gamma posterior rate parameter.",
  reliability = "Expected/(prior rate + expected); larger values indicate less shrinkage.",
  rate_ratio_eb = "Gamma-Poisson empirical-Bayes posterior mean rate ratio.",
  ci_lower_eb = "Lower endpoint of the 95% gamma posterior interval.",
  ci_upper_eb = "Upper endpoint of the 95% gamma posterior interval.",
  probability_rr_gt_1 = "Posterior probability that the ZIP rate ratio exceeds 1.",
  beta_eb = "Natural logarithm of the EB-smoothed rate ratio.",
  centered_log_rr = "Log EB rate ratio centered on the mean among analyzed ZIPs.",
  neighbor_count = "Number of queen-contiguous neighbors.",
  spatial_lag_centered_log_rr = "Row-standardized spatial lag of the centered log EB rate ratio.",
  local_morans_i = "Local Moran statistic for the centered log EB rate ratio.",
  local_permutation_p_value = "Two-sided local permutation p-value based on 9,999 permutations.",
  local_fdr_q_value = "Within-map Benjamini-Hochberg q-value for the local Moran test.",
  lisa_cluster = "FDR-supported local spatial association class; non-significant and island ZIPs retained.",
  morans_i = "Global Moran statistic for centered log EB rate ratios.",
  permutation_p_two_sided = "Two-sided permutation p-value for global Moran's I.",
  global_moran_fdr_q_spatial_family = "Benjamini-Hochberg q-value across the locked global Moran family.",
  global_moran_fdr_q_five_maps = "Benjamini-Hochberg q-value across the five v1 global Moran tests.",
  global_moran_fdr_q_eight_maps = "Benjamini-Hochberg q-value across the eight amended-v2 global Moran tests.",
  spatial_concentration_conclusion = "Protocol-defined global spatial-concentration conclusion."
)

dictionary_for <- function(object, table_name) {
  data.table(
    table_name = table_name,
    column_name = names(object),
    storage_type = vapply(object, function(x) class(x)[1L], character(1L)),
    description = unname(description_map[names(object)])
  )[, description := fifelse(
    is.na(description),
    paste0("Supporting field generated by build_frozen_spatial_results_34kt.R: ",
      gsub("_", " ", column_name), "."),
    description
  )]
}

dictionary <- rbindlist(list(
  dictionary_for(targets_output, paste0("spatial_targets_34kt_", release_label)),
  dictionary_for(zip_output, paste0("spatial_zip_results_34kt_", release_label)),
  dictionary_for(global_output, paste0("spatial_global_moran_34kt_", release_label)),
  dictionary_for(
    diagnostic_output,
    paste0("spatial_model_diagnostics_34kt_", release_label)
  )
))

message("Writing frozen spatial outputs")
fwrite(targets_output, target_path)
fwrite(zip_output, zip_path)
fwrite(global_output, global_path)
fwrite(diagnostic_output, diagnostic_path)
fwrite(dictionary, dictionary_path)
fwrite(qa, qa_path)

if (file.exists(gpkg_path)) unlink(gpkg_path)
st_write(spatial_output, gpkg_path, layer = "spatial_results", quiet = TRUE)

manifest_targets <- c(
  target_path,
  zip_path,
  global_path,
  diagnostic_path,
  gpkg_path,
  dictionary_path,
  qa_path,
  "scripts/mortality/build_frozen_spatial_results_34kt.R",
  "scripts/mortality/cause_age_count_helpers.R",
  primary_master_path,
  geometry_cache_path
)
hash_output <- system2(
  "shasum",
  c("-a", "256", manifest_targets),
  stdout = TRUE
)
manifest_split <- tstrsplit(hash_output, "  ", fixed = TRUE)
manifest <- data.table(
  sha256 = manifest_split[[1L]],
  path = manifest_split[[2L]],
  bytes = as.numeric(file.info(manifest_split[[2L]])$size),
  analysis_version = analysis_version,
  freeze_date = freeze_date
)
fwrite(manifest, manifest_path)

cat("\nFrozen spatial results completed\n")
cat("Analysis version:", analysis_version, "\n")
cat("Targets:", nrow(targets_output), "\n")
cat("ZIP rows:", nrow(zip_output), "\n")
cat("Estimable ZIP-target rows:", nrow(estimated_zip), "\n")
cat("Global Moran tests:", nrow(global_output), "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
cat("Output directory:", output_dir, "\n")
