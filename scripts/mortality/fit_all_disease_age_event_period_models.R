suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(lubridate)
})

source("scripts/mortality/cause_age_count_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
wind_threshold <- if (length(args)) {
  suppressWarnings(as.integer(args[1L]))
} else {
  34L
}

if (is.na(wind_threshold) || !wind_threshold %in% c(34L, 55L, 64L)) {
  stop("Wind threshold must be one of 34, 55, or 64 kt.")
}

cause_name <- if (length(args) >= 2L) args[2L] else "All diseases"
available_causes <- names(build_cause_masks(integer(), character()))
if (!cause_name %in% available_causes) {
  stop(
    "Unknown cause '", cause_name, "'. Available causes: ",
    paste(available_causes, collapse = ", ")
  )
}
cause_slug <- gsub("_+$", "", gsub("[^A-Za-z0-9]+", "_", tolower(cause_name)))

daily_panel_path <- sprintf(
  "data/mortality/processed/death-panels/daily_panel_%dkt.csv",
  wind_threshold
)
deaths_with_zip_path <- sprintf(
  "data/mortality/processed/spatial-results/deaths_with_zip_%dkt.rds",
  wind_threshold
)
master_mortality_path <- "data/mortality/raw/all_data.Rdata"
cause_lookup_cache_path <-
  "data/mortality/derived/mortality_underlying_cause_lookup_1985_2022.rds"
model_dir <- "models/mortality/cause_age_event_periods"
output_dir <- "outputs/mortality/cause_age_event_period_models"
estimate_path <- sprintf(
  "%s/%s_age_event_period_estimates_%dkt.csv",
  output_dir,
  cause_slug,
  wind_threshold
)
sample_count_path <- sprintf(
  "%s/%s_age_event_period_sample_counts_%dkt.csv",
  output_dir,
  cause_slug,
  wind_threshold
)

period_levels <- c(
  "None",
  "Days -14 to -8",
  "Days -7 to -1",
  "Days 0 to +6",
  "Days +7 to +13"
)

group_levels <- c("All ages", "<65", "65-74", "75+")

period_from_rel_day <- function(rel_day) {
  fcase(
    rel_day >= -14L & rel_day <= -8L, "Days -14 to -8",
    rel_day >= -7L & rel_day <= -1L, "Days -7 to -1",
    rel_day >= 0L & rel_day <= 6L, "Days 0 to +6",
    rel_day >= 7L & rel_day <= 13L, "Days +7 to +13",
    default = NA_character_
  )
}

extract_estimates <- function(model, group_name, cause_name) {
  beta <- coef(model)
  standard_error <- se(model)
  p_value <- pvalue(model)
  keep <- grepl("^event_period::", names(beta))

  result <- data.table(
    cause = cause_name,
    group = group_name,
    period = sub("^event_period::", "", names(beta)[keep]),
    log_rate_ratio = unname(beta[keep]),
    standard_error = unname(standard_error[keep]),
    p_value = unname(p_value[keep]),
    observations = nobs(model)
  )

  result[, `:=`(
    rate_ratio = exp(log_rate_ratio),
    ci_lower = exp(log_rate_ratio - 1.96 * standard_error),
    ci_upper = exp(log_rate_ratio + 1.96 * standard_error),
    percent_change = 100 * (exp(log_rate_ratio) - 1)
  )]

  result[, period_order := match(period, period_levels)]
  setorder(result, period_order)
  result[, period_order := NULL]
  result
}

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Recover ZIP-event impact dates from the existing relative-day panel and then
# extend each event to the requested -14 through +13 day window.
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

rm(event_calendar_raw, overlap_zip_dates)
gc()

# Retain the original model's May-November seasonal restriction, plus every
# calendar date needed to complete a requested event window. The latter keeps
# late-November post-impact windows intact and includes all ZIPs on those dates.
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

calendar[, `:=`(
  zip = factor(zip),
  yw = year(as.Date(date)) * 100L + isoweek(as.Date(date)),
  dow = as.integer(strftime(as.Date(date), "%u")),
  covid = as.integer(
    date >= as.IDate("2020-01-01") & date <= as.IDate("2022-12-31")
  )
)]
calendar[, zip_covid := interaction(zip, covid, drop = TRUE)]

stopifnot(!anyDuplicated(calendar[, .(zip, date)]))

rm(calendar_source, event_calendar)
gc()

# Build daily death counts for the all-age and mutually exclusive age samples.
deaths_sf <- readRDS(deaths_with_zip_path)
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

if (cause_name != "All diseases") {
  message("Attaching underlying-cause codes for ", cause_name)
  deaths[, `:=`(
    year = year(as.Date(date)),
    join_year = as.character(year(as.Date(date)))
  )]
  death_keys <- unique(deaths[, .(Death_ID, join_year)])

  if (file.exists(cause_lookup_cache_path)) {
    message("Reading cached underlying-cause lookup")
    cause_master <- as.data.table(readRDS(cause_lookup_cache_path))
  } else {
    message("Building reusable underlying-cause lookup")
    env <- new.env(parent = emptyenv())
    loaded_objects <- load(master_mortality_path, envir = env)
    if (!"all_data" %in% loaded_objects) {
      stop("Expected an object named 'all_data' in ", master_mortality_path)
    }

    cause_master <- as.data.table(env$all_data)[, .(
      Death_ID = ID,
      join_year = as.character(EVENT_YEAR),
      code = as.character(ICD_CODE)
    )]
    rm(env)
    gc()
    dir.create(
      dirname(cause_lookup_cache_path),
      recursive = TRUE,
      showWarnings = FALSE
    )
    saveRDS(cause_master, cause_lookup_cache_path)
  }

  cause_lookup <- cause_master[
    death_keys,
    on = .(join_year, Death_ID),
    nomatch = 0L,
    .(
      Death_ID = i.Death_ID,
      join_year = i.join_year,
      code
    )
  ]

  rm(cause_master, death_keys)
  gc()

  if (anyDuplicated(cause_lookup[, .(Death_ID, join_year)])) {
    stop("Underlying-cause lookup is not unique by death ID and year.")
  }

  n_before_cause_join <- nrow(deaths)
  deaths <- merge(
    deaths,
    cause_lookup,
    by = c("Death_ID", "join_year"),
    all.x = TRUE,
    sort = FALSE
  )
  if (nrow(deaths) != n_before_cause_join) {
    stop(
      "Cause-code join changed the row count of candidate deaths: ",
      n_before_cause_join, " before versus ", nrow(deaths), " after."
    )
  }

  missing_cause_codes <- sum(is.na(deaths$code) | deaths$code == "")
  if (missing_cause_codes) {
    warning(
      missing_cause_codes,
      " candidate deaths lack a matched underlying-cause code and will be ",
      "excluded from the cause-specific outcome."
    )
  }

  cause_mask <- build_cause_masks(deaths$year, deaths$code)[[cause_name]]
  deaths <- deaths[!is.na(cause_mask) & cause_mask]

  rm(cause_lookup, cause_mask)
  gc()
}

deaths[, age_group := as.character(make_age_group(age))]

daily_age_counts <- deaths[, .(deaths = .N), by = .(zip, date, age_group)]
daily_all_counts <- deaths[, .(deaths = .N), by = .(zip, date)]

rm(deaths)
gc()

all_estimates <- vector("list", length(group_levels))
all_sample_counts <- vector("list", length(group_levels))

for (group_index in seq_along(group_levels)) {
  group_name <- group_levels[group_index]
  message("Fitting ", cause_name, " model for ", group_name)

  if (group_name == "All ages") {
    group_counts <- daily_all_counts
  } else {
    group_counts <- daily_age_counts[
      age_group == group_name,
      .(zip, date, deaths)
    ]
  }

  group_counts[, zip := factor(zip, levels = levels(calendar$zip))]

  model_data <- group_counts[
    calendar,
    on = .(zip, date)
  ]
  model_data[is.na(deaths), deaths := 0L]

  sample_counts <- model_data[, .(
    zip_days = .N,
    deaths = sum(deaths),
    nonzero_zip_days = sum(deaths > 0L)
  ), by = event_period]
  sample_counts[, `:=`(cause = cause_name, group = group_name)]
  setcolorder(
    sample_counts,
    c(
      "cause", "group", "event_period", "zip_days", "deaths",
      "nonzero_zip_days"
    )
  )

  model <- fepois(
    deaths ~ i(event_period, ref = "None") |
      zip + yw + dow + zip_covid,
    data = model_data,
    cluster = ~ zip + yw,
    notes = TRUE
  )

  model_path <- sprintf(
    "%s/%s_%s_%dkt.rds",
    model_dir,
    cause_slug,
    gsub("[^A-Za-z0-9]+", "_", tolower(group_name)),
    wind_threshold
  )
  saveRDS(model, model_path)

  all_estimates[[group_index]] <- extract_estimates(
    model,
    group_name,
    cause_name
  )
  all_sample_counts[[group_index]] <- sample_counts

  rm(group_counts, model_data, model, sample_counts)
  gc()
}

estimates <- rbindlist(all_estimates)
estimates[, group_order := match(group, group_levels)]
estimates[, period_order := match(period, period_levels)]
setorder(estimates, group_order, period_order)
estimates[, c("group_order", "period_order") := NULL]

sample_counts <- rbindlist(all_sample_counts)
sample_counts[, group_order := match(group, group_levels)]
sample_counts[, period_order := match(as.character(event_period), period_levels)]
setorder(sample_counts, group_order, period_order)
sample_counts[, c("group_order", "period_order") := NULL]

fwrite(estimates, estimate_path)
fwrite(sample_counts, sample_count_path)

cat("Wind threshold: >=", wind_threshold, " kt\n", sep = "")
cat("ZIP-events:", nrow(zip_events), "\n")
cat("Distinct impact dates:", uniqueN(zip_events$impact_date), "\n")
cat("Overlapping ZIP-date assignments resolved:", overlap_assignments, "\n")
cat("Estimate output:", estimate_path, "\n")
cat("Sample-count output:", sample_count_path, "\n\n")
print(estimates)
