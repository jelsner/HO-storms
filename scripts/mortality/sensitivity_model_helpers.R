suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(lubridate)
})

source("scripts/mortality/cause_age_count_helpers.R")

sensitivity_version <- "exposure_event_time_v1_2026-11-18"
sensitivity_output_dir <- file.path(
  "outputs/mortality/paper_sensitivity_34kt",
  sensitivity_version
)
dir.create(sensitivity_output_dir, recursive = TRUE, showWarnings = FALSE)

sensitivity_period_levels <- c(
  "None",
  "Days -14 to -8",
  "Days -7 to -1",
  "Days 0 to +6",
  "Days +7 to +13"
)
sensitivity_age_levels <- c("All ages", "<65", "65-74", "75+")
sensitivity_retained_causes <- c(
  "All diseases",
  "Cardiovascular",
  "Respiratory",
  "Infectious diseases",
  "Injuries",
  "Neuropsychiatric disorders",
  "Renal diseases",
  "Neoplasms"
)

period_from_rel_day <- function(rel_day) {
  fcase(
    rel_day >= -14L & rel_day <= -8L, "Days -14 to -8",
    rel_day >= -7L & rel_day <= -1L, "Days -7 to -1",
    rel_day >= 0L & rel_day <= 6L, "Days 0 to +6",
    rel_day >= 7L & rel_day <= 13L, "Days +7 to +13",
    default = NA_character_
  )
}

sensitivity_slugify <- function(value) {
  gsub("_+$", "", gsub("[^A-Za-z0-9]+", "_", tolower(value)))
}

safe_sensitivity_fitstat <- function(model, statistic) {
  result <- tryCatch(
    fitstat(model, statistic, simplify = TRUE),
    error = function(e) NA_real_
  )
  if (!length(result)) NA_real_ else as.numeric(result[1L])
}

build_sensitivity_calendar <- function(wind_threshold, daily = FALSE) {
  daily_panel_path <- sprintf(
    "data/mortality/processed/death-panels/daily_panel_%dkt.csv",
    wind_threshold
  )
  if (!file.exists(daily_panel_path)) {
    stop("Missing daily panel: ", daily_panel_path)
  }

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
  complete_window_dates <- unique(event_calendar$date)

  calendar <- calendar_source[, .(zip, date)]
  calendar[, month := month(as.Date(date))]
  calendar <- calendar[month %in% 5:11 | date %in% complete_window_dates]
  calendar[, month := NULL]

  if (daily) {
    calendar <- event_calendar[
      calendar,
      on = .(zip, date),
      .(
        zip = i.zip,
        date = i.date,
        rel_day = x.rel_day,
        impact_date = x.impact_date
      )
    ]
    calendar[, rel_day_factor := fifelse(
      is.na(rel_day),
      "None",
      as.character(rel_day)
    )]
    calendar[, rel_day_factor := factor(
      rel_day_factor,
      levels = c("None", as.character(-14L:13L))
    )]
  } else {
    event_calendar[, event_period := period_from_rel_day(rel_day)]
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
  }

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
    panel_zips = length(all_panel_zips),
    daily_panel_path = daily_panel_path
  )
}

load_sensitivity_deaths <- function(wind_threshold, complete_window_dates) {
  deaths_path <- sprintf(
    "data/mortality/processed/spatial-results/deaths_with_zip_%dkt.rds",
    wind_threshold
  )
  cause_lookup_path <-
    "data/mortality/derived/mortality_underlying_cause_lookup_1985_2022.rds"
  if (!file.exists(deaths_path) || !file.exists(cause_lookup_path)) {
    stop("Missing mortality or cause-lookup input for ", wind_threshold, " kt.")
  }

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
    stop("Underlying-cause lookup is not unique by death ID and year.")
  }
  deaths <- merge(
    deaths,
    cause_lookup,
    by = c("Death_ID", "join_year"),
    all.x = TRUE,
    sort = FALSE
  )
  deaths
}

build_cause_daily_counts <- function(deaths, cause_name, cause_mask = NULL) {
  if (cause_name == "All diseases") {
    selected <- deaths
  } else {
    if (is.null(cause_mask)) {
      stop("A precomputed cause mask is required for ", cause_name, ".")
    }
    selected <- deaths[!is.na(cause_mask) & cause_mask]
  }
  list(
    all_ages = selected[, .(deaths = .N), by = .(zip, date)],
    by_age = selected[, .(deaths = .N), by = .(zip, date, age_group)]
  )
}

build_group_model_data <- function(counts, calendar, age_group_name) {
  if (age_group_name == "All ages") {
    group_counts <- copy(counts$all_ages)
  } else {
    group_counts <- counts$by_age[
      age_group == age_group_name,
      .(zip, date, deaths)
    ]
  }
  group_counts[, zip := factor(zip, levels = levels(calendar$zip))]
  model_data <- group_counts[calendar, on = .(zip, date)]
  model_data[is.na(deaths), deaths := 0L]
  model_data
}

extract_sensitivity_estimates <- function(model, term_prefix) {
  beta <- coef(model)
  standard_error <- se(model)
  p_value <- pvalue(model)
  keep <- startsWith(names(beta), term_prefix)
  data.table(
    term = sub(paste0("^", term_prefix), "", names(beta)[keep]),
    log_rate_ratio = unname(beta[keep]),
    standard_error = unname(standard_error[keep]),
    p_value = unname(p_value[keep]),
    observations = nobs(model),
    rate_ratio = exp(unname(beta[keep])),
    ci_lower = exp(unname(beta[keep]) - 1.96 * unname(standard_error[keep])),
    ci_upper = exp(unname(beta[keep]) + 1.96 * unname(standard_error[keep])),
    percent_change = 100 * (exp(unname(beta[keep])) - 1)
  )
}

get_sensitivity_source_commit <- function() {
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
