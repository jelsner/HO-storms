suppressPackageStartupMessages({
  library(data.table)
})

source("scripts/mortality/cause_age_count_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
wind_threshold <- if (length(args)) {
  suppressWarnings(as.integer(args[1L]))
} else {
  64L
}

if (is.na(wind_threshold) || !wind_threshold %in% c(34L, 55L, 64L)) {
  stop("Wind threshold must be one of 34, 55, or 64 kt.")
}

statewide_count_path <- "outputs/mortality/mortality_counts_by_cause_age_1985_2022.csv"
daily_panel_path <- sprintf(
  "data/mortality/processed/death-panels/daily_panel_%dkt.csv",
  wind_threshold
)
deaths_with_zip_path <- sprintf(
  "data/mortality/processed/spatial-results/deaths_with_zip_%dkt.rds",
  wind_threshold
)
master_mortality_path <- "data/mortality/raw/all_data.Rdata"
output_path <- sprintf(
  paste0(
    "outputs/mortality/mortality_counts_by_cause_age_and_event_period_",
    "%dkt_1985_2022.csv"
  ),
  wind_threshold
)

period_levels <- c(
  "Statewide (1985-2022)",
  "Days -14 to -8",
  "Days -7 to -1",
  "Days 0 to +6",
  "Days +7 to +13"
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

if (!file.exists(statewide_count_path)) {
  stop(
    "Statewide count table is missing. Run scripts/mortality/build_cause_age_counts.R first."
  )
}

# Recover the ZIP-specific impact dates used by the existing wind-threshold panel.
# Every ZIP-event contributes 17 rows (-8 through +8) to this calendar.
panel_exposure <- fread(
  daily_panel_path,
  select = c("zip", "date", "rel_day")
)[!is.na(rel_day)]

panel_exposure[, `:=`(
  zip = as.character(zip),
  date = as.IDate(date),
  rel_day = as.integer(rel_day)
)]

zip_events <- unique(panel_exposure[, .(
  zip,
  impact_date = date - rel_day
)])

expected_rows <- nrow(zip_events) * 17L
source_overlap_rows_resolved <- expected_rows - nrow(panel_exposure)
if (source_overlap_rows_resolved < 0L) {
  stop("The source exposure calendar contains unexpected duplicate rows.")
}

rm(panel_exposure)
gc()

# Extend each observed ZIP-event to the four requested seven-day periods.
event_calendar_raw <- zip_events[, .(
  rel_day = -14L:13L
), by = .(zip, impact_date)]

event_calendar_raw[, `:=`(
  date = impact_date + rel_day,
  period = period_from_rel_day(rel_day)
)]

overlap_zip_dates <- event_calendar_raw[, .N, by = .(zip, date)][N > 1L]
overlap_assignments <- if (nrow(overlap_zip_dates)) {
  sum(overlap_zip_dates$N - 1L)
} else {
  0L
}

# Match the mortality panel convention: for overlapping event windows, assign a
# ZIP-date to the event day closest to impact; prefer the pre-impact day on ties.
event_calendar <- event_calendar_raw[
  order(zip, date, abs(rel_day), rel_day, impact_date)
][, .SD[1L], by = .(zip, date)]

rm(event_calendar_raw)
gc()

# Reuse the ZIP assignments already created for this mortality panel.
deaths_sf <- readRDS(deaths_with_zip_path)
deaths_zip <- as.data.table(deaths_sf)[, .(
  Death_ID,
  date = as.IDate(Death_Date),
  age = suppressWarnings(as.integer(age)),
  zip = as.character(zip)
)]
rm(deaths_sf)
gc()

deaths_zip <- deaths_zip[age >= 0L & age <= 120L & !is.na(zip)]
deaths_zip[, age_group := make_age_group(age)]

event_deaths <- merge(
  deaths_zip,
  event_calendar[, .(zip, date, impact_date, rel_day, period)],
  by = c("zip", "date"),
  all = FALSE,
  sort = FALSE
)

if (anyDuplicated(event_deaths[, .(Death_ID, date)])) {
  stop("A death was assigned to more than one event period after overlap resolution.")
}

event_deaths[, year := as.integer(format(as.Date(date), "%Y"))]
event_keys <- unique(event_deaths[, .(
  Death_ID,
  join_year = as.character(year)
)])

rm(deaths_zip, event_calendar)
gc()

# Attach underlying-cause codes only for deaths in the four event periods.
env <- new.env(parent = emptyenv())
loaded_objects <- load(master_mortality_path, envir = env)
if (!"all_data" %in% loaded_objects) {
  stop("Expected an object named 'all_data' in ", master_mortality_path)
}

master <- as.data.table(env$all_data)
cause_lookup <- master[
  event_keys,
  on = .(EVENT_YEAR = join_year, ID = Death_ID),
  nomatch = 0L,
  .(
    Death_ID = i.Death_ID,
    year = as.integer(i.join_year),
    code = as.character(ICD_CODE)
  )
]

rm(master, env, event_keys)
gc()

if (anyDuplicated(cause_lookup[, .(Death_ID, year)])) {
  stop("Underlying-cause lookup is not unique by death ID and year.")
}

n_before_cause_join <- nrow(event_deaths)
event_deaths <- merge(
  event_deaths,
  cause_lookup,
  by = c("Death_ID", "year"),
  all = FALSE,
  sort = FALSE
)

if (nrow(event_deaths) != n_before_cause_join) {
  stop(
    "Cause-code join changed the number of event-period deaths: ",
    n_before_cause_join, " before versus ", nrow(event_deaths), " after."
  )
}

rm(cause_lookup)
gc()

cause_order <- names(build_cause_masks(integer(), character()))

event_counts <- rbindlist(lapply(period_levels[-1L], function(period_name) {
  period_deaths <- event_deaths[period == period_name]
  masks <- build_cause_masks(period_deaths$year, period_deaths$code)
  count_causes_by_age(
    masks,
    period_deaths$age_group,
    extra_columns = list(period = period_name)
  )
}))

statewide_counts <- fread(statewide_count_path)
statewide_counts[, period := period_levels[1L]]
setcolorder(
  statewide_counts,
  c("cause", "period", "<65", "65-74", "75+", "All ages")
)

event_counts[, `All ages` := `<65` + `65-74` + `75+`]
setcolorder(
  event_counts,
  c("cause", "period", "<65", "65-74", "75+", "All ages")
)

combined_counts <- rbindlist(
  list(statewide_counts, event_counts),
  use.names = TRUE
)

combined_counts[, `:=`(
  cause_order = match(cause, cause_order),
  period_order = match(period, period_levels)
)]
setorder(combined_counts, cause_order, period_order)
combined_counts[, c("cause_order", "period_order") := NULL]

stopifnot(
  all(
    combined_counts[["All ages"]] ==
      combined_counts[["<65"]] +
      combined_counts[["65-74"]] +
      combined_counts[["75+"]]
  )
)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
fwrite(combined_counts, output_path)

cat("Wind threshold: >=", wind_threshold, " kt\n", sep = "")
cat("ZIP-events:", nrow(zip_events), "\n")
cat("Distinct impact dates:", uniqueN(zip_events$impact_date), "\n")
cat(
  "Overlapping rows already resolved in source +/-8-day calendar:",
  source_overlap_rows_resolved,
  "\n"
)
cat("Overlapping ZIP-date assignments resolved:", overlap_assignments, "\n")
cat("Deaths in any requested event period:", nrow(event_deaths), "\n")
cat("Output:", output_path, "\n\n")
print(combined_counts)
