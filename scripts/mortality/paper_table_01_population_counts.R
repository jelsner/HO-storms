#!/usr/bin/env Rscript

if (!exists("paper_table_dir")) {
  source("scripts/mortality/paper_table_helpers.R")
}

daily_panel_path <- "data/mortality/processed/death-panels/daily_panel_34kt.csv"
count_path <- "outputs/mortality/mortality_counts_by_cause_age_1985_2022.csv"
sample_count_path <- paste0(
  "outputs/mortality/cause_age_event_period_models/",
  "all_diseases_age_event_period_sample_counts_34kt.csv"
)

required_paths <- c(daily_panel_path, count_path, sample_count_path)
if (any(!file.exists(required_paths))) {
  stop("Missing Table 1 inputs: ", paste(required_paths[!file.exists(required_paths)], collapse = ", "))
}

message("Building Table 1: study population, exposure, and mortality counts")
panel <- fread(daily_panel_path, select = c("zip", "date", "rel_day"))
panel[, `:=`(
  zip = as.character(zip),
  date = as.IDate(date),
  rel_day = suppressWarnings(as.integer(rel_day))
)]

zip_impacts <- unique(panel[!is.na(rel_day), .(
  zip,
  impact_date = date - rel_day
)])
event_calendar_raw <- zip_impacts[, .(
  rel_day = -14L:13L
), by = .(zip, impact_date)]
event_calendar_raw[, date := impact_date + rel_day]
overlap_counts <- event_calendar_raw[, .N, by = .(zip, date)][N > 1L]
overlap_assignments <- if (nrow(overlap_counts)) {
  sum(overlap_counts$N - 1L)
} else {
  0L
}

sample_counts <- fread(sample_count_path)
sample_counts <- sample_counts[group == "All ages"]
if (nrow(sample_counts) != 5L) {
  stop("Expected five all-age model-sample count rows.")
}

event_sample <- sample_counts[event_period != "None"]
reference_sample <- sample_counts[event_period == "None"]
study_years <- sort(unique(as.integer(format(as.Date(panel$date), "%Y"))))

panel_a <- data.table(
  metric_order = 1:12,
  characteristic = c(
    "Observation period",
    "Calendar years",
    "Florida ZIP codes",
    "ZIP codes with at least one ≥34-kt impact",
    "ZIP-specific impact events",
    "Distinct calendar impact dates",
    "Modeled ZIP-days",
    "Reference ZIP-days",
    "Event-window ZIP-days",
    "Deaths on modeled ZIP-days",
    "Deaths on reference ZIP-days",
    "Deaths in event windows"
  ),
  value = c(
    paste0(min(study_years), "–", max(study_years)),
    as.character(length(study_years)),
    as.character(uniqueN(panel$zip)),
    as.character(uniqueN(zip_impacts$zip)),
    as.character(nrow(zip_impacts)),
    as.character(uniqueN(zip_impacts$impact_date)),
    as.character(sum(sample_counts$zip_days)),
    as.character(reference_sample$zip_days),
    as.character(sum(event_sample$zip_days)),
    as.character(sum(sample_counts$deaths)),
    as.character(reference_sample$deaths),
    as.character(sum(event_sample$deaths))
  ),
  numeric_value = c(
    NA_real_,
    length(study_years),
    uniqueN(panel$zip),
    uniqueN(zip_impacts$zip),
    nrow(zip_impacts),
    uniqueN(zip_impacts$impact_date),
    sum(sample_counts$zip_days),
    reference_sample$zip_days,
    sum(event_sample$zip_days),
    sum(sample_counts$deaths),
    reference_sample$deaths,
    sum(event_sample$deaths)
  )
)
panel_a[, `:=`(
  wind_threshold_kt = 34L,
  overlap_assignments_resolved = overlap_assignments,
  artifact_version = paper_table_version
)]

panel_b <- fread(count_path)
panel_b <- panel_b[cause %chin% paper_retained_causes]
setnames(
  panel_b,
  c("<65", "65-74", "75+", "All ages"),
  c("deaths_under65", "deaths_65_74", "deaths_75plus", "deaths_all_ages")
)
panel_b[, cause_order := match(cause, paper_retained_causes)]
panel_b[, artifact_version := paper_table_version]
setorder(panel_b, cause_order)

panel_a_path <- file.path(
  paper_table_dir,
  "table_01a_study_population_exposure.csv"
)
panel_b_path <- file.path(
  paper_table_dir,
  "table_01b_mortality_counts.csv"
)
markdown_path <- file.path(
  paper_table_dir,
  "table_01_study_population_exposure_mortality.md"
)

fwrite(panel_a, panel_a_path)
fwrite(panel_b, panel_b_path)

panel_a_display <- panel_a[, .(
  Characteristic = characteristic,
  Value = fifelse(
    is.na(numeric_value),
    value,
    formatC(numeric_value, format = "f", digits = 0L, big.mark = ",")
  )
)]

panel_b_display <- panel_b[, .(
  Outcome = cause,
  `All ages` = formatC(deaths_all_ages, format = "f", digits = 0L, big.mark = ","),
  `<65` = formatC(deaths_under65, format = "f", digits = 0L, big.mark = ","),
  `65–74` = formatC(deaths_65_74, format = "f", digits = 0L, big.mark = ","),
  `75+` = formatC(deaths_75plus, format = "f", digits = 0L, big.mark = ",")
)]

writeLines(c(
  "# Table 1. Study population, exposure, and mortality counts",
  "",
  "## A. Study population and exposure",
  "",
  capture.output(kable(panel_a_display, format = "pipe", align = c("l", "r"))),
  "",
  "## B. Mortality counts, 1985–2022",
  "",
  capture.output(kable(panel_b_display, format = "pipe", align = c("l", "r", "r", "r", "r"))),
  "",
  paste0(
    "Notes: The modeled sample includes May–November ZIP-days plus dates required to complete ",
    "event windows through day +13. Event windows cover days −14 through +13 relative to each ",
    "ZIP-specific impact date; all other retained ZIP-days are the reference. Ages were restricted ",
    "to valid recorded values from 0 through 120 years. The three age groups are mutually exclusive. ",
    "Cause categories are based on underlying cause and are not necessarily mutually exclusive; ",
    "counts therefore must not be summed across outcomes. Overlapping event windows were resolved ",
    "by closest impact date, with exact-distance ties assigned to the pre-impact event (",
    formatC(overlap_assignments, format = "f", digits = 0L, big.mark = ","),
    " competing ZIP-date assignments resolved)."
  )
), markdown_path)

rm(panel, zip_impacts, event_calendar_raw, overlap_counts)
gc()

