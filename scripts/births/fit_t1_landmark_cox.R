#!/usr/bin/env Rscript

required_packages <- c("data.table", "survival")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "))
}

library(data.table)
library(survival)

corrected_file <-
  "data/births/processed/All_Births_Corrected_Storm_Exposure-trimester-eligible-v2.rds"
interval_file <- "data/births/raw/all_births_hurricane.csv"
output_dir <- "models/births/t1-landmark"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

progress_file <- file.path(output_dir, "t1-landmark-progress.log")
if (file.exists(progress_file)) file.remove(progress_file)

log_progress <- function(stage, detail) {
  line <- sprintf(
    "%s | %-18s | %s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), stage, detail
  )
  message(line)
  write(line, progress_file, append = TRUE)
  invisible(line)
}

stopifnot(file.exists(corrected_file), file.exists(interval_file))
log_progress("setup", "first-trimester landmark workflow started")

# The corrected exposure file has one row per birth. acute_direct_T1_34 equals
# one when at least one residence-specific 34-kt local impact date occurred in
# trimester 1. Unlike a symmetric window, it does not use a future impact to
# classify an earlier trimester as exposed.
births <- readRDS(corrected_file)
setDT(births)

source_columns <- c(
  "ID", "DATE_OF_BIRTH", "conception_date", "GESTATION_WEEKS",
  "PLURALITY_CODE", "county_geoid", "conception_year", "conception_month",
  "exposure_eligible_T1", "acute_direct_T1_34",
  "Mother_CalculatedRace", "Mother_CalculatedHisp", "MOTHER_EDCODE",
  "Father_CalculatedRace", "Father_CalculatedHisp", "FATHER_EDCODE",
  "MOTHER_AGE", "PrePregnancy_BMI", "MR_PREV_PRETERM",
  "MOTHER_WIC_YESNO", "PRINCIPAL_SRCPAY_CODE"
)
missing_columns <- setdiff(source_columns, names(births))
if (length(missing_columns)) {
  stop("Corrected data missing: ", paste(missing_columns, collapse = ", "))
}
births <- births[, ..source_columns]

# Use the actual start of trimester 2 as the landmark. This is the first day
# after the completed T1 exposure-assessment period.
landmarks <- fread(
  interval_file,
  select = c("ID", "trimester2_s_date"),
  colClasses = c(ID = "character")
)
landmarks[, trimester2_s_date := as.IDate(trimester2_s_date)]
stopifnot(!anyDuplicated(landmarks$ID))

births <- merge(births, landmarks, by = "ID", all.x = TRUE, sort = FALSE)
rm(landmarks)
gc()
log_progress("read", sprintf("read and linked %s births",
                              format(nrow(births), big.mark = ",")))

normalize_missing_code <- function(
    x, missing = c("", "NULL", "NA", "N/A", "U", "UNK", "UNKNOWN")) {
  value <- trimws(toupper(as.character(x)))
  value[value %in% missing] <- NA_character_
  value
}

binary_yes_no <- function(x) {
  value <- normalize_missing_code(x)
  fcase(
    value %in% c("Y", "YES", "1", "TRUE"), 1L,
    value %in% c("N", "NO", "0", "FALSE"), 0L,
    default = NA_integer_
  )
}

collapse_race <- function(x) {
  value <- suppressWarnings(as.integer(normalize_missing_code(x)))
  factor(fcase(
    value == 1L, "White",
    value == 2L, "Black",
    !is.na(value) & value != 99L, "Other",
    default = NA_character_
  ), levels = c("White", "Black", "Other"))
}

collapse_ethnicity <- function(x) {
  value <- suppressWarnings(as.integer(normalize_missing_code(x)))
  factor(fcase(
    value == 0L, "Non-Hispanic",
    value %in% 1:98, "Hispanic",
    default = NA_character_
  ), levels = c("Non-Hispanic", "Hispanic"))
}

collapse_education <- function(x) {
  value <- suppressWarnings(as.integer(normalize_missing_code(x)))
  factor(fcase(
    value %in% 1:2, "Less than high school",
    value == 3L, "High school or GED",
    value %in% 4:5, "Some college or associate degree",
    value %in% 6:8, "Bachelor degree or higher",
    default = NA_character_
  ), levels = c(
    "Less than high school", "High school or GED",
    "Some college or associate degree", "Bachelor degree or higher"
  ))
}

collapse_payment <- function(x) {
  value <- suppressWarnings(as.integer(normalize_missing_code(x)))
  factor(fcase(
    value == 1L, "Medicaid",
    value == 2L, "Private insurance",
    value == 3L, "Self-pay",
    !is.na(value) & value != 9L, "Other",
    default = NA_character_
  ), levels = c("Medicaid", "Private insurance", "Self-pay", "Other"))
}

births[, `:=`(
  DATE_OF_BIRTH = as.IDate(DATE_OF_BIRTH),
  conception_date = as.IDate(conception_date),
  gest_weeks_reported = suppressWarnings(as.numeric(GESTATION_WEEKS)),
  singleton = suppressWarnings(as.integer(PLURALITY_CODE)) == 1L,
  exposed_T1_34 = as.integer(acute_direct_T1_34),
  maternal_race = collapse_race(Mother_CalculatedRace),
  maternal_ethnicity = collapse_ethnicity(Mother_CalculatedHisp),
  maternal_education = collapse_education(MOTHER_EDCODE),
  paternal_race = collapse_race(Father_CalculatedRace),
  paternal_ethnicity = collapse_ethnicity(Father_CalculatedHisp),
  paternal_education = collapse_education(FATHER_EDCODE),
  maternal_age = suppressWarnings(as.numeric(MOTHER_AGE)),
  previous_preterm = factor(binary_yes_no(MR_PREV_PRETERM), levels = 0:1),
  wic = factor(binary_yes_no(MOTHER_WIC_YESNO), levels = 0:1),
  payment = collapse_payment(PRINCIPAL_SRCPAY_CODE),
  county_geoid = factor(county_geoid),
  conception_year_month = factor(fifelse(
    !is.na(conception_year) & conception_month %in% 1:12,
    sprintf("%04d-%02d", as.integer(conception_year),
            as.integer(conception_month)),
    NA_character_
  ))
)]

bmi_numeric <- suppressWarnings(as.numeric(normalize_missing_code(
  births$PrePregnancy_BMI
)))
births[, bmi_category := cut(
  bmi_numeric,
  breaks = c(-Inf, 18.5, 25, 30, Inf),
  right = FALSE,
  labels = c("Underweight", "Normal", "Overweight", "Obesity")
)]
rm(bmi_numeric)

covariates <- c(
  "maternal_race", "maternal_ethnicity", "maternal_education",
  "paternal_race", "paternal_ethnicity", "paternal_education",
  "maternal_age", "bmi_category", "previous_preterm", "wic", "payment"
)

births[, `:=`(
  gestational_day = as.integer(DATE_OF_BIRTH - conception_date),
  landmark_day = as.integer(trimester2_s_date - conception_date),
  county_month_stratum = interaction(
    county_geoid, conception_year_month, drop = TRUE, lex.order = TRUE
  )
)]

complete <- complete.cases(births[, c(
  covariates, "gestational_day", "landmark_day", "county_geoid",
  "conception_year_month", "county_month_stratum", "exposed_T1_34"
), with = FALSE])

analysis_births <- births[
  exposure_eligible_T1 %in% TRUE & singleton %in% TRUE & complete &
    exposed_T1_34 %in% 0:1 & gestational_day > landmark_day &
    between(gestational_day, 0L, 45L * 7L) &
    between(landmark_day, 12L * 7L, 15L * 7L)
]

analysis_births <- analysis_births[, c(
  "ID", "gestational_day", "landmark_day", "exposed_T1_34",
  "county_geoid", "conception_year_month", "county_month_stratum",
  covariates
), with = FALSE]

cohort_qa <- data.table(
  source_births = nrow(births),
  complete_case_singleton_landmark_births = nrow(analysis_births),
  exposed_births = sum(analysis_births$exposed_T1_34 == 1L),
  unexposed_births = sum(analysis_births$exposed_T1_34 == 0L),
  counties = uniqueN(analysis_births$county_geoid),
  county_month_strata = uniqueN(analysis_births$county_month_stratum),
  median_landmark_day = median(analysis_births$landmark_day),
  min_landmark_day = min(analysis_births$landmark_day),
  max_landmark_day = max(analysis_births$landmark_day)
)
fwrite(cohort_qa, file.path(output_dir, "t1-landmark-cohort-qa.csv"))
log_progress(
  "cohort",
  sprintf("retained %s complete-case singleton pregnancies; %s exposed",
          format(nrow(analysis_births), big.mark = ","),
          format(sum(analysis_births$exposed_T1_34), big.mark = ","))
)
rm(births)
gc()

formula <- as.formula(paste0(
  "Surv(landmark_day, followup_stop, event) ~ exposed_T1_34 + ",
  paste(covariates, collapse = " + "),
  " + strata(county_month_stratum) + cluster(county_geoid)"
))

outcome_plan <- data.table(
  outcome = c("PTB", "vPTB"),
  cutoff_week = c(37L, 32L)
)
results <- vector("list", nrow(outcome_plan))
event_qa <- vector("list", nrow(outcome_plan))

for (i in seq_len(nrow(outcome_plan))) {
  outcome_name <- outcome_plan$outcome[i]
  cutoff_day <- outcome_plan$cutoff_week[i] * 7L
  model_data <- copy(analysis_births)
  model_data[, `:=`(
    followup_stop = pmin(gestational_day, cutoff_day),
    event = as.integer(gestational_day < cutoff_day)
  )]
  model_data <- model_data[followup_stop > landmark_day]

  by_exposure <- model_data[, .(
    births = .N,
    events = sum(event),
    event_percent = 100 * mean(event),
    person_days = sum(followup_stop - landmark_day)
  ), by = exposed_T1_34]
  by_exposure[, outcome := outcome_name]
  event_qa[[i]] <- by_exposure

  log_progress(
    "model",
    sprintf("fitting %s model with %s births and %s events",
            outcome_name, format(nrow(model_data), big.mark = ","),
            format(sum(model_data$event), big.mark = ","))
  )
  fit <- coxph(
    formula,
    data = model_data,
    ties = "efron",
    model = FALSE,
    x = FALSE,
    y = FALSE
  )

  beta <- unname(coef(fit)["exposed_T1_34"])
  robust_se <- sqrt(vcov(fit)["exposed_T1_34", "exposed_T1_34"])
  results[[i]] <- data.table(
    outcome = outcome_name,
    cutoff_week = outcome_plan$cutoff_week[i],
    exposure = "Any corrected 34-kt local impact during T1",
    reference = "No corrected 34-kt local impact during T1",
    HR = exp(beta),
    conf_low = exp(beta - qnorm(0.975) * robust_se),
    conf_high = exp(beta + qnorm(0.975) * robust_se),
    p_value = 2 * pnorm(-abs(beta / robust_se)),
    beta = beta,
    robust_standard_error = robust_se,
    births = nrow(model_data),
    exposed_births = sum(model_data$exposed_T1_34 == 1L),
    events = sum(model_data$event),
    exposed_events = sum(
      model_data$event[model_data$exposed_T1_34 == 1L]
    )
  )
  rm(fit, model_data)
  gc()
}

estimates <- rbindlist(results)
estimates[, estimate := sprintf(
  "%.3f (%.3f, %.3f)", HR, conf_low, conf_high
)]
setcolorder(estimates, c(
  "outcome", "exposure", "reference", "HR", "conf_low", "conf_high",
  "p_value", "estimate", "cutoff_week", "births", "exposed_births",
  "events", "exposed_events", "beta", "robust_standard_error"
))

fwrite(estimates, file.path(output_dir, "t1-landmark-cox-estimates.csv"))
fwrite(rbindlist(event_qa),
       file.path(output_dir, "t1-landmark-events-by-exposure.csv"))
print(estimates)
log_progress("complete", "first-trimester landmark workflow finished")
