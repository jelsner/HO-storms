suppressPackageStartupMessages(library(data.table))

source_path <- "data/births/processed/All_Births_Corrected_Storm_Exposure.rds"
output_dir <- "outputs/births/revised_table1"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

normalize_code <- function(x) {
  value <- trimws(toupper(as.character(x)))
  value[value %in% c("", "NULL", "NA", "N/A", "U", "UNK", "UNKNOWN")] <- NA_character_
  value
}

integer_code <- function(x) suppressWarnings(as.integer(normalize_code(x)))

race_category <- function(x) {
  code <- integer_code(x)
  fcase(
    code == 1L, "White",
    code == 2L, "Black",
    code %in% c(4L, 5L, 7L, 8L, 10L, 11L, 12L), "Asian",
    code %in% c(3L, 6L, 9L, 13L, 14L, 15L), "Other",
    code == 20L, "More than One Race",
    default = "Missing"
  )
}

ethnicity_category <- function(x) {
  code <- integer_code(x)
  fcase(
    code == 0L, "Non-Hispanic",
    code == 1L, "Mexican",
    code == 2L, "Puerto Rican",
    code == 3L, "Cuban",
    code == 5L, "Other",
    code == 6L, "Haitian",
    default = "Missing"
  )
}

education_category <- function(x) {
  code <- integer_code(x)
  fcase(
    code == 1L, "8th Grade or Less",
    code == 2L, "9th Thru 12th Grade; No Diploma",
    code == 3L, "High School Graduate or GED",
    code == 4L, "Some College",
    code == 5L, "Associate Degree",
    code == 6L, "Bachelor's Degree",
    code == 7L, "Master's Degree",
    code == 8L, "Doctorate Degree",
    default = "Missing"
  )
}

yes_no_category <- function(x) {
  code <- normalize_code(x)
  fcase(code == "N", "No", code == "Y", "Yes", default = "Missing")
}

payment_category <- function(x) {
  code <- integer_code(x)
  fcase(
    code == 1L, "Medicaid",
    code == 2L, "Private Insurance",
    code == 3L, "Self-Pay",
    code == 8L, "Other",
    default = "Missing"
  )
}

bmi_category <- function(x) {
  value <- suppressWarnings(as.numeric(normalize_code(x)))
  fcase(
    !is.na(value) & value < 18.5, "Underweight",
    !is.na(value) & value < 25, "Normal Weight",
    !is.na(value) & value < 30, "Overweight",
    !is.na(value), "Obese",
    default = "Missing"
  )
}

format_n_pct <- function(n, denominator) {
  sprintf("%s (%.1f%%)", format(n, big.mark = ",", scientific = FALSE), 100 * n / denominator)
}

format_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("< 0.001")
  sprintf("%.3f", p)
}

categorical_p <- function(category, exposed) {
  result <- suppressWarnings(chisq.test(table(category, exposed), correct = FALSE))
  result$p.value
}

x <- readRDS(source_path)
setDT(x)

cohort <- x[
  exposure_eligible %in% TRUE &
    suppressWarnings(as.integer(PLURALITY_CODE)) == 1L &
    !is.na(suppressWarnings(as.numeric(GESTATION_WEEKS))) &
    !is.na(suppressWarnings(as.numeric(BIRTH_WEIGHT_GRAMS))) &
    !is.na(suppressWarnings(as.integer(birth_year))) &
    !is.na(conception_season) & !is.na(county_geoid)
]

cohort[, any_storm_effect := as.integer(
  exposed_window_T1_34 == 1L |
    exposed_window_T2_34 == 1L |
    exposed_window_T3_34 == 1L
)]

cohort[, `:=`(
  maternal_race_table1 = race_category(Mother_CalculatedRace),
  maternal_ethnicity_table1 = ethnicity_category(Mother_CalculatedHisp),
  maternal_education_table1 = education_category(MOTHER_EDCODE),
  marital_table1 = yes_no_category(MOTHER_MARRIED),
  alcohol_table1 = yes_no_category(ALCOHOL_USE),
  wic_table1 = yes_no_category(MOTHER_WIC_YESNO),
  previous_preterm_table1 = yes_no_category(MR_PREV_PRETERM),
  bmi_table1 = bmi_category(PrePregnancy_BMI),
  payment_table1 = payment_category(PRINCIPAL_SRCPAY_CODE),
  paternal_education_table1 = education_category(FATHER_EDCODE),
  paternal_race_table1 = race_category(Father_CalculatedRace),
  paternal_ethnicity_table1 = ethnicity_category(Father_CalculatedHisp),
  maternal_age_table1 = suppressWarnings(as.numeric(MOTHER_AGE))
)]

overall_n <- nrow(cohort)
exposed_n <- cohort[any_storm_effect == 1L, .N]
unexposed_n <- cohort[any_storm_effect == 0L, .N]
stopifnot(overall_n == exposed_n + unexposed_n, overall_n == 4484317L)

rows <- list()
add_row <- function(label, overall = "", exposed = "", unexposed = "", p = "", row_type = "category") {
  rows[[length(rows) + 1L]] <<- data.table(
    label = label, overall = overall, exposed = exposed,
    unexposed = unexposed, p_value = p, row_type = row_type
  )
}

age_p <- t.test(maternal_age_table1 ~ any_storm_effect, data = cohort)$p.value
age_stats <- function(data) sprintf("%.1f (%.2f)", mean(data, na.rm = TRUE), sd(data, na.rm = TRUE))
add_row(
  "Mother's Age", age_stats(cohort$maternal_age_table1),
  age_stats(cohort[any_storm_effect == 1L]$maternal_age_table1),
  age_stats(cohort[any_storm_effect == 0L]$maternal_age_table1),
  format_p(age_p), "continuous"
)

add_section <- function(label, variable, levels) {
  p <- format_p(categorical_p(cohort[[variable]], cohort$any_storm_effect))
  add_row(label, p = p, row_type = "section")
  for (level in levels) {
    overall_count <- cohort[get(variable) == level, .N]
    exposed_count <- cohort[get(variable) == level & any_storm_effect == 1L, .N]
    unexposed_count <- cohort[get(variable) == level & any_storm_effect == 0L, .N]
    add_row(
      paste0("  ", level),
      format_n_pct(overall_count, overall_n),
      format_n_pct(exposed_count, exposed_n),
      format_n_pct(unexposed_count, unexposed_n),
      p, "category"
    )
  }
}

add_section("Mother's Race", "maternal_race_table1",
            c("White", "Black", "Asian", "Other", "More than One Race", "Missing"))
add_section("Mother's Ethnicity", "maternal_ethnicity_table1",
            c("Non-Hispanic", "Mexican", "Puerto Rican", "Cuban", "Other", "Haitian", "Missing"))
add_section("Mother's Education", "maternal_education_table1",
            c("8th Grade or Less", "9th Thru 12th Grade; No Diploma",
              "High School Graduate or GED", "Some College", "Associate Degree",
              "Bachelor's Degree", "Master's Degree", "Doctorate Degree", "Missing"))
add_section("Marital Status", "marital_table1", c("No", "Yes", "Missing"))
add_section("Alcohol Use", "alcohol_table1", c("Yes", "No", "Missing"))
add_section("WIC", "wic_table1", c("No", "Yes", "Missing"))
add_section("Previous Preterm Infant", "previous_preterm_table1", c("No", "Yes", "Missing"))
add_section("Pre-Pregnancy BMI", "bmi_table1",
            c("Underweight", "Normal Weight", "Overweight", "Obese", "Missing"))
add_section("Principal Source of Payment", "payment_table1",
            c("Medicaid", "Private Insurance", "Self-Pay", "Other", "Missing"))
add_section("Father's Education", "paternal_education_table1",
            c("8th Grade or Less", "9th Thru 12th Grade; No Diploma",
              "High School Graduate or GED", "Some College", "Associate Degree",
              "Bachelor's Degree", "Master's Degree", "Doctorate Degree", "Missing"))
add_section("Father's Race", "paternal_race_table1",
            c("White", "Black", "Asian", "Other", "More than One Race", "Missing"))
add_section("Father's Ethnicity", "paternal_ethnicity_table1",
            c("Non-Hispanic", "Mexican", "Puerto Rican", "Cuban", "Other", "Haitian", "Missing"))

table_rows <- rbindlist(rows)
stopifnot(nrow(table_rows) == 79L)

fwrite(table_rows, file.path(output_dir, "revised_table1_values.csv"))
fwrite(
  data.table(
    cohort = "Revised model-eligible singleton births with complete gestational age and birthweight",
    overall_n = overall_n,
    any_storm_effect_n = exposed_n,
    no_storm_effect_n = unexposed_n,
    any_storm_effect_pct = 100 * exposed_n / overall_n,
    exposure_definition = "Any corrected 34-kt residence-specific +/-7-day exposure window overlapping T1, T2, or T3"
  ),
  file.path(output_dir, "revised_table1_qa.csv")
)

cat(sprintf("overall=%s exposed=%s unexposed=%s rows=%s\n",
            format(overall_n, big.mark = ","),
            format(exposed_n, big.mark = ","),
            format(unexposed_n, big.mark = ","), nrow(table_rows)))
