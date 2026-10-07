suppressPackageStartupMessages(library(data.table))

source_path <- "data/births/processed/All_Births_Corrected_Storm_Exposure.rds"
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

cat("cohort", nrow(cohort), "\n")
cat("exposed", sum(cohort$any_storm_effect), "\n")
cat("unexposed", sum(cohort$any_storm_effect == 0L), "\n")

variables <- c(
  "Mother_CalculatedRace", "Mother_CalculatedHisp", "MOTHER_EDCODE",
  "MOTHER_MARRIED", "ALCOHOL_USE", "MOTHER_WIC_YESNO",
  "MR_PREV_PRETERM", "PRINCIPAL_SRCPAY_CODE", "PRINCIPAL_SOURCE_PAY",
  "Father_CalculatedRace", "Father_CalculatedHisp", "FATHER_EDCODE"
)

dir.create("outputs/births/revised_table1", recursive = TRUE, showWarnings = FALSE)
for (variable in variables) {
  frequency <- cohort[, .N, by = c(variable, "any_storm_effect")]
  setorderv(frequency, c(variable, "any_storm_effect"), na.last = TRUE)
  fwrite(frequency, file.path("outputs/births/revised_table1", paste0(variable, "_codes.csv")))
}
