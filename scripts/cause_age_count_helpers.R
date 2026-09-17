age_levels <- c("<65", "65-74", "75+")

make_age_group <- function(age) {
  cut(
    age,
    breaks = c(-Inf, 64L, 74L, Inf),
    labels = age_levels,
    right = TRUE
  )
}

build_cause_masks <- function(year, code) {
  code <- toupper(gsub("[^A-Z0-9]", "", trimws(as.character(code))))

  is_icd9 <- year <= 1998L
  is_icd10 <- year >= 1999L

  icd9_numeric <- suppressWarnings(as.integer(substr(code, 1L, 3L)))
  icd9_external_numeric <- suppressWarnings(as.integer(substr(code, 2L, 4L)))
  icd10_letter <- substr(code, 1L, 1L)
  icd10_number <- suppressWarnings(as.integer(substr(code, 2L, 3L)))

  icd9_between <- function(lower, upper) {
    is_icd9 & !is.na(icd9_numeric) &
      icd9_numeric >= lower & icd9_numeric <= upper
  }

  icd10_between <- function(letter, lower, upper) {
    is_icd10 & icd10_letter == letter & !is.na(icd10_number) &
      icd10_number >= lower & icd10_number <= upper
  }

  list(
    "All diseases" = rep(TRUE, length(year)),
    "Cardiovascular" =
      (is_icd9 & (icd9_numeric == 362L |
        (!is.na(icd9_numeric) & icd9_numeric >= 390L & icd9_numeric <= 459L))) |
      icd10_between("I", 0L, 99L) |
      icd10_between("G", 45L, 46L),
    "Respiratory" =
      (is_icd9 & (icd9_numeric == 34L |
        (!is.na(icd9_numeric) & icd9_numeric >= 460L & icd9_numeric <= 519L))) |
      icd10_between("J", 0L, 99L),
    "Infectious diseases" =
      icd9_between(1L, 139L) |
      icd10_between("A", 0L, 99L) |
      icd10_between("B", 0L, 99L),
    "Injuries" =
      (is_icd9 & substr(code, 1L, 1L) == "E" &
        !is.na(icd9_external_numeric) &
        icd9_external_numeric >= 0L & icd9_external_numeric <= 999L) |
      icd10_between("V", 0L, 99L) |
      icd10_between("W", 0L, 99L) |
      icd10_between("X", 0L, 99L) |
      icd10_between("Y", 0L, 98L),
    "Neuropsychiatric disorders" =
      icd9_between(290L, 389L) |
      (is_icd9 & icd9_numeric == 781L) |
      icd10_between("F", 0L, 99L) |
      icd10_between("R", 40L, 46L) |
      icd10_between("G", 0L, 99L),
    "Renal diseases" =
      icd9_between(580L, 593L) |
      icd10_between("N", 0L, 19L),
    "Digestive diseases" =
      icd9_between(520L, 579L) |
      icd10_between("K", 0L, 93L),
    "Diabetes" =
      (is_icd9 & icd9_numeric == 250L) |
      icd10_between("E", 10L, 14L),
    "Endocrine, nutritional, and metabolic disorders" =
      icd9_between(240L, 279L) |
      icd10_between("E", 0L, 90L),
    "Neoplasms" =
      icd9_between(140L, 239L) |
      icd10_between("C", 0L, 99L) |
      icd10_between("D", 0L, 49L)
  )
}

count_causes_by_age <- function(cause_masks, age_group, extra_columns = list()) {
  data.table::rbindlist(lapply(names(cause_masks), function(cause_name) {
    mask <- cause_masks[[cause_name]]
    by_age <- table(factor(age_group[mask], levels = age_levels))

    data.table::as.data.table(c(
      list(cause = cause_name),
      extra_columns,
      list(
        `<65` = unname(as.integer(by_age["<65"])),
        `65-74` = unname(as.integer(by_age["65-74"])),
        `75+` = unname(as.integer(by_age["75+"]))
      )
    ))
  }))
}
