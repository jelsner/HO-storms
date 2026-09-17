suppressPackageStartupMessages({
  library(data.table)
})

source("scripts/cause_age_count_helpers.R")

input_path <- "data/all_data.Rdata"
output_path <- "outputs/mortality_counts_by_cause_age_1985_2022.csv"

analysis_start_year <- 1985L
analysis_end_year <- 2022L

env <- new.env(parent = emptyenv())
loaded_objects <- load(input_path, envir = env)

if (!"all_data" %in% loaded_objects) {
  stop("Expected an object named 'all_data' in ", input_path)
}

required_columns <- c("ID", "EVENT_YEAR", "COMPUTED_AGE", "ICD_CODE")
missing_columns <- setdiff(required_columns, names(env$all_data))
if (length(missing_columns)) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

deaths <- as.data.table(env$all_data)[, ..required_columns]
rm(env)
gc()

deaths[, `:=`(
  year = suppressWarnings(as.integer(EVENT_YEAR)),
  age = suppressWarnings(as.integer(COMPUTED_AGE)),
  code = as.character(ICD_CODE)
)]

# Florida mortality records use ICD-9 through 1998 and ICD-10 beginning in 1999.
deaths <- deaths[
  year >= analysis_start_year & year <= analysis_end_year &
    age >= 0L & age <= 120L
]

deaths[, age_group := make_age_group(age)]

cause_masks <- build_cause_masks(deaths$year, deaths$code)

counts <- rbindlist(lapply(names(cause_masks), function(cause_name) {
  mask <- cause_masks[[cause_name]]
  by_age <- table(factor(deaths$age_group[mask], levels = age_levels))

  data.table(
    cause = cause_name,
    `<65` = unname(as.integer(by_age["<65"])),
    `65-74` = unname(as.integer(by_age["65-74"])),
    `75+` = unname(as.integer(by_age["75+"]))
  )
}))

counts[, `All ages` := `<65` + `65-74` + `75+`]

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
fwrite(counts, output_path)

cat("Analysis years:", analysis_start_year, "to", analysis_end_year, "\n")
cat("Deaths with valid age (0-120):", nrow(deaths), "\n")
cat("Unique death IDs:", uniqueN(deaths$ID), "\n")
cat("Output:", output_path, "\n\n")
print(counts)
