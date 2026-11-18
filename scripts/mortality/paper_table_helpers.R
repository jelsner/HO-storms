suppressPackageStartupMessages({
  library(data.table)
  library(knitr)
})

paper_table_version <- "environmental_research_main_tables_v2_2026-11-18"
paper_table_output_dir <- file.path("outputs/mortality", paper_table_version)
paper_table_dir <- file.path(paper_table_output_dir, "tables")
paper_table_master_path <- paste0(
  "outputs/mortality/paper_master_34kt/amended_v2_2026-11-18/",
  "master_results_34kt_amended_v2.csv"
)

dir.create(paper_table_dir, recursive = TRUE, showWarnings = FALSE)

paper_period_levels <- c(
  "Days -14 to -8",
  "Days -7 to -1",
  "Days 0 to +6",
  "Days +7 to +13"
)
paper_age_levels <- c("All ages", "<65", "65-74", "75+")
paper_retained_causes <- c(
  "All diseases",
  "Cardiovascular",
  "Respiratory",
  "Infectious diseases",
  "Injuries",
  "Neuropsychiatric disorders",
  "Renal diseases",
  "Neoplasms"
)
paper_prioritized_causes <- c(
  "Cardiovascular",
  "Respiratory",
  "Injuries",
  "Neuropsychiatric disorders"
)

read_paper_table_results <- function() {
  if (!file.exists(paper_table_master_path)) {
    stop("Missing amended master results: ", paper_table_master_path)
  }
  results <- fread(paper_table_master_path)
  if (
    nrow(results) != 128L ||
      uniqueN(results$result_id) != 128L ||
      !identical(unique(results$analysis_version), "amended_v2_2026-11-18")
  ) {
    stop("The amended master results failed the 128-row/version check.")
  }
  results
}

format_probability <- function(value) {
  fcase(
    is.na(value), "—",
    value < 0.001, "<0.001",
    default = sprintf("%.3f", value)
  )
}

format_rr_ci <- function(rate_ratio, ci_lower, ci_upper) {
  sprintf("%.3f (%.3f–%.3f)", rate_ratio, ci_lower, ci_upper)
}

format_rr_ci_q <- function(rate_ratio, ci_lower, ci_upper, q_value) {
  paste0(
    format_rr_ci(rate_ratio, ci_lower, ci_upper),
    "; ",
    fcase(
      is.na(q_value), "q=—",
      q_value < 0.001, "q<0.001",
      default = sprintf("q=%.3f", q_value)
    )
  )
}

write_markdown_table <- function(
  title,
  display_data,
  notes,
  path,
  align = NULL
) {
  table_output <- capture.output(kable(
    display_data,
    format = "pipe",
    align = align,
    row.names = FALSE
  ))
  writeLines(c(
    paste0("# ", title),
    "",
    table_output,
    "",
    paste0("Notes: ", notes)
  ), path)
}

get_paper_table_source_commit <- function() {
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
