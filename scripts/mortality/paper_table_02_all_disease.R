#!/usr/bin/env Rscript

if (!exists("paper_table_dir")) {
  source("scripts/mortality/paper_table_helpers.R")
}

message("Building Table 2: all-disease mortality by age and period")
results <- read_paper_table_results()
table2 <- results[cause == "All diseases", .(
  age_order,
  period_order,
  age_group,
  period,
  event_deaths,
  event_zip_days,
  rate_ratio,
  ci_lower,
  ci_upper,
  percent_change,
  p_value,
  primary_fdr_q_16 = main_fdr_q_within_analysis_tier,
  fdr_supported = main_fdr_q_within_analysis_tier < 0.05,
  result_id,
  artifact_version = paper_table_version
)]
setorder(table2, age_order, period_order)

csv_path <- file.path(paper_table_dir, "table_02_all_disease_timing.csv")
markdown_path <- file.path(paper_table_dir, "table_02_all_disease_timing.md")
fwrite(table2, csv_path)

display <- table2[, .(
  Age = age_group,
  Period = period,
  `Event deaths` = formatC(event_deaths, format = "f", digits = 0L, big.mark = ","),
  `RR (95% CI)` = format_rr_ci(rate_ratio, ci_lower, ci_upper),
  `% change` = sprintf("%+.1f", percent_change),
  `p-value` = format_probability(p_value),
  `Primary q` = format_probability(primary_fdr_q_16)
)]

write_markdown_table(
  "Table 2. All-disease mortality by age and event-relative period",
  display,
  paste0(
    "Rate ratios are from fixed-effects Poisson models with ZIP, ISO year-week, day-of-week, ",
    "and ZIP-by-COVID-period fixed effects and two-way cluster-robust standard errors by ZIP ",
    "and ISO year-week. The reference comprises retained ZIP-days outside days −14 through +13. ",
    "Primary q-values use Benjamini-Hochberg correction across all 16 all-disease tests."
  ),
  markdown_path,
  align = c("l", "l", "r", "c", "r", "r", "r")
)

