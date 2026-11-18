#!/usr/bin/env Rscript

if (!exists("paper_table_dir")) {
  source("scripts/mortality/paper_table_helpers.R")
}

message("Building Table 3: prioritized causes and age heterogeneity")
results <- read_paper_table_results()
priority <- results[
  cause %chin% paper_prioritized_causes,
  .(
    cause_order,
    period_order,
    cause,
    period,
    age_group,
    rate_ratio,
    ci_lower,
    ci_upper,
    conservative_q_112 = main_fdr_q_all_cause_specific,
    age_heterogeneity_chisq,
    age_heterogeneity_df,
    age_heterogeneity_p_value,
    age_heterogeneity_q_28 = age_heterogeneity_fdr_q_cause_specific
  )
]

estimate_wide <- dcast(
  priority,
  cause_order + period_order + cause + period ~ age_group,
  value.var = c("rate_ratio", "ci_lower", "ci_upper", "conservative_q_112")
)
setnames(
  estimate_wide,
  old = c(
    "rate_ratio_All ages", "rate_ratio_<65", "rate_ratio_65-74", "rate_ratio_75+",
    "ci_lower_All ages", "ci_lower_<65", "ci_lower_65-74", "ci_lower_75+",
    "ci_upper_All ages", "ci_upper_<65", "ci_upper_65-74", "ci_upper_75+",
    "conservative_q_112_All ages", "conservative_q_112_<65",
    "conservative_q_112_65-74", "conservative_q_112_75+"
  ),
  new = c(
    "rr_all_ages", "rr_under65", "rr_65_74", "rr_75plus",
    "ci_lower_all_ages", "ci_lower_under65", "ci_lower_65_74", "ci_lower_75plus",
    "ci_upper_all_ages", "ci_upper_under65", "ci_upper_65_74", "ci_upper_75plus",
    "q112_all_ages", "q112_under65", "q112_65_74", "q112_75plus"
  )
)

heterogeneity <- unique(priority[, .(
  cause_order,
  period_order,
  cause,
  period,
  age_heterogeneity_chisq,
  age_heterogeneity_df,
  age_heterogeneity_p_value,
  age_heterogeneity_q_28
)])
if (nrow(heterogeneity) != 16L) {
  stop("Expected 16 unique prioritized cause-period heterogeneity tests.")
}

table3 <- merge(
  estimate_wide,
  heterogeneity,
  by = c("cause_order", "period_order", "cause", "period"),
  all.x = TRUE,
  sort = FALSE
)
table3[, `:=`(
  age_heterogeneity_fdr_supported = age_heterogeneity_q_28 < 0.05,
  artifact_version = paper_table_version
)]
setorder(table3, cause_order, period_order)

csv_path <- file.path(paper_table_dir, "table_03_prioritized_causes_age.csv")
markdown_path <- file.path(paper_table_dir, "table_03_prioritized_causes_age.md")
fwrite(table3, csv_path)

display <- table3[, .(
  Cause = cause,
  Period = period,
  `All ages, RR (95% CI); q` = format_rr_ci_q(
    rr_all_ages, ci_lower_all_ages, ci_upper_all_ages, q112_all_ages
  ),
  `<65, RR (95% CI); q` = format_rr_ci_q(
    rr_under65, ci_lower_under65, ci_upper_under65, q112_under65
  ),
  `65–74, RR (95% CI); q` = format_rr_ci_q(
    rr_65_74, ci_lower_65_74, ci_upper_65_74, q112_65_74
  ),
  `75+, RR (95% CI); q` = format_rr_ci_q(
    rr_75plus, ci_lower_75plus, ci_upper_75plus, q112_75plus
  ),
  `Age heterogeneity χ² (df)` = sprintf(
    "%.2f (%d)", age_heterogeneity_chisq, age_heterogeneity_df
  ),
  `Age p` = format_probability(age_heterogeneity_p_value),
  `Age q` = format_probability(age_heterogeneity_q_28)
)]

write_markdown_table(
  "Table 3. Prioritized cause-specific mortality and age heterogeneity",
  display,
  paste0(
    "Each age-specific cell gives the rate ratio (95% CI) and conservative Benjamini-Hochberg ",
    "q-value across all 112 retained cause-specific tests. Age heterogeneity is the two-degree-of-",
    "freedom Wald test of equality among the three mutually exclusive age-specific coefficients ",
    "from the jointly fitted stacked-age model; its q-value is adjusted across the 28 retained ",
    "cause-specific cause-period tests. All-age estimates are not included in the heterogeneity test."
  ),
  markdown_path,
  align = c("l", "l", "c", "c", "c", "c", "c", "r", "r")
)

