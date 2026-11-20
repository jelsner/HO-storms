#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(knitr)
  library(scales)
})

source("scripts/mortality/sensitivity_model_helpers.R")

permutation_version <- "strict_temporal_permutation_v1_2026-11-19"
artifact_version <- "environmental_research_strict_placebo_v1_2026-11-19"
source_dir <- file.path(
  "outputs/mortality/paper_sensitivity_34kt",
  permutation_version
)
figure_dir <- file.path("figs/mortality", artifact_version)
artifact_dir <- file.path("outputs/mortality", artifact_version)
table_dir <- file.path(artifact_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

estimate_source <- file.path(
  source_dir,
  "strict_temporal_permutation_estimates_34kt.csv"
)
summary_source <- file.path(
  source_dir,
  "strict_temporal_permutation_summary_34kt.csv"
)
diagnostic_source <- file.path(
  source_dir,
  "strict_temporal_permutation_diagnostics_34kt.csv"
)
candidate_source <- file.path(
  source_dir,
  "strict_temporal_candidate_diagnostics_34kt.csv"
)
assignment_source <- file.path(
  source_dir,
  "strict_temporal_permutation_assignments_34kt.csv"
)
model_qa_source <- file.path(
  source_dir,
  "strict_temporal_permutation_qa_34kt.csv"
)
required_sources <- c(
  estimate_source,
  summary_source,
  diagnostic_source,
  candidate_source,
  assignment_source,
  model_qa_source
)
if (any(!file.exists(required_sources))) {
  stop(
    "Missing strict temporal permutation inputs: ",
    paste(required_sources[!file.exists(required_sources)], collapse = ", ")
  )
}

permutation_estimates <- fread(estimate_source)
summary_results <- fread(summary_source)
model_qa <- fread(model_qa_source)

period_labels <- c(
  "Days -14 to -8" = "Days -14 to -8",
  "Days -7 to -1" = "Days -7 to -1",
  "Days 0 to +6" = "Days 0 to +6",
  "Days +7 to +13" = "Days +7 to +13"
)
permutation_estimates[, period_label := factor(
  period_labels[period],
  levels = unname(period_labels)
)]
summary_results[, period_label := factor(
  period_labels[period],
  levels = unname(period_labels)
)]

annotation <- summary_results[, .(
  period_label,
  label = sprintf(
    "Observed RR %.3f\nEmpirical p %.3f\nMax-T p %.3f",
    observed_rate_ratio,
    empirical_p_upper,
    empirical_p_upper_max_t
  )
)]

figure <- ggplot(permutation_estimates, aes(x = rate_ratio)) +
  geom_histogram(
    bins = 20,
    boundary = 1,
    color = "white",
    fill = "#80B1D3",
    linewidth = 0.25
  ) +
  geom_vline(
    data = summary_results,
    aes(xintercept = null_rr_median),
    color = "#4D4D4D",
    linewidth = 0.65,
    linetype = "22"
  ) +
  geom_vline(
    data = summary_results,
    aes(xintercept = observed_rate_ratio),
    color = "#D55E00",
    linewidth = 1.05
  ) +
  geom_text(
    data = annotation,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.1,
    lineheight = 1.05
  ) +
  facet_wrap(~ period_label, ncol = 2, scales = "free") +
  scale_x_continuous(labels = number_format(accuracy = 0.001)) +
  labs(
    title = "Observed all-disease rate ratios against strict temporal placebos",
    subtitle = paste0(
      "199 year-, weekday-, season-, and footprint-matched calendars with no ",
      "overlap with observed event windows"
    ),
    x = "Rate ratio",
    y = "Permutation count",
    caption = paste0(
      "Blue histograms show the strict permutation null; dashed lines mark null ",
      "medians and orange lines mark frozen primary estimates.\nEmpirical p-values ",
      "are one-sided; max-T p-values control the four event periods jointly."
    )
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.4), margin = margin(b = 4)),
    plot.subtitle = element_text(margin = margin(b = 8)),
    plot.caption = element_text(size = rel(0.8), color = "grey30", hjust = 0),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey94", color = "grey82"),
    panel.grid.minor = element_blank()
  )

figure_png <- file.path(
  figure_dir,
  "figure_s05d_strict_temporal_permutation.png"
)
figure_pdf <- file.path(
  figure_dir,
  "figure_s05d_strict_temporal_permutation.pdf"
)
ggsave(figure_png, figure, width = 9.5, height = 7.2, dpi = 400, bg = "white")
ggsave(
  figure_pdf,
  figure,
  width = 9.5,
  height = 7.2,
  device = "pdf",
  family = "Helvetica",
  bg = "white"
)

table_csv <- file.path(
  table_dir,
  "table_s08_strict_temporal_permutation_component.csv"
)
fwrite(summary_results, table_csv)

summary_display <- summary_results[, .(
  Period = period,
  `Observed RR (95% CI)` = sprintf(
    "%.3f (%.3f-%.3f)",
    observed_rate_ratio,
    observed_ci_lower,
    observed_ci_upper
  ),
  `Null median RR` = sprintf("%.3f", null_rr_median),
  `Null 95% interval` = sprintf("%.3f-%.3f", null_rr_025, null_rr_975),
  `Upper-tail p` = sprintf("%.3f", empirical_p_upper),
  `Centered two-sided p` = sprintf("%.3f", empirical_p_two_sided_centered),
  `Max-T p` = sprintf("%.3f", empirical_p_upper_max_t)
)]
global_p <- unique(summary_results$global_empirical_p_max_t)
summary_md <- file.path(
  table_dir,
  "strict_temporal_permutation_summary.md"
)
writeLines(c(
  "# Strict temporal permutation summary",
  "",
  capture.output(kable(summary_display, format = "pipe")),
  "",
  paste0(
    "Notes: Each of 199 calendars preserves the observed event year, weekday, and ",
    "ZIP footprint; keeps the complete days -14 through +13 window within May-November; ",
    "and places every placebo impact at least 28 days from every observed impact ",
    "affecting that footprint. Thus no placebo ZIP-day overlaps an observed event ",
    "window. The same all-age fixed-effects Poisson specification is fit to every ",
    "calendar. The global standardized max-T p-value is ", sprintf("%.3f", global_p), "."
  ),
  "",
  paste0(
    "Interpretation: The placebo distributions remain positively shifted, with median ",
    "RRs of 1.033-1.038, demonstrating residual temporal structure unrelated to exact ",
    "storm timing. The observed days 0 to +6 RR of 1.057 exceeds all 199 permutations ",
    "and remains significant after four-period max-T adjustment (p=0.005). Days +7 to ",
    "+13 has upper-tail p=0.035 but max-T p=0.085. Neither pre-impact period survives ",
    "max-T adjustment (days -14 to -8 p=0.745; days -7 to -1 p=0.195). Accordingly, ",
    "the strict temporal falsification strongly supports the impact-week association ",
    "but weakens a causal interpretation of the pre-impact elevations."
  )
), summary_md)

caption_path <- file.path(table_dir, "figure_s05d_caption.md")
writeLines(c(
  "# Figure S5D",
  "",
  paste0(
    "Empirical null distributions from 199 strict temporal placebo calendars for the ",
    "four all-age event periods. Randomized impact dates preserve event year, weekday, ",
    "season, and ZIP footprint, while ensuring that no placebo window overlaps any ",
    "observed event window affecting the same ZIPs. Orange lines indicate the frozen ",
    "primary rate ratios and dashed lines indicate permutation medians. The global ",
    "standardized maximum-statistic test across all four periods yields p=0.005."
  )
), caption_path)

artifact_qa <- data.table(
  check_name = c(
    "Model QA checks all pass",
    "Figure S5D PNG and PDF written",
    "Permutation table contains four periods",
    "Permutation distribution contains 796 estimates",
    "All four null distributions contain 199 estimates",
    "Global max-T p-value is unique and valid"
  ),
  status = c(
    if (all(model_qa$status == "PASS")) "PASS" else "FAIL",
    if (all(file.exists(c(figure_png, figure_pdf)))) "PASS" else "FAIL",
    if (nrow(summary_results) == 4L) "PASS" else "FAIL",
    if (nrow(permutation_estimates) == 796L) "PASS" else "FAIL",
    if (all(permutation_estimates[, .N, by = period]$N == 199L))
      "PASS" else "FAIL",
    if (length(global_p) == 1L && global_p > 0 && global_p <= 1)
      "PASS" else "FAIL"
  ),
  observed = c(
    sum(model_qa$status == "PASS"),
    sum(file.exists(c(figure_png, figure_pdf))),
    nrow(summary_results),
    nrow(permutation_estimates),
    paste(unique(permutation_estimates[, .N, by = period]$N), collapse = ","),
    paste(global_p, collapse = ",")
  ),
  expected = c(nrow(model_qa), 2L, 4L, 796L, 199L, "(0,1]")
)
artifact_qa_path <- file.path(
  artifact_dir,
  "strict_temporal_permutation_artifact_qa.csv"
)
fwrite(artifact_qa, artifact_qa_path)
if (any(artifact_qa$status != "PASS")) {
  print(artifact_qa)
  stop("One or more strict temporal permutation artifact checks failed.")
}

manifest_targets <- c(
  figure_png,
  figure_pdf,
  table_csv,
  summary_md,
  caption_path,
  artifact_qa_path,
  required_sources,
  "scripts/mortality/run_strict_temporal_permutation_34kt.R",
  "scripts/mortality/build_strict_temporal_permutation_artifacts.R",
  "scripts/mortality/sensitivity_model_helpers.R"
)
hash_output <- system2("shasum", c("-a", "256", manifest_targets), stdout = TRUE)
manifest_split <- tstrsplit(hash_output, "  ", fixed = TRUE)
manifest <- data.table(
  sha256 = manifest_split[[1L]],
  path = manifest_split[[2L]],
  bytes = as.numeric(file.info(manifest_split[[2L]])$size),
  artifact_version = artifact_version,
  source_commit = get_sensitivity_source_commit(),
  build_date = as.IDate("2026-11-19")
)
manifest_path <- file.path(
  artifact_dir,
  "strict_temporal_permutation_manifest.csv"
)
fwrite(manifest, manifest_path)

cat("\nStrict temporal permutation artifacts completed\n")
cat("Figure S5D:", figure_png, "\n")
cat("Table S8 component:", table_csv, "\n")
cat("Global max-T empirical p:", global_p, "\n")
cat("Artifact QA checks:", nrow(artifact_qa), "all PASS\n")
