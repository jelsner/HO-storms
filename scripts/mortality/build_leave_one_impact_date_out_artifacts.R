#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(knitr)
  library(scales)
})

source("scripts/mortality/sensitivity_model_helpers.R")

artifact_version <- "environmental_research_event_influence_v1_2026-11-19"
influence_version <- "event_influence_v1_2026-11-19"
influence_dir <- file.path(
  "outputs/mortality/paper_sensitivity_34kt",
  influence_version,
  "leave_one_impact_date_out_34kt"
)
figure_dir <- file.path("figs/mortality", artifact_version)
artifact_dir <- file.path("outputs/mortality", artifact_version)
table_dir <- file.path(artifact_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

estimate_source <- file.path(
  influence_dir,
  "leave_one_impact_date_out_estimates_34kt.csv"
)
summary_source <- file.path(
  influence_dir,
  "leave_one_impact_date_out_summary_34kt.csv"
)
diagnostic_source <- file.path(
  influence_dir,
  "leave_one_impact_date_out_diagnostics_34kt.csv"
)
date_source <- file.path(
  influence_dir,
  "leave_one_impact_date_out_dates_34kt.csv"
)
model_estimate_source <- file.path(
  influence_dir,
  "leave_one_impact_date_out_model_estimates_34kt.csv"
)
model_qa_source <- file.path(
  influence_dir,
  "leave_one_impact_date_out_qa_34kt.csv"
)
required_sources <- c(
  estimate_source,
  summary_source,
  diagnostic_source,
  date_source,
  model_estimate_source,
  model_qa_source
)
if (any(!file.exists(required_sources))) {
  stop(
    "Missing leave-one-impact-date-out inputs: ",
    paste(required_sources[!file.exists(required_sources)], collapse = ", ")
  )
}

estimates <- fread(estimate_source)
summary_results <- fread(summary_source)
diagnostics <- fread(diagnostic_source)
model_qa <- fread(model_qa_source)

age_labels <- c(
  "All ages" = "All ages",
  "<65" = "Younger than 65",
  "65-74" = "Ages 65-74",
  "75+" = "Ages 75+"
)
period_labels <- c(
  "Days -14 to -8",
  "Days -7 to -1",
  "Days 0 to +6",
  "Days +7 to +13"
)

estimates[, `:=`(
  age_label = factor(age_labels[age_group], levels = unname(age_labels)),
  period_label = factor(period, levels = rev(period_labels))
)]
summary_results[, `:=`(
  age_label = factor(age_labels[age_group], levels = unname(age_labels)),
  period_label = factor(period, levels = rev(period_labels)),
  reversal_label = fifelse(direction_reversal_count > 0L, "Direction reversal", "No reversal")
)]

figure_s4 <- ggplot() +
  geom_vline(
    xintercept = 1,
    color = "grey40",
    linewidth = 0.45,
    linetype = "22"
  ) +
  geom_segment(
    data = summary_results,
    aes(
      x = full_ci_lower,
      xend = full_ci_upper,
      y = period_label,
      yend = period_label
    ),
    color = "grey82",
    linewidth = 5,
    lineend = "round"
  ) +
  geom_point(
    data = estimates,
    aes(x = jackknife_rate_ratio, y = period_label),
    color = "#56B4E9",
    alpha = 0.18,
    size = 1.15,
    position = position_jitter(height = 0.08, width = 0, seed = 20261119)
  ) +
  geom_segment(
    data = summary_results,
    aes(
      x = jackknife_min_rate_ratio,
      xend = jackknife_max_rate_ratio,
      y = period_label,
      yend = period_label,
      color = reversal_label
    ),
    linewidth = 1.2,
    lineend = "round"
  ) +
  geom_point(
    data = summary_results,
    aes(x = full_rate_ratio, y = period_label),
    shape = 23,
    size = 2.8,
    fill = "white",
    color = "black",
    stroke = 0.75
  ) +
  facet_wrap(~ age_label, ncol = 2) +
  scale_color_manual(
    values = c("No reversal" = "#0072B2", "Direction reversal" = "#D55E00"),
    name = NULL,
    drop = FALSE
  ) +
  scale_x_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "All-disease estimates after leaving out each impact date",
    subtitle = "Seventy-seven exact deletion refits for each age-specific four-period model",
    x = "Rate ratio",
    y = NULL,
    caption = paste0(
      "Blue ranges and points show leave-one-impact-date-out point estimates; diamonds show ",
      "the full-sample estimate.\nThick gray bars are the frozen full-sample 95% confidence ",
      "intervals. No jackknife confidence intervals were estimated."
    )
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.4), margin = margin(b = 4)),
    plot.subtitle = element_text(margin = margin(b = 8)),
    plot.caption = element_text(size = rel(0.78), color = "grey30", hjust = 0),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey94", color = "grey82"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

figure_png <- file.path(figure_dir, "figure_s04_leave_one_impact_date_out.png")
figure_pdf <- file.path(figure_dir, "figure_s04_leave_one_impact_date_out.pdf")
ggsave(figure_png, figure_s4, width = 10.5, height = 7.5, dpi = 400, bg = "white")
ggsave(
  figure_pdf,
  figure_s4,
  width = 10.5,
  height = 7.5,
  device = "pdf",
  family = "Helvetica",
  bg = "white"
)

table_csv <- file.path(table_dir, "table_s07_leave_one_impact_date_out.csv")
fwrite(estimates[, !c("age_label", "period_label")], table_csv)

summary_display <- summary_results[, .(
  `Age group` = age_labels[age_group],
  Period = period,
  `Full RR (95% CI)` = sprintf(
    "%.3f (%.3f-%.3f)",
    full_rate_ratio,
    full_ci_lower,
    full_ci_upper
  ),
  `Deletion RR range` = sprintf(
    "%.3f-%.3f",
    jackknife_min_rate_ratio,
    jackknife_max_rate_ratio
  ),
  `Most influential date` = format(
    as.Date(most_influential_impact_date),
    "%Y-%m-%d"
  ),
  `Maximum |delta log(RR)|` = sprintf(
    "%.4f",
    maximum_absolute_delta_log_rate_ratio
  ),
  `Direction reversals` = direction_reversal_count
)]
summary_md <- file.path(table_dir, "table_s07_leave_one_impact_date_out_summary.md")
writeLines(c(
  "# Table S7. Leave-one-impact-date-out analysis",
  "",
  capture.output(kable(
    summary_display,
    format = "pipe",
    align = c("l", "l", "l", "l", "l", "r", "r")
  )),
  "",
  paste0(
    "Notes: Each of the 77 distinct 34-kt impact dates was omitted in turn from the ",
    "assigned event-window observations, and the four-period fixed-effects Poisson point ",
    "estimates were refit. The complete machine-readable table contains 1,232 estimates ",
    "(77 dates x 16 all-disease estimands). The eight spatial targets were excluded from ",
    "this sensitivity analysis under protocol amendment A02."
  )
), summary_md)

caption_path <- file.path(table_dir, "figure_s04_caption.md")
writeLines(c(
  "# Figure S4",
  "",
  paste0(
    "Leave-one-impact-date-out ranges for the 16 all-disease age-period estimates. Each ",
    "blue point represents an exact point-estimate refit after omitting all ZIP-day event-window ",
    "observations assigned to one of 77 distinct impact dates; blue line segments show the ",
    "resulting range. Diamonds and thick gray bars show the frozen full-sample estimate and ",
    "95% confidence interval, respectively. No jackknife confidence intervals were estimated."
  )
), caption_path)

artifact_qa <- data.table(
  check_name = c(
    "Model QA checks all pass",
    "Figure S4 PNG and PDF written",
    "Table S7 contains 1,232 estimates",
    "Summary contains 16 estimands",
    "Seventy-seven dates per estimand",
    "All deletion estimates finite and positive",
    "Direction-reversal accounting complete"
  ),
  status = c(
    if (all(model_qa$status == "PASS")) "PASS" else "FAIL",
    if (file.exists(figure_png) && file.exists(figure_pdf)) "PASS" else "FAIL",
    if (nrow(estimates) == 1232L) "PASS" else "FAIL",
    if (nrow(summary_results) == 16L) "PASS" else "FAIL",
    if (all(estimates[, .N, by = estimand_id]$N == 77L)) "PASS" else "FAIL",
    if (all(is.finite(estimates$jackknife_rate_ratio) & estimates$jackknife_rate_ratio > 0)) "PASS" else "FAIL",
    if (sum(estimates$direction_reversal) == sum(summary_results$direction_reversal_count)) "PASS" else "FAIL"
  ),
  observed = c(
    sum(model_qa$status == "PASS"),
    sum(file.exists(c(figure_png, figure_pdf))),
    nrow(estimates),
    nrow(summary_results),
    paste(unique(estimates[, .N, by = estimand_id]$N), collapse = ","),
    sum(is.finite(estimates$jackknife_rate_ratio) & estimates$jackknife_rate_ratio > 0),
    sum(estimates$direction_reversal)
  ),
  expected = c(nrow(model_qa), 2L, 1232L, 16L, 77L, 1232L, sum(summary_results$direction_reversal_count))
)
artifact_qa_path <- file.path(artifact_dir, "leave_one_impact_date_out_artifact_qa.csv")
fwrite(artifact_qa, artifact_qa_path)
if (any(artifact_qa$status != "PASS")) {
  print(artifact_qa)
  stop("One or more leave-one-impact-date-out artifact checks failed.")
}

manifest_targets <- c(
  figure_png,
  figure_pdf,
  table_csv,
  summary_md,
  caption_path,
  artifact_qa_path,
  required_sources,
  "scripts/mortality/run_leave_one_impact_date_out_34kt.R",
  "scripts/mortality/build_leave_one_impact_date_out_artifacts.R",
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
manifest_path <- file.path(artifact_dir, "leave_one_impact_date_out_manifest.csv")
fwrite(manifest, manifest_path)

cat("\nLeave-one-impact-date-out artifacts completed\n")
cat("Figure S4:", figure_png, "\n")
cat("Table S7:", table_csv, "\n")
cat("Direction reversals:", sum(estimates$direction_reversal), "\n")
cat("Artifact QA checks:", nrow(artifact_qa), "all PASS\n")
