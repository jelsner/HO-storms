#!/usr/bin/env Rscript

# Build the locked Environmental Research spatial artifacts from frozen inputs:
# main Figures 5 and 6, Supplementary Figure S8, and Supplementary Table S9.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(grid)
  library(knitr)
  library(scales)
  library(sf)
})

freeze_date <- as.IDate("2026-11-17")
artifact_version <- "environmental_research_spatial_v1_2026-11-17"

spatial_freeze_dir <- paste0(
  "outputs/mortality/paper_spatial_34kt/",
  "spatial_frozen_v1_2026-11-17"
)
sensitivity_dir <- paste0(
  "outputs/mortality/paper_spatial_34kt/",
  "spatial_weight_sensitivity_v1_2026-11-17"
)

gpkg_path <- file.path(
  spatial_freeze_dir,
  "spatial_results_34kt_frozen_v1.gpkg"
)
target_path <- file.path(
  spatial_freeze_dir,
  "spatial_targets_34kt_frozen_v1.csv"
)
zip_path <- file.path(
  spatial_freeze_dir,
  "spatial_zip_results_34kt_frozen_v1.csv"
)
global_path <- file.path(
  spatial_freeze_dir,
  "spatial_global_moran_34kt_frozen_v1.csv"
)
sensitivity_path <- file.path(
  sensitivity_dir,
  "spatial_weight_sensitivity_34kt_v1.csv"
)

figure_dir <- file.path(
  "figs/mortality",
  artifact_version
)
table_dir <- file.path(
  "outputs/mortality",
  artifact_version,
  "tables"
)
artifact_output_dir <- dirname(table_dir)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

figure5_png <- file.path(figure_dir, "figure_05_spatial_eb_rate_ratios.png")
figure5_pdf <- file.path(figure_dir, "figure_05_spatial_eb_rate_ratios.pdf")
figure6_png <- file.path(figure_dir, "figure_06_spatial_lisa_clusters.png")
figure6_pdf <- file.path(figure_dir, "figure_06_spatial_lisa_clusters.pdf")
figure_s8_png <- file.path(figure_dir, "figure_s08_spatial_reliability_sensitivity.png")
figure_s8_pdf <- file.path(figure_dir, "figure_s08_spatial_reliability_sensitivity.pdf")
table_s9_csv <- file.path(table_dir, "table_s09_spatial_summary.csv")
table_s9_md <- file.path(table_dir, "table_s09_spatial_summary.md")
caption_path <- file.path(table_dir, "spatial_figure_captions.md")
qa_path <- file.path(artifact_output_dir, "spatial_artifact_qa.csv")
manifest_path <- file.path(artifact_output_dir, "spatial_artifact_manifest.csv")

required_paths <- c(
  gpkg_path,
  target_path,
  zip_path,
  global_path,
  sensitivity_path
)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths)) {
  stop("Missing required inputs: ", paste(missing_paths, collapse = ", "))
}

get_source_commit <- function() {
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

source_commit <- get_source_commit()

save_plot_pair <- function(plot, png_path, pdf_path, width, height) {
  ggsave(
    png_path,
    plot,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    bg = "white"
  )
  ggsave(
    pdf_path,
    plot,
    width = width,
    height = height,
    units = "in",
    device = "pdf",
    family = "Helvetica",
    bg = "white"
  )
}

draw_two_panel <- function(
  top_plot,
  bottom_plot,
  png_path,
  pdf_path,
  width,
  height
) {
  draw <- function() {
    grid.newpage()
    layout <- grid.layout(
      nrow = 2L,
      ncol = 1L,
      heights = unit(c(2.1, 1), "null")
    )
    pushViewport(viewport(layout = layout))
    print(top_plot, vp = viewport(layout.pos.row = 1L, layout.pos.col = 1L))
    print(bottom_plot, vp = viewport(layout.pos.row = 2L, layout.pos.col = 1L))
    popViewport()
  }

  png(
    png_path,
    width = width,
    height = height,
    units = "in",
    res = 600,
    type = "quartz",
    bg = "white"
  )
  draw()
  dev.off()

  pdf(pdf_path, width = width, height = height, family = "Helvetica", bg = "white")
  draw()
  dev.off()
}

message("Reading frozen spatial and sensitivity inputs")
spatial <- st_read(gpkg_path, layer = "spatial_results", quiet = TRUE)
spatial$zip <- as.character(spatial$zip)
targets <- fread(target_path)
zip_results <- fread(zip_path)
global <- fread(global_path)
sensitivity <- fread(sensitivity_path)
setorder(targets, target_order)

targets[, target_label := sprintf(
  "%s, age %s\n%s\nStatewide RR %.3f",
  cause,
  age_group,
  period,
  pooled_rate_ratio
)]
target_levels <- targets$target_label
target_lookup <- targets[, .(target_id, target_order, target_label)]

spatial <- merge(
  spatial,
  as.data.frame(target_lookup),
  by = c("target_id", "target_order"),
  all.x = TRUE,
  sort = FALSE
)
spatial <- st_as_sf(spatial)
spatial$target_label <- factor(spatial$target_label, levels = target_levels)

message("Building Figure 5: EB-smoothed ZIP rate-ratio maps")
center_limit <- as.numeric(quantile(
  abs(spatial$centered_log_rr),
  probs = 0.98,
  na.rm = TRUE
))
if (!is.finite(center_limit) || center_limit <= 0) center_limit <- 0.05

figure5 <- ggplot(spatial) +
  geom_sf(
    aes(fill = centered_log_rr),
    color = "white",
    linewidth = 0.05
  ) +
  facet_wrap(~ target_label, ncol = 3, drop = FALSE) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-center_limit, center_limit),
    oob = squish,
    na.value = "grey82",
    breaks = pretty(c(-center_limit, center_limit), n = 5),
    labels = function(value) percent(exp(value) - 1, accuracy = 0.1),
    name = "EB rate ratio\nrelative to target\ngeometric mean"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "ZIP-level variation in older-adult mortality associations",
    subtitle = paste0(
      "Gamma-Poisson empirical-Bayes estimates; color represents within-target ",
      "deviation from the target geometric mean"
    ),
    caption = paste(
      "All five cause-age-period combinations met the locked statewide spatial-eligibility rule. ",
      "Gray ZIPs lack an estimable counterfactual expected count. Color limits use the pooled ",
      "98th percentile of absolute centered log rate ratios and are identical across panels.",
      sep = "\n"
    )
  ) +
  theme_void(base_family = "Helvetica", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, margin = margin(b = 8)),
    plot.caption = element_text(size = 8, color = "grey30", hjust = 0),
    strip.text = element_text(face = "bold", size = 9, lineheight = 1.05),
    panel.spacing = unit(5, "mm"),
    legend.position = "right"
  )

save_plot_pair(figure5, figure5_png, figure5_pdf, 12, 9)

message("Building Figure 6: local Moran cluster maps")
global_labels <- global[, .(
  target_id,
  global_label = sprintf(
    "Global Moran's I = %.3f; q = %.3f",
    morans_i,
    global_moran_fdr_q_five_maps
  )
)]
global_levels <- paste(
  target_levels,
  global_labels[match(targets$target_id, target_id), global_label],
  sep = "\n"
)
spatial <- merge(
  spatial,
  as.data.frame(global_labels),
  by = "target_id",
  all.x = TRUE,
  sort = FALSE
)
spatial <- st_as_sf(spatial)
spatial$target_label <- factor(spatial$target_label, levels = target_levels)
spatial$target_global_label <- factor(
  paste(
    as.character(spatial$target_label),
    spatial$global_label,
    sep = "\n"
  ),
  levels = global_levels
)
spatial$lisa_cluster <- factor(
  spatial$lisa_cluster,
  levels = c(
    "High-High",
    "Low-Low",
    "High-Low",
    "Low-High",
    "Not significant",
    "Island",
    "Not estimable"
  )
)

lisa_colors <- c(
  "High-High" = "#7A0177",
  "Low-Low" = "#238B45",
  "High-Low" = "#E6550D",
  "Low-High" = "#2CA9A1",
  "Not significant" = "#F2F2F2",
  "Island" = "#252525",
  "Not estimable" = "#BDBDBD"
)

figure6 <- ggplot(spatial) +
  geom_sf(
    aes(fill = lisa_cluster),
    color = "white",
    linewidth = 0.05
  ) +
  facet_wrap(~ target_global_label, ncol = 3, drop = FALSE) +
  scale_fill_manual(
    values = lisa_colors,
    drop = TRUE,
    name = "Local Moran class"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "No FDR-supported local clusters in the five older-adult targets",
    subtitle = paste0(
      "Queen contiguity; 9,999 permutations; global q across five maps and ",
      "local q across ZIPs within each map"
    ),
    caption = paste0(
      "High-high and low-low denote local clusters; high-low and low-high denote spatial outliers. ",
      "No target met the protocol criterion for global spatial concentration, and no local test ",
      "survived within-map Benjamini-Hochberg correction."
    )
  ) +
  theme_void(base_family = "Helvetica", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, margin = margin(b = 8)),
    plot.caption = element_text(size = 8, color = "grey30", hjust = 0),
    strip.text = element_text(face = "bold", size = 8.5, lineheight = 1.05),
    panel.spacing = unit(5, "mm"),
    legend.position = "right"
  )

save_plot_pair(figure6, figure6_png, figure6_pdf, 12, 9)

message("Building Supplementary Figure S8")
reliability_limit <- as.numeric(quantile(
  spatial$reliability,
  probs = 0.99,
  na.rm = TRUE
))
if (!is.finite(reliability_limit) || reliability_limit <= 0) {
  reliability_limit <- max(spatial$reliability, na.rm = TRUE)
}

reliability_plot <- ggplot(spatial) +
  geom_sf(aes(fill = reliability), color = "white", linewidth = 0.04) +
  facet_wrap(~ target_label, ncol = 3, drop = FALSE) +
  scale_fill_gradientn(
    colours = viridisLite::viridis(8, option = "C"),
    limits = c(0, reliability_limit),
    oob = squish,
    na.value = "grey82",
    labels = percent_format(accuracy = 0.1),
    name = "EB reliability"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "A. Empirical-Bayes reliability",
    subtitle = paste0(
      "Reliability = expected deaths / (EB prior rate + expected deaths); ",
      "common scale truncated at the pooled 99th percentile"
    )
  ) +
  theme_void(base_family = "Helvetica", base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, margin = margin(b = 5)),
    strip.text = element_text(face = "bold", size = 8, lineheight = 1.0),
    panel.spacing = unit(4, "mm"),
    legend.position = "right"
  )

sensitivity <- merge(
  sensitivity,
  target_lookup,
  by = c("target_id", "target_order"),
  all.x = TRUE,
  sort = FALSE
)
sensitivity$target_label <- factor(
  sensitivity$target_label,
  levels = rev(target_levels)
)
sensitivity$weight_definition <- factor(
  sensitivity$weight_definition,
  levels = c("queen", "rook", "symmetric_6nn"),
  labels = c("Queen", "Rook", "Symmetric 6NN")
)

sensitivity_plot <- ggplot(
  sensitivity,
  aes(
    x = morans_i,
    y = target_label,
    color = weight_definition,
    shape = weight_definition
  )
) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_line(
    aes(group = target_label),
    color = "grey75",
    linewidth = 0.45
  ) +
  geom_point(size = 2.7, position = position_dodge(width = 0.42)) +
  scale_color_manual(
    values = c("Queen" = "#3B4CC0", "Rook" = "#1B9E77", "Symmetric 6NN" = "#D95F02")
  ) +
  labs(
    title = "B. Global Moran's I sensitivity to spatial weights",
    subtitle = "All within-weight five-map FDR q-values are at least 0.067",
    x = "Global Moran's I",
    y = NULL,
    color = "Weights",
    shape = "Weights",
    caption = paste0(
      "Queen contiguity is primary. Rook contiguity and symmetric six-nearest-neighbor weights ",
      "are prespecified sensitivity definitions. All tests use 9,999 permutations."
    )
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(size = 8, color = "grey30", hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 7.5)
  )

draw_two_panel(
  reliability_plot,
  sensitivity_plot,
  figure_s8_png,
  figure_s8_pdf,
  width = 12,
  height = 13
)

message("Building Supplementary Table S9")
reliability_summary <- zip_results[status == "estimated", .(
  reliability_median = median(reliability, na.rm = TRUE),
  reliability_q1 = quantile(reliability, 0.25, na.rm = TRUE),
  reliability_q3 = quantile(reliability, 0.75, na.rm = TRUE),
  local_fdr_supported_clusters = sum(
    lisa_cluster %chin% c("High-High", "Low-Low", "High-Low", "Low-High"),
    na.rm = TRUE
  )
), by = target_id]

sensitivity_wide <- dcast(
  sensitivity,
  target_id + target_order + cause + age_group + period ~ weight_definition,
  value.var = c(
    "morans_i",
    "permutation_p_two_sided",
    "fdr_q_within_weight_five_maps"
  )
)
setnames(
  sensitivity_wide,
  gsub(" ", "_", tolower(names(sensitivity_wide)))
)

table_s9 <- merge(
  targets[, .(
    target_id,
    target_order,
    cause,
    age_group,
    period,
    total_panel_zips,
    estimable_zips,
    aggregate_observed_deaths,
    aggregate_expected_deaths,
    pooled_rate_ratio,
    eb_prior_mean_rr,
    eb_prior_sd_rr
  )],
  reliability_summary,
  by = "target_id",
  all.x = TRUE,
  sort = FALSE
)
table_s9 <- merge(
  table_s9,
  sensitivity_wide,
  by = c("target_id", "target_order", "cause", "age_group", "period"),
  all.x = TRUE,
  sort = FALSE
)
table_s9[, `:=`(
  artifact_version = artifact_version,
  freeze_date = freeze_date,
  source_commit = source_commit,
  spatial_concentration_conclusion =
    "No evidence of global spatial concentration"
)]
setorder(table_s9, target_order)
fwrite(table_s9, table_s9_csv)

display_table <- table_s9[, .(
  Target = paste0(cause, ", age ", age_group, ", ", period),
  `ZIPs estimated` = paste0(estimable_zips, "/", total_panel_zips),
  `Observed / expected deaths` = sprintf(
    "%s / %.1f",
    comma(aggregate_observed_deaths),
    aggregate_expected_deaths
  ),
  `Statewide RR` = sprintf("%.3f", pooled_rate_ratio),
  `EB prior mean (SD)` = sprintf("%.3f (%.3f)", eb_prior_mean_rr, eb_prior_sd_rr),
  `Reliability median [IQR]` = sprintf(
    "%.4f [%.4f, %.4f]",
    reliability_median,
    reliability_q1,
    reliability_q3
  ),
  `Queen I (p; q)` = sprintf(
    "%.3f (%.4f; %.4f)",
    morans_i_queen,
    permutation_p_two_sided_queen,
    fdr_q_within_weight_five_maps_queen
  ),
  `Rook I (p; q)` = sprintf(
    "%.3f (%.4f; %.4f)",
    morans_i_rook,
    permutation_p_two_sided_rook,
    fdr_q_within_weight_five_maps_rook
  ),
  `6NN I (p; q)` = sprintf(
    "%.3f (%.4f; %.4f)",
    morans_i_symmetric_6nn,
    permutation_p_two_sided_symmetric_6nn,
    fdr_q_within_weight_five_maps_symmetric_6nn
  ),
  `Local FDR clusters` = local_fdr_supported_clusters
)]

table_lines <- c(
  "# Table S9. Spatial model and global Moran results",
  "",
  capture.output(kable(display_table, format = "pipe", align = "lccccccccc")),
  "",
  paste0(
    "Notes: ZIP rate ratios were stabilized with gamma-Poisson empirical-Bayes smoothing. ",
    "Global Moran tests used centered log EB rate ratios, row-standardized weights, and ",
    "9,999 permutations. q-values are Benjamini-Hochberg adjustments across the five ",
    "targets within each weight definition. Queen contiguity is primary; rook and symmetric ",
    "six-nearest-neighbor weights are sensitivity analyses. Local FDR clusters use ",
    "within-map Benjamini-Hochberg correction."
  )
)
writeLines(table_lines, table_s9_md)

caption_lines <- c(
  "# Spatial figure captions",
  "",
  "## Figure 5",
  paste0(
    "Empirical-Bayes-smoothed ZIP-level rate ratios for the five older-adult ",
    "cause-period associations meeting the locked statewide spatial-eligibility rule. ",
    "Color represents each ZIP's centered log EB rate ratio, displayed as a percentage ",
    "deviation from the target-specific geometric mean. The same color limits are used ",
    "across panels; gray indicates a ZIP without an estimable counterfactual expected count."
  ),
  "",
  "## Figure 6",
  paste0(
    "Local Moran cluster classifications for the same five older-adult targets using ",
    "row-standardized queen contiguity and 9,999 permutations. Global p-values were ",
    "Benjamini-Hochberg adjusted across the five maps, and local p-values were adjusted ",
    "across ZIPs within each map. No global or local test survived the prespecified FDR ",
    "correction."
  ),
  "",
  "## Figure S8",
  paste0(
    "Empirical-Bayes reliability and spatial-weight sensitivity. Panel A maps EB reliability, ",
    "defined as expected deaths divided by the sum of the EB prior rate and expected deaths. ",
    "Panel B compares global Moran's I under queen contiguity, rook contiguity, and symmetric ",
    "six-nearest-neighbor weights. The conclusion of no FDR-supported global spatial ",
    "concentration was unchanged across all three definitions."
  )
)
writeLines(caption_lines, caption_path)

qa <- data.table(
  check_name = c(
    "Five figure facets",
    "Five Table S9 rows",
    "No missing target labels",
    "Figure 5 files written",
    "Figure 6 files written",
    "Figure S8 files written",
    "Table S9 files written",
    "All conclusions agree across weights"
  ),
  status = c(
    if (uniqueN(spatial$target_global_label) == 5L) "PASS" else "FAIL",
    if (nrow(table_s9) == 5L) "PASS" else "FAIL",
    if (!anyNA(spatial$target_label)) "PASS" else "FAIL",
    if (all(file.exists(c(figure5_png, figure5_pdf)))) "PASS" else "FAIL",
    if (all(file.exists(c(figure6_png, figure6_pdf)))) "PASS" else "FAIL",
    if (all(file.exists(c(figure_s8_png, figure_s8_pdf)))) "PASS" else "FAIL",
    if (all(file.exists(c(table_s9_csv, table_s9_md)))) "PASS" else "FAIL",
    if (all(
      sensitivity[, uniqueN(
        morans_i > 0 & fdr_q_within_weight_five_maps < 0.05
      ), by = target_id]$V1 == 1L
    )) "PASS" else "FAIL"
  ),
  observed = c(
    uniqueN(spatial$target_global_label),
    nrow(table_s9),
    sum(is.na(spatial$target_label)),
    sum(file.exists(c(figure5_png, figure5_pdf))),
    sum(file.exists(c(figure6_png, figure6_pdf))),
    sum(file.exists(c(figure_s8_png, figure_s8_pdf))),
    sum(file.exists(c(table_s9_csv, table_s9_md))),
    sum(
      sensitivity[, uniqueN(
        morans_i > 0 & fdr_q_within_weight_five_maps < 0.05
      ), by = target_id]$V1 == 1L
    )
  ),
  expected = c(5L, 5L, 0L, 2L, 2L, 2L, 2L, 5L)
)
fwrite(qa, qa_path)
if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more spatial artifact checks failed.")
}

manifest_targets <- c(
  figure5_png,
  figure5_pdf,
  figure6_png,
  figure6_pdf,
  figure_s8_png,
  figure_s8_pdf,
  table_s9_csv,
  table_s9_md,
  caption_path,
  qa_path,
  "scripts/mortality/build_spatial_publication_artifacts_34kt.R",
  gpkg_path,
  target_path,
  zip_path,
  global_path,
  sensitivity_path
)
hash_output <- system2(
  "shasum",
  c("-a", "256", manifest_targets),
  stdout = TRUE
)
manifest_split <- tstrsplit(hash_output, "  ", fixed = TRUE)
manifest <- data.table(
  sha256 = manifest_split[[1L]],
  path = manifest_split[[2L]],
  bytes = as.numeric(file.info(manifest_split[[2L]])$size),
  artifact_version = artifact_version,
  freeze_date = freeze_date
)
fwrite(manifest, manifest_path)

cat("\nSpatial publication artifacts completed\n")
cat("Figure 5:", figure5_png, "\n")
cat("Figure 6:", figure6_png, "\n")
cat("Figure S8:", figure_s8_png, "\n")
cat("Table S9:", table_s9_csv, "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
