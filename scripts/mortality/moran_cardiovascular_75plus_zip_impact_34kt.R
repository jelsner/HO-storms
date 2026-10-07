#!/usr/bin/env Rscript

# Global and local Moran statistics for empirical-Bayes-smoothed cardiovascular
# mortality rate ratios among adults age 75+, days 0 to +6, >=34 kt winds.
# Queen contiguity is constructed from the 2020 ZCTA polygons. Permutation tests
# are reproducible and local p-values are also adjusted with Benjamini-Hochberg.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(Matrix)
  library(scales)
  library(sf)
})

input_path <- paste0(
  "outputs/mortality/spatial_cardiovascular_75plus_34kt/",
  "cardiovascular_75plus_impact_zip_eb_34kt.gpkg"
)
output_dir <- "outputs/mortality/spatial_cardiovascular_75plus_34kt"
figure_dir <- "figs/mortality"
local_output_path <- file.path(
  output_dir,
  "cardiovascular_75plus_impact_zip_local_moran_34kt.csv"
)
global_output_path <- file.path(
  output_dir,
  "cardiovascular_75plus_impact_global_moran_34kt.csv"
)
spatial_output_path <- file.path(
  output_dir,
  "cardiovascular_75plus_impact_local_moran_34kt.gpkg"
)
figure_output_path <- file.path(
  figure_dir,
  "cardiovascular_75plus_impact_local_moran_34kt.png"
)

nsim <- 9999L
random_seed <- 20260916L

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(input_path)) {
  stop(
    "Missing spatial EB results: ", input_path,
    ". Run scripts/mortality/map_cardiovascular_75plus_zip_impact_34kt.R first."
  )
}

message("Reading EB-smoothed ZIP estimates")
map_all <- st_read(input_path, quiet = TRUE)
required_columns <- c("zip", "rate_ratio_eb")
if (!all(required_columns %in% names(map_all))) {
  stop(
    "Spatial input lacks required column(s): ",
    paste(setdiff(required_columns, names(map_all)), collapse = ", ")
  )
}

analysis_map <- map_all[
  is.finite(map_all$rate_ratio_eb) & map_all$rate_ratio_eb > 0,
]
if (nrow(analysis_map) < 3L) {
  stop("At least three ZIPs with finite positive EB rate ratios are required.")
}

# Projecting avoids spherical-topology ambiguity when determining whether two
# polygons share a boundary point or edge. st_touches implements queen
# contiguity for non-overlapping polygons.
analysis_projected <- st_transform(st_make_valid(analysis_map), 5070)
message("Constructing queen-contiguity neighbors")
neighbors <- st_touches(analysis_projected, sparse = TRUE)
neighbor_count <- lengths(neighbors)

row_index <- rep(seq_len(nrow(analysis_projected)), neighbor_count)
column_index <- unlist(neighbors, use.names = FALSE)
if (!length(column_index)) {
  stop("No queen-contiguous ZCTA pairs were found.")
}

# Row-standardized weights. Island rows contain only zeros.
weights <- sparseMatrix(
  i = row_index,
  j = column_index,
  x = 1 / neighbor_count[row_index],
  dims = c(nrow(analysis_projected), nrow(analysis_projected))
)
non_island <- neighbor_count > 0L
s0 <- sum(weights)

log_rr <- log(analysis_projected$rate_ratio_eb)
centered <- log_rr - mean(log_rr)
n_zips <- length(centered)
m2 <- sum(centered^2) / n_zips
spatial_lag <- as.numeric(weights %*% centered)

local_i <- centered * spatial_lag / m2
local_i[!non_island] <- NA_real_
global_i <- (n_zips / s0) *
  sum(centered * spatial_lag) /
  sum(centered^2)
expected_global_i <- -1 / (n_zips - 1)

message("Running ", comma(nsim), " spatial permutations")
set.seed(random_seed)
permuted_global_i <- numeric(nsim)
local_extreme_count <- integer(n_zips)
progress <- txtProgressBar(min = 0L, max = nsim, style = 3L)
progress_step <- max(1L, floor(nsim / 100L))

for (simulation in seq_len(nsim)) {
  permuted <- sample(centered, replace = FALSE)
  permuted_lag <- as.numeric(weights %*% permuted)
  permuted_local_i <- permuted * permuted_lag / m2
  permuted_global_i[simulation] <- (n_zips / s0) *
    sum(permuted * permuted_lag) /
    sum(permuted^2)
  local_extreme_count <- local_extreme_count +
    as.integer(abs(permuted_local_i) >= abs(local_i))

  if (simulation %% progress_step == 0L || simulation == nsim) {
    setTxtProgressBar(progress, simulation)
  }
}
close(progress)

global_p_two_sided <- (
  1 + sum(
    abs(permuted_global_i - expected_global_i) >=
      abs(global_i - expected_global_i)
  )
) / (nsim + 1)
global_z <- (
  global_i - mean(permuted_global_i)
) / stats::sd(permuted_global_i)

local_p <- (local_extreme_count + 1) / (nsim + 1)
local_p[!non_island] <- NA_real_
local_p_adjusted <- rep(NA_real_, n_zips)
local_p_adjusted[non_island] <- p.adjust(
  local_p[non_island],
  method = "BH"
)

cluster <- rep("Not significant", n_zips)
significant <- non_island & !is.na(local_p_adjusted) &
  local_p_adjusted < 0.05
cluster[significant & centered > 0 & spatial_lag > 0] <- "High-High"
cluster[significant & centered < 0 & spatial_lag < 0] <- "Low-Low"
cluster[significant & centered > 0 & spatial_lag < 0] <- "High-Low"
cluster[significant & centered < 0 & spatial_lag > 0] <- "Low-High"
cluster[!non_island] <- "Island"

local_results <- data.table(
  zip = as.character(analysis_projected$zip),
  rate_ratio_eb = analysis_projected$rate_ratio_eb,
  centered_log_rr = centered,
  neighbor_count = neighbor_count,
  spatial_lag_centered_log_rr = spatial_lag,
  local_moran_i = local_i,
  permutation_p_value = local_p,
  fdr_p_value = local_p_adjusted,
  lisa_cluster = cluster
)
setorder(local_results, zip)
fwrite(local_results, local_output_path)

undirected_edges <- sum(neighbor_count) / 2
global_results <- data.table(
  variable = "log EB-smoothed cardiovascular mortality RR",
  contiguity = "queen",
  weight_style = "row-standardized",
  zips_analyzed = n_zips,
  non_island_zips = sum(non_island),
  island_zips = sum(!non_island),
  undirected_neighbor_links = undirected_edges,
  mean_neighbors = mean(neighbor_count),
  morans_i = global_i,
  expected_i = expected_global_i,
  permutation_mean_i = mean(permuted_global_i),
  permutation_sd_i = stats::sd(permuted_global_i),
  permutation_z = global_z,
  permutation_p_two_sided = global_p_two_sided,
  permutations = nsim,
  random_seed = random_seed
)
fwrite(global_results, global_output_path)

# Join the results back to every mapped ZCTA, retaining no-data polygons.
map_output <- merge(
  map_all,
  as.data.frame(local_results[, .(
    zip,
    centered_log_rr,
    neighbor_count,
    spatial_lag_centered_log_rr,
    local_moran_i,
    permutation_p_value,
    fdr_p_value,
    lisa_cluster
  )]),
  by = "zip",
  all.x = TRUE,
  sort = FALSE
)
map_output <- st_as_sf(map_output)

if (file.exists(spatial_output_path)) {
  unlink(spatial_output_path)
}
st_write(map_output, spatial_output_path, quiet = TRUE)

map_limit <- max(abs(quantile(
  local_results$local_moran_i,
  probs = c(0.02, 0.98),
  na.rm = TRUE
)))
if (!is.finite(map_limit) || map_limit <= 0) {
  map_limit <- 1
}

local_plot <- ggplot(map_output) +
  geom_sf(aes(fill = local_moran_i), color = "white", linewidth = 0.08) +
  scale_fill_gradient2(
    low = "#762A83",
    mid = "#F7F7F7",
    high = "#1B7837",
    midpoint = 0,
    limits = c(-map_limit, map_limit),
    oob = scales::squish,
    na.value = "grey85",
    name = "Local Moran's I"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Local spatial association in cardiovascular mortality effects",
    subtitle = paste0(
      "Adults age 75+, days 0 to +6, winds >=34 kt | ",
      "Queen contiguity; Global Moran's I = ",
      sprintf("%.3f", global_i),
      ", permutation p ",
      if (global_p_two_sided <= 1 / (nsim + 1)) {
        paste0("< ", format(1 / nsim, scientific = FALSE))
      } else {
        paste0("= ", sprintf("%.4f", global_p_two_sided))
      }
    ),
    caption = paste0(
      "Local Moran's I is calculated from centered log EB-smoothed rate ratios. ",
      "Positive values indicate similar neighboring deviations; negative values ",
      "indicate spatial outliers. Gray ZIPs lack an estimate or neighbor."
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, margin = margin(b = 8)),
    plot.caption = element_text(size = 8, color = "grey35", hjust = 0),
    legend.position = "right"
  )

ggsave(
  figure_output_path,
  local_plot,
  width = 8.5,
  height = 7.5,
  dpi = 300,
  bg = "white"
)

cat("\nCompleted Moran analysis\n")
cat("ZIPs analyzed:", n_zips, "\n")
cat("Queen neighbor links:", undirected_edges, "\n")
cat("Islands:", sum(!non_island), "\n")
cat("Global Moran's I:", sprintf("%.6f", global_i), "\n")
cat("Expected I:", sprintf("%.6f", expected_global_i), "\n")
cat("Permutation z:", sprintf("%.3f", global_z), "\n")
cat("Two-sided permutation p:", sprintf("%.6f", global_p_two_sided), "\n")
cat("Global results:", global_output_path, "\n")
cat("Local results:", local_output_path, "\n")
cat("Spatial output:", spatial_output_path, "\n")
cat("Map:", figure_output_path, "\n")
