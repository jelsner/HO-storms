#!/usr/bin/env Rscript

# Prespecified spatial-weight sensitivity for the five frozen older-adult
# mortality targets. Global Moran's I is evaluated with queen contiguity
# (primary), rook contiguity, and symmetric six-nearest-neighbor weights.

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(sf)
})

freeze_date <- as.IDate("2026-11-17")
analysis_version <- "spatial_weight_sensitivity_v1_2026-11-17"
nsim <- 9999L
random_seed <- 20260916L

spatial_freeze_dir <- paste0(
  "outputs/mortality/paper_spatial_34kt/",
  "spatial_frozen_v1_2026-11-17"
)
input_gpkg <- file.path(
  spatial_freeze_dir,
  "spatial_results_34kt_frozen_v1.gpkg"
)
primary_global_path <- file.path(
  spatial_freeze_dir,
  "spatial_global_moran_34kt_frozen_v1.csv"
)
target_path <- file.path(
  spatial_freeze_dir,
  "spatial_targets_34kt_frozen_v1.csv"
)

output_dir <- paste0(
  "outputs/mortality/paper_spatial_34kt/",
  analysis_version
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

estimate_path <- file.path(
  output_dir,
  "spatial_weight_sensitivity_34kt_v1.csv"
)
agreement_path <- file.path(
  output_dir,
  "spatial_weight_agreement_34kt_v1.csv"
)
qa_path <- file.path(
  output_dir,
  "spatial_weight_sensitivity_qa_v1.csv"
)
manifest_path <- file.path(
  output_dir,
  "spatial_weight_sensitivity_manifest_v1.csv"
)

required_paths <- c(input_gpkg, primary_global_path, target_path)
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

neighbors_to_weights <- function(neighbors) {
  neighbor_count <- lengths(neighbors)
  row_index <- rep(seq_along(neighbors), neighbor_count)
  column_index <- unlist(neighbors, use.names = FALSE)
  if (!length(column_index)) stop("The spatial weights contain no links.")
  weights <- sparseMatrix(
    i = row_index,
    j = column_index,
    x = 1 / neighbor_count[row_index],
    dims = c(length(neighbors), length(neighbors))
  )
  list(
    weights = weights,
    neighbor_count = neighbor_count,
    non_island = neighbor_count > 0L,
    s0 = sum(weights)
  )
}

build_symmetric_knn <- function(projected_geometry, k = 6L) {
  points <- st_point_on_surface(projected_geometry)
  coordinates <- st_coordinates(points)
  distance_matrix <- as.matrix(stats::dist(coordinates))
  diag(distance_matrix) <- Inf
  n <- nrow(distance_matrix)
  directed <- matrix(FALSE, nrow = n, ncol = n)
  for (index in seq_len(n)) {
    nearest <- order(distance_matrix[index, ], na.last = NA)[seq_len(k)]
    directed[index, nearest] <- TRUE
  }
  symmetric <- directed | t(directed)
  lapply(seq_len(n), function(index) which(symmetric[index, ]))
}

calculate_global_moran_set <- function(analysis_map, target_id, target_order) {
  projected <- st_transform(st_make_valid(analysis_map), 5070)
  queen_neighbors <- st_touches(projected, sparse = TRUE)
  rook_neighbors <- st_relate(
    projected,
    pattern = "F***1****",
    sparse = TRUE
  )
  knn_neighbors <- build_symmetric_knn(projected, k = 6L)

  weight_sets <- list(
    queen = neighbors_to_weights(queen_neighbors),
    rook = neighbors_to_weights(rook_neighbors),
    symmetric_6nn = neighbors_to_weights(knn_neighbors)
  )

  log_rr <- log(analysis_map$rate_ratio_eb)
  centered <- log_rr - mean(log_rr)
  n_zips <- length(centered)
  denominator <- sum(centered^2)
  expected_i <- -1 / (n_zips - 1)
  if (!is.finite(denominator) || denominator <= 0) {
    stop("No finite spatial variance for ", target_id)
  }

  observed <- rbindlist(lapply(names(weight_sets), function(weight_name) {
    item <- weight_sets[[weight_name]]
    spatial_lag <- as.numeric(item$weights %*% centered)
    data.table(
      target_id = target_id,
      target_order = target_order,
      weight_definition = weight_name,
      weight_style = "row-standardized",
      zips_analyzed = n_zips,
      non_island_zips = sum(item$non_island),
      island_zips = sum(!item$non_island),
      undirected_neighbor_links = sum(item$neighbor_count) / 2,
      mean_neighbors = mean(item$neighbor_count),
      min_neighbors = min(item$neighbor_count),
      max_neighbors = max(item$neighbor_count),
      morans_i = (n_zips / item$s0) *
        sum(centered * spatial_lag) / denominator,
      expected_i = expected_i
    )
  }))

  permuted <- lapply(weight_sets, function(x) numeric(nsim))
  message("Running ", format(nsim, big.mark = ","), " shared permutations")
  set.seed(random_seed)
  progress <- txtProgressBar(min = 0L, max = nsim, style = 3L)
  progress_step <- max(1L, floor(nsim / 100L))

  for (simulation in seq_len(nsim)) {
    shuffled <- sample(centered, replace = FALSE)
    shuffled_denominator <- sum(shuffled^2)
    for (weight_name in names(weight_sets)) {
      item <- weight_sets[[weight_name]]
      shuffled_lag <- as.numeric(item$weights %*% shuffled)
      permuted[[weight_name]][simulation] <- (n_zips / item$s0) *
        sum(shuffled * shuffled_lag) / shuffled_denominator
    }
    if (simulation %% progress_step == 0L || simulation == nsim) {
      setTxtProgressBar(progress, simulation)
    }
  }
  close(progress)

  permutation_summary <- rbindlist(lapply(names(weight_sets), function(weight_name) {
    values <- permuted[[weight_name]]
    observed_i <- observed[
      weight_definition == weight_name,
      morans_i
    ]
    p_value <- (
      1 + sum(abs(values - expected_i) >= abs(observed_i - expected_i))
    ) / (nsim + 1)
    data.table(
      weight_definition = weight_name,
      permutation_mean_i = mean(values),
      permutation_sd_i = stats::sd(values),
      permutation_z = (observed_i - mean(values)) / stats::sd(values),
      permutation_p_two_sided = p_value,
      permutations = nsim,
      random_seed = random_seed
    )
  }))

  merge(
    observed,
    permutation_summary,
    by = "weight_definition",
    all.x = TRUE,
    sort = FALSE
  )
}

message("Reading the frozen five-target spatial dataset")
spatial <- st_read(input_gpkg, layer = "spatial_results", quiet = TRUE)
spatial$zip <- as.character(spatial$zip)
targets <- fread(target_path)
primary_global <- fread(primary_global_path)
setorder(targets, target_order)

all_results <- vector("list", nrow(targets))
for (target_index in seq_len(nrow(targets))) {
  target <- targets[target_index]
  message(
    "[", target_index, "/", nrow(targets), "] ", target$cause,
    ", age ", target$age_group, ", ", target$period
  )
  analysis_map <- spatial[
    spatial$target_id == target$target_id &
      spatial$status == "estimated" &
      is.finite(spatial$rate_ratio_eb) &
      spatial$rate_ratio_eb > 0,
  ]
  results <- calculate_global_moran_set(
    analysis_map,
    target$target_id,
    target$target_order
  )
  results[, `:=`(
    cause = target$cause,
    age_group = target$age_group,
    period = target$period
  )]
  all_results[[target_index]] <- results
}

output <- rbindlist(all_results, use.names = TRUE, fill = TRUE)
output[, fdr_q_within_weight_five_maps := p.adjust(
  permutation_p_two_sided,
  method = "BH"
), by = weight_definition]
output[, fdr_q_all_15_sensitivity_tests := p.adjust(
  permutation_p_two_sided,
  method = "BH"
)]
output[, spatial_concentration_conclusion := fifelse(
  morans_i > 0 & fdr_q_within_weight_five_maps < 0.05,
  "Evidence of global spatial concentration",
  "No evidence of global spatial concentration"
)]
output[, `:=`(
  analysis_version = analysis_version,
  freeze_date = freeze_date,
  source_commit = source_commit,
  wind_threshold_kt = 34L,
  weight_order = match(
    weight_definition,
    c("queen", "rook", "symmetric_6nn")
  )
)]
setorder(output, target_order, weight_order)

agreement <- output[, .(
  queen_morans_i = morans_i[weight_definition == "queen"],
  rook_morans_i = morans_i[weight_definition == "rook"],
  symmetric_6nn_morans_i = morans_i[weight_definition == "symmetric_6nn"],
  queen_q = fdr_q_within_weight_five_maps[weight_definition == "queen"],
  rook_q = fdr_q_within_weight_five_maps[weight_definition == "rook"],
  symmetric_6nn_q = fdr_q_within_weight_five_maps[
    weight_definition == "symmetric_6nn"
  ],
  sign_agreement_all_weights = uniqueN(sign(morans_i)) == 1L,
  inference_agreement_all_weights = uniqueN(
    morans_i > 0 & fdr_q_within_weight_five_maps < 0.05
  ) == 1L
), by = .(target_id, target_order, cause, age_group, period)]
agreement[, `:=`(
  analysis_version = analysis_version,
  freeze_date = freeze_date,
  source_commit = source_commit,
  wind_threshold_kt = 34L
)]
setorder(agreement, target_order)

qa <- data.table(
  check_name = character(),
  status = character(),
  observed = character(),
  expected = character(),
  details = character()
)
add_check <- function(name, pass, observed, expected, details = "") {
  qa <<- rbind(
    qa,
    data.table(
      check_name = name,
      status = if (isTRUE(pass)) "PASS" else "FAIL",
      observed = as.character(observed),
      expected = as.character(expected),
      details = details
    )
  )
}

add_check("Sensitivity row count", nrow(output) == 15L, nrow(output), 15L)
add_check(
  "Five targets",
  uniqueN(output$target_id) == 5L,
  uniqueN(output$target_id),
  5L
)
add_check(
  "Three weight definitions",
  uniqueN(output$weight_definition) == 3L,
  paste(sort(unique(output$weight_definition)), collapse = ", "),
  "queen, rook, symmetric_6nn"
)
add_check(
  "Permutation count",
  all(output$permutations == nsim),
  paste(unique(output$permutations), collapse = ","),
  nsim
)
add_check(
  "Finite Moran statistics",
  all(is.finite(output$morans_i)),
  sum(is.finite(output$morans_i)),
  15L
)
add_check(
  "P and q values in [0,1]",
  all(
    output$permutation_p_two_sided >= 0 &
      output$permutation_p_two_sided <= 1 &
      output$fdr_q_within_weight_five_maps >= 0 &
      output$fdr_q_within_weight_five_maps <= 1 &
      output$fdr_q_all_15_sensitivity_tests >= 0 &
      output$fdr_q_all_15_sensitivity_tests <= 1
  ),
  paste0(
    "p ", paste(signif(range(output$permutation_p_two_sided), 4L), collapse = " to "),
    "; q5 ", paste(signif(range(output$fdr_q_within_weight_five_maps), 4L), collapse = " to "),
    "; q15 ", paste(signif(range(output$fdr_q_all_15_sensitivity_tests), 4L), collapse = " to ")
  ),
  "0 to 1"
)
add_check(
  "Symmetric 6NN has no islands",
  all(output[weight_definition == "symmetric_6nn", island_zips] == 0L),
  paste(output[weight_definition == "symmetric_6nn", island_zips], collapse = ","),
  0L
)

queen_new <- output[weight_definition == "queen", .(
  target_id,
  morans_i_new = morans_i,
  p_new = permutation_p_two_sided
)]
queen_validation <- merge(
  queen_new,
  primary_global[, .(
    target_id,
    morans_i_primary = morans_i,
    p_primary = permutation_p_two_sided
  )],
  by = "target_id"
)
max_queen_i_difference <- max(abs(
  queen_validation$morans_i_new - queen_validation$morans_i_primary
))
max_queen_p_difference <- max(abs(
  queen_validation$p_new - queen_validation$p_primary
))
add_check(
  "Queen Moran statistics reproduce primary freeze",
  nrow(queen_validation) == 5L & max_queen_i_difference < 1e-10,
  paste0(
    "rows ", nrow(queen_validation), "; max difference ",
    format(max_queen_i_difference, scientific = TRUE, digits = 4L)
  ),
  "5 rows; difference < 1e-10"
)
add_check(
  "Queen permutation p-values reproduce primary freeze",
  max_queen_p_difference < 1e-12,
  format(max_queen_p_difference, scientific = TRUE, digits = 4L),
  "< 1e-12"
)

if (any(qa$status != "PASS")) {
  print(qa)
  stop("One or more spatial-weight sensitivity checks failed.")
}

fwrite(output, estimate_path)
fwrite(agreement, agreement_path)
fwrite(qa, qa_path)

manifest_targets <- c(
  estimate_path,
  agreement_path,
  qa_path,
  "scripts/mortality/run_spatial_weight_sensitivity_34kt.R",
  input_gpkg,
  primary_global_path,
  target_path
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
  analysis_version = analysis_version,
  freeze_date = freeze_date
)
fwrite(manifest, manifest_path)

cat("\nSpatial-weight sensitivity completed\n")
cat("Rows:", nrow(output), "\n")
cat("Targets:", uniqueN(output$target_id), "\n")
cat("Weight definitions:", uniqueN(output$weight_definition), "\n")
cat("QA checks:", nrow(qa), "all PASS\n")
cat("Output directory:", output_dir, "\n")
