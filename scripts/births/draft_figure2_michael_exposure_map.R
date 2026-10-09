#!/usr/bin/env Rscript

# Alternative draft Figure 2: spatial exposure assignment illustrated with
# Hurricane Michael (2018). Residence points are simulated within the Florida
# Panhandle and do not represent actual birth addresses.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(lubridate)
  library(sf)
})

ibtracs_dir <- "data/shared/storm/ibtracs-na-points-v04r01"
county_file <- paste0(
  "data/births/raw/FL_county_export/",
  "FL_counties_boundaries.gpkg"
)
output_dir <- "figs/births/revised-manuscript"
output_png <- file.path(output_dir, "figure-2-michael-exposure-map.png")
output_pdf <- file.path(output_dir, "figure-2-michael-exposure-map.pdf")

analysis_crs <- 3086
nautical_mile_m <- 1852
michael_sid <- "2018280N18273"
snapshot_time <- ymd_hms("2018-10-10 17:30:00", tz = "UTC")

publication_pdf <- function(filename, ...) {
  if (identical(unname(Sys.info()["sysname"]), "Darwin")) {
    grDevices::quartz(file = filename, type = "pdf", ...)
  } else {
    grDevices::pdf(file = filename, ...)
  }
}

ibtracs_file <- list.files(
  ibtracs_dir,
  pattern = "[.]shp$",
  full.names = TRUE,
  ignore.case = TRUE
)
if (length(ibtracs_file) != 1L || !file.exists(county_file)) {
  stop("Required IBTrACS point or Florida county geometry is missing.")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

make_snapshot_field <- function(observation) {
  projected <- st_transform(observation, analysis_crs)
  center <- st_coordinates(projected)[1, ]
  quadrants <- data.table(
    name = c("NE", "SE", "SW", "NW"),
    start = c(0, 90, 180, 270),
    end = c(90, 180, 270, 360)
  )
  radii_m <- vapply(seq_len(nrow(quadrants)), function(index) {
    radius_nm <- suppressWarnings(as.numeric(projected[[paste0(
      "USA_R34_", quadrants$name[index]
    )]][1]))
    if (!is.finite(radius_nm) || radius_nm <= 0 || radius_nm >= 999) {
      return(NA_real_)
    }
    radius_nm * nautical_mile_m
  }, numeric(1))
  if (anyNA(radii_m)) {
    stop("The selected Michael observation lacks one or more 34-kt quadrant radii.")
  }

  # Construct the perimeter directly from the four quadrant arcs. This keeps
  # the true asymmetric radii while avoiding radial seams through the center.
  arcs <- lapply(seq_len(nrow(quadrants)), function(index) {
    bearings <- seq(
      quadrants$start[index], quadrants$end[index], length.out = 91L
    )
    radians <- bearings * pi / 180
    cbind(
      center[1] + radii_m[index] * sin(radians),
      center[2] + radii_m[index] * cos(radians)
    )
  })
  perimeter <- do.call(rbind, arcs)
  perimeter <- rbind(perimeter, perimeter[1, ])
  st_sf(
    field = "34-kt wind field",
    geometry = st_sfc(st_polygon(list(perimeter)), crs = analysis_crs)
  )
}

message("Reading Hurricane Michael observations")
storm_points <- st_read(ibtracs_file, quiet = TRUE) |>
  transform(
    observation_time = ymd_hms(ISO_TIME, tz = "UTC", quiet = TRUE),
    USA_WIND = suppressWarnings(as.numeric(USA_WIND))
  ) |>
  subset(SID == michael_sid)

if (!nrow(storm_points)) stop("Hurricane Michael was not found in IBTrACS.")

snapshot <- storm_points[storm_points$observation_time == snapshot_time, ]
if (nrow(snapshot) != 1L) {
  stop("Expected exactly one Michael observation at the selected snapshot time.")
}
wind_field <- make_snapshot_field(snapshot)

# Keep the approach, Florida crossing, and immediate inland segment. The line
# is constructed in chronological order rather than from the shapefile row
# order implicitly.
track_points <- storm_points[
  storm_points$observation_time >= ymd_hms("2018-10-09 12:00:00", tz = "UTC") &
    storm_points$observation_time <= ymd_hms("2018-10-11 03:00:00", tz = "UTC"),
]
track_points <- track_points[order(track_points$observation_time), ]
track_projected <- st_transform(track_points, analysis_crs)
track_line <- st_sf(
  feature = "Hurricane Michael track",
  geometry = st_sfc(
    st_linestring(st_coordinates(track_projected)[, 1:2]),
    crs = analysis_crs
  )
)

counties <- st_read(county_file, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(analysis_crs)

# Simulate residence locations over Panhandle land for a privacy-safe methods
# illustration. The fixed seed makes the figure reproducible.
residence_bbox <- st_bbox(
  c(xmin = -87.8, ymin = 28.6, xmax = -83.6, ymax = 31.1),
  crs = st_crs(4326)
) |>
  st_as_sfc() |>
  st_transform(analysis_crs)
map_bbox <- st_bbox(
  c(xmin = -88.0, ymin = 27.4, xmax = -82.1, ymax = 31.25),
  crs = st_crs(4326)
) |>
  st_as_sfc() |>
  st_transform(analysis_crs)
panhandle_land <- suppressWarnings(st_intersection(
  st_union(counties),
  residence_bbox
))
displayed_land <- suppressWarnings(st_intersection(
  st_union(counties),
  map_bbox
))

set.seed(20261009)
candidate_points <- st_sample(panhandle_land, size = 2500, type = "random") |>
  st_as_sf()
candidate_points$inside_field <- lengths(
  st_within(candidate_points, wind_field)
) > 0L

inside_index <- which(candidate_points$inside_field)
outside_index <- which(!candidate_points$inside_field)
if (length(inside_index) < 16L || length(outside_index) < 16L) {
  stop("Insufficient simulated points inside or outside the Michael wind field.")
}

# Add a few exposed locations southeast of landfall and additional unexposed
# locations farther down the peninsula so the spatial contrast is explicit.
extended_candidates <- st_sample(displayed_land, size = 6000, type = "random") |>
  st_as_sf()
extended_candidates$inside_field <- lengths(
  st_within(extended_candidates, wind_field)
) > 0L
candidate_lonlat <- st_coordinates(st_transform(extended_candidates, 4326))
landfall_lonlat <- st_coordinates(st_transform(snapshot, 4326))[1, ]
southeast_inside_index <- which(
  extended_candidates$inside_field &
    candidate_lonlat[, "X"] > landfall_lonlat["X"] &
    candidate_lonlat[, "Y"] < landfall_lonlat["Y"]
)
peninsula_outside_index <- which(
  !extended_candidates$inside_field &
    candidate_lonlat[, "X"] > -84.0 &
    candidate_lonlat[, "Y"] < 30.2
)
if (length(southeast_inside_index) < 6L ||
    length(peninsula_outside_index) < 10L) {
  stop("Insufficient simulated points in the southeast or peninsula strata.")
}
illustrative_residences <- rbind(
  candidate_points[sample(inside_index, 16L), ],
  candidate_points[sample(outside_index, 16L), ],
  extended_candidates[sample(southeast_inside_index, 6L), ],
  extended_candidates[sample(peninsula_outside_index, 10L), ]
)
illustrative_residences$classification <- factor(
  ifelse(
    illustrative_residences$inside_field,
    "Inside 34-kt wind field",
    "Outside 34-kt wind field"
  ),
  levels = c("Inside 34-kt wind field", "Outside 34-kt wind field")
)

snapshot_projected <- st_transform(snapshot, analysis_crs)
snapshot_label <- st_coordinates(snapshot_projected)
snapshot_label <- data.frame(
  x = snapshot_label[1, 1],
  y = snapshot_label[1, 2],
  label = "Michael at landfall\nOct 10, 17:30 UTC"
)

map_colors <- c(
  "Inside 34-kt wind field" = "#D1495B",
  "Outside 34-kt wind field" = "#2A9D8F"
)
map_shapes <- c(
  "Inside 34-kt wind field" = 16,
  "Outside 34-kt wind field" = 17
)

michael_map <- ggplot() +
  geom_sf(data = counties, fill = "#F4F1EA", color = "white", linewidth = 0.30) +
  geom_sf(
    data = wind_field,
    fill = "#5DADE2",
    color = "#2471A3",
    alpha = 0.28,
    linewidth = 0.8
  ) +
  geom_sf(data = track_line, color = "#263746", linewidth = 1.25) +
  geom_sf(
    data = track_projected,
    color = "#263746",
    size = 0.85,
    alpha = 0.65
  ) +
  geom_sf(
    data = snapshot_projected,
    shape = 21,
    fill = "white",
    color = "#172B4D",
    stroke = 1.1,
    size = 3.8
  ) +
  geom_sf(
    data = illustrative_residences,
    aes(color = classification, shape = classification),
    size = 2.7,
    stroke = 0.8
  ) +
  geom_label(
    data = snapshot_label,
    aes(x = x, y = y, label = label),
    nudge_x = 110000,
    nudge_y = -45000,
    hjust = 0,
    size = 3.3,
    lineheight = 0.95,
    color = "#172B4D",
    fill = "white",
    linewidth = 0.25,
    label.padding = grid::unit(0.18, "lines")
  ) +
  scale_color_manual(values = map_colors, name = "Illustrative residence") +
  scale_shape_manual(values = map_shapes, name = "Illustrative residence") +
  coord_sf(
    xlim = st_bbox(map_bbox)[c("xmin", "xmax")],
    ylim = st_bbox(map_bbox)[c("ymin", "ymax")],
    datum = NA,
    expand = FALSE
  ) +
  labs(
    title = "Residence-specific wind-field exposure",
    subtitle = paste0(
      "Hurricane Michael track and 34-kt quadrant wind field at Florida ",
      "landfall, October 10, 2018"
    ),
    caption = paste0(
      "Residence points are simulated and do not represent actual birth addresses.\n",
      "The shaded area illustrates the spatial component of exposure; analytic exposure also requires\n",
      "the local-impact window to overlap the pregnancy risk period."
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold", size = 17, color = "#172B4D", margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 11, color = "#44546A", margin = margin(b = 10)
    ),
    plot.caption = element_text(
      size = 9, color = "#5B6573", hjust = 0, margin = margin(t = 10)
    ),
    legend.position = c(0.79, 0.18),
    legend.background = element_rect(fill = "white", color = "#CBD2D9"),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 18, 12, 18)
  )

ggsave(
  output_png,
  michael_map,
  width = 11.5,
  height = 7.3,
  units = "in",
  dpi = 400,
  bg = "white"
)
ggsave(
  output_pdf,
  michael_map,
  device = publication_pdf,
  width = 11.5,
  height = 7.3,
  units = "in",
  bg = "white"
)

message("Wrote: ", output_png)
message("Wrote: ", output_pdf)
