#!/usr/bin/env Rscript

# Draft Figure 2 for the revised birth-outcomes manuscript.
#
# This map summarizes the corrected individual-level exposure classification
# by residence county for display only. The outcome models continue to use the
# individual birth records rather than county-level exposure prevalence.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
  library(sf)
})

birth_file <- paste0(
  "data/births/processed/",
  "All_Births_Corrected_Storm_Exposure-trimester-eligible-v2.rds"
)
county_file <- paste0(
  "data/births/raw/FL_county_export/",
  "FL_counties_boundaries.gpkg"
)
output_dir <- "figs/births/revised-manuscript"
summary_dir <- "outputs/births"
output_png <- file.path(output_dir, "figure-2-county-exposure-map.png")
output_pdf <- file.path(output_dir, "figure-2-county-exposure-map.pdf")
summary_file <- file.path(summary_dir, "figure-2-county-exposure-summary.csv")

required_files <- c(birth_file, county_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing required input(s): ", paste(missing_files, collapse = ", "))
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)

message("Reading corrected birth-exposure data")
births <- as.data.table(readRDS(birth_file))

required_columns <- c(
  "county_geoid", "county_name",
  paste0("exposure_eligible_T", 1:3),
  paste0("exposed_window_T", 1:3, "_34")
)
missing_columns <- setdiff(required_columns, names(births))
if (length(missing_columns)) {
  stop(
    "Corrected birth data lack required column(s): ",
    paste(missing_columns, collapse = ", ")
  )
}

message("Summarizing exposure by county and trimester")
trimester_names <- c(
  T1 = "First trimester",
  T2 = "Second trimester",
  T3 = "Third trimester"
)

county_summaries <- rbindlist(lapply(seq_len(3L), function(trimester_number) {
  trimester <- paste0("T", trimester_number)
  eligible_column <- paste0("exposure_eligible_", trimester)
  exposure_column <- paste0("exposed_window_", trimester, "_34")

  births[
    get(eligible_column) %in% TRUE & !is.na(county_geoid),
    .(
      eligible_births = .N,
      exposed_births = sum(get(exposure_column) == 1L, na.rm = TRUE)
    ),
    by = .(county_geoid = as.character(county_geoid), county_name)
  ][, `:=`(
    trimester = trimester,
    exposure_prevalence = exposed_births / eligible_births
  )]
}), use.names = TRUE)

statewide_summaries <- county_summaries[, .(
  statewide_eligible_births = sum(eligible_births),
  statewide_exposed_births = sum(exposed_births)
), by = trimester]
statewide_summaries[, statewide_exposure_prevalence :=
                      statewide_exposed_births / statewide_eligible_births]

county_summaries <- statewide_summaries[
  county_summaries,
  on = "trimester"
]
county_summaries[, panel_label := sprintf(
  "%s\nStatewide: %.1f%% exposed",
  unname(trimester_names[trimester]),
  100 * statewide_exposure_prevalence
)]

setcolorder(
  county_summaries,
  c(
    "county_geoid", "county_name", "trimester", "eligible_births",
    "exposed_births", "exposure_prevalence", "statewide_eligible_births",
    "statewide_exposed_births", "statewide_exposure_prevalence",
    "panel_label"
  )
)
setorder(county_summaries, trimester, county_geoid)
fwrite(county_summaries, summary_file)

message("Joining county boundaries")
counties <- st_read(county_file, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(3086)
counties$county_geoid <- as.character(counties$GEOID)

map_data <- merge(
  counties,
  as.data.frame(county_summaries),
  by = "county_geoid",
  all.x = TRUE,
  sort = FALSE
)
map_data <- st_as_sf(map_data)

expected_panels <- unname(trimester_names)
if (nrow(county_summaries) != 3L * 67L ||
    anyNA(county_summaries$exposure_prevalence)) {
  stop("Expected complete exposure summaries for 67 counties and 3 trimesters.")
}

panel_levels <- county_summaries[
  match(names(trimester_names), trimester),
  panel_label
]
map_data$panel_label <- factor(map_data$panel_label, levels = panel_levels)

fill_limits <- range(county_summaries$exposure_prevalence)
fill_limits <- c(
  floor(fill_limits[1] * 100) / 100,
  ceiling(fill_limits[2] * 100) / 100
)

state_outline <- st_union(counties)

exposure_map <- ggplot(map_data) +
  geom_sf(
    aes(fill = exposure_prevalence),
    color = "white",
    linewidth = 0.22
  ) +
  geom_sf(
    data = state_outline,
    fill = NA,
    color = "#273444",
    linewidth = 0.55,
    inherit.aes = FALSE
  ) +
  facet_wrap(vars(panel_label), nrow = 1) +
  scale_fill_gradientn(
    colours = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    limits = fill_limits,
    labels = label_percent(accuracy = 1),
    breaks = pretty_breaks(n = 5),
    oob = squish,
    na.value = "grey88",
    name = "Eligible births\nexposed"
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(
    title = "Tropical-cyclone exposure during pregnancy",
    subtitle = paste0(
      "Residence-specific 34-kt wind-field exposure using a symmetric ",
      "\u00b17-day window around local storm impact, Florida, 2000-2022"
    ),
    caption = paste0(
      "County shading is a descriptive aggregation of individual birth records; ",
      "the outcome models use individual-level exposure."
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold", size = 16, color = "#172B4D", margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 10.5, color = "#44546A", margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 9, color = "#5B6573", hjust = 0, margin = margin(t = 10)
    ),
    strip.text = element_text(
      face = "bold", size = 11.5, color = "#172B4D",
      margin = margin(b = 6)
    ),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.spacing.x = grid::unit(1.2, "lines"),
    plot.margin = margin(15, 18, 12, 18)
  )

ggsave(
  output_png,
  exposure_map,
  width = 14,
  height = 5.8,
  units = "in",
  dpi = 400,
  bg = "white"
)
ggsave(
  output_pdf,
  exposure_map,
  width = 14,
  height = 5.8,
  units = "in",
  bg = "white"
)

message("Wrote: ", output_png)
message("Wrote: ", output_pdf)
message("Wrote: ", summary_file)
