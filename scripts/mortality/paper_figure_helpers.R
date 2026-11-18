suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(grid)
  library(scales)
})

paper_figure_version <- "environmental_research_main_v1_2026-11-18"
paper_figure_dir <- file.path("figs/mortality", paper_figure_version)
paper_figure_output_dir <- file.path("outputs/mortality", paper_figure_version)
paper_results_path <- paste0(
  "outputs/mortality/paper_master_34kt/frozen_v1_2026-11-17/",
  "master_results_34kt_frozen_v1.csv"
)

dir.create(paper_figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(paper_figure_output_dir, recursive = TRUE, showWarnings = FALSE)

paper_period_levels <- c(
  "Days -14 to -8",
  "Days -7 to -1",
  "Days 0 to +6",
  "Days +7 to +13"
)
paper_age_levels <- c("All ages", "<65", "65-74", "75+")
paper_age_specific_levels <- c("<65", "65-74", "75+")
paper_age_colors <- c(
  "<65" = "#0072B2",
  "65-74" = "#D55E00",
  "75+" = "#6A3D9A"
)
paper_age_shapes <- c("<65" = 16, "65-74" = 17, "75+" = 15)
paper_tier_colors <- c(
  "Prioritized secondary" = "#0072B2",
  "Exploratory" = "#C05A00"
)

paper_theme <- function(base_size = 10) {
  theme_minimal(base_family = "Helvetica", base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.45), margin = margin(b = 4)),
      plot.subtitle = element_text(size = rel(1.0), margin = margin(b = 8)),
      plot.caption = element_text(size = rel(0.78), color = "grey30", hjust = 0),
      strip.text = element_text(face = "bold", size = rel(1.0), lineheight = 1.0),
      strip.background = element_rect(fill = "grey94", color = "grey82"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

read_paper_results <- function() {
  if (!file.exists(paper_results_path)) {
    stop("Missing frozen results: ", paper_results_path)
  }
  results <- fread(paper_results_path)
  if (nrow(results) != 176L || uniqueN(results$result_id) != 176L) {
    stop("Frozen result row or identifier count is not 176.")
  }
  if (unique(results$analysis_version) != "frozen_v1_2026-11-17") {
    stop("Unexpected frozen analysis version.")
  }
  results
}

save_paper_plot <- function(plot, stem, width, height, dpi = 400) {
  png_path <- file.path(paper_figure_dir, paste0(stem, ".png"))
  pdf_path <- file.path(paper_figure_dir, paste0(stem, ".pdf"))

  ggsave(
    png_path,
    plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
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

  invisible(c(png_path, pdf_path))
}

format_q_value <- function(value) {
  fifelse(
    is.na(value),
    "q not available",
    fifelse(value < 0.001, "q<0.001", sprintf("q=%.3f", value))
  )
}

format_period_short <- function(value) {
  fcase(
    value == "Days -14 to -8", "-14 to -8",
    value == "Days -7 to -1", "-7 to -1",
    value == "Days 0 to +6", "0 to +6",
    value == "Days +7 to +13", "+7 to +13",
    default = value
  )
}

