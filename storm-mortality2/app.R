# app.R
library(shiny)
library(leaflet)
library(sf)
library(dplyr)

rds_path <- "zcta_results_simplified.rds"  # or your original rds

ui <- fluidPage(
  titlePanel("Relative All-Cause Mortality Rates by ZCTA (click a region)"),
  fluidRow(
    column(8, leafletOutput("map", height = 650)),
    column(4,
           checkboxInput("show_ci", "Show 95% Confidence Intervals", TRUE),
           tags$hr(),
           uiOutput("selected_zcta"),
           tableOutput("rr_table"),
           tags$hr()
    )
  )
)

server <- function(input, output, session) {
  
  # Auto-reload the RDS if it changes
  zcta_data <- reactiveFileReader(2000, session, rds_path, function(p) {
    dat <- readRDS(p)
    
    # --- Make sure polygons are valid, polygonal, and WGS84 ---
    dat <- st_make_valid(dat)
    
    # Keep only polygonal features; drop collections/empties
    dat <- dat %>%
      mutate(.gtype = st_geometry_type(geometry)) %>%
      filter(.gtype %in% c("POLYGON", "MULTIPOLYGON")) %>%
      select(-.gtype) %>%
      filter(!st_is_empty(geometry))
    
    # CRS for Leaflet
    if (is.na(st_crs(dat)) || st_crs(dat)$epsg != 4326) {
      dat <- st_transform(dat, 4326)
    }
    
    # --- Ensure layerId is clean character (no NAs) ---
    if (!"GEOID20" %in% names(dat)) stop("GEOID20 not found in data.")
    dat <- dat %>% mutate(GEOID20 = as.character(GEOID20)) %>% filter(!is.na(GEOID20))
    
    dat
  })
  
  selected <- reactiveVal(NULL)
  
  output$map <- renderLeaflet({
    dat <- zcta_data()
    
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = dat,
        layerId = ~GEOID20,                 # <-- crucial for click id
        weight = 0.6, color = "#666",
        fill = TRUE, fillColor = "#bcd4e6", fillOpacity = 0.5,
        label = ~sprintf("ZCTA %s", GEOID20),
        options = pathOptions(interactive = TRUE),  # <-- ensure clickable
        highlightOptions = highlightOptions(
          weight = 2, color = "#000", fillOpacity = 0.7, bringToFront = TRUE
        ),
        group = "zctas"
      ) %>%
      addScaleBar(position = "bottomleft")
  })
  
  # Click handler
  observeEvent(input$map_shape_click, {
    evt <- input$map_shape_click
    req(evt$id)
    
    selected(evt$id)
    
    # Draw an outline around the selected polygon (don’t reset data on proxy)
    dat <- zcta_data()
    sel <- dat %>% filter(GEOID20 == evt$id)
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("selected_poly")
    if (nrow(sel)) {
      proxy %>% addPolygons(
        data = sel,
        group = "selected_poly",
        weight = 3, color = "#000",
        fill = FALSE, opacity = 1
      )
    }
  })
  
  output$rr_table <- renderTable({
    id <- selected(); req(id)
    
    row <- zcta_data() %>% sf::st_drop_geometry() %>% dplyr::filter(GEOID20 == id)
    if (!nrow(row)) return(NULL)
    
    base <- tibble::tibble(
      Storm = c("Threat", "Impact", "Cleanup"),
      RR      = c(row$RR_Threat, row$RR_Impact, row$RR_Cleanup),
      SE_RR   = c(row$SE_Threat, row$SE_Impact, row$SE_Cleanup),
      SE_log  = c(row$SElog_Threat, row$SElog_Impact, row$SElog_Cleanup)
    )
    
    # Always compute CI columns; show/hide later
    zcrit <- 1.96
    with_ci <- base %>%
      dplyr::mutate(
        lower = ifelse(!is.na(RR) & !is.na(SE_log), exp(log(RR) - zcrit * SE_log), NA_real_),
        upper = ifelse(!is.na(RR) & !is.na(SE_log), exp(log(RR) + zcrit * SE_log), NA_real_)
      ) %>%
      dplyr::mutate(
        RR    = round(RR, 3),
        SE_RR = round(SE_RR, 3),
        lower = round(lower, 3),
        upper = round(upper, 3)
      )
    
    if (isTRUE(input$show_ci)) {
      dplyr::select(with_ci, Storm, RR, SE_RR, lower, upper)
    } else {
      dplyr::select(with_ci, Storm, RR, SE_RR)
    }
  })
}

shinyApp(ui, server)

