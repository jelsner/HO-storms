library(shiny)
library(dplyr)
library(readr)
library(sf)

rds_path <- "zcta_results.rds"   # path to your saved object

ui <- fluidPage(
  titlePanel("Relative Mortality Rates by ZCTA"),
  sidebarLayout(
    sidebarPanel(
      textInput("zcta", "Enter ZCTA (5 digits)", value = "32301"),
      helpText("Example: 32301. ZCTAs are Census approximations of USPS ZIP Codes."),
      hr(),
      checkboxInput("show_ci", "Show 95% Confidence Intervals", value = TRUE)
    ),
    mainPanel(
      h4(textOutput("which_zcta")),
      tableOutput("rr_table"),
      uiOutput("not_found")
    )
  )
)

server <- function(input, output, session) {
  # Auto-reload the RDS if it changes
  zcta_data <- reactiveFileReader(2000, session, rds_path, readRDS)
  
  # Normalize user input to 5-character GEOID (left-pad if needed)
  norm_geo <- reactive({
    z <- gsub("\\D", "", input$zcta)
    if (nchar(z) == 5) z else sprintf("%05s", z)
  })
  
  output$which_zcta <- renderText({
    paste("ZCTA:", norm_geo())
  })
  
  output$rr_table <- renderTable({
    dat <- zcta_data()
    geo <- norm_geo()
    
    row <- dat %>%
      st_drop_geometry() %>%
      filter(GEOID20 == geo)
    
    if (nrow(row) == 0) return(NULL)
    
    # Build a tidy table
    out <- tibble::tibble(
      Measure = c("Threat", "Impact", "Cleanup"),
      RR      = c(row$RR_Threat, row$RR_Impact, row$RR_Cleanup),
      SE_RR   = c(row$SE_Threat, row$SE_Impact, row$SE_Cleanup),
      SE_log  = c(row$SElog_Threat, row$SElog_Impact, row$SElog_Cleanup)
    )
    
    # Optionally compute 95% CIs on the RR scale using log-normal assumption
    if (isTRUE(input$show_ci)) {
      z <- 1.96
      out <- out %>%
        mutate(
          lower = ifelse(!is.na(RR) & !is.na(SE_log), exp(log(RR) - z * SE_log), NA_real_),
          upper = ifelse(!is.na(RR) & !is.na(SE_log), exp(log(RR) + z * SE_log), NA_real_)
        )
    }
    
    # Nice rounding for display
    out %>%
      mutate(
        RR    = round(RR, 3),
        SE_RR = round(SE_RR, 3),
        lower = round(lower, 3),
        upper = round(upper, 3)
      )
  })
  
  output$not_found <- renderUI({
    dat <- zcta_data()
    geo <- norm_geo()
    if (nrow(dat %>% st_drop_geometry() %>% filter(GEOID20 == geo)) == 0) {
      tags$div(style = "color:#a00; font-weight:bold; margin-top:10px;",
               "ZCTA not found in results (no model fit / insufficient data).")
    } else {
      NULL
    }
  })
}

shinyApp(ui, server)
