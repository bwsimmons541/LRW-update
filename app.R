# app.R

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(readr)
library(dplyr)
library(ggplot2)
library(flextable)
library(htmltools)
library(lubridate)
library(cuyem)

# Source your helper functions
source("R/report_helpers.R")

# Preload available years
available_years <- read_csv("data/yearly_estimates.csv")$year |> 
  unique() |> 
  sort(decreasing = TRUE)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Lostine River Weir - Chinook Summary"),
  
  dashboardSidebar(
    selectInput("year", "Select Trap Year:", 
                choices = available_years, 
                selected = max(available_years)),
    br(),
    downloadButton("download_pdf", "Download PDF Report", 
                   class = "btn-primary"),
    br(), br(),
    downloadButton("download_html", "Download HTML Report", 
                   class = "btn-info"),
    br(), br(),
    p("Last Data Update:", style = "font-size: 12px; color: gray;"),
    textOutput("last_update")
  ),
  
  dashboardBody(
    # Custom CSS to match your report styling
    tags$head(
      tags$style(HTML("
        .main-header .navbar {
          background-color: #2c3e50 !important;
        }
        .content-wrapper {
          background-color: #ecf0f1;
        }
        .box {
          border-top-color: #2c3e50;
        }
        .no-data-message {
          font-style: italic;
          text-align: center;
          padding: 20px;
          background-color: #f8f9fa;
          border: 1px solid #dee2e6;
          border-radius: 5px;
          margin: 10px 0;
        }
      "))
    ),
    
    fluidRow(
      # Header with NPT logo (if available)
      column(12,
             box(width = 12, status = "primary",
                 div(style = "text-align: center;",
                     h1("Lostine River Weir"),
                     h3(paste("Weekly Chinook Summary:", format(Sys.Date(), "%B %d, %Y")))
                 )
             )
      )
    ),
    
    fluidRow(
      # Disposition Summary
      column(12,
             box(width = 12, title = "Disposition Summary", status = "primary", solidHeader = TRUE,
                 htmlOutput("disposition_summary")
             )
      )
    ),
    
    fluidRow(
      # Tables
      column(6,
             box(width = 12, title = "Hatchery Chinook Disposition", status = "info", solidHeader = TRUE,
                 htmlOutput("hatchery_table")
             )
      ),
      column(6,
             box(width = 12, title = "Natural Chinook Disposition", status = "info", solidHeader = TRUE,
                 htmlOutput("natural_table")
             )
      )
    ),
    
    fluidRow(
      # Broodstock table
      column(12,
             box(width = 12, title = "Broodstock Collection Summary", status = "info", solidHeader = TRUE,
                 htmlOutput("broodstock_table")
             )
      )
    ),
    
    fluidRow(
      # Main plot
      column(12,
             box(width = 12, title = "Current and Historic Catch and River Flows", status = "success", solidHeader = TRUE,
                 plotlyOutput("megaplot", height = "600px")
             )
      )
    ),
    
    fluidRow(
      # NOAA link section
      column(12,
             box(width = 12, title = "Current River Conditions", status = "warning", solidHeader = TRUE,
                 div(style = "text-align: center; padding: 20px;",
                     h4("🌊 Real-Time River Data"),
                     p("Current gauge shows both observed flow data and official 7-day forecasts for the Lostine River above Lostine (NWSLI: LSTO3)."),
                     br(),
                     actionButton("noaa_link", "View Live NOAA Gauge Data & Forecast", 
                                  onclick = "window.open('https://water.noaa.gov/gauges/lsto3', '_blank')",
                                  class = "btn-primary btn-lg"),
                     br(), br(),
                     p("Click above for interactive graphs, current readings, and flood predictions", 
                       style = "font-style: italic; color: gray;")
                 )
             )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values
  selected_year <- reactive({ as.integer(input$year) })
  
  # Load data reactively
  estimates_data <- reactive({
    load_yearly_estimates(selected_year())
  })
  
  trap_data <- reactive({
    # Use simplified function - only reads from FINS CSV
    get_trap_data(trap.year = selected_year())
  })
  
  grsme_df <- reactive({
    trap_data()$grsme_df
  })
  
  dispositions_data <- reactive({
    calculate_dispositions(grsme_df(), selected_year())
  })
  
  mega_data <- reactive({
    prepare_megadf(
      trap_year = selected_year(),
      grsme_df = grsme_df(),
      weir_data_clean = trap_data()$AdultWeirData_clean
    )
  })
  
  broodstock_data <- reactive({
    sumGRSMEbrood(data = grsme_df(), trap.year = selected_year())
  })
  
  # Last update time
  output$last_update <- renderText({
    if(file.exists("data/TrappingData.csv")) {
      file_time <- file.info("data/TrappingData.csv")$mtime
      format(file_time, "%m/%d/%Y %H:%M")
    } else {
      "No data file found"
    }
  })
  
  # Disposition Summary
  output$disposition_summary <- renderUI({
    estimates <- estimates_data()
    dispositions <- dispositions_data()
    
    HTML(paste(
      "<ul>",
      paste0("<li>", estimates$type, " adult return-to-tributary estimates were updated on ", 
             estimates$estimate_date, " to ", estimates$nat_adults, " natural-origin and ", 
             estimates$hat_adults, " hatchery-origin adults.</li>"),
      paste0("<li>Brood stock collection goals are ", estimates$n_brood_goal, " natural-origin adults, ", 
             estimates$h_brood_goal, " hatchery-origin adults, and ", estimates$hj_brood_goal, " hatchery-origin jacks.</li>"),
      paste0("<li>Composition of adults passed upstream: ", dispositions$h_upstream_calc, "% Hatchery (Sliding scale goal ≤ ", estimates$ss_upstream, ")</li>"),
      paste0("<li>Composition of adults kept for brood: ", dispositions$n_brood_calc, "% Natural (Sliding scale goal ≥ ", estimates$ss_brood, ")</li>"),
      "</ul>"
    ))
  })
  
  # Helper function to create safe HTML tables
  create_safe_table <- function(data, table_type, trap_year) {
    if(is.null(data) || nrow(data) == 0) {
      message <- case_when(
        table_type == "hatchery" ~ paste0("There is currently no data available for the capture of hatchery-origin Chinook for ", trap_year, "."),
        table_type == "natural" ~ paste0("There is currently no data available for the capture of natural-origin Chinook for ", trap_year, "."),
        table_type == "broodstock" ~ paste0("There is currently no broodstock collection data available for ", trap_year, "."),
        TRUE ~ paste0("There is currently no data available for ", trap_year, ".")
      )
      return(HTML(paste0('<div class="no-data-message">', message, '</div>')))
    }
    
    # Check if all numeric values are zero (for disposition tables)
    if(table_type %in% c("hatchery", "natural") && nrow(data) > 0) {
      count_cols <- data[, 2:ncol(data), drop = FALSE]
      numeric_values <- c()
      for(i in 1:nrow(count_cols)) {
        for(j in 1:ncol(count_cols)) {
          cell_value <- as.character(count_cols[i, j])
          numbers <- as.numeric(unlist(regmatches(cell_value, gregexpr("\\d+", cell_value))))
          numeric_values <- c(numeric_values, numbers)
        }
      }
      numeric_values <- numeric_values[!is.na(numeric_values)]
      if(length(numeric_values) > 0 && all(numeric_values == 0)) {
        message <- paste0("There is currently no data available for the capture of ", 
                          ifelse(table_type == "hatchery", "hatchery", "natural"), 
                          "-origin Chinook for ", trap_year, ".")
        return(HTML(paste0('<div class="no-data-message">', message, '</div>')))
      }
    }
    
    # Create DT table for display
    datatable(data, options = list(
      dom = 't',  # Only show table
      pageLength = -1,  # Show all rows
      ordering = FALSE,
      searching = FALSE,
      info = FALSE
    )) |> 
      formatStyle(columns = 1:ncol(data), textAlign = 'center')
  }
  
  # Disposition Tables
  output$hatchery_table <- renderUI({
    dispositions <- dispositions_data()
    create_safe_table(dispositions$h_df, "hatchery", selected_year())
  })
  
  output$natural_table <- renderUI({
    dispositions <- dispositions_data()
    create_safe_table(dispositions$n_df, "natural", selected_year())
  })
  
  output$broodstock_table <- renderUI({
    brood_data <- broodstock_data()
    create_safe_table(brood_data, "broodstock", selected_year())
  })
  
  # Main plot
  output$megaplot <- renderPlotly({
    mega_list <- mega_data()
    
    plot <- generate_lrw_megaplot(
      megadf = mega_list$lrw_megadf,
      lrw_catch = mega_list$lrw_megadf |> filter(facet == as.character(selected_year())),
      save_plot = FALSE
      # No output_path needed since save_plot = FALSE
    )
    
    ggplotly(plot, tooltip = c("x", "y")) |>
      layout(showlegend = TRUE)
  })
  
  # Download handlers
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("Lostine_Report_", selected_year(), "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Create temporary qmd file with current parameters
      temp_qmd <- tempfile(fileext = ".qmd")
      
      # Read the original qmd and modify parameters
      qmd_content <- readLines("documents/LRW-Weekly-Chinook-Summary.qmd")
      
      # Update the trap_year parameter
      qmd_content <- gsub(
        "trap_year: !expr as\\.numeric\\(format\\(Sys\\.Date\\(\\), \"%Y\"\\)\\)",
        paste0("trap_year: ", selected_year()),
        qmd_content
      )
      
      writeLines(qmd_content, temp_qmd)
      
      # Render to PDF
      quarto::quarto_render(
        input = temp_qmd,
        output_file = file,
        output_format = "pdf"
      )
    }
  )
  
  output$download_html <- downloadHandler(
    filename = function() {
      paste0("Lostine_Report_", selected_year(), "_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Similar process for HTML
      temp_qmd <- tempfile(fileext = ".qmd")
      qmd_content <- readLines("documents/LRW-Weekly-Chinook-Summary.qmd")
      qmd_content <- gsub(
        "trap_year: !expr as\\.numeric\\(format\\(Sys\\.Date\\(\\), \"%Y\"\\)\\)",
        paste0("trap_year: ", selected_year()),
        qmd_content
      )
      writeLines(qmd_content, temp_qmd)
      
      quarto::quarto_render(
        input = temp_qmd,
        output_file = file,
        output_format = "html"
      )
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)