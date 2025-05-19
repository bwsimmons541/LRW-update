# app.R

library(shiny)
library(readr)
library(ggplot2)
library(flextable)
source("R/report_helpers.R")

# Preload available years from the estimates file
available_years <- read_csv("data/yearly_estimates.csv")$year |> unique() |> sort(decreasing = TRUE)

ui <- fluidPage(
  titlePanel("Lostine River Weir - Chinook Summary"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Trap Year:", choices = available_years),
      downloadButton("download_report", "Download PDF Report")
    ),
    mainPanel(
      h3("Summary Plot"),
      plotOutput("megaplot"),
      h3("Disposition Table (Sample)"),
      tableOutput("sample_table")  # Replace with dynamic flextable/gt later
    )
  )
)

server <- function(input, output, session) {
  # Reactive data loading
  selected_year <- reactive({ as.integer(input$year) })
  
  weir_df <- reactive({
    clean_weir_data(load_weir_data())
  })
  
  grsme_df <- reactive({
    weir_df() |> filter(facility == "NPT GRSME Program", trap_year == selected_year())
  })
  
  flow_df <- reactive({
    prepare_flow_data(selected_year())
  })
  
  catch_df <- reactive({
    prepare_catch_data(grsme_df(), selected_year())
  })
  
  output$megaplot <- renderPlot({
    generate_lrw_megaplot(flow_df(), catch_df(), selected_year())
  })
  
  output$sample_table <- renderTable({
    estimates <- load_yearly_estimates(selected_year())
    tibble(
      Estimate_Date = estimates$estimate_date,
      Nat_Adults = estimates$nat_adults,
      Hat_Adults = estimates$hat_adults,
      Brood_N_Goal = estimates$n_brood_goal,
      Brood_H_Goal = estimates$h_brood_goal
    )
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Lostine_Report_", selected_year(), ".pdf")
    },
    content = function(file) {
      quarto::quarto_render(
        input = "documents/LRW-Weekly-Chinook-Summary.qmd",
        output_file = file,
        execute_params = list(trap_year = selected_year()),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)