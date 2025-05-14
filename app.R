#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)

# Load years once for UI dropdown
years <- read_csv("../data/yearly_estimates.csv")$year |> unique() |> sort(decreasing = TRUE)

ui <- fluidPage(
  titlePanel("Lostine River Weir – Weekly Chinook Summary"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Trap Year:", choices = years),
      downloadButton("report", "Download PDF Report")
    ),
    mainPanel(
      tags$p("Select a year and download the corresponding PDF report.")
    )
  )
)

server <- function(input, output, session) {
  
  output$report <- downloadHandler(
    filename = function() {
      paste0("Lostine_Report_", input$year, ".pdf")
    },
    content = function(file) {
      quarto::quarto_render(
        input = "LRW-Weekly-Chinook-Summary.qmd",
        output_file = file,
        execute_params = list(trap_year = as.integer(input$year)),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)