# Load necessary libraries
library(shiny)
library(ipeadatar)
library(ggplot2)
library(dplyr)

# Retrieve data
datasets <- ipeadatar::available_series("br")

# UI (User Interface)
ui <- fluidPage(
  titlePanel("Explore Ipeadata Series"),
  sidebarLayout(
    sidebarPanel(
      textOutput("nameDisplay"),
      selectInput("series", "Choose a Series:", datasets$code),
      selectInput("theme", "Theme", choices = unique(datasets$theme)),
      selectInput("source", "Source", choices = unique(datasets$source), multiple = TRUE),
      selectInput("freq", "Frequency", choices = unique(datasets$freq)),
      radioButtons("status", "Status", choices = c("Active" = "active", "Inactive" = "inactive"))
    ),
    mainPanel(
      plotOutput("seriesPlot")
    )
  )
)

# Server logic
server <- function(input, output) {

## Temas <- unique(datasets$theme)
  ## Source <- unique(datasets$source)
  ## Frequencia <- unique(datasets$freq)

  output$nameDisplay <- renderText({
    # Example: Display series code as name
    # You should replace this with actual logic to get the 'name' variable.
    input$series
  })

  selected_series <- reactive({
    series_data <- datasets %>%
      filter(
        code == input$series,
        ## theme %in% input$theme,
        ## source %in% input$source,
        ## freq == input$freq,
        ## status == input$status
      )
    if(nrow(series_data) > 0) ipeadatar::ipeadata(series_data$code) else NULL
  })

  output$seriesPlot <- renderPlot({
    if(is.null(selected_series())) return(NULL)
    ggplot(selected_series(), aes(x = date, y = value, color = tcode)) +
      geom_line() +
      labs(
        title = paste("Series:", input$series),
        x = "Data",
        y = "Value"
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
