# Load necessary libraries
library(shiny)
library(ipeadatar)
library(ggplot2)

# Retrieve data
datasets <- ipeadatar::available_series("br")

# UI (User Interface)
ui <- fluidPage(
  titlePanel("Explore Ipeadata Series"),
  sidebarLayout(
    sidebarPanel(
      selectInput("series", "Choose a Series:", datasets$code)
    ),
    mainPanel(
      plotOutput("seriesPlot")
    )
  )
)

# Server logic
server <- function(input, output) {
  output$seriesPlot <- renderPlot({
    selected_series_code <- input$series
    selected_series <- ipeadatar::ipeadata(selected_series_code)

    ggplot(selected_series, aes(x = date, y = value, color = tcode)) +
      geom_line() +
      labs(
        title = paste("Series:", selected_series_code),
        x = "Data",
        y = "Value"
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
