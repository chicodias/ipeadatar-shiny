# Load necessary libraries
library(shiny)
library(ipeadatar)
library(tidyverse)
library(plotly)
library(shinybusy)

# Retrieve data
datasets <- ipeadatar::available_series("br")
#subject <- ipeadatar::available_subjects(language = c("en","br"))

# UI (User Interface)
ui <- fluidPage(
  add_busy_bar(),
  navbarPage(
             "Ipeadata Explorer",
             tabPanel("Dashboard",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Aqui você pode escolher entre as bases disponíveis no pacote IpeaDataR"),
                          tableOutput("nameDisplay"),
                          selectizeInput("code", "Escolha a(s) série(s):", unique(datasets$code), multiple = TRUE),
                          selectizeInput("freq", "Frequência", choices = unique(datasets$freq), selected = unique(datasets$freq)),
                          selectizeInput("theme", "Tema", choices = unique(datasets$theme), selected = unique(datasets$theme), multiple = TRUE),
                          selectizeInput("source", "Fonte", choices = unique(datasets$source), selected = unique(datasets$theme), multiple = TRUE),
                          selectizeInput("status", "Status", choices = c("Ativa", "Inativa"), selected = c("Ativa", "Inativa")),
                          ),
                        mainPanel(
                          plotlyOutput("seriesPlot"),
                          downloadButton("downloadData", "Baixar CSV")
                        )
                      )
                      ),
             tabPanel("Explorador",
                      mainPanel(
                        dataTableOutput("dataTab"),
                        )
                      )
             )
)

# Server logic
server <- function(input, output, session) {

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("selected_series_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(selected_series(), file, row.names = FALSE)
    }
  )

## Temas <- unique(datasets$theme)
  ## Source <- unique(datasets$source)
  ## Frequencia <- unique(datasets$freq)
  output$dataTab <- renderDataTable({
    datasets  |>
      rename(
        `Código` = code,
        `Nome` = name,
        `Fonte` = source,
        `Última atualização` = lastupdate,
        `Tema` = theme,
        `Frequência` = freq,
        `Status` = status
        )
  })

  output$nameDisplay <- renderTable({
    # Example: Display series code as name
    # You should replace this with actual logic to get the 'name' variable.
    req(input$code)
    datasets |> filter(code %in% input$code) |> select(code, name, source, lastupdate) |>
      mutate_if(is.Date,~format(.,"%d-%m-%Y"))  |>
      rename(
          `Código` = code,
          `Nome` = name,
          `Fonte` = source,
          `Última atualização` = lastupdate
        )
  })

  selected_series <- reactive({
    req(input$code)
    ipeadatar::ipeadata(input$code)
  })

  ## observe({
  ##     req(input$theme, input$source, input$freq, input$status)
  ##   codigos <- datasets %>%
  ##     filter(
  ##       theme %in% input$theme,
  ##       source %in% input$source,
  ##       freq %in% input$freq,
  ##       status %in% input$status
  ##     ) |> select(code)
  ##     updateSelectizeInput(session, "code", choices = codigos)
  ## })

  output$seriesPlot <- renderPlotly({
    if(is.null(selected_series())) return(NULL)

    plot_ly(data = selected_series(), x = ~date, y = ~value, color = ~code, type = 'scatter', mode = 'lines') %>%
      layout(
        #title = paste("Series:", selected_series() |> select(code) |> distinct()),
        xaxis = list(title = "Data"),
        yaxis = list(title = "Valor")
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
