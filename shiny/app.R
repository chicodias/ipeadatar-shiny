# Load necessary libraries
library(shiny)
library(ipeadatar)
library(tidyverse)
library(plotly)
library(shinybusy)
library(fpp3)
library(shinyWidgets)

# Retrieve data
datasets <- ipeadatar::available_series("br")
#subject <- ipeadatar::available_subjects(language = c("en","br"))

# UI (User Interface)
ui <- fluidPage(
  navbarPage(
             "Ipeadata Explorer",
             tabPanel("Dashboard",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Aqui você pode escolher entre as bases disponíveis no pacote IpeaDataR"),
                          tableOutput("nameDisplay"),
                          selectizeInput("code", "Escolha a(s) série(s):", choices = unique(datasets$code), multiple = TRUE),
                          pickerInput("freq", "Frequência", choices = unique(datasets$freq), selected = unique(datasets$freq), options = list(`actions-box` = TRUE), multiple = TRUE),
                          pickerInput("theme", "Tema", choices = unique(datasets$theme), selected = unique(datasets$theme), multiple = TRUE),
                          pickerInput("source", "Fonte", choices = unique(datasets$source), selected = unique(datasets$source), multiple = TRUE, options = list(`actions-box` = TRUE)),
                          pickerInput("status", "Status", choices = c("Ativa", "Inativa"), selected = c("Ativa", "Inativa"), multiple = TRUE),
                          downloadButton("downloadData", "Baixar CSV")
                          ),
                        mainPanel(
                          add_busy_bar(),
                          plotlyOutput("seriesPlot"),
                          plotOutput("seasonalPlot"),
                          plotOutput("subseriesPlot"),
                          plotOutput("corrPlot"),
                          plotOutput("lagPlot"),
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

  # Objeto dinâmico que armazena as séries exibidas
  selected_series <- reactive({

    req(input$code)
    ipeadatar::ipeadata(input$code, quiet = T)
  })

  # Armazena as séries que o usuário filtrou
  current_datasets <- reactive({
    req(input$code)
      datasets |> filter(code %in% input$code)
  })

  filtered_series <- reactive({

  })

  # Armazena as séries em forma de ts
  selected_series_ts <- reactive({
    selected_series() %>%
#      mutate(date = yearmonth(date)) %>%
        as_tsibble(index=date, key = code)
  })

  ## Tabela com os datasets
  ## TODO: coluna "Adicionar ao gráfico"
  output$dataTab <- renderDataTable({
    current_datasets()  |>
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


  ## Baixa um csv com as séries selecionadas
  ## FIXME: retorno quando selected_series() é NULL
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("selected_series_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(selected_series(), file, row.names = FALSE)
    }
  )
  ## Tabela da sidebar que exibe as séries selecionadas pelo usuário
  output$nameDisplay <- renderTable({
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


  # armazena as séries disponiveis
  series_info <- reactive({
      req(input$theme, input$source, input$freq, input$status)

  opts <- datasets %>%
      filter(
        theme %in% input$theme,
        source %in% input$source,
        freq %in% input$freq,
        status %in% input$status
      ) |>
      select(code)

      updateSelectizeInput(session = session, inputId = "code", choices = opts, server = TRUE)
  })


  ## Grafico interativo com as séries selecionadas pelo usuário
  output$seriesPlot <- renderPlotly({
    if(is.null(selected_series())) return(NULL)

    plot_ly(data = selected_series(), x = ~date, y = ~value, color = ~code, type = 'scatter', mode = 'lines') %>%
      layout(
        #title = paste("Series:", selected_series() |> select(code) |> distinct()),
        xaxis = list(title = "Data"),
        yaxis = list(title = "Valor")
      )
  })

  # ## escala log
  # output$seriesPlot <- renderPlotly({
  #   if(is.null(selected_series())) return(NULL)

  #   plot_ly(data = selected_series(), x = ~date, y = ~value, color = ~code, type = 'scatter', mode = 'lines') %>%
  #     layout(
  #       #title = paste("Series:", selected_series() |> select(code) |> distinct()),
  #       xaxis = list(title = "Data"),
  #       yaxis = list(title = "Valor")
  #     )
  # })

  ## Correlograma
  output$corrPlot <- renderPlot({
    if(is.null(selected_series())) return(NULL)

    selected_series() %>%
      mutate(date = yearmonth(date)) %>%
      as_tsibble(index=date) %>%
      ACF(value, lag_max=50) %>%
      autoplot()

  })

  output$seasonalPlot <- renderPlot({
    if(is.null(selected_series())) return(NULL)

    selected_series() %>%
      mutate(date = yearmonth(date)) %>%
      as_tsibble(index=date) %>%
      gg_season(value, labels = "both")

  })

  output$subseriesPlot <- renderPlot({
    if(is.null(selected_series())) return(NULL)

    selected_series() %>%
      mutate(date = yearmonth(date)) %>%
      as_tsibble(index=date) %>%
      gg_subseries(value)

  })

  output$lagPlot <- renderPlot({
    if(is.null(selected_series())) return(NULL)

    selected_series() %>%
      mutate(date = yearmonth(date)) %>%
      as_tsibble(index=date) %>%
      gg_lag(value, geom = "point", period = "year")

  })



}

# Run the Shiny app
shinyApp(ui = ui, server = server)
