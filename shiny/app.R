# Load necessary libraries
library(shiny)
library(ipeadatar)
library(tidyverse)
library(plotly)
library(shinybusy)
library(fpp3)
library(shinyWidgets)
library(DT)

# Retrieve data
# write_csv(ipeadatar::available_series("br"), "datasets.csv")

datasets <- read_csv("datasets.csv")
#subject <- ipeadatar::available_subjects(language = c("en","br"))

# UI (User Interface)
ui <- fluidPage(
  navbarPage(
             "Ipeadata Explorer",
             tabPanel("Explorador",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Aqui você pode escolher entre as bases disponíveis no pacote IpeaDataR"),
                          textOutput("nSeries"),
                          selectizeInput("code", "Escolha a(s) série(s):", choices = NULL, multiple = TRUE),
                          pickerInput("freq", "Frequência", choices = unique(datasets$freq), selected = unique(datasets$freq), options = list(`actions-box` = TRUE), multiple = TRUE),
                          pickerInput("theme", "Tema", choices = unique(datasets$theme), selected = unique(datasets$theme), multiple = TRUE),
                          pickerInput("source", "Fonte", choices = unique(datasets$source), selected = unique(datasets$source), multiple = TRUE, options = list(`actions-box` = TRUE)),
                          pickerInput("status", "Status", choices = c("Ativa", "Inativa"), selected = c("Ativa", "Inativa"), multiple = TRUE),
                          downloadButton("downloadData", "Baixar CSV"),
                          ),
                        mainPanel(
                        DT::dataTableOutput("dataTab")
                        )
                      )
                      ),
             tabPanel("Modelagem",
                      sidebarLayout(
                        sidebarPanel(
                          tableOutput("nameDisplay"),
                          helpText("Aqui você pode escolher entre as bases disponíveis no pacote IpeaDataR"),
                          sliderInput("lambda", "Selecione lambda de box-cox", min= -2, max = 2, step = 0.5, value = 1)
                          ),
                      mainPanel(
                        add_busy_spinner(spin = "fading-circle"),
                        plotlyOutput("seriesPlot"),
                        h1("Correlograma"),
                        plotlyOutput("corrPlot"),
                        h1("Sazonal"),
                        plotlyOutput("seasonalPlot"),
                        h1("Subséries"),
                        plotlyOutput("subseriesPlot"),
                        h1("Lag plot"),
                        plotlyOutput("lagPlot"),
                        )
                      )
                      )
             )
)

# Server logic
server <- function(input, output, session) {

  # Objeto dinâmico que armazena as séries exibidas
  selected_series <- reactive({
    req(input$code)
    dados <- ipeadatar::ipeadata(input$code, quiet = T) |>
      mutate(value = box_cox(value, input$lambda))
#      group_by(code) |> mutate(value =)

  })

  eventReactive(input$lambda, {
    selected_series <- {
      req(input$code)
      dados <- ipeadatar::ipeadata(input$code, quiet = T) |>
        mutate(value = box_cox(value, input$lambda))
    }
  })

  # infos da série selecionada
  series_data <- reactive({
    req(input$code)
    datasets |>
      filter(code == input$code)
  })


  # armazena as séries disponiveis
  series_info <- reactive({
      req(input$theme, input$source, input$freq, input$status)

      opts <- datasets |>
      filter(
        theme %in% input$theme,
        source %in% input$source,
        freq %in% input$freq,
        status %in% input$status
      )

      updateSelectizeInput(session = session, inputId = "code", choices = opts$code, server = TRUE)
      opts
  })


  # Armazena as séries que o usuário filtrou
  current_datasets <- reactive({
    req(input$code)
      datasets |> filter(code %in% input$code)
  })

  # Armazena as séries em forma de ts
  selected_series_ts <- reactive({
    selected_series() |>
      select(!c("uname", "tcode")) |>
#      mutate(date = yearmonth(date)) %>%
        as_tsibble(index=date)
  })

  ## Tooltip com o numero de séries que atendam aos filtros
  output$nSeries <- renderText({
    len <- series_info()$code |> unique() |> length()

    paste(c("Total de séries disponíveis:",len))
  })

  ## Tabela com os datasets
  ## TODO: coluna "Adicionar ao gráfico"
  output$dataTab <- DT::renderDataTable({
    series_info()  |>
      rename(
        `Código` = code,
        `Nome` = name,
        `Fonte` = source,
        `Última atualização` = lastupdate,
        `Tema` = theme,
        `Frequência` = freq,
        `Status` = status
        )
  }, selection = 'single')

  ## Observador que inclui uma célula da tabela no input de código da série
  observeEvent(input$dataTab_row_last_clicked, {
    rowNum <- input$dataTab_row_last_clicked
    if(is.null(rowNum)) return(NULL)
    selectedCode <- series_info() |> slice(rowNum) |> pull(code) |> as.character()


    updateSelectizeInput(session = session, inputId = "code",
                         choices = series_info()$code, selected = selectedCode, server = TRUE)
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
    series_data() |> select(code, name, source, lastupdate) |>
      mutate_if(is.Date,~format(.,"%d-%m-%Y"))  |>
      rename(
          `Código` = code,
          `Nome` = name,
          `Fonte` = source,
          `Última atualização` = lastupdate
        )

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

  ## Decomp STL da série de acordo com janela e parametros
  output$stlPlot <- renderPlotly({

    selected_series_ts() |>
    model(
      STL(value ~ trend(window = 7) +
            season(window = "periodic"),
          robust = TRUE)) |>
      components() |>
      autoplot() |>
      ggplotly()

  })
  
  ## Correlograma
  output$corrPlot <- renderPlotly({
    if(is.null(selected_series())) return(NULL)

    selected_series() %>%
      mutate(date = yearmonth(date)) %>%
      as_tsibble(index=date) %>%
      ACF(value, lag_max=50) %>%
      autoplot()  |>
      ggplotly()


  })

  output$seasonalPlot <- renderPlotly({
    if(is.null(selected_series())) return(NULL)

    selected_series() %>%
      mutate(date = yearmonth(date)) %>%
      as_tsibble(index=date) %>%
      gg_season(value, labels = "both") |>
      ggplotly()


  })

  output$subseriesPlot <- renderPlotly({
    if(is.null(selected_series())) return(NULL)

    selected_series() %>%
      mutate(date = yearmonth(date)) %>%
      as_tsibble(index=date) %>%
      gg_subseries(value) |>
      ggplotly()


  })

  output$lagPlot <- renderPlotly({
    if(is.null(selected_series())) return(NULL)

    selected_series() %>%
      mutate(date = yearmonth(date)) %>%
      as_tsibble(index=date) %>%
      gg_lag(value, geom = "point", period = "year") |>
      ggplotly()
    
  })



}

# Run the Shiny app
shinyApp(ui = ui, server = server)
