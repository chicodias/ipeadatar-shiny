# Server logic
server <- function(input, output, session){
  ## Explorador
  # Objeto dinâmico que armazena todas as séries disponiveis que atendam aos filtros
  all_series <- reactive({
      req(input$theme, input$source, input$freq, input$status)

      opts <- datasets |>
      filter(
        theme %in% input$theme,
        source %in% input$source,
        freq %in% input$freq,
        status %in% input$status
      )
      # Atualiza o input do código da série
      updateSelectizeInput(session = session, inputId = "code", choices = opts$code, server = TRUE)
      opts |> arrange(desc(lastupdate))
  })

  ## Tooltip com o total de séries
  output$nSeries <- renderText({
    len <- all_series()$code |> unique() |> length()

    paste(c("Total de séries disponíveis:", len))
  })



  # Armazena os dados obtidos externamente e informaçoes
  dados <- reactiveValues(df = NULL, info = NULL)


  ## Tabela com os datasets
  ## TODO: coluna "Adicionar ao gráfico"
  output$dataTab <- DT::renderDataTable({
    all_series()  |>
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

  ## Observador que inclui a célula da tabela no input de código da série
  observeEvent(input$dataTab_row_last_clicked, {
    rowNum <- input$dataTab_row_last_clicked
    if(is.null(rowNum)) return(NULL)
    selectedCode <- all_series() |> slice(rowNum) |> pull(code) |> as.character()

    ## Seleciona a série clicada na tabela
    updateSelectizeInput(session = session, inputId = "code",
                         choices = all_series()$code, selected = selectedCode, server = TRUE)
    # Muda para o próximo panel após selecionar a série
    updateTabsetPanel(session, "tabs", selected = "mod")

    })

  ## Modelagem
  # Obtém os dados a partir da mudança no input com a série selecionada
  observeEvent(input$code,
  {
    req(input$code)
  # Obtem os dados da API do IPEA
    dados$info <- datasets |> filter(code %in% input$code)
    # Os dados originais são armazenados
    dados$df <- ipeadatar::ipeadata(input$code, quiet = T)
    min_date <- min(dados$df$date)
    max_date <- max(dados$df$date)
    # Atualiza slider de data
    updateDateRangeInput(session = session, inputId = "date",  min = min_date, max = max_date, start = min_date, end =  max_date)

    dados
  })

  # Retorna os valores da série transformados
  selected_series <- reactive({
    req(input$code)
    dados$df |>
  # Transforma o conjunto de dados de acordo com lambda
    mutate(value = box_cox(value, input$lambda) ) |>
    # filtra pela data do slider
    subset(date >= input$date[1] & date <= input$date[2]) |>
  # média móvel de acordo com a janela estipulada
    mutate(value = slider::slide_dbl(value, mean,
                                     .before = input$rollMean))
  })

  # Armazena informaçoes sobre a série selecionada
  series_data <- reactive({
    req(input$code)
    dados$info
  })

  # Retorna a função utilizada de acordo com a frequência da série
  current_season_period <- reactive({
    switch(series_data()$freq,
           "Mensal" = yearmonth,
           "Anual" = year,
           "Diária" = as_date)
  })

  # Armazena a série em forma de tsibble
  selected_series_ts <- reactive({
    req(input$code)
    selected_series() |>
      select(!c("uname", "tcode", "code")) |> #  \/ isso daqui é bem tricky haha
              mutate(#date = current_season_period()(date),
             index = row_number()
             ) |>
      as_tsibble(index= index)
          ## INFO: os lapsos na série são imputados com o valor anterior abaixo
     # fill_gaps() |>
     # fill(code, value)
  })

  selected_series_ts_decomp <- reactive({
    if(is.null(selected_series_ts())) return(NULL)

    ## Decomp STL da série de acordo com janela e parametros
    if(input$decompType == "STL")
    {
      selected_series_ts() |>
        model(
          STL(value ~ trend(window = input$trendWindow) +
                season(window = input$seasonWindow),
              robust = T))
    }
    else if(input$decompType == "Nula")
    {
      selected_series_ts()
    }
    ## TODO decomposicoes diferentes, de acordo com input$decompype
    else return(NULL)
  })

 # Calcula o melhor lambda e atualiza o slider
  observeEvent(input$bestLambda, {
    lambda <- selected_series_ts() |>
      features(value, features = guerrero) |>
      pull(lambda_guerrero)

    updateSliderInput(session, "lambda", value = lambda)
  })

  ## Baixa um csv com as séries selecionadas
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$code,"_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (is.null(selected_series())) {
        writeLines("No data available to download.", file)
      } else {
        write.csv(selected_series() |> select(date, value), file, row.names = FALSE)
      }
    }
  )

  ## Tabela da sidebar que exibe infos da série selecionada
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
    req(selected_series_ts())
    plot_ly(data = selected_series_ts(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(
        #title = paste("Series:", selected_series() |> select(code) |> distinct()),
        xaxis = list(title = "Data"),
        yaxis = list(title = "Valor")
      )

  })

  ## Título da série
 output$seriesTitle <- renderText({
    if(is.null(series_data())) return(NULL)
    paste(c(series_data()$name, " - ", series_data()$freq))
  })

  output$stlPlot <- renderPlotly({

    if(input$decompType == "Nula")  return(NULL)

    req(selected_series_ts_decomp())
    selected_series_ts_decomp() |>
      components() |>
      autoplot() |>
      ggplotly()

  })
  
  ## Correlograma
  output$corrPlot <- renderPlotly({
    req(selected_series_ts_decomp())

    if(input$decompType == "Nula")
    {
        selected_series_ts()  |>
        ACF(value, lag_max=input$lagMax) |>
        autoplot() |>
        ggplotly()
    }

    else
    {
      selected_series_ts_decomp()  |>
        components() |>
        ACF(remainder, lag_max=input$lagMax) |>
        autoplot() |>
        ggplotly()
    }

  })

  ## Plot Sazonal
  output$seasonalPlot <- renderPlot({
    if(is.null(selected_series())) return(NULL)

    selected_series_ts() |>
      as_tsibble(index =  date, regular = FALSE) |>
      gg_season(value, labels = "both", period = lag_plot_period())


  })

  ## Subséries
  ## FIXME: alguns casos ele não plota a subsérie, como nas séries mais recentes diárias
  output$subseriesPlot <- renderPlot({
    req(selected_series_ts())

    selected_series_ts() |>
      as_tsibble(index =  date, regular = FALSE) |>
      gg_subseries(value, period = lag_plot_period())

  })

  ## Retorna o período utilizado pelo lag plot
  ## FIXME: Em alguns casos fica colorido, outros não
  lag_plot_period <- reactive({
    req(series_data())
    switch(series_data()$freq,
           "Mensal" = "month",
           "Anual" = "year",
           "day" = "day")
  })

  is_monthly <- reactive({
    ifelse(lag_plot_period() == 'month',
           TRUE,
           FALSE)
  })

  is_daily <- reactive({
    ifelse(lag_plot_period() == 'day',
           TRUE,
           FALSE)
  })


  ## Lag plot
  output$lagPlot <- renderPlot({
    req(selected_series_ts())
    selected_series_ts() |>
      gg_lag(value, geom = "point", period = lag_plot_period())

  })
  ## Modelagem
  ## TODO: modelos arima e sarima reaproveitar o código em
  ## https://github.com/predict-icmc/covid19/blob/cbbae41ed7433df41f384183780cc14f652b1223/shiny/site_final/covid-19/app.R#L394

    ## Grafico interativo com as séries selecionadas pelo usuário
  output$prediction <- renderPlotly({
    req(selected_series_ts())

    plot_ly(data = selected_series_ts(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
      layout(
        #title = paste("Series:", selected_series() |> select(code) |> distinct()),
        xaxis = list(title = "Data"),
        yaxis = list(title = "Valor")
      )
  })

  observeEvent(input$nextButton, {
    updateTabsetPanel(session, "tabs", selected = "pre")
  })

  ## output$title <- renderText({
  ##   dfit$title
  ## })
  ## ## Previsão
  ## # objeto reativo que armazena o modelo utilizado
  ## dfit <- reactiveValues(data = NULL, xreg = NULL, title = NULL)

  ##   # recebe um modelo e calcula a previsao com a confiança estipulada
  ## calcula_pred <- reactive({
  ##   lwr <- input$maxScore
  ##   upr <- input$minScore
  ##   rng <- input$pred_rng
  ##   fit <- dfit$data
  ##   #xreg <- dfit$xreg

  ##   f <- forecast_c(fit, 7 * rng, PI = T, level = c(lwr/100, upr/100))#, xreg = xreg$mean)
  ##   tmp <- autoplot(f)
  ##   dfit$title <- tmp$labels$title
  ##   f
  ## })
}