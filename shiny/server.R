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

    st <- selected_series() |>
      select(!c("uname", "tcode", "code")) |> #  \/ isso daqui é bem tricky haha
              mutate(date = current_season_period()(date),
             index = row_number()
             ) |>
      as_tsibble(index= date)

    if(has_gaps(st) |> any())
    {
      showNotification("A série possui lapsos, algumas decomposições podem não funcionar.")
      ## INFO: os lapsos na série são imputados com o valor anterior abaixo
      st <- st |> fill_gaps() #|>
#        fill(value)
    }

    st
  })

  selected_series_ts_decomp <- reactive({
    req(selected_series_ts())

    tryCatch({
    ## Decomp STL da série de acordo com janela e parametros
    if(input$decompType == "STL")
    {
      selected_series_ts() |>
        model(
          STL(value ~ trend(window = input$trendWindow) +
                season(window = input$seasonWindow),
              robust = T))
    }

    else if(input$decompType == "Aditiva")
    {
      selected_series_ts() |>
      model(
        classical_decomposition(value, type="additive")
      )
    }
    else if(input$decompType == "Multiplicativa")
    {
      selected_series_ts() |>
      model(
        classical_decomposition(value, type="multiplicative")
      )
    }
    else if(input$decompType == "X11")
    {
      selected_series_ts() |>
      model(
        x11 = X_13ARIMA_SEATS(value ~ x11())
      )
    }
    else if (input$decompType == "SEATS")
    {
      selected_series_ts() |>
      model(
        x11 = X_13ARIMA_SEATS(value ~ seats())
      )
    }
    else return(NULL)
    }, error = function(e) {
    # Display a notification in case of an error
      showNotification("Erro decompondo a série. Considere diminuir a janela de tempo ou selecione uma série diferente", type = "error")
      NULL
  })
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
    plot_ly(data = selected_series_ts(), x = ~as_date(date), y = ~value, type = 'scatter', mode = 'lines') %>%
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
      ac <- selected_series_ts()  |>
        ACF(value, lag_max=input$lagMax) |>
        autoplot() |>
        ggplotly()
    }
    else if(input$decompType == "STL")
    {
      ac <- selected_series_ts_decomp()  |>
        components() |>
        ACF(remainder, lag_max=input$lagMax) |>
        autoplot() |>
        ggplotly()
    }
    else if(input$decompType %in% c("X11", "SEATS"))
    {
      ac <- selected_series_ts_decomp()  |>
        components() |>
        ACF(irregular, lag_max=input$lagMax) |>
        autoplot() |>
        ggplotly()
    }
    else
    {
      ac <- selected_series_ts_decomp()  |>
        components() |>
        ACF(random, lag_max=input$lagMax) |>
        autoplot() |>
        ggplotly()
    }


    ac
  })

    ## Correlograma parcial
  output$parCorrPlot <- renderPlotly({
    req(selected_series_ts_decomp())

      if(input$decompType == "Nula")
    {
      ac <- selected_series_ts()  |>
        PACF(value, lag_max=input$lagMax) |>
        autoplot() |>
        ggplotly()
    }
    else if(input$decompType == "STL")
    {
      ac <- selected_series_ts_decomp()  |>
        components() |>
        PACF(remainder, lag_max=input$lagMax) |>
        autoplot() |>
        ggplotly()
    }
    else if(input$decompType %in% c("X11", "SEATS"))
    {
      ac <- selected_series_ts_decomp()  |>
        components() |>
        PACF(irregular, lag_max=input$lagMax) |>
        autoplot() |>
        ggplotly()
    }
    else
    {
      ac <- selected_series_ts_decomp()  |>
        components() |>
        PACF(random, lag_max=input$lagMax) |>
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
  # output$prediction <- renderPlotly({
  #   req(selected_series_ts())

  #   plot_ly(data = selected_series_ts(), x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  #     layout(
  #       #title = paste("Series:", selected_series() |> select(code) |> distinct()),
  #       xaxis = list(title = "Data"),
  #       yaxis = list(title = "Valor")
  #     )
  # })

  observeEvent(input$nextButton, {
    updateTabsetPanel(session, "tabs", selected = "pre")
  })

  fitted_model <- reactive({

    req(selected_series_ts())
    rng <- input$pred_rng
    data <- selected_series_ts()

    # modelo selecionado
    if(input$radio3 == 0){ #ARIMA
      #fit$title <- "ARIMA"
      model <- data |>  
        model(auto_arima=ARIMA(value))
    } else if(input$radio3 == 1){ # SARIMA
      # print(input$pNonSea)
      #fit$title <- "SARIMA"
      model <- data |>  
        model(
          arima012011 = ARIMA(value ~ pdq(
            input$pNonSea,
            input$dNonSea,
            input$qNonSea
          ) + PDQ(
            input$pSeasonal,
            input$dSeasonal,
            input$qSeasonal)
        ),
          # auto_arima=ARIMA(value)
        )

    } else if(input$radio3 == 2){
      #fit$title <- "NNAR"
      model <- data |>
        model(NNETAR(value))
    }
    model
  })


  # gráfico da previsão
  output$prediction <- renderPlotly({

    if(is.null(fitted_model())) return(NULL)

    pred <- fitted_model() |>
      forecast(h=input$pred_rng, PI = T, level = c(input$maxScore/100, input$minScore/100)) |>
      mutate(upper1 = quantile(value,  1- (1- input$minScore/100)/2),
             lower1 = quantile(value,  (1- input$minScore/100)/2),
             upper2 = quantile(value,  1- (1- input$maxScore/100)/2),
             lower2 = quantile(value,  (1- input$maxScore/100)/2)
             )

      trace2 <- list(
        fill = "toself",
        line = list(
          color = "rgba(242,242,242,1)",
          fillcolor = "rgba(242,242,242,1)"
        ),
        mode = "lines",
        name = paste0("Previsão com ",input$maxScore,"% de confiança)"),
        type = "scatter",
        x = c(pred$date, rev(pred$date)),
        y = round(c(pred$upper2, rev(pred$lower2))),
        xaxis = "x",
        yaxis = "y",
        hoveron = "points"
      )
      trace3 <- list(
        fill = "toself",
        line = list(
          color = "rgba(204,204,204,1)",
          fillcolor = "rgba(204,204,204,1)"
        ),
        mode = "lines",
        name = paste0("Previsão com ",input$minScore,"% de confiança)"),
        type = "scatter",
        x = c(pred$date, rev(pred$date)),
        y = round(c(pred$upper1, rev(pred$lower1))),
        xaxis = "x",
        yaxis = "y",
        hoveron = "points"
      )
      trace4 <- list(
        line = list(
          dash = 3,
          color = "rgba(0,0,255,1)",
          fillcolor = "rgba(0,0,255,1)"
        ),
        mode = "lines",
        name = paste0("previsão média"),
        type = "scatter",
        x = pred$date,
        y = round(pred$.mean),
        xaxis = "x",
        yaxis = "y"
      )
      data <- list( trace2, trace3, trace4)
      layout <- list(
        title = "PREVISÃO",
        xaxis = list(
          # title = "Data",
          domain = range(selected_series_ts()$date)
        ),
        yaxis = list(
          title = paste0("Valor"),
          domain = c(0, 1)
        ),
        margin = list(
          b = 40,
          l = 60,
          r = 10,
          t = 25
        )
      )
      p <- plot_ly()
      p <- add_trace(p, fill = trace2$fill, line = trace2$line, mode = trace2$mode, name = trace2$name, type = trace2$type, x = trace2$x, y = trace2$y, xaxis = trace2$xaxis, yaxis = trace2$yaxis, hoveron = trace2$hoveron)
      p <- add_trace(p, fill = trace3$fill, line = trace3$line, mode = trace3$mode, name = trace3$name, type = trace3$type, x = trace3$x, y = trace3$y, xaxis = trace3$xaxis, yaxis = trace3$yaxis, hoveron = trace3$hoveron)
      p <- add_trace(p, line = trace4$line, mode = trace4$mode, name = trace4$name, type = trace4$type, x = trace4$x, y = trace4$y, xaxis = trace4$xaxis, yaxis = trace4$yaxis)
      p %>% add_lines(x = ~date, y = ~ value)
      p <- layout(p, title = layout$title, xaxis = layout$xaxis, yaxis = layout$yaxis, margin = layout$margin, legend = list(orientation = "h"))
      p %>% add_lines(y = selected_series_ts()$value, x = selected_series_ts()$date)


  })

  output$residuoPlot <- renderPlot({
    if(is.null(fitted_model())) return (NULL)

    fitted_model() |>
      gg_tsresiduals() 
    
  })

  ## output$modelTitle <- renderText({
  ##   fit$title
  ## })

  output$modelReport <- renderUI({
    r = capture.output(fabletools::report(fitted_model()))

    HTML(paste(
      r, sep = '<br/>', collapse = '<br/>'
    ))
      
  })

  output$testesBox <- DT::renderDataTable({
    boxp <- augment(fitted_model()) |>
      features(.innov, box_pierce, lag = input$dfTestbox)
    
    lbox <- augment(fitted_model()) |>
      features(.innov, ljung_box, lag = input$dfTestbox)

    shapiro <- residuals(fitted_model())$.resid |> shapiro.test()

    tibble(teste = c("Box Pierce", "Ljung-Box", "Shapiro-Wilk (resíduos)"),
           Estatistica = c(boxp$bp_stat, lbox$lb_stat, shapiro$statistic),
           Pvalor = c(boxp$bp_pvalue, lbox$lb_pvalue, shapiro$p.value))

  })

  output$rootPlot <- renderPlot({
    gg_arma(fitted_model())
  })

  selected_series_ts_diff <- reactive({
    if(is.null(selected_series_ts())) return(NULL)

    st <- selected_series_ts()
    aux <- Reduce(function(x, y) difference(x), rep(1, input$degreeDiff), init=st$value, accumulate=FALSE)

    tibble(index = 1:length(aux), value = aux) |>
      as_tsibble(index = index)
  })

  output$corrDiffPlot <- renderPlotly({

    selected_series_ts_diff() |>
      ACF(value) |>
      autoplot() |>
      ggplotly()

  })

  output$diffPlot <- renderPlotly({

    selected_series_ts_diff() |>
      autoplot() |>
      ggplotly()

  })


  output$corrDiffTable <- DT::renderDataTable({
    lb <- selected_series_ts_diff() |> features(value, ljung_box)
    lc <-  selected_series_ts_diff() |> features(value, unitroot_ndiffs)
    tibble("Parâmetro"=c("Estatística de Ljung-Box", "Valor p", "Numero de difs necessárias"),
           Valor=c(lb$lb_stat, lb$lb_pvalue, lc$ndiffs)
           )
  })


}
