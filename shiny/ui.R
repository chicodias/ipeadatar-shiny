exploradorSidebar <- function() {
  sidebarPanel(
    helpText("Aqui você pode escolher entre as bases disponíveis no pacote IpeaDataR"),
    textOutput("nSeries"),
    selectizeInput("code", "Escolha a(s) série(s):", choices = NULL, multiple = TRUE),
    pickerInput("freq", "Frequência", choices = c("Mensal", "Anual", "Diária"), selected =  c("Mensal", "Anual", "Diária"), options = list(`actions-box` = TRUE), multiple = TRUE),
    pickerInput("theme", "Tema", choices = unique(datasets$theme), selected = unique(datasets$theme), multiple = TRUE),
    pickerInput("source", "Fonte", choices = unique(datasets$source), selected = unique(datasets$source), multiple = TRUE, options = list(`actions-box` = TRUE)),
    pickerInput("status", "Status", choices = c("Ativa", "Inativa"), selected = c("Ativa", "Inativa"), multiple = TRUE)
  )
}

exploradorMainPanel <- function() {
  mainPanel(
    DT::dataTableOutput("dataTab")
  )
}

modelagemSidebar <- function() {
  sidebarPanel(
    h4(textOutput("seriesTitle")),
    plotlyOutput("seriesPlot"),
    dateRangeInput("date", " ", language = "pt-BR", format = "dd-mm-yyyy", separator = "até"),
    h2("Transformações"),
    sliderInput("lambda", "Selecione lambda de box-cox", min = -2, max = 2, step = 0.01, value = 1),
    checkboxInput("bestLambda", "Use lambda ótimo (Guerrero)"),
    sliderInput("rollMean", "Número de defasagens para a media móvel", min = 0, max = 30, step = 1, value = 0),
    tableOutput("nameDisplay"),
    downloadButton("downloadData", "Baixar CSV")
  )
}

modelagemMainPanel <- function() {
  mainPanel(
    h2("Parâmetros"),
    helpText("Aqui você pode escolher diferentes parâmetros para as análises"),
    # Decomposição Tab
    tabPanel("Decomposição",
      sidebarLayout(
        mainPanel(
          plotlyOutput("stlPlot")
        ),
        sidebarPanel(
          h2("Decomposição"),
          radioButtons("decompType", "Selecione tipo de decomposição", choices = c("STL", "Nula"), selected = "STL"), # incluir: "Clássica", "X-11", "SEATS"
          sliderInput("trendWindow", "Selecione janela de tendência", min = 1, max = 21, step = 1, value = 14),
          sliderInput("seasonWindow", "Selecione janela de sazonalidade", min = 5, max = 21, step = 1, value = 7)
        )
      )
    ),
    # Correlograma Tab
    tabPanel("Correlograma",
      sidebarLayout(
        mainPanel(
          plotlyOutput("corrPlot")
        ),
        sidebarPanel(
          h2("Correlograma"),
          sliderInput("lagMax", "Selecione lagmax do correlograma", min = 0, max = 180, step = 1, value = 30)
        )
      )
    ),

    actionButton("nextButton", "Previsão", class = "btn-primary")
    ## # Lag Plot Tab
    ## tabPanel("Lag plot",
    ##   sidebarLayout(
    ##     mainPanel(
    ##       plotOutput("lagPlot")
    ##     ),
    ##     sidebarPanel(
    ##       h2("Lag plot")
    ##       # Add any additional inputs or controls for the Lag plot here
    ##     )
    ##   )
    ## )

  )
}


previsaoSidebar <- function() {
  sidebarPanel(
    numericInput("pred_rng", "Janela de previsão", min = 1, max = 90, value = 8),
    radioButtons("radio3", h3("Modelo"), choices = list("ARIMA" = 0, "NNAR" = 1), selected = 0),
    numericInput("minScore", "I.C. Mínimo (%)", min = 60, max = 99, value = 80),
    numericInput("maxScore", "I.C. Máximo (%)", min = 60, max = 99.9, value = 95)
  )
}


previsaoMainPanel <- function() {
  mainPanel(
    h4(textOutput("title")),
    plotlyOutput("prediction")
  )
}


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
     .shiny-spinner-output-container.full-page {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
      }
    "))
  ),
  add_busy_spinner(spin = "pixel", position = "full-page"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Explorador", value = "exp", sidebarLayout(exploradorSidebar(), exploradorMainPanel())),
    tabPanel("Modelagem", value = "mod", sidebarLayout(modelagemSidebar(), modelagemMainPanel())),
    tabPanel("Previsão", value = "pre", sidebarLayout(previsaoSidebar(), previsaoMainPanel()))
  )
)
