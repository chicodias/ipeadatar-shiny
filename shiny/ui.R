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
    sliderInput("rollMean", "Número de defasagens para a média móvel", min = 0, max = 30, step = 1, value = 0),
    tableOutput("nameDisplay"),
    downloadButton("downloadData", "Baixar CSV")
  )
}

modelagemMainPanel <- function() {
  mainPanel(
                                        #    h2("Parâmetros"),
                                        #    helpText("Aqui você pode escolher diferentes parâmetros para as análises"),

    tabsetPanel(
                                        # Correlograma Tab
      tabPanel("Decomposições",
               plotlyOutput("stlPlot", width = "65vw", height = "74vh"),
#               h2("Decomposições"),
               fluidRow(
                 column(4,
                        radioButtons("decompType", "Selecione tipo de decomposição", choices = c("STL", "Aditiva", "Multiplicativa", "X11", "SEATS", "Nula"), selected = "STL"),
                        ),
                 column(4,
                        sliderInput("trendWindow", "Selecione janela de tendência", min = 1, max = 21, step = 1, value = 12),
                        ),

                 column(4,
                        sliderInput("seasonWindow", "Selecione janela de sazonalidade", min = 5, max = 21, step = 1, value = 12),
                        )
               ),
                                        #         actionButton("nextButton", "Previsão", class = "btn-primary")

               ),

                                        # Corrrelogramas Tab
      tabPanel("Correlogramas",
               plotlyOutput("corrPlot"),
               plotlyOutput("parCorrPlot"),
               h2("Correlogramas"),
               sliderInput("lagMax", "Defasagem máxima", min = 0, max = 180, step = 1, value = 60),

               ),
      tabPanel("Estacionariedade",
              plotlyOutput("corrDiffPlot"),
              # h2("Grau de diferenciação"),
              sliderInput("degreeDiff", "Grau de diferenciação", min = 1, max = 6, step = 1, value = 1),
              dataTableOutput("corrDiffTable"),


      )
    )
  )
}


previsaoSidebar <- function() {
  sidebarPanel(
    numericInput("pred_rng", "Janela de previsão", min = 1, max = 90, value = 8),
    radioButtons("radio3", h3("Modelo"), choices = list("ARIMA" = 0, "SARIMA" = 1, "NNAR"=2), selected = 0),
    numericInput("minScore", "I.C. Mínimo (%)", min = 60, max = 99, value = 80),
    numericInput("maxScore", "I.C. Máximo (%)", min = 60, max = 99.9, value = 95),
    h4("Parametros Sarima"),
    h5("Non-seasonal:"),
    sliderInput("pNonSea", "p", min=0, max=10, value=0),
    # sliderInput("dNonSea", "d", min=0, max=10, value=1),
    sliderInput("qNonSea", "q", min=0, max=10, value=2),
    h5("Seasonal"),
    sliderInput("pSeasonal", "P", min=0, max=10, value=0),
    # sliderInput("dSeasonal", "D", min=0, max=10, value=1),
    sliderInput("qSeasonal", "Q", min=0, max=10, value=1),
  )
}


previsaoMainPanel <- function() {
  mainPanel(
    h4(textOutput("title")),
    plotOutput("prediction")
  )
}

diagnosticoSidebar <- function(){
  sidebarPanel(
    h2("Diagnóstico"),
    h3("Modelo:"),
    h4(textOutput("modelTitle")),
    htmlOutput("modelReport"),
    h3("Parâmetros:"),
    sliderInput("dfTestbox", "G.l.", min = 1, max = 20, step = 1, value = 10),

  )
}

diagnosticoMainPanel <- function(){
  mainPanel(
    h4("Plot de Resíduos:"),
    plotOutput("residuoPlot"),
    h4("Testes de hipóteses:"),
    dataTableOutput("testesBox"),
    h4("Raízes características:"),
    plotOutput("rootPlot"),
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
    tabPanel("Previsão", value = "pre", sidebarLayout(previsaoSidebar(), previsaoMainPanel())),
    tabPanel("Diagnóstico", value="diag", sidebarLayout(diagnosticoSidebar(), diagnosticoMainPanel()))
  )
)
