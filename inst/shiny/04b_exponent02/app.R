library(shiny)
library(shinylogs)
library(shinydashboard)
library(shinyFeedback)

SciViews::R()

titlePanel_h4 <- function(title, windowTitle = title) {
  tagList(tags$head(tags$title(windowTitle)), h4(title))
}

myToastOptions <- list(
  positionClass = "toast-bottom-center",
  progressBar = FALSE,
  closeButton = TRUE)

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  titlePanel_h4("Ajustement manuel d'un modèle : courbe exponentielle"),
  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      p("$$y(t) = y_0 \\ e^{k \\ t}$$"),
      sliderInput(
        inputId = "y0_ui", label = "y0",
        value = 0, min = -5, max = 5, step = 0.5),
      sliderInput(
        inputId = "k_ui", label = "k",
        value = 0.00, min = -0.20, max = 0.20, step = 0.05),
      hr(),
      loadingButton(
        "submit",
        label = "Submit Answer"
      )
    ),
    mainPanel(
      plotOutput("model_plot"),
      hr(),
      withMathJax(),
      fluidRow(
        column(width = 6,
          p("Modèle paramétré :"),
          uiOutput("model_equation")),
        column(width = 6,
          p("Somme des carrés des résidus :"),
          uiOutput("model_resid"))
      ),
      conditionalPanel(
        condition = "false",
        textInput("result", ""))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  set.seed(42)

  exponent <- function(x, y0, k)
    y0 * exp(k * x)

  y0_init <- 3.5
  k_init <- 0.10

  expo_exo <- tibble::tibble(
    t = seq(0, 20, by = 0.5),
    y = exponent(
      t, y0 = y0_init, k = k_init) + rnorm(n = length(t), sd = 0.5))

  output$model_equation <- renderUI({
    withMathJax(
      sprintf("$$y(t) \\ = %.02f \\ e^{%.02f \\ t}$$",
        input$y0_ui, input$k_ui))
  })


  expo_predict <- reactive({
    dplyr::mutate(expo_exo,
      y_predit = exponent(t, y0 = input$y0_ui, k = input$k_ui),
      distance2 = (y_predit - y)^2,
    )
  })

  output$model_resid <- renderUI({
    data <- expo_predict()

    withMathJax(
      sprintf("$$ \\ %.02f \\ $$",
        sum(data$distance2)))
  })

  output$model_plot <- renderPlot({
    data <- expo_predict()

    chart::chart(data, y ~ t) +
      ggplot2::geom_point() +
      geom_line(f_aes(y_predit ~ t), color = "red") +
      xlab("t") +
      ylab("y")
  })


  observe({
    user_info <- parseQueryString(session$clientData$url_search)
    # user_info <- parseQueryString("_ga=2.157999162.1421030749.1598002717-1781870437.1597214250")

    user_tracking <- function(session, query = user_info) {
      #query <- getOption("user_info")
      if (length(query) == 0 | is.null(query[["iemail"]]))
      { "" }
      else {
        #query[["iemail"]]
        as.character(jsonlite::toJSON(query))
      }
    }

    track_usage(storage_mode = store_rds(path = "../logs1/"),
      get_user = user_tracking)

  })

  # observe({
  #     input$submit
  #     res <- y0_init == isolate(input$y0_ui) & k_init == isolate(input$k_ui)
  #
  #     # resul <- as.character(jsonlite::toJSON(list(
  #     #     correct = res,
  #     #     value = list(
  #     #         y0_ui = isolate(input$y0_ui),
  #     #         k_ui = isolate(input$k_ui)))))
  #     #
  #     val <- paste(
  #         res,
  #         paste("y0_ui", isolate(input$y0_ui), sep = "="),
  #         paste("k_ui", isolate(input$k_ui), sep = "="), sep = ","
  #     )
  #
  #     updateTextInput(
  #         session, "result", value = val)
  # })



  observeEvent(input$submit, {
    req(input$submit)
    resetLoadingButton("submit")

    res <- y0_init == input$y0_ui & k_init == input$k_ui

    if (isTRUE(res)) {
      showToast(
        "success",
        "Correct, c'est le meilleur modèle",
        .options = myToastOptions
      )
    } else {
      showToast(
        "error",
        "Incorrect, un modèle mieux ajusté existe",
        .options = myToastOptions)
    }

    if (isTRUE(res)) {
      showFeedbackSuccess("y0_ui")
      showFeedbackSuccess("k_ui")
    }

    val <- paste(
      res,
      paste("y0_ui", isolate(input$y0_ui), sep = "="),
      paste("k_ui", isolate(input$k_ui), sep = "="), sep = ","
    )

    updateTextInput(
      session, "result", value = val)

  })


  #onStop(function(){cat(dir("../logs1/"))}, session = NULL)
}

shinyApp(ui, server)
