learndown::learndownShinyVersion("1.2.1")
conf <- BioDataScience::config()

library(shiny)
library(learndown)
library(BioDataScience2)

asym_init <- 1
b2_init <- 5
b3_init <- 0.5
error_sd <- 0.05
set.seed(42)


model_data <- tibble::tibble(
  x = seq(0, 10, by = 0.1),
  y = SSgompertz(x, Asym = asym_init, b2 = b2_init, b3 = b3_init) +
    rnorm(n = length(x), sd = error_sd))

graph <- chart::chart(model_data, y ~ x) +
  ggplot2::geom_point() +
  ggplot2::xlab("x") +
  ggplot2::ylab("y")

ui <- fluidPage(
  learndownShiny("Ajustement manuel d'un modèle : modèle de Gompertz"),

  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      p("$$y(x) = Asym * e^{- b_{2} * b_{3}^x}$$"),

      sliderInput("asym", label = "Asym",
        value = 0.00, min = -5.00, max = 5.00, step = 0.5),
      sliderInput("b2", label = "b2",
                  value = 1.00, min = 0, max = 10.00, step = 0.5),
      sliderInput("b3", label = "b3",
        value = 1.00, min = -1.00, max = 2.00, step = 0.25),
      hr(),
      submitQuitButtons()
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
          p("Somme des carrés des résidus (valeur à minimiser) :"),
          uiOutput("model_resid"))
      )
    )
  )
)


server <- function(input, output, session) {

  model_predict <- reactive({

    dplyr::mutate(model_data,
      y_predit = SSgompertz(x, Asym = input$asym, b2  = input$b2, b3 = input$b3),
      distance2 = (y_predit - y)^2
    )
  })

  output$model_equation <- renderUI({
    withMathJax(
      sprintf("$$y(x) = %.02f * e^{- %.02f * %.02f^x}$$", input$asym, input$b2, input$b3)
    )
  })

  output$model_resid <- renderUI({
    data <- model_predict()
    withMathJax(sprintf("$$ \\ %.02f \\ $$", sum(data$distance2)))
  })

  output$model_plot <- renderPlot({
    data <- model_predict()
    p <- graph

    if(!any(is.nan(data$y_predit))) {
      p <- p +
        ggplot2::geom_line(chart::f_aes(y_predit ~ x), color = "red", data = data)
    }

    # if(any(is.nan(data$y_predit))) {
    #   p  <- graph
    # } else {
    #   p <- graph +
    #     ggplot2::geom_line(chart::f_aes(y_predit ~ x), color = "red", data = data)
    # }
    p
  })

  trackEvents(session, input, output,
    sign_in.fun = BioDataScience::sign_in, config = conf)
  trackSubmit(session, input, output, max_score = 3, solution =
    list(asym = asym_init, b2 = b2_init, b3 = b3_init),
    comment = "y(x) = Asym * e^(- b2 * b3^x)",
    message.success = "Correct, c'est le meilleur modèle.",
    message.error = "Incorrect, un modèle mieux ajusté existe.")
  trackQuit(session, input, output, delay = 20)
}

shinyApp(ui, server)
