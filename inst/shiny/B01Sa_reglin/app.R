learndown::learndownShinyVersion("1.0.0")
conf <- BioDataScience::config()

library(shiny)
library(learndown)
library(BioDataScience2)

a_init <- -1.5
b_init <- 3.5
error_sd <- 0.25
set.seed(42)

reglin <- function(x, a, b)
  (a * x) + b

model_data <- tibble::tibble(
  x = seq(0, 10, by = 0.25),
  y = reglin(x, a = a_init, b = b_init) +
    rnorm(n = length(x), sd = error_sd))

ui <- fluidPage(
  learndownShiny("Ajustement manuel d'un modèle : régression linéaire"),

  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      p("$$y(x) = a \\ x + \\ b $$"),
      sliderInput("a", label = "a",
        value = 0, min = -5, max = 5, step = 0.5),
      sliderInput("b", label = "b",
        value = 0, min = -5, max = 5, step = 0.5),
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
      y_predit = reglin(x, a = input$a, b = input$b),
      distance2 = (y_predit - y)^2
    )
  })

  output$model_equation <- renderUI({
    withMathJax(
      sprintf("$$y(x) \\ = %.02f \\ x + \\ %.02f$$",
              input$a, input$b))
  })

  output$model_resid <- renderUI({
    data <- model_predict()
    withMathJax(sprintf("$$ \\ %.02f \\ $$", sum(data$distance2)))
  })

  output$model_plot <- renderPlot({
    data <- model_predict()

    chart::chart(data, y ~ x) +
      ggplot2::geom_point() +
      ggplot2::geom_line(chart::f_aes(y_predit ~ x), color = "red") +
      ggplot2::xlab("x") +
      ggplot2::ylab("y")
  })

  trackEvents(session, input, output,
              sign_in.fun = BioDataScience::sign_in, conf = conf)
  trackSubmit(session, input, output, max_score = 2,
    solution = list(a = a_init, b = b_init),
    comment = "y = a.x + b",
    message.success = "Correct, c'est le meilleur modèle. a est la pente et b est l'ordonnée à l'origine de la droite.",
    message.error = "Incorrect, un modèle mieux ajusté existe.")
  trackQuit(session, input, output, delay = 20)
}

shinyApp(ui, server)
