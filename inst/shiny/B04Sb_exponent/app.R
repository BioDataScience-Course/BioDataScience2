learndown::learndownShinyVersion("1.1.0")
conf <- BioDataScience::config()

library(shiny)
library(learndown)
library(BioDataScience2)

y0_init <- 3.5
k_init <- 0.10
error_sd <- 0.5
set.seed(42)

exponent <- function(x, y0, k)
  y0 * exp(k * x)

model_data <- tibble::tibble(
  x = seq(0, 20, by = 0.5),
  y = exponent(x, y0 = y0_init, k = k_init) +
    rnorm(n = length(x), sd = error_sd))

graph <- chart::chart(model_data, y ~ x) +
  ggplot2::geom_point() +
  ggplot2::xlab("x") +
  ggplot2::ylab("y")

ui <- fluidPage(
  learndownShiny("Ajustement manuel d'un modèle : courbe exponentielle"),

  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      p("$$y(x) = y_0 \\ e^{k \\ x}$$"),
      sliderInput("y0", label = "y0",
        value = 1, min = -5, max = 5, step = 0.5),
      sliderInput("k", label = "k",
        value = 0.025, min = -0.20, max = 0.20, step = 0.025),
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
      y_predit = exponent(x, y0 = input$y0, k = input$k),
      distance2 = (y_predit - y)^2
    )
  })

  output$model_equation <- renderUI({
    withMathJax(
      sprintf("$$y(x) = %.02f \\ e^{ %.02f \\ x}$$", input$y0, input$k)
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

    p
  })

  trackEvents(session, input, output,
    sign_in.fun = BioDataScience::sign_in, config = conf)
  trackSubmit(session, input, output, max_score = 2,
    solution = list(y0 = y0_init, k = k_init),
    comment = "y = y0.e^(k.x)",
    message.success = "Correct, c'est le meilleur modèle. y0 et la valeur de y pour x = 0 et k est la vitesse de croissance.",
    message.error = "Incorrect, un modèle mieux ajusté existe.")
  trackQuit(session, input, output, delay = 20)
}

shinyApp(ui, server)
