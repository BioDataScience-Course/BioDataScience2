learndown::learndownShinyVersion("1.1.0")
conf <- BioDataScience::config()

library(shiny)
library(learndown)
library(BioDataScience2)

asym_init <- 3
lrc_init <- 0.1
c0_init <- 0.4
m_init <- 6
error_sd <- 0.05
set.seed(42)


richards <- function(x, Asym, lrc, c0, m) Asym*(1 - exp(-exp(lrc) * (x - c0)))^m

model_data <- tibble::tibble(
  x = seq(1, 7.5, by = 0.05),
  y = richards(x, Asym = asym_init, lrc = lrc_init, c0 = c0_init, m = m_init) +
    rnorm(n = length(x), sd = error_sd))

graph <- chart::chart(model_data, y ~ x) +
  ggplot2::geom_point() +
  ggplot2::xlab("x") +
  ggplot2::ylab("y")

ui <- fluidPage(
  learndownShiny("Ajustement manuel d'un modèle : modèle de Richards"),

  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      p("$$ y(x) = Asym * ( 1-e^{- e^{lrc} * (t-c0) } )^m $$"),

      sliderInput("asym", label = "Asym",
        value = 1.00, min = 0.00, max = 5.00, step = 0.5),
      sliderInput("lrc", label = "lrc",
                  value = 0.2, min = 0.00, max = 1.00, step = 0.1),
      sliderInput("c0", label = "c0",
        value = 1.00, min = -2.00, max = 2.00, step = 0.20),
      sliderInput("m", label = "m",
                  value = 0.00, min = 0.00, max = 10.00, step = 1),
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
      y_predit = richards(x, Asym = input$asym, lrc = input$lrc, c0 = input$c0, m = input$m),
      distance2 = (y_predit - y)^2
    )
  })

  output$model_equation <- renderUI({
    withMathJax(
      sprintf("$$ y(x) = %.02f * ( 1-e^{- e^{ %.02f } * (x- %.02f ) } )^{ %.02f } $$",
              input$asym, input$lrc, input$c0, input$m)
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
  trackSubmit(session, input, output, max_score = 4, solution =
    list(asym = asym_init, lrc = lrc_init, c0 = c0_init, m = m_init),
    comment = "y(x) = Asym * e^(- b2 * b3^x)",
    message.success = "Correct, c'est le meilleur modèle.",
    message.error = "Incorrect, un modèle mieux ajusté existe.")
  trackQuit(session, input, output, delay = 20)
}

shinyApp(ui, server)
