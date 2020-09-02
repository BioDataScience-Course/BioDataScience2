learndown::learndownShinyVersion("0.0.9000") # Set app version
BioDataScience::init()

library(shiny)
library(learndown)


ui <- fluidPage(
  # Initialize a learndown-specific Shiny application
  learndownShiny("Ajustement manuel d'un modèle : courbe logistique"),

  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      p("$$y(x) = \\frac{Asym}{1 + e^{\\frac{xmid - x}{scal}}}$$"),

      sliderInput("asym", label = "Asym",
                  value = 1.00, min = 0.50, max = 10.00, step = 0.5),
      sliderInput("xmid", label = "Xmid",
                  value = 1.00, min = 0.25, max = 10.00, step = 0.25),
      sliderInput("scal", label = "Scal",
                  value = 1.00, min = 0.25, max = 10.00, step = 0.25),

      hr(),

      submitQuitButtons() # The learndown-specific buttons
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
  asym_init <- 5
  xmid_init <- 4
  scal_init <- 0.5
  error_sd <- 0.1
  set.seed(42)

  model_data <- tibble::tibble(
    x = seq(0, 8, by = 0.1),
    y = SSlogis(x, Asym = asym_init, xmid = xmid_init, scal = scal_init) +
      rnorm(n = length(x), sd = error_sd))

  model_predict <- reactive({
    dplyr::mutate(model_data,
           y_predit = SSlogis(x,
                              Asym = input$asym,
                              xmid = input$xmid,
                              scal = input$scal),
           distance2 = (y_predit - y)^2
    )
  })

  output$model_equation <- renderUI({
    withMathJax(
      sprintf("$$y(x) = \\frac{%.02f}{1 + e^{\\frac{%.02f - x}{%.02f}}}$$",
              input$asym, input$xmid, input$scal))
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

  # This is the learndown-specific behaviour
  # Track start, stop, inputs, errors (and possibly outputs)
  trackEvents(session, input, output)
  # Track the submit button and check answer
  trackSubmit(session, input, output,
              solution = list(
                asym = asym_init, xmid = xmid_init, scal = scal_init),
              comment = "y = asym/1+e(xmid-x/scal)",
              message.success = "Correct, c'est le meilleur modèle.",
              message.error = "Incorrect, un modèle mieux ajusté existe.")
  # Track the quit button, save logs and close app after a delay (in sec)
  trackQuit(session, input, output, delay = 60)
}

shinyApp(ui, server)
