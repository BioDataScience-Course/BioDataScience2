learndown::learndownShinyVersion("0.0.9000") # Set app version
BioDataScience::init()

library(shiny)
library(learndown)


ui <- fluidPage(
  # Initialize a learndown-specific Shiny application
  learndownShiny("Ajustement manuel d'un modèle : Michaelis-Menten"),

  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      p("$$y(x) = \\frac{V_{m} * x}{K + x}$$"),

      sliderInput("vm", label = "Vm",
                  value = 1, min = 0, max = 10, step = 0.5),
      sliderInput("k", label = "K",
                  value = 1, min = 0, max = 10, step = 0.5),

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
  vm_init <- 6
  k_init <- 2
  error_sd <- 0.05
  set.seed(42)

  model_data <- tibble::tibble(
    x = seq(0, 25, by = 0.1),
    y = SSmicmen(x, Vm = vm_init, K = k_init) +
      rnorm(n = length(x), sd = error_sd))

  model_predict <- reactive({
    dplyr::mutate(model_data,
           y_predit = SSmicmen(x, Vm = input$vm, K = input$k),
           distance2 = (y_predit - y)^2
    )
  })

  output$model_equation <- renderUI({
    withMathJax(
      sprintf("$$y(x) = \\frac{%.02f * x}{%.02f + x}$$",
              input$vm, input$k))
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
              solution = list(vm = vm_init, k = k_init),
              comment = "y = Vm*x/K+x",
              message.success = "Correct, c'est le meilleur modèle.",
              message.error = "Incorrect, un modèle mieux ajusté existe.")
  # Track the quit button, save logs and close app after a delay (in sec)
  trackQuit(session, input, output, delay = 60)
}

shinyApp(ui, server)
