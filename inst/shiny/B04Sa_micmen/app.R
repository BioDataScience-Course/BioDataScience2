learndown::learndownShinyVersion("1.1.0")
conf <- BioDataScience::config()

library(shiny)
library(learndown)
library(BioDataScience2)

vm_init <- 6
k_init <- 2
error_sd <- 0.05
set.seed(42)

model_data <- tibble::tibble(
  x = seq(0, 25, by = 0.1),
  y = SSmicmen(x, Vm = vm_init, K = k_init) +
    rnorm(n = length(x), sd = error_sd))

graph <- chart::chart(model_data, y ~ x) +
  ggplot2::geom_point() +
  ggplot2::xlab("x") +
  ggplot2::ylab("y")

ui <- fluidPage(
  learndownShiny("Ajustement manuel d'un modèle : Michaelis-Menten"),

  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      p("$$y(x) = \\frac{V_{m} * x}{K + x}$$"),

      sliderInput("vm", label = "Vm",
        value = 1, min = 0, max = 10, step = 0.5),
      sliderInput("k", label = "K",
        value = 1, min = -3, max = 10, step = 0.5),
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
    solution = list(vm = vm_init, k = k_init),
    comment = "y = Vm*x/K+x",
    message.success = "Correct, c'est le meilleur modèle.",
    message.error = "Incorrect, un modèle mieux ajusté existe.")
  trackQuit(session, input, output, delay = 20)
}

shinyApp(ui, server)
