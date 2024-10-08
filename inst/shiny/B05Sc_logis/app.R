learnitdown::learnitdownShinyVersion("2.0.0")
conf <- BioDataScience::config()

library(shiny)
library(learnitdown)
library(BioDataScience2)

asym_init <- 5
xmid_init <- 4
scal_init <- 0.5
error_sd <- 0.1
set.seed(42)

model_data <- tibble::tibble(
  x = seq(0, 8, by = 0.1),
  y = SSlogis(x, Asym = asym_init, xmid = xmid_init, scal = scal_init) +
    rnorm(n = length(x), sd = error_sd))

graph <- chart::chart(model_data, y ~ x) +
  ggplot2::geom_point() +
  ggplot2::xlab("x") +
  ggplot2::ylab("y")

ui <- fluidPage(
  learnitdownShiny("Ajustement manuel d'un modèle : courbe logistique"),

  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      p("$$y(x) = \\frac{Asym}{1 + e^{\\frac{xmid - x}{scal}}}$$"),

      sliderInput("asym", label = "Asym",
        value = 1.00, min = 0.50, max = 10.00, step = 0.5),
      sliderInput("xmid", label = "Xmid",
        value = 1.00, min = 0.25, max = 10.00, step = 0.25),
      sliderInput("scal", label = "Scal",
        value = 1.00, min = -2.00, max = 6.00, step = 0.25),

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
      y_predit = SSlogis(x, Asym = input$asym, xmid = input$xmid,
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
    p <- graph

    if (!any(is.nan(data$y_predit))) {
      p <- p +
        ggplot2::geom_line(chart::f_aes(y_predit ~ x), color = "red", data = data)
    }
    p
  })

  trackEvents(session, input, output,
    sign_in.fun = BioDataScience::sign_in, config = conf)
  trackSubmit(session, input, output, max_score = 3, solution =
    list(asym = asym_init, xmid = xmid_init, scal = scal_init),
    comment = "y = asym/1+e(xmid-x/scal)",
    message.success = "Correct, c'est le meilleur modèle.",
    message.error = "Incorrect, un modèle mieux ajusté existe.")
  trackQuit(session, input, output, delay = 20)
}

shinyApp(ui, server)
