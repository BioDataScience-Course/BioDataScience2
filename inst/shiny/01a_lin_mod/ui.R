library(shiny)

titlePanel_h4 <- function(title, windowTitle = title) {
  tagList(tags$head(tags$title(windowTitle)), h4(title))
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel_h4("Le modèle linéaire intuitif"),
    sidebarLayout(
        sidebarPanel(
          withMathJax(),
          p("Une droite suit l'équation mathématique suivante: $$ y = ax + b $$ "),
          numericInput(inputId = "slope_ui", label = "Valeur de la pente (a)",
            value = 0.00, min = -10.00, max = 10.00, step = 0.5),
          p("Valeur par défaut : 0 "),
          numericInput(inputId = "intercept_ui", label = "Valeur de l'ordonnée à l'origine (b)",
            value = 1.00, min = -10.00, max = 10.00, step = 0.5),
          p("Valeur par défaut : 1 ")
        ),
        mainPanel(
          h4("Ajustez aux mieux la droite dans ce nuage de points"),
          p("Nous pouvons traduire cette formule pour notre cas concret $$Volume = pente \\times diametre \\ + \\ ordonnee \\ à \\ l \\ origine$$ "),
          plotOutput("trees_plot")
        )
    )
))
