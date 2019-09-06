#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
SciViews::R()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    trees <- data.io::read("trees", package = "datasets", lang = "FR") %>.%
        dplyr::select(., -height)

    output$trees_plot <- renderPlot({

        chart::chart(trees, volume ~ diameter) +
            ggplot2::geom_point() +
            geom_abline(aes(slope = input$slope_ui, intercept = input$intercept_ui))
    })
})
