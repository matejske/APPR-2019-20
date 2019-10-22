library(shiny)
source('vizualizacija/vizualizacija.r')

function(input, output) {
  
  output$zemljevid.leto <- renderPlot({
    zemljevid.leto(input$leto) + theme(axis.text.x = element_blank())
  })
}
