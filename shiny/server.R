library(shiny)

function(input, output) {
  
  output$zemljevid.leto <- renderPlot({
    zemljevid.leto(input$leto, input$sektor) + theme(axis.text.x = element_blank())
  })
}


