library(shiny)
source('vizualizacija/vizualizacija.r')
shinyUI(fluidPage(
  
  titlePanel("Zemljevid"),
  
  
  
  sidebarPanel(
    sliderInput("leto", "Izberi leto", min=2008, max=2017,
                value=2008, step=1, sep='', animate = 
                  animationOptions(interval = 250))
  ),
  mainPanel(plotOutput("zemljevid.leto")))
)

