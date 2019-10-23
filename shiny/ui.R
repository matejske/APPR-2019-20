library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Skupne vrednosti emisij v državah EU"),
  
  
  
  sidebarPanel(
    sliderInput("leto", "Izberi leto", min=2008, max=2017,
                value=2008, step=1, sep='', animate = 
                  animationOptions(interval = 250))
  ),
  mainPanel(plotOutput("zemljevid.leto")))
)
