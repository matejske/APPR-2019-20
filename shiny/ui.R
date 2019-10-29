library(shiny)

shinyUI(fluidPage(
  titlePanel("Skupne vrednosti emisij v državah EU"),
  
  #Levi uporabniški vmesnik
  sidebarLayout(
    sidebarPanel(width = 7,
      selectInput("sektor", 
                  "Izberi sektor gospodarstva",
                  choices = unique(plini.sektorji$Sector.gospodarstva)),
      sliderInput("leto", 
                  "Izberi leto", 
                  min = 2008, max = 2017, value = 2008, step=1, 
                  sep='', animate = animationOptions(interval = 650)
                  )
    ),
  #Izris zemljevida  
    mainPanel(
      plotOutput("zemljevid.leto")
    )
  ))
)

