library(shiny)
#Ustvarimo seznam moznih izbir ===============================================
izbire <- unique(emisije$Sector.gospodarstva)
izbire <- izbire[-19]


# Uporabniski vmesnik ========================================================
shinyUI(fluidPage(
  titlePanel("Skupne vrednosti emisij v državah EU"),
  
  #Levi uporabniški vmesnik
  sidebarLayout(
    sidebarPanel(width=7,
      selectInput("sektor", 
                  "Izberi sektor gospodarstva",
                  choices=izbire,
                  selected = "Agriculture, forestry and fishing"
                  ),
      sliderInput("leto", 
                  "Izberi leto", 
                  min=1997, max=2017, value=2008, step=1, 
                  sep='', 
                  animate=animationOptions(interval=650)
                  )
    ),
  #Izris zemljevida  
    mainPanel(
      plotOutput("zemljevid.leto")
    )
  ))
)

