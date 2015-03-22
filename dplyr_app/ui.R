library(shiny)
library(dplyr)
# ui.R

shinyUI(fluidPage(
  titlePanel("dplyr-tutorial app"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Scatterplot tab"),
      
      sliderInput("vols", label = "number of flights", min = 1500 , max = 5000, value = 0)
                    
        
        ),
    
    mainPanel(
      plotOutput("plot_2")
        
        
        )
      
      )
    )
  
)