#Mohith Manjunath
#2015-2016
#Upgraded version of the app on R Shiny website

library(shiny)

shinyUI(
  
  # Create a bootstrap fluid layout
  fluidPage(
    titlePanel("Real-time clustering (Affinity Propagation)"),
    
    # Add a row for the main content
    fluidRow(      
      # Create a space for the plot output
      mainPanel(plotOutput(
          "clusterPlot", "100%", "500px", clickId="clusterClick"
      )),
      sidebarPanel("Heatmap of similarity matrix of data points: ",
                   plotOutput("heatmap"))
    ),
    
    fluidRow(
      mainPanel("Number of Points: ", verbatimTextOutput("numPoints")),
      sidebarPanel(actionButton("clear", "Clear Points"))
    )    
  )
)
