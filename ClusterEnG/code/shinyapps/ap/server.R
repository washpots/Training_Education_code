#Mohith Manjunath
#2015-2016
#Upgraded version of the app on R Shiny website

library(shiny)
library(apcluster)

shinyServer(function(input, output, session) {
  val <- reactiveValues(x=NULL, y=NULL)    
  
  # Listen for clicks
  observe({
    # Initially will be empty
    if (is.null(input$clusterClick)){
      return()
    }
    
    isolate({
      val$x <- c(val$x, input$clusterClick$x)
      val$y <- c(val$y, input$clusterClick$y)
    })
  })
  
  # Count the number of points
  output$numPoints <- renderText({
    length(val$x)
  })
  
  # Clear the points on button click
  observe({
    if (input$clear > 0){
      val$x <- NULL
      val$y <- NULL
      output$heatmap <- renderPlot({
        plot.new()
      })
    }
  })
  
  # Generate the plot of the clustered points
  output$clusterPlot <- renderPlot({
    
    tryCatch({
      # Format the data as a matrix
      data <- matrix(c(val$x, val$y), ncol=2)
      
      # Try to cluster       
      if (length(val$x) <= 1){
        stop("We can't cluster less than 2 points")
      } 
      suppressWarnings({
        fit <- apcluster(negDistMat(r=2), data, q=0.2)
      })
      #output$summary <- renderTable({
      #  summary(fit)
      #})
      output$heatmap <- renderPlot({
        tryCatch({
          heatmap(fit, cexRow = 2, cexCol = 2, main = "Clustering samples")#, col = terrain.colors(12), legend = "col")
        }, error=function(warn){
          text(0, 0, "Add more points")
        })
      })
      par(mar = c(5, 5, 2, 2))
      plot(fit, data, xlab="X", ylab="Y", cex.lab = 2, cex.axis = 2)
    }, error=function(warn){
      # Otherwise just plot the points and instructions
      par(mar = c(5, 5, 2, 2))
      plot(val$x, val$y, xlim=c(-2, 2), ylim=c(-2, 2), xlab="X", ylab="Y", cex.lab = 2, cex.axis = 2)
      text(0, 0, "Click to add more points.")
    })
  })

})
