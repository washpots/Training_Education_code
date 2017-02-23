#Mohith Manjunath
#2015-2016

# Global variables can go here
options(shiny.maxRequestSize=200*1024^2)
system("rm NA_* prefix*")

# Define the UI
ui <- fluidPage(
  titlePanel("MACS: Model-based Analysis of ChIP-seq"),
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style(
        type = 'text/css',
        'pre { max-height: 200px; overflow-y: auto; }')),
      h3("Learn MACS"),
      h4("Step-by-step tutorial on analyzing ChIP-seq data using MACS software."),
      tags$h4(tags$a(href = "https://github.com/taoliu/MACS", "Source code")),
      textInput("text", label = h4("Enter command here (see steps below)")),
      tabsetPanel(type = "tabs", 
                  tabPanel("Step 1", h4("Sample file Treatment_tags_chr1.bed uploaded. Please enter the following command in the textbox above to call peaks:"),
                           h4("macs2 callpeak -t Treatment_tags_chr1.bed", style = "color:green"),
                           h4("If the program ran successfully then click 'Step 2' above.")), 
                  tabPanel("Step 2", h4("Enter the following to generate a PDF file containing peak model plots:"),
                           h4("Rscript NA_model.r", style = "color:green"),
                           h4("Now go to Step 3.")), 
                  tabPanel("Step 3", h4("Enter the following to generate plots in PNG format and show output on the right:"),
                           h4("pdftoppm -png NA_model.pdf prefix", style = "color:green"))
      ),
      hr(),
      #actionButton("showImage", "See results"),
      checkboxInput('plot', 'Plot', TRUE),
      submitButton("Submit")
    ),
    mainPanel(
      tableOutput("contents"),
      verbatimTextOutput("value"),
      textOutput("status"),
      imageOutput("myImage")
    )
  )
)

# Define the server code
server <- function(input, output) {
  updateOut <- reactive({ input$text })
  #checkPlot <- reactive({ input$plot })
  output$value <- renderPrint({
    dataOut <- updateOut()
    if(dataOut == "") 
      return("Command output section")
    else {
      #output$status <- renderText({ "Job running..." })
      textOut <- system(paste(as.character(dataOut), " 2>&1", collapse = "", sep = ""), intern = TRUE)
      output$status <- renderText({ getwd() })
      output$status <- renderText({ "Done! Now enter the next command." })
      output$myImage <- renderImage({
        filename <- normalizePath(file.path('./prefix-1.png'))
        list(src = filename,
             alt = paste("Plot"),
		width = 700,
         	height = 500)
      }, deleteFile = FALSE)
      textOut
    }
  })

  

  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.table(inFile$datapath, header=input$header, sep=input$sep)
    head(data)
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
