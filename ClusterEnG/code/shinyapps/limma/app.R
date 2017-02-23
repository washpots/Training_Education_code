#Mohith Manjunath
#2015-2016

# Global variables can go here
options(shiny.maxRequestSize=200*1024^2)
#source("https://bioconductor.org/biocLite.R")
#biocLite(c("affy", "limma"))
library(affy)   # Affymetrix pre-processing
library(limma)  # two-color pre-processing; differential
# expression

# Define the UI
ui <- fluidPage(
  titlePanel("limma (R Package): Differential gene expression"),
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style(
        type = 'text/css',
        'pre { max-height: 200px; overflow-y: auto; }')),
      h3("Learn limma!"),
      h4("Perform quality check on your sequence data, learn about input and output formats usign this application."),
      tags$h4(tags$a(href = "http://www.bioinformatics.babraham.ac.uk/projects/fastqc/", "Website")),
      textInput("text", label = h4("Enter command here (see steps below)")),
      tabsetPanel(type = "tabs", 
                  tabPanel("Step 1", h4("Sample file sequence1.fastq has been uploaded:"),
                           h4("fastqc sequence1.fastq", style = "color:green"),
                           h4("If the program ran successfully then click 'Step 2' above.")), 
                  tabPanel("Step 2", h4("Another sample file sequence2.fastq has been uploaded:"),
                           h4("fastqc sequence2.fastq", style = "color:green"),
                           h4("If the program ran successfully then compare results."))
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
      htmlOutput("results")
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
      textOut <- "design <- model.matrix(~combn) # describe model to be fit

fit <- lmFit(eset, design)  # fit each probeset to model
efit <- eBayes(fit)        # empirical Bayes adjustment
topTable(efit, coef=2)      # table of differentially expressed probesets"
    }
    textOut
  })
  
  
  
  output$contents <- renderTable({
    
    
    ## import "phenotype" data, describing the experimental design
    #phenoData <- read.AnnotatedDataFrame(system.file("extdata", "pdata.txt", package="arrays"))
    
    ## RMA normalization
    #celfiles <- system.file("extdata", package="arrays")
    #eset <- justRMA(phenoData=phenoData,
                    #celfile.path=celfiles)
    ## differential expression
    #combn <- factor(paste(pData(phenoData)[,1],
                          #pData(phenoData)[,2], sep = "_"))
    #design <- model.matrix(~combn) # describe model to be fit
    #fit <- lmFit(eset, design)  # fit each probeset to model
    #efit <- eBayes(fit)        # empirical Bayes adjustment
    #data <- topTable(efit, coef=2)      # table of differentially expressed probesets
    #data.frame(data)
    data.frame("dd")
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
