#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output){
    
    output$contentsOfInputFile <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, header = input$header)
    })
    output$OutputFile2 <- renderTable(
        {
            inFile <- input$file1
            
            if (is.null(inFile))
                return(NULL)
            
            read.csv(inFile$datapath, header = input$header)
        }
    )
   
       # OutputFile2()
    #})
    
    
    output$downloadData <- downloadHandler(
        filename =function(){
            paste(input$file1, "OutputLCPipeline.csv", sep = "")
        },
        content = function(inFile2) {
            write.csv(input$file1,inFile2)
        }
    )
    
})
