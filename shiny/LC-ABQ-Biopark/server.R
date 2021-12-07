#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
packages <- c("keyring")

library(shiny)

# Define server logic 
shinyServer(function(input, output){
    
   #Taxonomy File input 
    output$contentsOfTaxonomy <- renderTable({
        # input$file1 will be NULL initially. After the user selects and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath' column will contain the local filenames where the data can be found.
        TaxonomyFile <- input$taxonomy
        
        if (is.null(TaxonomyFile))
            return(NULL)
        
        read.csv(TaxonomyFile$datapath, header = input$header)
      
    })
    
    #darwincore file input
    output$DarwinCore<- renderTable({
            DarwinCoreFile <- input$darwinInput

            if (is.null(DarwinCoreFile))
                return(NULL)

            read.csv(DarwinCoreFile$datapath, header = input$header)
   })
   # spec.list <- inFile Breaks code
   # 
   #     # OutputFile2()
   #  #})
   #  
   #  submit 
   # output$usertxt <- renderText({
       # paste("username", input$GBIF_User)

   #})
    
    output$GBIF_users <- renderText({
       req(input$submit_GBIF)
       isolate(input$GBIF_User)
       gbif_user <<- input$GBIF_User
       #set to keychain 
       })
    
    output$GBIF_emails <- renderText({
       req(input$submit_GBIF)
       isolate(input$GBIF_email)
       gbif_email <<- gbif_email
       #set to keychain 
       })
    
    output$GBIF_password <- renderText({
       req(input$submit_GBIF)
       isolate(input$GBIF_Password)
       pwd = key_get("GBIF_Password")
       #set to keychain
       })
    
    
    
    
    
   #  output$userEmail <-renderText({
   #      paste("email", input$GBIF_email)
   #  })
   # #output$usergbifpass <<- input$GBIF_Password
   #  
   #  #big submit 
   #  #submit gbif credentials 
   #  #defalt values csv 
   #  output$downloadData <- downloadHandler(
   #      filename = function(){
   #          paste(input$file1, "OutputLCPipeline.csv", sep = "")
   #      }
   #      content = function(inFile) {
   #          write.csv(input$file1,inFile)
   #      }
   #  )
   #  output$downloadData <- downloadHandler(
   #      filename = function(){
   #          paste(input$file2, "OutputLCPipeline.csv", sep = "")
   #      }
   #      content = function(inFile2) {
   #          write.csv(input$file2,inFile2)
   #      }
   #  )
   #  output$value <- renderPrint({ input$num })
   # 
    
})
