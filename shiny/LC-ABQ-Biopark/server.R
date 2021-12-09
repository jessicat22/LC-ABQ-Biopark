#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
packages <- c("keyring")
packages <- c("bit64","rgbif","data.table","stringr")
library(shiny)
library(rgbif)

# Define server logic 
shinyServer(function(input, output){
    
   #Taxonomy File input 
    output$contentsOfTaxonomy <- renderTable({
        # input$file1 will be NULL initially. After the user selects and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath' column will contain the local filenames where the data can be found.
        TaxonomyFile <- input$taxonomy
        
        if (is.null(TaxonomyFile))
            return(NULL)
        
        spec.list <- data.frame(read.csv(TaxonomyFile$datapath, header = input$header))
      
    })
    
    #darwincore file input
    output$DarwinCore<- renderTable({
            DarwinCoreFile <- input$darwinInput

            if (is.null(DarwinCoreFile))
                return(NULL)

            read.csv(DarwinCoreFile$datapath, header = input$header)
   })
  
   # output$usertxt <- renderText({
       # paste("username", input$GBIF_User)

   #})
    
    #output gbif cred
    output$GBIF_users <- renderText({
       req(input$submit_GBIF)
       isolate(input$GBIF_User)
       gbif_user <<- input$GBIF_User
       })
    
    output$GBIF_emails <- renderText({
       req(input$submit_GBIF)
       isolate(input$GBIF_email)
       gbif_email <<- gbif_email
       })
    
    output$GBIF_password <- renderText({
       req(input$submit_GBIF)
       isolate(input$GBIF_Password)
       pwd = key_get("GBIF_Password")
       #set to keychain
       })
    
    
    output$gbif_sucess <- renderText({
       req(input$submit_GBIF)
       occ_download_list(
          user = input$GBIF_User,
          pwd = input$GBIF_Password,
          limit = 20,
          start = 0,
          curlopts = list()
       )
    })
    spacial_collect_toggle <<- renderText({ 
       req(input$submit_file)
       isolate(input$Spacial_calc)
    })
    
    
    
    
    #with everything from csv file create a csv and append each row in order 
    #when submit button is pressed req(input$submit_file)
    #maybe req(input$taxonomycsv)
    
    
    presence_code <<- renderText({
       req(input$submit_file)
       isolate(input$presence_code)
    })
    
    seasonal_code <<- renderText({
       req(input$submit_file)
       isolate(input$seasonal_code)
    })
    
    
    
    
    #submit button variables set to their variable names when big submit is pressed 
  output$sample<- renderTable({
    
    rendercsvuser = data.frame(ID = 1:4, Name = c("A","B","C","D"),
                      Post=c("Peon","SDE","Manager","SDE"), 
                      Age = c(23,39,28,39))
    
    # write.table(rendercsvuser, file = "sample.csv")
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
