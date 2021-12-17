#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


packages <- c("keyring")
packages <- c("bit64","rgbif","data.table","stringr")
library(shiny)
library("rgbif")
library("keyring")
# Define server logic 
shinyServer(function(input, output){
    
   #Taxonomy File input 
    output$contentsOfTaxonomy <- renderTable({
        # input$file1 will be NULL initially. After the user selects and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath' column will contain the local filenames where the data can be found.
        TaxonomyFile <- input$taxonomy
        
        if (is.null(TaxonomyFile))
            return(NULL)
        
        spec.list <<- data.frame(read.csv(TaxonomyFile$datapath, header = input$header))
      
    })
    
    #darwincore file input
    output$DarwinCore<- renderTable({
            DarwinCoreFile <- input$darwinInput

            if (is.null(DarwinCoreFile))
                return(NULL)

            DC_point_data<<- data.frame(read.csv(DarwinCoreFile$datapath, header = input$header))
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
       #gbif_email <<- input$GBIF_email
       })
    
    output$GBIF_password <- renderText({
       req(input$submit_GBIF)
       isolate(input$GBIF_Password)
       
       #set to keychain
       })
    
    
    output$gbif_success <- renderPrint({
       req(input$submit_GBIF)
        occ_download_list(
          user = input$GBIF_User,
          pwd = input$GBIF_Password,
          limit = 20,
          start = 0
          )
       "successful login"
            
    })
    
    
    #with everything from csv file create a csv and append each row in order 
    #when submit button is pressed req(input$submit_file)
    #maybe req(input$taxonomycsv)
    
    
  
   
    
    
  
    #submit button variables set to their variable names when big submit is pressed 
  output$sample<- renderTable({
    req(input$submit_file)
    allfields.template <<- data.frame(
      Var_Name = c("presence_code","seasonal_code",
                   "orgin_code","pop.data.qual","aoo just",
                   "eoo just", "del year","sens", 
                   "inat","nothreats","threats.unkown",
                   "rationale.text","threats.text", "outlier_threshold",
                   "uncertainty_tolerance","min_decimals",
                   "precision_method",
                   "occurrenceRemarks_introduced","is.restricted.eoo.cutoff",
                   "is.restricted.aoo.cutoff","is.restricted.justification",
                  "pop.narrative", "threats.narrative",
                   "throttle_points","throttle_level"
                   ),
      Value = c(isolate(input$presence_code), isolate(input$seasonal_code), 
                isolate(input$origin_code), input$pop.data.qual, isolate(input$aoo_just),
                isolate(input$eoo_just), isolate(input$del_year),isolate(input$sens),
                isolate(input$inat),isolate(input$nothreats),isolate(input$threats.unknown),
                isolate(input$rationale.text),isolate(input$threats.text),isolate(input$outlier_threshold),
                isolate(input$uncertainty_tolerance),isolate(input$min_decimals),
                isolate(input$precision_method),
                isolate(input$occurrenceRemarks_introduced), isolate(input$is.restricted.eoo.cutoff),
                isolate(input$is.restricted.aoo.cutoff), isolate(input$is.restricted.justification),
                isolate(input$pop.narrative), isolate(input$threats.narrative),
                isolate(input$throttle.points), input$throttle_level
               ))
   gbif_user <<- input$GBIF_User
   gbif_email <<- input$GBIF_email
   gbif_password <<- input$GBIF_Password
   TaxonomyFile <- input$taxonomy
   spec.list <<- data.frame(read.csv(TaxonomyFile$datapath, header = input$header))
   source("LC_pipeline_main.R")
    
    # write.table(rendercsvuser, file = "sample.csv")
    })
  output$POW_result<-renderTable({
    req(input$submit_file)
    
   POW_results
    
  })
  output$all_fields<-renderTable({
    req(input$submit_file)
    
    allfields.template
    
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
