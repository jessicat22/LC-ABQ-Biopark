#### User Interface for test site ####
## Version 0.1
# Started: Sept2021
# Last worked on: dec27th 2021
# Author: JT
# File: UI.r
# Description: User interface for LC app 

citation(package = "rgbif")
library(shiny)
library(rgbif)

#makes the title for the application
#bug with application name 
shinyUI(fluidPage(# Application title
  title = "LC Pipeline Test Site",
  titlePanel(
    h1("LC-Pipeline Test Site", align = "center")
  ),
  
 
  
  tabsetPanel(
    
    tabPanel(
      "Home", fluid = T, 
      h1("Welcome to the LC-Pipeline test site! "),
      h2("In Colaboration with the EPICS program at UNM and the ABQ BioPark LC-Pipeline has gone online!"),
      h3("The next tab is the input and run tab in which you input and run the program"),
      h3("App is currently under major construction and not in a major version :)"),
      h2("v3.0.2"),
      h3("has the ability to: input file, gbif info, base package load, spec load")
      ),
    
    
    tabPanel(
      "Input and Run",
      fluid = T,
      
      #this layout contains a sidebar and a main page
      sidebarLayout(#start of sidebar panel
        sidebarPanel(
          tabsetPanel(
            tabPanel("Inputs", source(file = "User_Inputs/UI/UI_inputTab1.R", local = T)[1]),
            tabPanel("Occ", source(file = "User_Inputs/UI/UI_inputTab2.R", local = T)[1]),
            tabPanel("Threats", source(file = "User_Inputs/UI/UI_inputTab3.R", local = T)[1]),
            tabPanel("More Toggles", source(file = "User_Inputs/UI/UI_inputTab4.R", local = T)[1])
            
          )
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(source(file = "User_Inputs/UI/UI_mainPannel.R", local = T)[1]))
    ),
    
    tabPanel(
      "Output Tables for testing",
      fluid = T,
      mainPanel(
      h3("download all fields data"),
      downloadHandler("DownloadFile", "all_fields"),
      
      h3("Synonym Tables"),
      h4("Nature Serve"),
      tableOutput("NS_syn"),
      h4("POW"),
      tableOutput("POW_syn"),
      
      h3("Occurrence Tables"),
      h4("Nature Serve"),
      tableOutput("NS_occur"),
      h4("POW"),
      tableOutput("POW_occur"),
      
      h3("Common Names Tables"),
      h4("Nature Serve"),
      tableOutput("NS_cn"),
      h4("POW"),
      tableOutput("POW_cn"),
      tableOutput("VC_cn"),
      
      h3("sorced Files that have been ran are:"),
      tableOutput("sample"),
      textOutput("spec_load"),
      textOutput("GBIF_init"),
      textOutput("data_load")
      
      )
      
      
      
      
      
      # downloadButton("downloadData", "Download"),
      
      # textOutput("userEmail"),
      # numericInput("num", label = "Make changes", value = 1),
      # #submitButton("Submit Credentials", icon("refresh")),
      # helpText("When you click the button above, you should see",
      #          "the output below update to reflect the value you",
      #          "entered at the top:"),
      # verbatimTextOutput("value"),
      # textOutput("usertxt"),
      # textOutput("userEmail")
    ),
    tabPanel(
      "File Downloads", fluid = T
    )
  )))
