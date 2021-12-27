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
shinyUI(fluidPage(# Application title
  titlePanel(
    h1("LC-Pipeline Test Site", align = "center")
  ),
  
  
  tabsetPanel(
    tabPanel(
      "Input and Run",
      fluid = T,
      
      #this layout contains a sidebar and a main page
      sidebarLayout(#start of sidebar panel
        sidebarPanel(
          tabsetPanel(
            tabPanel("inputs", source(file = "User_Inputs/UI/UI_inputTab1.R", local = T)[1]),
            tabPanel("occ", source(file = "User_Inputs/UI/UI_inputTab2.R", local = T)[1]),
            tabPanel("Threats", source(file = "User_Inputs/UI/UI_inputTab3.R", local = T)[1]),
            tabPanel("more Toggles", source(file = "User_Inputs/UI/UI_inputTab4.R", local = T)[1])
            
          )
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(source(
          file = "User_Inputs/UI/UI_mainPannel.R", local = T
        )[1]))
    ),
    
    tabPanel(
      "Output Tables",
      fluid = T,
      mainPanel(
      h3("download all fields data"),
      downloadHandler("DownloadFile", "all_fields"),
      
      h3("Synonym Tables"),
      tableOutput("NS_syn"),
      tableOutput("POW_syn"),
      
      h3("Occurrence Tables"),
      tableOutput("NS_occur"),
      tableOutput("POW_occur"),
      
      h3("Common Names Tables"),
      tableOutput("NS_cn"),
      tableOutput("POW_cn"),
      tableOutput("VC_cn"),
      
      
      h3("POW Result"),
      tableOutput("POW_result"),
      
      tableOutput("sample")
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
    )
  )))
