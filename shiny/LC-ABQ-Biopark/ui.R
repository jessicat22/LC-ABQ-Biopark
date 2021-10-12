#
#

library(shiny)

shinyUI(fluidPage(
    # Application title
    titlePanel(
        h1("LC-Pipeline Test Site", align = "center")
    ),
    #this layout contains a sidebar and a main page 
    sidebarLayout(
        
        #start of sidebar panel 
        sidebarPanel(
            tabsetPanel(
                
                #first tab panel for general toggles and options 
                tabPanel( "tab1",
                    h3("Step 1: Enter your CSV file"),
            #spec number 
                    sliderInput("Species","Number of Species in uploaded file:", min = 1, max = 15, value = 10),br(),
            #file submit
                    h4("Upload your .CSV below for varification:"),
                    fileInput("file1", "Choose CSV File", multiple = FALSE,
                    accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
            # Horizontal line ----
                    checkboxInput("header", "Header", TRUE),
                    tags$hr(),
            #passwords for logins 
                    h3("Step 2: Provide login information for specific sites (optional)"),br(),
                    h4("GBIF Credentials"),
                    splitLayout(
                        textInput("GBIF_User","GBIF Username:",value = "", width = 200),#username
                        textInput("GBIF_email","GBIF Email:"),
                        passwordInput("GBIF_Password", "GBIF Password:",value = "", width = 200)#password
                    ),
                    actionButton("submit_GBIF", "Submit Credentials"),br(), tags$hr(),
            #IUCN data
                    h4("IUCN Red List Credentials"),
                    passwordInput("RL_Password", "Red List API Token:", value = "", width = 200),#password
                    actionButton("submit_RL", "Submit Credentials"),br(), tags$hr(),
            #spacial Calculations
                    checkboxInput("Spacial_calc", "Collect spatial statistics data (significantly slows runtime)?", FALSE),
            #submit all info
                    tags$hr(),h3("Step 3: Submit information"), 
                    actionButton("submit_file", "Submit")
                ), 
            
            #beginning of second panel for toggle options 
                tabPanel( "tab2", 
                    h5("title"),
                    splitLayout(
                        checkboxInput("del_year","Delete Year",1),checkboxInput("sens","Sens",0),
                        checkboxInput("inat","Inat",1)
                    ),
                    h5("threats"),
                    checkboxInput("nothreats","No Threats",1),
                    checkboxInput("threats_unknown", "Threats Unknown",0),
                    h5("title"),
                    splitLayout(
                         numericInput("presence_code", "Presence Code",1,0,3,1), 
                         numericInput("seasonal_code", "Seasonal Code",1,0,3,1),
                         numericInput("origin_code","Origin Code",1,0,3,1)
                    ),
                    
                    h4("AOO, EOO toggle"),
                    h4("aoo eoo directions"),
                    h5("AOO AOO was calculated based on publicly available point data from GBIF . 
                        The reported upper value is calculated based on the number of 2 km x 2 km grid
                        cells occupied by points occurring within WGSRPD regions where the species is 
                        confirmed to be native. The lower value is derived from the same data with the 
                        10% of points furthest from the species' centroid removed. Both figures should 
                        be considered underestimates given incomplete sampling of the species."),
                    
                    h5("EOO calculated based on GBIF.org (2020) occurrence records 
                        trimmed to include only the species' native range as defined by Plants of the World Online, 
                        NatureServe, and VASCAN. 
                        The lower value is derived from the same data with the 10% of points furthest from the species' centroid removed."),
                    
                 )
            )
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Input: Data file ----
            h4("ABQ BioPark’s LC Pipeline is intended to streamline the processing 
               of data for widely distributed species in preparation for submission 
               of Least Concern assessments to the IUCN Red List. The script is 
               intended to scrape a variety of sources for data, parse point data, 
               and return spreadsheets formatted for upload using SIS Connect. 
               The script is not intended to be a substitute for human assessors 
               and all assessments generated using the tool will require review 
               according to the regular processes outlined in the IUCN Red List Rules of Procedure."),
            tableOutput("contentsOfInputFile"),
            tableOutput("OutputFile2"),
            downloadButton("downloadData", "Download")
            
        )
    )
))