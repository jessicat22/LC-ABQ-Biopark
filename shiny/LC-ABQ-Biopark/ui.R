#### User Interface for test site ####
## Version 0.1
# Started: Sept2021
# Last worked on: oct 14th 2021
# Author: JT
# File: UI.r
# Description: User interface for LC app 

library(shiny)
source("LC-ABQ-Biopark/ui_csv.r")
shinyUI(fluidPage(# Application title
    titlePanel(
        h1("LC-Pipeline Test Site", align = "center")
    ),
    #this layout contains a sidebar and a main page
    sidebarLayout(
        #start of sidebar panel
        sidebarPanel(tabsetPanel(
            #first tab panel for general toggles and options
            tabPanel(
                "Inputs",
                #file submit
                h4("Upload your .CSV below for varification:"),
                fileInput(
                    "file1",
                    "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                ),
                # Horizontal line ----
                checkboxInput("header", "Header", TRUE),
                tags$hr(),
                #passwords for logins
                h3("Step 2: Provide login information for specific sites (optional)"),
                br(),
                h4("GBIF Credentials"),
                splitLayout(
                    textInput(
                        "GBIF_User",
                        "GBIF Username:",
                        value = "",
                        width = 200
                    ),
                    #username
                    textInput("GBIF_email", "GBIF Email:"),
                    passwordInput(
                        "GBIF_Password",
                        "GBIF Password:",
                        value = "",
                        width = 200
                    )#password
                ),
                actionButton("submit_GBIF", "Submit Credentials"),
                br(),
                tags$hr(),
                #IUCN data
                h4("IUCN Red List Credentials"),
                passwordInput(
                    "RL_Password",
                    "Red List API Token:",
                    value = "",
                    width = 200),
                #password
                actionButton("submit_RL", "Submit Credentials"),
                br(),
                tags$hr(),
                #spacial Calculations
                checkboxInput(
                    "Spacial_calc",
                    "Collect spatial statistics data (significantly slows runtime)?",
                    FALSE),
                #submit all info
                tags$hr(),
                h3("Step 3: Submit information"),
                actionButton("submit_file", "Submit")
            ),
            
            
            #beginning of second panel for toggle options
            tabPanel(
                "Occurance",
                
                h4("Occurance Remarks"),
                radioButtons(
                    "ocurrenceRemarks_cull",
                    " Occurrence Remarks",
                    c(" Cult" = "cult", "Garden" = " garden")
                ),
                
                checkboxInput(
                    "occurrenceRemarks_introduced",
                    "Occurrence Remarks Introduced Escaped",
                    1
                ),
                h4("AOO, EOO toggle"),
                splitLayout(
                    numericInput("is.restricted.aoo.cutoff", "Restricted AOO Cutoff", value =
                                     500),
                    numericInput("is.restricted.eoo.cutoff", "Restricted EOO Cutoff", value =
                                     40000)
                ),
                textAreaInput("eoo_just", "EOO Justification",
                              value = "EOO calculated based on GBIF.org (2020) occurrence records trimmed to include only the species' native range as defined by Plants of the World Online, NatureServe, and VASCAN. The lower value is derived from the same data with the 10% of points furthest from the species' centroid removed.",
                              400, 150),
                
                textAreaInput("aoo_just", "AOO Justification",
                              value = "AOO AOO was calculated based on publicly available point data from GBIF. The reported upper value is calculated based on the number of 2 km x 2 km grid cells occupied by points occurring within WGSRPD regions where the species is confirmed to be native. The lower value is derived from the same data with the 10% of points furthest from the species' centroid removed. Both figures should be considered underestimates given incomplete sampling of the species.",
                              400, 220),
                
                textAreaInput(
                    "is.restricted.justification",
                    "Restricted Justification",
                    value = "Records of the species are abundant and there is currently no indication that the species would qualify for listing under a threatened category based on its distribution.",
                    400,
                    100
                )
                #is restricted just
            ),
            tabPanel(
                "Threats",
                h5("threats"),
                checkboxInput("nothreats", "No Threats", 1),
                checkboxInput("threats_unknown", "Threats Unknown", 0),
                #threats.narrative	rationale.text	threats.unknown	threats.text	nothreats
                textAreaInput("threats.narrative", "Threats Narrative",
                              value = "The species has a large range, and numerous records of the species are available. At present, no threats likely to drive the species into a threatened category are known.",
                              400, 100),
                textAreaInput("rationale.text", "Rationale Text",
                              value = "The species has a large range, and there is currently no indication of widespread population decline. It is therefore listed as Least Concern",
                              400, 100),
                textAreaInput("threats.text", "Threats Text",
                              value = "There are no known significant threats to the species at this time",
                              400, 50)
                
            ),
            tabPanel(
                "More Toggles",
                h5("title"),
                splitLayout(
                    checkboxInput("del_year", "Delete Year", 1),
                    checkboxInput("sens", "Sens", 0),
                    checkboxInput("inat", "Inat", 1)
                ),
                h5("title"),
                splitLayout(
                    numericInput("presence_code", "Presence Code", 1, 0, 3, 1),
                    numericInput("origin_code", "Origin Code", 1, 0, 3, 1)
                ),
                
                
            )
        )),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Input: Data file ----
            h4(
                "ABQ BioPark’s LC Pipeline is intended to streamline the processing
               of data for widely distributed species in preparation for submission
               of Least Concern assessments to the IUCN Red List. The script is
               intended to scrape a variety of sources for data, parse point data,
               and return spreadsheets formatted for upload using SIS Connect.
               The script is not intended to be a substitute for human assessors
               and all assessments generated using the tool will require review
               according to the regular processes outlined in the IUCN Red List Rules of Procedure."
            ),
            tableOutput("contentsOfInputFile"),
            tableOutput("OutputFile2"),
            downloadButton("downloadData", "Download")
            
        )
    )))