#
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel(
        h1("LC-Pipeline Test Site", align = "center")
        ),

    # Sidebar with a slider input for number of bins

    sidebarLayout(
        sidebarPanel(
            h3("Step 1: Enter your CSV file"),
            #spec number 
            sliderInput("Species","Number of Species in uploaded file:", min = 1, max = 20, value = 10),
            br(),
            #file submit
            h4("Upload your .CSV below for varification:"),
            fileInput("file1", "Choose CSV File", multiple = FALSE,
                      accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
            # Horizontal line ----
            checkboxInput("header", "Header", TRUE),
            tags$hr(),
            #passwords for logins 
            h3("Step 2: Provide login information for specific sites (optional)"),
            h5("input a"),
            passwordInput("password1", "Password:"),
            h5("input b"),
            passwordInput("passwordb", "Password:"),
            h5("input c"),
            passwordInput("passwordc", "Password:")
        ),
        # Main panel for displaying outputs ----
        mainPanel(
            # Output: Data file ----
            h3("Double Check Uploaded CSV Below:"),
            tableOutput("contents"),
            h3("Step 3: Submit information"), 
            actionButton("submit_file", "Run species in LC pipeline")
            
        )
    )
))
