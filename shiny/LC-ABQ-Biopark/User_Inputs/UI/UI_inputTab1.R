list(

#file submit
#taxonoly file and darwincore file taken in 
#TODO rename file input to match pipeline backend 
#TODO Add to dataload a
h4("File Upload + GBIF Credentials + Submit"),
h3("File Uploads (Taxonomy Required)"),
helpText("Upload a taxonomy.csv file associated with the working set of your choosing. Species can be removed from the file, but the headers should be consistent with the download. Files with more than 20 species can be time consuming. Files with more than 50 species may cause errors. For larger working sets, running 20-30 species at a time is recommended."),
fileInput(
  "taxonomy",
  "Taxonomy File - Required",
  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
),
fileInput(
  "darwinInput",
  "Optional: Upload DarwinCore distribution data associated with the species in your taxonomy file.",
  
  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
),
# Horizontal line ----
checkboxInput("header", "Header", TRUE),
tags$hr(),
#passwords for logins
h3("GBIF Credential (optional)"),
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
textOutput("gbif_success"),
tags$hr(),

actionButton("submit_file", "Submit -> Run Program")
)


