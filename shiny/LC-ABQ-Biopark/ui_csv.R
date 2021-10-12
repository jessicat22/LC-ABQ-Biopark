##ui.r for the csv graphics 
library(shiny)
shinyAppFile(){
CSV_Setup<- function(){
  sliderInput("Species",
              "Number of Species in uploaded file:",
              min = 1,
              max = 20,
              value = 10),
  br(),
  h4("Submit your .CSV below for varification:"),
  fileInput("file1", "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  tableOutput("contents")
}
  
}
  