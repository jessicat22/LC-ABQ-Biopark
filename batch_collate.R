#### IUCN Red List Batch Collate ####
## Version 1.0
# Started: 1 July 2021
# Last worked on: 1 July 2021
# Author: Clay Meredith
# File: Back_end/Dependent_scripts/batch_collate.R
# Description: Collates output tables from least concern pipeline into individual files
#              for upload using SIS Connect.

#### Main function ####
LC_collate_main <- function (){
  parameters <- c("Assessments","Countries","Credits","Point","References",
                  "species_inputs","Allfields","common_names","Synonyms")
  lapply(parameters, FILE_COLLATE)
  
}

#### Dependent Functions ####
FILE_COLLATE <- function (x){
  # Identify files fitting pattern
  files_to_collate <- paste("Outputs/",
                            list.files(path = "Outputs/", pattern = x),
                            sep = "")
  # Extract batch numbers
  batch_names <- files_to_collate
  batch_names <- gsub("[^0-9]", "", batch_names)
  # Read .csv files
  collated_files <- lapply(files_to_collate, read.csv)
  # Apply names to 
  names(collated_files) <- batch_names
  # Collate files into single table
  collated_files <- data.frame(rbindlist(collated_files, 
                                         fill = TRUE,
                                         idcol = "batch_no"))
  # Write table to new .csv file
  write.csv(collated_files, file = paste("Outputs/",x,".csv", sep = ""),
            row.names = FALSE)
}

LC_collate_main()
