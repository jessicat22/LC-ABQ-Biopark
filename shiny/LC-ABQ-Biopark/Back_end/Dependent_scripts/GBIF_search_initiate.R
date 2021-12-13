#### GBIF Search Initiate ####
## Version 3.1
# Started: 7 April 2021
# Last worked on: 30 June 2021
# Author: Clay Meredith
# File: Dependent_scripts/GBIF_search_initiate.R
# Description: Prepares data query and begins data compile for GBIF download.

#### Load Packages ####
#### Load packages ####
packages <- c("rgbif","dplyr")

#lapply(packages, package.check)

#### Main function ####
GBIF_search_main <- function (){
  # Prepare GBIF queries
  GBIF_qprep()
  print(GBIF_users)
  # Generate pred fields and initiates search
  gbif.out <<- occ_download(pred_in("taxonKey", GBIF_queries),
                           pred("hasCoordinate", TRUE),
                           pred("hasGeospatialIssue", FALSE),
                           user = output$GBIF_users,
                           pwd = output$GBIF_password,
                           email = output$GBIF_emails)
  
  # Save GBIF_key_results as .csv file
  print("GBIF search initiated.")
}

#### GBIF Functions ####

# Parameters: name (spec.list$Species)
# Returns: key_result (GBIF key for accepted species)
# Throws: none
# Purpose: Searches GBIF taxonomy and returns species key for later data retrieval.
#          Function is called within GBIF_qprep.

GBIF_key_check <- function (name) {
  key_result <-name_backbone(name, strict=TRUE)
  return(key_result)
}

# Parameters: spec.list$Species (list), GBIF_key_check (function)
# Returns: GBIF_key_result (table),  GBIF_queries (list)
# Throws: none
# Purpose: Searches GBIF taxonomy and returns species keys and taxonomy table.
#          Also generates GBIF_queries list which is later passed to GBIF to generate
#          point data.

GBIF_qprep <- function (){
  # Search GBIF for species key
  GBIF_key_result1 <- lapply(spec.list$Species, GBIF_key_check)
  # Rename key files with species id
  names(GBIF_key_result1) <- spec.list$id
  
  # Reformat results as a table
  GBIF_key_result <- data.frame(bind_rows(GBIF_key_result1, .id="name"))
  colnames(GBIF_key_result)[1] <- "internal_taxon_id"
  
  # Choose GBIF key to use for search
  GBIF_key_result$usageKey[GBIF_key_result$synonym==TRUE] <-
    GBIF_key_result$acceptedUsageKey[GBIF_key_result$synonym==TRUE]
  
  # Add entered name to GBIF_key_result table
  GBIF_key_result$name_entered <- spec.list
  
  # Assign gbif_key_result as global variable
  GBIF_key_result <<- GBIF_key_result
  
  # Write GBIF_key_result as global variable
  write.csv(GBIF_key_result, file = "Back_end/Datasets/GBIF_key_result.csv")

  # Asign queries to global variable for tables
  GBIF_queries <<- GBIF_key_result$usageKey[!is.na(GBIF_key_result$usageKey)]
}

#### Execute search functions ####
GBIF_search_main()