#### ITIS Extract functions ####
## Version 3.1
# Started: 2 April 2021
# Last worked on: 23 April 2021
# Author: Clay Meredith
# File: Dependent_scripts/ITIS_functions.R
# Description: Queries ITIS database for potential taxonomic matches,
#              classifies match type, and returns ITIS data table for
#              taxonomic output.

#### Load packages ####
#### Load packages ####
packages <- c("tidyverse","taxize","ritis","httr")

lapply(packages, package.check)

#### Main Function ####
# Parameters: spec.list
# Returns: ITIS_data (table), ITIS_raw (table)
# Throws: none
# Purpose: Executes ITIS taxonomy search. Reformats results. 
#          Returns ITIS_data (table)
ITIS_main <- function() {
  print("Executing ITIS search.")
  # Search species names for exact matches
  ITIS_raw <<- itis_terms(spec.list$Species)
  # Reformat results
  ITIS_raw_reformat()
  # Define Match Type
  ITIS_match_type()
  # Split data into list of tables
  ITIS_data <- ITIS_split(ITIS_data)
  # Isolate match results for each taxon
  ITIS_data <- lapply(ITIS_data,ITIS_count_matches)
  # Search for synonym data
  ITIS_data <- lapply(ITIS_data,ITIS_synonym_search)
  # Collapse into single data frame
  ITIS_data <<- data.frame(bind_rows(ITIS_data, .id="id"))
  # Extract common names
  ITIS_common_names <<- ITIS_common_extract()
  
}

#### Functions ####

# Parameters: ITIS_raw (table)
# Returns: ITIS_data (table)
# Throws: none
# Purpose: Reformats results into single table
ITIS_raw_reformat <- function (){
  ITIS_temp <- ITIS_raw
  # Convert TSN to numeric field
  ITIS_temp$ids <- as.numeric(ITIS_raw$ids)
  # Rename list with species id
  names(ITIS_temp) <- spec.list$id
  # Remove elements of length 0
  ITIS_temp <- ITIS_temp[-which(lapply(ITIS_temp, length)<1)]
  # Collapse all fields into single data frame
  ITIS_temp <- data.frame(bind_rows(ITIS_temp, .id="id"))
  # Add field for name entered
  ITIS_temp$name_entered <- spec.list$Species[
    match(ITIS_temp$id,spec.list$id)]
  # Add field for author entered
  ITIS_temp$author_entered <- spec.list$author[
    match(ITIS_temp$id,spec.list$id)]
  # Generate column for ITIS match type
  ITIS_temp$itis_match <- NA
  # Convert from local to global variable
  ITIS_data <<- ITIS_temp
}

# Parameters: ITIS_data (table)
# Returns: ITIS_data (table)
# Throws: none
# Purpose: Specify match type based on number of results and accepted status.
#          Adds ITIS_data$itis_match column
ITIS_match_type <- function (){
  # Create column
  ITIS_data$itis_match <- NA
  # Specify type of result
  # Identify exact matches
  ITIS_data$itis_match[which(ITIS_data$scientificName == 
                               ITIS_data$name_entered &
                               ITIS_data$author == 
                               ITIS_data$author_entered)] <- "Exact Match"
  # Identify binomial matches
  ITIS_data$itis_match[which(ITIS_data$scientificName == 
                               ITIS_data$name_entered &
                               ITIS_data$author != 
                               ITIS_data$author_entered)] <- "Binomial Match"
  
  # Check for accepted exact matches
  ITIS_data$itis_match[which(ITIS_data$itis_match == 
                               "Exact Match" &
                               ITIS_data$nameUsage == 
                               "accepted")] <- "Exact Match Accepted"
  
  # Check for accepted matches matches
  ITIS_data$itis_match[which(ITIS_data$itis_match == 
                               "Binomial Match" &
                               ITIS_data$nameUsage == 
                               "accepted")] <- "Binomial Match Accepted"
  
  # Identify probable synonyms
  ITIS_data$itis_match[which(ITIS_data$itis_match %in% c("Binomial Match",
                                                         "Exact Match"))] <- 
    "Probable synonym"
  # Set as global variable
  ITIS_data <<- ITIS_data

}

# Parameters: ITIS_data (table)
# Returns: ITIS_data (list of tables)
# Throws: none
# Purpose: Return table to list of tables
ITIS_split <- function (x) {
  split(x , f=x$id)
}

# Parameters: ITIS_data (list of tables)
# Returns: ITIS_data (list of tables)
# Throws: none
# Purpose: Check match type and subset data accordingly. Order of operations
#          dictates prioritization of results. Exact matches (including author)
#          are subset before function proceeds. If exact match exists, others are 
#          discarded prior to further checks. This pattern continues for binomial
#          matches, then possible synonyms.
ITIS_count_matches <- function(x) {
  # Subset exact matches if one exact match exists
  if (nrow(x[which("Exact Match Accepted" %in% x$itis_match),]) == 1) {
    print("Exact Match Accepted")
    x <- x[which("Exact Match Accepted" %in% x$itis_match),]
  } else {
    # Subset accepted binomial matches
    if (nrow(x[which("Binomial Match Accepted" %in% x$itis_match),]) == 1) {
      print("Binomial Match Accepted")
      x <- x[which("Binomial Match Accepted" %in% x$itis_match),]
    } else {
      # Subset probable synonym matches
      if (nrow(x[which("Probable synonym" %in% x$itis_match),]) == 1) {
        print("Probable synonym")
        x <- x[which("Probable synonym" %in% x$itis_match),]
      } else {
        print("Taxonomic issues require inspection")
        x$itis_match <- "Taxonomic issues require inspection"
      }
      
    }
  }
  return(x)
}

# Parameters: ITIS_data (table)
# Returns: ITIS_data (table)
# Throws: none
# Purpose: Searches for accepted data for probable synonyms
ITIS_synonym_search <- function(x) {
  # Apply only if match is a probable synonym
  if (x$itis_match[1] == "Probable synonym") {
    # Query ITIS for accepted data associated with synonym
    ITIS_synonym_temp <-
      as.data.frame(accepted_names(x$tsn, wt = "json", raw = FALSE))
    # Check row length (flags species for additional scrutiny if more than
    # one accepted result)
    if (nrow(ITIS_synonym_temp) == 1) {
      # If one result, replace data with accepted result
      x$author <- ITIS_synonym_temp$author
      x$tsn <- ITIS_synonym_temp$acceptedTsn
      x$scientificName <- ITIS_synonym_temp$acceptedName
      x$itis_match <- "Synonym"
      x$nameUsage <- "accepted"
    } else {
      x$itis_match <- "Taxonomic issues require inspection"
    }
    return(x)
  } else {
    return(x)
  }
}

# Parameters: ITIS_data (table)
# Returns: ITIS_common_names (table)
# Throws: none
# Purpose: Extracts common names into separate table
ITIS_common_extract <- function (){
  common_row_num <- sapply(ITIS_data$commonNames, length)
  ITIS_common_names <-
    data.frame(
      internal_taxon_id = rep(ITIS_data$id, common_row_num),
      name = unlist(ITIS_data$commonNames),
      language = rep("English", sum(common_row_num)),
      primary = rep(FALSE , sum(common_row_num)),
      row.names = NULL
    )
  ITIS_common_names$source <- "ITIS"
  return(ITIS_common_names)
}

#### Execute script ####
ITIS_main()