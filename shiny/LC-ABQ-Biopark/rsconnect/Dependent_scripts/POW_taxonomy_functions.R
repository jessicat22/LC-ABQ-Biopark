#### Kew Taxonomy Check and distribution scrape ####
## Version 3.1
# Started: 13 April 2021
# Last worked on: 18 June 2021
# Author: Clay Meredith
# File: Dependent_scripts/POW_taxonomy_functions.R
# Description: Searches Kew's Plants of the World Online database
#              and returns data regarding distribution, and taxonomy.
# Error Notes: Currently produces no results for some infraspecific taxa
#              Error observed for Delphinium ramosum var. alpestre, and 
#              Asclepias uncialis subsp. ruthiae. Both taxa produce results
#              using POW_search_throttled, but results not appended to 
#              POW_raw_data for unknown reasons.

#### Load packages ####
packages <- c("taxize","tidyverse")

lapply(packages, package.check)

#### Main Function ####
POW_tax_main <- function (){
  print("Reformatting Kew POWO results")
  # Build POW Results Table
  POW_results <<- data.frame(spec_id = spec.list$id)
  # Run search function
  POW_raw_data <<- lapply(spec.list$Species,POW_search_throttled)
  # Append species ID to POW results
  POW_id_append()
  # Bind results into single list of data frames
  POW_data <<- POW_data_combine()
  # Count number of POW results for each taxon
  POW_count_results()
  # Add column for data entered
  POW_data_entered()
  # Split POW_data into list of tables
  POW_data_split()
  # Flag match type
  POW_data <- lapply(POW_data, POW_taxonomy_parse)
  # Subset data based on match type
  POW_data <- lapply(POW_data,POW_subset_results)
  # Recombine data into single data frame
  POW_data <<- bind_rows(POW_data, .id = "id")
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Define throttled kew search function
POW_search_throttled <- function (x) {
  POW_temp <- NULL
  POW_temp <- tryCatch(
    pow_search(x),
    error = function(e)
      NA
  )
  Sys.sleep(0.25)
  return(POW_temp)
}

# Parameters: spec.list$id
# Returns: POW_data
# Throws: none
# Purpose: Append species ID numbers to raw results
POW_id_append <- function() {
  # Rename raw results with species id numbers
  names(POW_raw_data) <- spec.list$id
  # Subset only species with returned results
  POW_raw_data <- POW_raw_data[which(!is.na(POW_raw_data))]
  # Add species id number to meta list
  for (i in 1:length(POW_raw_data)) {
    #nrow(spec.list)
    POW_raw_data[[i]]$meta$internal_taxon_id <- spec.list$id[i]
    POW_raw_data[[i]]$meta$input_name <- spec.list$Species[i]
  }
  POW_data <<- POW_raw_data
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Function to collapse data section of POW_data_combine 
# (used in POW_data_entered)
pow_collate <- function (x){
  x$data
}

# Parameters: POW_data
# Returns: POW_data
# Throws: none
# Purpose: Collapse data fields into single list of data frames
POW_data_combine <- function (){
  foo <- lapply(POW_data, pow_collate)
  names(foo) <- names(POW_data)
  bind_rows(foo, .id = "id")
}

# Parameters: POW_data
# Returns: POW_results (adds n_results column)
# Throws: none
# Purpose: Add count of species results to POW_results
POW_count_results <- function (){
  # Count number of results for each taxon
  temp <- data.frame(table(POW_data$id))
  # Create number of results column
  POW_results$n_results <- 0
  # Add column for number of results
  POW_results$n_results[which(POW_results$spec_id %in% temp$Var1)] <- 
    temp$Freq[which(temp$Var1 %in% POW_results$spec_id)]
  # Assign as global variable
  POW_results <<- POW_results 
}

# Parameters: POW_data (table)
# Returns: POW_data
# Throws: none
# Purpose: Add columns for entered species name, and authority
POW_data_entered <- function (){
  # Append binomial from spec.list to POW results
  POW_data$name_entered <- spec.list$Species[match(POW_data$id,spec.list$id)]
  # Append authority from spec.list to POW results
  POW_data$author_entered <- spec.list$author[match(POW_data$id,spec.list$id)]
  # Build column for match type
  POW_data$match_type <- NA
  # Assign to global variable
  POW_data <<- POW_data
}

# Parameters: POW_data (table)
# Returns: POW_data (list of tables)
# Throws: none
# Purpose: Splits POW_data from single table to list of tables
POW_data_split <- function (){
  # Divide POW_data into list of lists
  POW_data <- split(POW_data , f = POW_data$id)
  # Convert list elements to data frames
  POW_data <<- lapply(POW_data, function(x){data.frame(x)})
}

# Parameters: POW_data (list of table)
# Returns: POW_data (list of tables)
# Throws: none
# Purpose: Flags match type for Kew Results
POW_taxonomy_parse <- function (y){
  y$match_type <- NA
  # If entered author field exists, check for exact match, else do parse
  if (!y$author_entered[[1]]==""){
    # Check for binomial & author exact match
    if (length(y$accepted[which(
      y$name == y$name_entered[[1]] & y$author == y$author_entered[[1]])])==1){
      # Check if match is accepted and pass results
      if (y$accepted[which(
        y$name == y$name_entered[[1]] & y$author == y$author_entered[[1]])]){
        # Define match type
        y$match_type[which(
          y$name == y$name_entered[[1]] & 
            y$author == y$author_entered[[1]])] <- "Exact match"
        # Check if 
      } else {
        y$match_type[which(
          y$name == y$name_entered[[1]] & 
            y$author == y$author_entered[[1]])] <- "Synonym match"
      }
      # Value entered for author, but no exact match
    } else if (length(y$accepted[which(
      y$name == y$name_entered[[1]])][y$accepted[which(
        y$name == y$name_entered[[1]])]==TRUE])==1){
      # Single accepted species with binomial match but author mismatch
      # Pass results with author mismatch flag
      y$match_type[which(y$name == y$name_entered[[1]] &
                           y$accepted!=y$author_entered[[1]])] <- "Exact Binomial"
      # Check if single synonym exists
    } else if(length(y$match_type[which(y$name != y$name_entered[[1]])==TRUE])==1){
      y$match_type[which(y$name != y$name_entered[[1]])==TRUE] <- "Probable synonym match"
    } else {
      # More than one accepted synonym exists
      y$match_type <- "Taxonomic issues require inspection."
    }
    # No Author entered, follow same procedure as no author match  
    # Counts number of accepted results which exactly match input
    # If one, proceed. Else, produce error.
  }  else if (length(y$accepted[which(
    y$name == y$name_entered[[1]])][y$accepted[which(
      y$name == y$name_entered[[1]])]==TRUE])==1){
    # Single accepted species with binomial match but author mismatch
    # Pass results with author mismatch flag
    y$match_type[which(y$name == y$name_entered[[1]] &
                         y$accepted!=y$author_entered[[1]])] <- "Exact Binomial"
    # Check if single synonym exists
  } else if(length(y$match_type[which(y$name != y$name_entered[[1]])==TRUE])==1){
    y$match_type[which(y$name != y$name_entered[[1]])==TRUE] <- "Probable synonym match"
  } else {
    # More than one accepted synonym exists
    y$match_type <- "Taxonomic issues require inspection."
  }
  return(y)
}
    
# Parameters: POW_data (list of table)
# Returns: POW_data (list of tables)
# Throws: none
# Purpose: Removes rows not flagged with 
POW_subset_results <- function (x) {
  # Subset results for exact/probable matches
  if(any(x$match_type[which(!is.na(x$match_type))] %in% c("Exact Match", 
                                                      "Exact Binomial"))){
    # Return rows with a specified match type
    x <- x[which(!is.na(x$match_type)),]
    # Else subset results for synonym matches
  } else if(any(x$match_type[which(!is.na(x$match_type))] %in% c("Synonym Match", 
                                                             "Probable Synonym Match"))){
    # Subset results for synonyms
    # Check if accepted before proceeding
    if(any(x$synonymOf$accepted[which(x$match_type 
                                  %in% c("Synonym Match",
                                         "Probable Synonym Match"))])){
      # Subset only synonym result
      x <- x[which(!is.na(x$match_type)),]
      # Redefine results with Synonym data
      # Redefine Name
      x$name <- x$synonymOf$name[which(x$match_type 
                                       %in% c("Synonym Match",
                                              "Probable Synonym Match"))]
      # Redefine Author
      x$author <- x$synonymOf$author[which(x$match_type 
                                           %in% c("Synonym Match",
                                                  "Probable Synonym Match"))]
      # Return FQID (from synonym URL)
      x$fqId <- x$synonymOf$url[which(x$match_type 
                                      %in% c("Synonym Match",
                                             "Probable Synonym Match"))]
      # Shorten FQID to remove URL data
      x$fqId <- substr(x$fqId, 8, nchar(x$fqId))
    }
  # } else if (x$match_type[which(!is.na(x$match_type))]==
  #            "Taxonomic issues require inspection."){
  #   x$name <- NA
  #   x$author <- NA
  #   x$fqId <- NA
  }
  # Remove unused columns
  x <- x[,which(names(x) %in% c("id","name","author","fqId","match_type","rank"))]
  # Remove rows which don't correspond to results
  x <- x[which(!is.na(x$match_type)),]
  # Return results
  return(x)
}

#### Execute Script ####
POW_tax_main()