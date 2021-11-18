#### Red List Search ####
## Version 3.1
# Started: 7 April 2021
# Last worked on: 20 May 2021
# Author: Clay Meredith
# File: Dependent_scripts/RL_functions.R
# Description: Searches using Red List API for previous Red List assessment data.
#              Returns Red List taxonomy information including taxon ID, and higher
#              level taxonomy data.

#### Load packages ####
packages <- c("rredlist")

lapply(packages, package.check)

#### Define main function ####
RL_main <- function (){
  # Execute Search
  RL_raw <<- lapply(spec.list$Species,RL_search_throttled)
  # Append internal id as list names
  names(RL_raw) <<- spec.list$id
  # Build and populate Red List table
  RL_reorg()
  print("Red List search successful.")
}

#### Functions ####

# Parameters: "RL_api" keyring value
# Returns: 
# Throws: none
# Purpose: Define search throttled search function
#         Function returns only the most recent assessment
RL_search_throttled <- function (x) {
  foo <- rl_search(x,key=key_get("RL_api"))
  Sys.sleep(2)
  return(foo)
}

# Parameters: RL_raw (list)
# Returns: RL_taxonomy (table)
# Throws: none
# Purpose: Identify species with no results, flag, and remove
#          process data, then add black rows with species id only
RL_reorg <- function (){
  # Count table results
  rl_num_results <- lapply(RL_raw, function (x){length(x$result)})
  
  # Subset species with results
  rl_hits <- RL_raw[-which(rl_num_results==0)]
  # Create raw table
  rl_raw_table <- data.frame(bind_rows(rl_hits, .id = 
                                         "internal_taxon_id"))
  rl_raw_table$result$internal_taxon_id <- 
    rl_raw_table$internal_taxon_id
  # Consolidate as single data frame and set as global variable
  RL_taxonomy <- data.frame(bind_rows(rl_raw_table$result,.id='name'))
  # Subset columnts
  RL_taxonomy <- RL_taxonomy[ , which(names(RL_taxonomy) %in% 
                                        c("taxonid","kingdom",
                                          "phylum","class","order",
                                          "family","genus","authority"
                                        ))]
  # Rename columns
  names(RL_taxonomy) <<- c("taxonid","kingdom",
                          "phylum","classname","ordername",
                          "family","genus","taxonomicAuthority")
  RL_taxonomy <<- RL_taxonomy
  
}

#### Run script ####
RL_main()

