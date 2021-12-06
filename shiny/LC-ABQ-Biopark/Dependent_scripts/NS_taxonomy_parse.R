#### NatureServe Taxonomy Parse ####
## Version 3.1
# Started: 1 April 2021
# Last worked on: 20 May 2021
# Author: Clay Meredith
# File: Dependent_scripts/NS_taxonomy_parse.R
# Description:
# This script is intended to search the NatureServe database
# and confirm the taxonomic validity of species listed in spec.list.
# It relies on the taxize package to retrieve data using 
# NatureServe's API. NS_data_retrieve.R calls this script to
# determine which species are valid and which species ids to query
# for additional data. The relevant output of this script is NS_taxonomy,
# a taxonomic table with the accepted nomenclature used by NatureServe
# and ancillary taxonomic data associated with that nomenclature (NS id, 
# NS uri, match type, authority, and number of results in search).

#### Load packages ####
packages <- c("taxize","dplyr")

lapply(packages, package.check)

#### NatureServe Taxonomy Search Functions ####
# Parameters: 
# Returns: 
# Throws: none
# Purpose: Main function executes lower functions. Function is called at bottom
#          of script.
NS_tax_main <- function () {
  NS_taxonomy_table()
  NS_ID_search()
  print("NatureServe IDs obtained.")
  NS_count_results()
  NS_id_populate()
}

# Parameters: spec.list$id (list)
# Returns: NS_taxonomy (table)
# Throws: none
# Purpose: Build results table
NS_taxonomy_table <- function () {
  NS_taxonomy <<- data.frame(id=spec.list$id,
                            name_entered=spec.list$Species) 
}
# Parameters: spec.list$Species (list)
# Returns: NS_tax_raw (list of tables)
# Throws: none
# Purpose: Execute NatureServe API search for all binomials listed on spec.list
NS_ID_search <- function () {
  NS_tax_raw <<- get_natservid_(spec.list$Species, 
                                   searchtype = "scientific",
                                   ask=FALSE, messages = FALSE)
  # Rename list elements with id numbers
  names(NS_tax_raw) <<- spec.list$id
}

# Parameters: NS_tax_raw (list of tables)
# Returns: NS_taxonomy$num_results (table column)
# Throws: none
# Purpose: Count NatrureServe species results
NS_count_results <- function (){
  
  # Calculate number of results for each input
  ns_count <- t(data.frame(
    bind_rows(lapply(NS_tax_raw, function (x){nrow(x)}))))
  
  # Add number of results to final output table
  NS_taxonomy$num_results <<- ns_count[,1]
}

# Parameters: NS_tax_raw (list of tables)
# Returns: NS_taxonomy (table)
# Throws: none
# Purpose: Populate NS taxonomy table data with ID, URI, and binomial
#          Note: The function keeps the first result (as determined by 
#          NatureServe). The search function typically selects the correct
#          closest match, but this may not always be accurate.
NS_id_populate <- function () {
  # Bind rows for results
  NS_id_temp <- data.frame(bind_rows(NS_tax_raw,.id="names"))
  # Populate NatServ ID
  NS_taxonomy$ns_id <<- NS_id_temp$id[match(
    NS_taxonomy$id,NS_id_temp$names)]
  # Populate NatServ uri
  NS_taxonomy$ns_uri <<- NS_id_temp$uri[match(
    NS_taxonomy$id,NS_id_temp$names)]
  # Populate NatServ binomial
  NS_taxonomy$ns_binom <<- NS_id_temp$scientificname[match(
    NS_taxonomy$id,NS_id_temp$names)]
  # Populate column for match type
  NS_taxonomy$match_type <<- NS_taxonomy$name_entered ==
    NS_taxonomy$ns_binom
  NS_taxonomy$match_type[which(
    is.na(NS_taxonomy$match_type))] <<- "No result"
  NS_taxonomy$match_type[which(
    NS_taxonomy$match_type==TRUE)] <<- "Exact Match"
  NS_taxonomy$match_type[which(
    NS_taxonomy$match_type==FALSE)] <<- "Inexact Match"

}

#### Execute script ####
NS_tax_main()