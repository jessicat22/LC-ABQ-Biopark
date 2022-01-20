#### Occurrence Recode Functions ####
## Version 3.1
# Started: 23 April 2021
# Last worked on: 23 April 2021
# Author: Clay Meredith
# File: Dependent_scripts/occurrence_recode.R
# Description: Compiles and re-codes occurrence records into occurrence table 
#              ready for export

#### Load Packages ####
# Load dependent functions
source("Back_end/Dependent_scripts/Occurrence_reformat.R")

#### Main Function ####
OCCURRENCE_RECODE_MAIN <- function (){
  print("Recoding occurrence records.")
  occurrence_merge()
  countries_table_remove_duplicates()
  countries_table_add_nationals()
}

# Parameters: VC_occurrence (table), NS_occurrence (table), POW_occurrence (table)
# Returns: countries_table (table)
# Throws: none
# Purpose: Merge occurrence data tables
occurrence_merge <- function () {
  # Build table template from whatever tables exist
  if (exists("POW_occurrence")) {
    countries_table <- POW_occurrence[0, ]
  } else if (exists("NS_occurrence")) {
    countries_table <- NS_occurrence[0, ]
  } else if (exists("VC_occurrence")) {
    countries_table <- VC_occurrence[0, ]
  }
  
  # Bind rows if they exist
  if (exists("POW_occurrence")) {
    countries_table <- rbind(countries_table, POW_occurrence)
  }
  if (exists("NS_occurrence")) {
    countries_table <- rbind(countries_table, NS_occurrence)
  }
  if (exists("VC_occurrence")) {
    countries_table <- rbind(countries_table, VC_occurrence)
  }
  # Set as a global variable
  countries_table <<- countries_table
  
}

# Parameters: countries_table (table)
# Returns: countries_table (table)
# Throws: none
# Purpose: Prioritize duplicate records, flag conflicts, and remove duplicates
countries_table_remove_duplicates <- function(){
  # Assign priority codes
  countries_table$source[which(countries_table$source=="VASCAN")] <- 1
  countries_table$source[which(countries_table$source=="NatureServe")] <- 2
  countries_table$source[which(countries_table$source=="Kew")] <- 3
  
  # Reorder based on priority
  countries_table[order(countries_table[,'id'],countries_table[,'source']),]
  # Remove exact duplicates
  countries_table <- countries_table[which(!duplicated(countries_table[,c('ORIGIN','id',
                               'occ')])),]
  # Redefine first native record for a region as origin uncertain and 
  # remove all others
  countries_table[duplicated(countries_table[,c('id',
                        'occ')]) | 
        duplicated(countries_table[,c('id',
                          'occ')], fromLast=TRUE),'ORIGIN'] <- 5
  # Set as global variable
  countries_table <<- countries_table
}

# # Parameters: countries_table (table)
# # Returns: countries_table (table)
# # Throws: none
# # Purpose: Add national codes where subnational codes exist
countries_table_add_nationals <- function() {
  # Create temp table
  occ_subnat_temp <- countries_table
  # Add country level code to subnational codes
  occ_subnat_temp$occ <-
    sub.occ.codes$iucn_nat[match(countries_table$occ,
                                 sub.occ.codes$iucn_subnat)]
  # Reorder based on origin
  occ_subnat_temp[order(occ_subnat_temp[,'id'],occ_subnat_temp[,'ORIGIN']),]

  # Remove exact duplicates
  occ_subnat_temp <- occ_subnat_temp[which(!duplicated(
    occ_subnat_temp[,c('id','occ')])),]
  # Bind national level records to subnational records
  countries_table <- rbind(countries_table, occ_subnat_temp)
  # Reorder based on origin
  countries_table[order(countries_table[,'id'],countries_table[,'ORIGIN']),]
  # Remove exact duplicates
  countries_table <- countries_table[which(!duplicated(
    countries_table[,c('id','occ')])),]
  # Remove NA values
  countries_table <- countries_table[which(!is.na(countries_table$occ)),]
  # Remove source column
  countries_table <<- countries_table[,-which(names(countries_table) %in% c('source'))]
}

#### Execute Script ####
OCCURRENCE_RECODE_MAIN()
