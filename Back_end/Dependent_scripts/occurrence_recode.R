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
  NS_occurrence_recode()
  POW_occurrence_recode()
  VC_occurrence_recode()
  occurrence_merge()
  countries_table_remove_duplicates()
  countries_table_add_nationals()
}

#### Occurrence recode lower functions ####
# Parameters: NS_occ (table)
# Returns: NS_occ (table)
# Throws: none
# Purpose: 
NS_occurrence_recode <- function() {
  if (exists("NS_occ")) {
    NS_occ$source <- "NatureServe"
    # Recode country codes to WGSRPD
    NS_occ$WGSRPD3 <-
      occ.codes$lvl3_display[match(NS_occ$occ, occ.codes$ns_codes)]
    # Recode country codes to match IUCN formatting
    NS_occ$occ <-
      occ.codes$iucn_code[match(NS_occ$occ, occ.codes$ns_codes)]
    # Build origin column
    NS_occ$ORIGIN <- NA
    # Recode origin column
    NS_occ$ORIGIN[which(NS_occ$native)] <- 1
    NS_occ$ORIGIN[which(!NS_occ$native)] <- 3
    # Build PRESENCE column
    NS_occ$PRESENCE <- NA
    # Recode presence column
    NS_occ$PRESENCE[which(NS_occ$rank == "SX")] <- 4
    NS_occ$PRESENCE[which(NS_occ$rank == "SH")] <- 3
    NS_occ$PRESENCE[which(is.na(NS_occ$PRESENCE))] <- 1

    # Remove improperly coded records (occurrence codes not translatable)
    NS_occ <- NS_occ[which(!is.na(NS_occ$occ)), ]
    # Add SEASONALITY column
    NS_occ$SEASONALITY <- 1
    # Remove unused columns
    NS_occ <<-
      NS_occ[, which(names(NS_occ) %in% 
                       c("id", "occ", "source", "ORIGIN", "PRESENCE",
                         "SEASONALITY","WGSRPD3"))]
  }
}

# Parameters: POW_occ (table)
# Returns: POW_occ (table)
# Throws: none
# Purpose: Recodes occurrence codes from WGSRPD to IUCN. Removes unused columns.
POW_occurrence_recode <- function() {
  if (exists("POW_occurrence")) {
    # Add source field
    POW_occurrence$source <- "Kew"
    # Index POWO Code to return IUCN occ code
    POW_occurrence$occ <-
      occ.codes$iucn_code[match(POW_occurrence$CountryOccurrenceLookup,
                                occ.codes$CountryOccurrenceLookup)]
    # Rename columns
    names(POW_occurrence) <- c("id","WGSRPD3","name","ORIGIN","PRESENCE",
                              "SEASONALITY","source","occ")
    POW_occurrence$CountryOccurrenceLookup
    
    # Remove unused name field
    POW_occurrence <<-
      POW_occurrence[,-which(
        names(POW_occurrence) %in% c(
          "name"
          )
      )]
  }
}

# Parameters: VC_occurrence (table)
# Returns: VC_occurrence (table)
# Throws: none
# Purpose: Recodes occurrence codes from VASCAN to IUCN. 
#          Recodes PRESENCE fields. Recodes ORIGIN fields.
#          Removes unused columns.
VC_occurrence_recode <- function () {
  if (exists("VC_occurrence") & length(VC_occurrence)>0) {
    # Index VC Code to return IUCN occ code
    VC_occurrence$occ <-
      occ.codes$iucn_code[match(VC_occurrence$CountryOccurrenceLookup,
                                occ.codes$vc_codes)]
    # Index VC code to return WGSRPD3 code
    VC_occurrence$WGSRPD3 <-
      occ.codes$lvl3_display[match(VC_occurrence$CountryOccurrenceLookup,
                                occ.codes$vc_codes)]
    
    # Drop unused column and rename remaining columns
    names(VC_occurrence) <- c("id", "WGSRPD3", "ORIGIN", "PRESENCE",
                              "occ")
    VC_occurrence <- VC_occurrence[, which(names(VC_occurrence) %in%
                                      c("id", "WGSRPD3", "ORIGIN", "PRESENCE", "occ"))]
    # Recode origin fields
    VC_occurrence$ORIGIN[which(VC_occurrence$ORIGIN == "native")] <- 1
    VC_occurrence$ORIGIN[which(VC_occurrence$ORIGIN == "introduced")] <- 3
    VC_occurrence$ORIGIN[which(VC_occurrence$ORIGIN == "")] <- 5
    # Recode presence fields
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "native")] <- 1
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "excluded")] <- 6
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "doubtful")] <- 6
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "introduced")] <- 1
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "ephemeral")] <- 6
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "extirpated")] <- 6
    # Add source column and SEASONALITY column
    VC_occurrence$source <- "VASCAN"
    VC_occurrence$SEASONALITY <- 1
    # Convert to global variable
    VC_occurrence <<- VC_occurrence
  }
}

# Parameters: VC_occurrence (table), NS_occ (table), POW_occurrence (table)
# Returns: countries_table (table)
# Throws: none
# Purpose: Merge occurrence data tables
occurrence_merge <- function () {
  # Build table template from whatever tables exist
  if (exists("POW_occurrence")) {
    countries_table <- POW_occurrence[0, ]
  } else if (exists("NS_occ")) {
    countries_table <- NS_occ[0, ]
  } else if (exists("VC_occurrence")) {
    countries_table <- VC_occurrence[0, ]
  }
  
  # Bind rows if they exist
  if (exists("POW_occurrence")) {
    countries_table <- rbind(countries_table, POW_occurrence)
  }
  if (exists("NS_occ")) {
    countries_table <- rbind(countries_table, NS_occ)
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
