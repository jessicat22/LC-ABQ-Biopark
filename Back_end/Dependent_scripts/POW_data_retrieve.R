#### POW Data Retrieve Functions ####
## Version 3.1
# Started: 19 April 2021
# Last worked on: 18 June 2021
# Author: Clay Meredith
# File: Dependent_scripts/POW_data_retrieve.R
# Description: Passes species info from POW_data table (generated from 
# POW_taxonomy_functions.R) back to Kew's POWO API for data related to distribution
# and synonyms.

#### Load Packages ####
#### Load packages ####
packages <- c("taxize")

lapply(packages, package.check)

#### Main Function ####
POW_data_main <- function(){
    print("Retrieving Kew POWO data.")
  # Execute POWO search
  POW_data <- lapply(POW_taxonomy$fqId,POW_data_query_catch)
  # Rename list elements with internal id
  names(POW_data) <<- POW_taxonomy$id
  # Extract native occurrence data
  # Reformat native occurrence data
  POW_native <- POW_native_occ_reformat()
  # Extract introduced occurrence data
  # Reformat introduced occurrence data
  POW_introduced <- POW_introduced_occ_reformat()
  # Extract and reformat extinct occurrences
  POW_extinct <- POW_extinct_occ_reformat()
  # Merge occurrence tables
  POW_occurrence <- POW_occurrence_merge()
  # Extract and compile synonyms
  POW_synonyms <- POW_syn_compile()
  # Reformat Synonyms Table
  POW_synonyms <<- POW_synonyms_reformat()
  # Remove working tables for occurrence and synonyms
  POW_remove_unused()
  # Recode occurrence records
  POW_occurrence_recode()
  # Build references table
  references <<- POW_citations_generate()
}

#### POW Functions ####

# Parameters: POW_data$fqId
# Returns: POW_data
# Throws: NA if search fails
# Purpose: Define POWO search function for additional data
POW_data_query_catch <- function (x) {
  return(tryCatch(pow_lookup(x,include="distribution"),
                  error=function(e) NA))
}

# Parameters: POW_data
# Returns: POW_data
# Throws: NA if search fails
# Purpose:
POW_native_occ_extract <- function(x)
{
  tryCatch(
    data.frame(
      CountryOccurrenceLookup =
        x[['meta']][['distribution']][['natives']][['tdwgCode']],
      CountryOccurrenceName =
        x[['meta']][['distribution']][['natives']][['name']],
      origin =
        rep(1, nrow(x[['meta']][['distribution']][['natives']])),
      presence =
        rep(1, nrow(x[['meta']][['distribution']][['natives']])),
      seasonality =
        rep(1, nrow(x[['meta']][['distribution']][['natives']])),
      stringsAsFactors = FALSE
    ),
    error = function(e)
      NA
  )
}

# Parameters: POW_native_occ_extract (function), POW_data 
# (list of data frames)
# Returns: POW_native (data_frame)
# Throws: NA if search fails
# Purpose: Execute POW native occurrence search and reformat into single table
POW_native_occ_reformat <- function (){
  # Execute search
  POW_native_temp <- lapply(POW_data,POW_native_occ_extract)
  # Convert to single data frame
  POW_native_temp <- bind_rows(
    POW_native_temp[which(!is.na(POW_native_temp))], 
                               .id = "id")
  return(POW_native_temp)
}

# Parameters: POW_data
# Returns: POW_introduced
# Throws: NA if search fails
# Purpose: Extract introduced occurrence data
POW_introduced_occ_extract <- function(x) {
  tryCatch(
    data.frame(
      CountryOccurrenceLookup =
        x[['meta']][['distribution']][['introduced']][['tdwgCode']],
      CountryOccurrenceName =
        x[['meta']][['distribution']][['introduced']][['name']],
      origin =
        rep(3, nrow(x[['meta']][['distribution']][['introduced']])),
      presence =
        rep(1, nrow(x[['meta']][['distribution']][['introduced']])),
      seasonality =
        rep(1, nrow(x[['meta']][['distribution']][['introduced']])),
      stringsAsFactors = FALSE
    ),
    error = function(e)
      NA
    
  )
}

# Parameters: POW_native_occ_extract (function), POW_data 
# (list of data frames)
# Returns: POW_native (data_frame)
# Throws: NA if search fails
# Purpose: Execute POW native occurrence search and reformat into single table
POW_introduced_occ_reformat <- function (){
  # Execute search
  POW_introduced_temp <- lapply(POW_data,POW_introduced_occ_extract)
  # Convert to single data frame
  POW_introduced_temp <- bind_rows(POW_introduced_temp[
    which(!is.na(POW_introduced_temp))], 
    .id = "id")
  return(POW_introduced_temp)
}

# Parameters: POW_data
# Returns: POW_extinct
# Throws: NA if search fails
# Purpose: Extract extinct occurrence data
POW_extinct_occ_extract <- function(x) {
  tryCatch(
    data.frame(
      CountryOccurrenceLookup =
        x[['meta']][['distribution']][['extinct']][['tdwgCode']],
      CountryOccurrenceName =
        x[['meta']][['distribution']][['extinct']][['name']],
      origin =
        rep(1, nrow(x[['meta']][['distribution']][['extinct']])),
      presence =
        rep(5, nrow(x[['meta']][['distribution']][['extinct']])),
      seasonality =
        rep(1, nrow(x[['meta']][['distribution']][['extinct']])),
      stringsAsFactors = FALSE
    ),
    error = function(e)
      NA
    
  )
}

# Parameters: POW_extinct_occ_extract (function), POW_data 
# (list of data frames)
# Returns: POW_extinct (data_frame)
# Throws: NA if search fails
# Purpose: Execute POW extinct occurrence search and reformat into single table
POW_extinct_occ_reformat <- function (){
  # Execute search
  POW_extinct_temp <- lapply(POW_data,POW_extinct_occ_extract)
  # Convert to single data frame
  POW_extinct_temp <- bind_rows(
    POW_extinct_temp[which(!is.na(POW_extinct_temp))], .id = "id")
  return(POW_extinct_temp)
}

# Parameters: POW_extinct, POW_introduced, POW_native 
# Returns: POW_occurrence (table)
# Throws: 
# Purpose: Merges recoded occurrence fields into single output table
POW_occurrence_merge <- function (){
  
  if (exists("POW_native") && exists("POW_introduced")){
    # If both native and introduced exist, merge to create occurrence table
    POW_occurrence_temp <- rbind(POW_native, POW_introduced)
  } else {
    # Else pass only native table to final occurrence
    POW_occurrence_temp <- POW_native}
  if (exists("POW_extinct")){
    # Merge extinct occurrences if table exists
    POW_occurrence_temp <- rbind(POW_occurrence_temp, POW_extinct)
  }
  names(POW_occurrence_temp) <- c("id","CountryOccurrenceLookup",
                              "CountryOccurrenceName","ORIGIN","PRESENCE",
                              "SEASONALITY")
  return(POW_occurrence_temp)
}

# Parameters: POW_data 
# Returns: 
# Throws: 
# Purpose: Extract synonym data
POW_syn_extract <- function(x) {
  tryCatch(
    data.frame(
      infraType = x[['meta']][['synonyms']][['rank']],
      infrarankAuthor = x[['meta']][['synonyms']][['author']],
      infrarankName = NA,
      name = x[['meta']][['synonyms']][['name']],
      speciesAuthor = NA,
      speciesName = NA,
      stringsAsFactors = FALSE
    ),
    error = function(e)
      NA
  )
}

# Parameters: POW_syn_extract (function), POW_data (list of data frames)
# Returns: POW_synonyms (reformatted data frame)
# Throws: 
# Purpose: Execute synonym extract and compile into single table
POW_syn_compile <- function (){
  # Execute synonym extract
  POW_synonyms_temp <- lapply(POW_data, POW_syn_extract)
  # Remove unused list elements
  if (length(-which(is.na(POW_synonyms_temp)))>0){
    POW_synonyms_temp <- 
    POW_synonyms_temp[-which(is.na(POW_synonyms_temp))]
  }
  # Collapse synonyms to single dataframe
  POW_synonyms_temp <- data.frame(bind_rows(POW_synonyms_temp,
                                            .id = "internal_taxon_id"))
  return(POW_synonyms_temp)
}

# Parameters: POW_synonyms
# Returns: POW_synonyms (reformatted data frame)
# Throws: 
# Purpose: Reformats synonym table to comply with IUCN standards
POW_synonyms_reformat <- function () {
  # Assign infrarankAuthor field to species where designation is for species
  POW_synonyms$speciesAuthor[which(POW_synonyms$infraType == "SPECIES")] <-
    POW_synonyms$infrarankAuthor[which(POW_synonyms$infraType == "SPECIES")]
  
  # Remove infrarankAuthor fields where designation is for species
  POW_synonyms$infrarankAuthor[which(POW_synonyms$infraType == "SPECIES")] <-
    NA
  
  # Assign subspecies names to the infrarankName column
  POW_synonyms$infrarankName[which(POW_synonyms$infraType == "SUBSPECIES")] <-
    substr(
      POW_synonyms$name[which(POW_synonyms$infraType == "SUBSPECIES")],
      str_locate(POW_synonyms$name, "subsp. ")[which(POW_synonyms$infraType ==
                                                       "SUBSPECIES")] + 7,
      nchar(POW_synonyms$name[which(POW_synonyms$infraType == "SUBSPECIES")])
    )
  
  # Assign variety names to the infrarankName column
  POW_synonyms$infrarankName[which(POW_synonyms$infraType == "VARIETY")] <-
    substr(
      POW_synonyms$name[which(POW_synonyms$infraType == "VARIETY")],
      str_locate(POW_synonyms$name, "var. ")[which(POW_synonyms$infraType ==
                                                     "VARIETY")] + 5,
      nchar(POW_synonyms$name[which(POW_synonyms$infraType == "VARIETY")])
    )
  
  # add and populate specific epithet column
  # Species level
  POW_synonyms$speciesName[which(POW_synonyms$infraType == "SPECIES")] <-
    substr(
      POW_synonyms$name[which(POW_synonyms$infraType == "SPECIES")],
      str_locate(POW_synonyms$name, " ")[which(POW_synonyms$infraType ==
                                                 "SPECIES")] + 1,
      nchar(POW_synonyms$name[which(POW_synonyms$infraType == "SPECIES")])
    )
  
  # Subspecies level
  POW_synonyms$speciesName[which(POW_synonyms$infraType == "SUBSPECIES")] <-
    substr(
      POW_synonyms$name[which(POW_synonyms$infraType == "SUBSPECIES")],
      str_locate(POW_synonyms$name, " ")[which(POW_synonyms$infraType ==
                                                 "SUBSPECIES")] + 1,
      str_locate(POW_synonyms$name, "subsp. ")[which(POW_synonyms$infraType ==
                                                       "SUBSPECIES")] - 2
    )
  
  
  # Variety level
  POW_synonyms$speciesName[which(POW_synonyms$infraType == "VARIETY")] <-
    substr(
      POW_synonyms$name[which(POW_synonyms$infraType == "VARIETY")],
      str_locate(POW_synonyms$name, " ")[which(POW_synonyms$infraType ==
                                                 "VARIETY")] + 1,
      str_locate(POW_synonyms$name, "var. ")[which(POW_synonyms$infraType ==
                                                     "VARIETY")] - 2
    )
  
  # Remove forma and subvarieties
  POW_synonyms <-
    POW_synonyms[!(POW_synonyms$infraType %in% c("Form", "Subvariety")), ]
  POW_synonyms$infraType[which(POW_synonyms$infraType == "SPECIES")] <- NA
  POW_synonyms$infraType <- lapply(POW_synonyms$infraType, standardize_level)
  # Assign NA value for species level
  POW_synonyms$infraType[which(lapply(POW_synonyms$infraType,class)=="list")] <- NA
  # Reorder table
  POW_synonyms <- POW_synonyms[,c("internal_taxon_id","name","speciesAuthor","speciesName",
                  "infraType", "infrarankName", "infrarankAuthor")]
  # Set table as global variable
  return(POW_synonyms)
}

# Parameters: POW_data, POW_extinct, POW_introduced, POW_native 
# Returns: 
# Throws: 
# Purpose: Removes temporary tables used to compile data
POW_remove_unused <- function (){
  rm(POW_extinct,pos = ".GlobalEnv")
  rm(POW_introduced,pos = ".GlobalEnv")
  rm(POW_native,pos = ".GlobalEnv")
}

# Parameters: NS_occ (table)
# Returns: references (table)
# Throws: none
# Purpose: Add NS citations to references table
POW_citations_generate <- function (){
  # Identify taxa using NS data
  POW_ids <- unique(POW_occurrence$id)
  # Generate references
  POW_citations <- ref.key[which(ref.key$keywords == "POWO"),]
  POW_citations <- rbind(POW_citations, POW_citations[rep(1, length(POW_ids)-1), ])
  # Append ids to table
  POW_citations$internal_taxon_id <- POW_ids
  # Bind to references table
  references <- rbind(references, POW_citations)
  # Return results
  return(references)
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

#### Execute script ####
POW_data_main()
