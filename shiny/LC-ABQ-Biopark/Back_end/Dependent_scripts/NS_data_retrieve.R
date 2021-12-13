#### NatureServe Data Retrieve ####
## Version 3.1
# Started: 1 April 2021
# Last worked on: 30 June 2021
# Author: Clay Meredith
# File: Dependent_scripts/NS_data_retrieve.R
# Description: This script retrieves data from NatureServe using identifying 
# data retrieved from NS_taxonomy_parse.R (run at the start
# of this script). Taxonomic information from NS_data describing which
# taxa are accepted are then passed to NS_data_retrieve to obtain
# NatureServe's data for the corresponding taxon. The remaining 
# functions reorganize the data into three output tables. NS_data
# contains relevant data for the taxon related to its rank, range, 
# habitat, population, and higher taxonomic classification. 
# NS_common_names contains common names for the accepted taxon,
# language for common name, and a logical field indicating status as
# primary common name (as determined by NatureServe).

#### Load Packages ####
packages <- c("natserv","dplyr")

lapply(packages, package.check)

source("Back_end/Dependent_scripts/NS_taxonomy_parse.R")

#### Run Taxonomic Script ####
# Parameters: spec.list (table)
# Returns: NS_taxonomy (table), NS_tax_raw (list of tables)
# Throws: none
# Purpose: Searches NS taxonomy and returns species key for later data retrieval.
#          Determines which taxa in the user's input are accepted by NatureServe
#          and passes identifiers to NS_data_retrieve to retrieve additional data.
#          See Dependent_scripts/NS_taxonomy_parse.R for additional details.
NS_tax_main()

#### Retrieve NS Data ####
# Parameters: spec.list (table)
# Returns: NS_common_names (table), NS_data_raw (list of tables), NS_data (table), 
#          NS_occ (table), NS_synonyms (table)
# Throws: none
# Purpose: Searches NatureServe database for details related to accepted species
#          as determined by NS_tax_main. Returned tables include common names,
#          occurrence data, synonyms, and data included in individual species rankings
NS_data_main <- function () {
  print("Retrieving NatureServe data.")
  # Search NS taxonomy for accepted species data
  NS_data_raw <<- NS_data_retrieve()
  # Restructure data
  NS_data_restructure()
  NS_occ_retrieve(NS_data_raw)
  NS_occ_restructure()
  NS_occurrence_recode()
  NS_common_extract()
  NS_common_reformat()
  NS_syn_reformat()
  NS_synonyms <<- NS_synonym_reformat2()
  NS_author_extract()
  if(nrow(NS_occ)>0){references <<- NS_citations_generate()}
  print("NatureServe Data reformatted.")
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Search function utilized by NS_data_retrieve
NS_taxon_search <- function (x) {
  ns_temp <- ns_altid(id=as.character(x))
  return(ns_temp)}

# Parameters: NS_taxonomy
# Returns: NS_data_raw (list of tables)
# Throws: none
# Purpose: Applies search function for species with an accepted result as
#          determined by the NS_tax_main script.
NS_data_retrieve <- function (){
  # Subset data for species with accepted results
  NS_data_raw <- lapply(NS_taxonomy$ns_id[which(
    NS_taxonomy$match_type!="No result")],
    NS_taxon_search)
  # Rename data tables with internal id numbers
  names(NS_data_raw) <- NS_taxonomy$id[which(
    NS_taxonomy$match_type!="No result")]
  # Assign global variable
  return(NS_data_raw)
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Restructure NatureServe raw data into single dataframe per taxon.
#          Results have many NULL values from NS database. 
#          These are replaced with NA. Applied later in script.
NS_data_format <- function (x) {
  foo <- data.frame(
    lvl = ifelse(is.null(
      x$classificationLevel$classificationLevelNameEn),NA,
      x$classificationLevel$classificationLevelNameEn),
    IUCN = ifelse(is.null(x$iucn$iucnCode),NA,x$iucn$iucnCode),
    rank_method = ifelse(is.null(
      x$rankMethodUsed$rankMethodUsedDescEn),NA,
      x$rankMethodUsed$rankMethodUsedDescEn),
    ns_binom = ifelse(is.null(x$scientificName),NA,x$scientificName),
    ns_aut = ifelse(is.null(x$scientificNameAuthor),NA,
                    x$scientificNameAuthor),
    comm_name = ifelse(is.null(x$primaryCommonName),NA,
                       x$primaryCommonName),
    grank = ifelse(is.null(x$roundedGRank),NA,x$roundedGRank),
    change_date = ifelse(is.null(x$grankChangeDate),NA,
                         x$grankChangeDate),
    trend = ifelse(is.null(
      x$rankInfo$longTermTrend$longTermTrendDescEn),NA,
      x$rankInfo$longTermTrend$longTermTrendDescEn),
    AOO = ifelse(is.null(
      x$rankInfo$areaOfOccupancy$areaOfOccupancyDescEn),NA,
      x$rankInfo$areaOfOccupancy$areaOfOccupancyDescEn),
    EOO = ifelse(is.null(x$rankInfo$rangeExtent$rangeExtentDescEn),
                 NA,x$rankInfo$rangeExtent$rangeExtentDescEn),
    family = ifelse(is.null(x$speciesGlobal$family),NA,
                    x$speciesGlobal$family),
    habitat = ifelse(is.null(
      x$speciesCharacteristics$habitatComments),NA,
      x$speciesCharacteristics$habitatComments),
    stringsAsFactors = FALSE)
}

# Parameters: NS_data_raw (list of tables), NS_data_format (function)
# Returns: NS_data (reformatted)
# Throws: none
# Purpose: Execute reformat function and collapse list of results 
#          into single data frame.
NS_data_restructure <- function (){
  # Restructure NS_data to extract relevant fields
  NS_data <<- lapply(NS_data_raw, NS_data_format)
  
  # Rename list elements with species ID
  names(NS_data) <- names(NS_data_raw)

  # Collapse ns_dat to single data frame
  NS_data <<- data.frame(bind_rows(NS_data, .id="internal_taxon_id"))
  
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Reformat NatureServe occurrence records from raw data from
#          NS_data_retrieve. Run in NS_occ_restructure
NS_occ_retrieve <- function (x) {
  NS_occ <<- if (length(x$elementNationals$elementSubnationals)==1){
    data.frame(
      occ =
        x$elementNationals$elementSubnationals[[1]]$subnation$subnationCode,
      native = x$elementNationals$elementSubnationals[[1]]$speciesSubnational$native,
      rank = x$elementNationals$elementSubnationals[[1]]$roundedSRank
    )
  } else if (length(x$elementNationals$elementSubnationals)==2){
    data.frame(occ =
                 c(x$elementNationals$elementSubnationals[[1]]$subnation$subnationCode,
                   x$elementNationals$elementSubnationals[[2]]$subnation$subnationCode),
               native =
                 c(x$elementNationals$elementSubnationals[[1]]$speciesSubnational$native,
                   x$elementNationals$elementSubnationals[[2]]$speciesSubnational$native),
               rank =
                 c(x$elementNationals$elementSubnationals[[1]]$roundedSRank,
                   x$elementNationals$elementSubnationals[[2]]$roundedSRank)
    )
    
  } else if (length(x$elementNationals$elementSubnationals)==3){
    data.frame(occ =
                 c(x$elementNationals$elementSubnationals[[1]]$subnation$subnationCode,
                   x$elementNationals$elementSubnationals[[2]]$subnation$subnationCode,
                   x$elementNationals$elementSubnationals[[3]]$subnation$subnationCode),
               native =
                 c(x$elementNationals$elementSubnationals[[1]]$speciesSubnational$native,
                   x$elementNationals$elementSubnationals[[2]]$speciesSubnational$native,
                   x$elementNationals$elementSubnationals[[3]]$speciesSubnational$native),
               rank =
                 c(x$elementNationals$elementSubnationals[[1]]$roundedSRank,
                   x$elementNationals$elementSubnationals[[2]]$roundedSRank,
                   x$elementNationals$elementSubnationals[[3]]$roundedSRank)
    )
    
  }
}

# Parameters: NS_data_raw (list of tables), NS_occ_retrieve (function)
# Returns: NS_occ (table)
# Throws: none
# Purpose: Restructure occurrence data and build single table
NS_occ_restructure <- function () {
  # Retrieve occurrence data from raw data download
  NS_occ <- lapply(NS_data_raw,NS_occ_retrieve)
  # Rename list elements with species id from input species
  names(NS_occ) <- 
    NS_taxonomy$id[which(NS_taxonomy$match_type != "No result")]
  
  # Collapse into single data frame
  NS_occ <<- data.frame(bind_rows(NS_occ, .id="id"))
}

# Parameters: NS_occ (table)
# Returns: NS_occ (table, restructured)
# Throws: none
# Purpose: Recode occurrence fields according to IUCN standards.
NS_occurrence_recode <- function () {
  # Recode presence field. Historic records
  # (SH) are considered to be "Presence Uncertain" (IUCN code 3). 
  # Extirpated (SX) records are considered to be "Presumed Extinct" 
  # (IUCN code 4).
  NS_occ$presence <- NS_occ$rank
  NS_occ$presence[which(NS_occ$presence == "SX")] <- 4
  NS_occ$presence[which(NS_occ$presence == "SH")] <- 3
  NS_occ$presence[which(NS_occ$presence %ni% c("3","4"))] <- 1
  
  # Recode origin field.
  NS_occ$origin <- NS_occ$native
  NS_occ$origin[which(NS_occ$origin == TRUE)] <- 1
  NS_occ$origin[which(NS_occ$origin != TRUE)] <- 3
  NS_occ <- NS_occ[ , -which(names(NS_occ) == "native")]
  
  # Populate seasonality field. Field only used in migratory animal
  # species. All plants are coded as "Resident" (1).
  NS_occ$seasonality <- 1
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Export common name fields
#          Reformat common names into data frame.
#          Run in NS_common_extract
NS_other_common_extract <- function (x) {
  data.frame(x$otherCommonNames)
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Function to retrieve primary NS common name from NS_data_raw.
#          Run in NS_common_extract
NS_primary_comm_extract <- function (x) {
  data.frame(name=
               x$primaryCommonName,
             language = x$primaryCommonNameLanguage,
             primary = rep(TRUE))
}

# Parameters: NS_data_raw (list of tables), NS_other_common_extract (function),
#             NS_primary_comm_extract (function), 
# Returns: NS_common_names (table)
# Throws: none
# Purpose: Execute search for common names and compile into single data frame
NS_common_extract <- function (){
  # Extract common names which are not the primary name
  NS_common_names <- lapply(NS_data_raw, 
                            NS_other_common_extract)
  
  # Append species ID numbers to common names
  names(NS_common_names) <-
    NS_taxonomy$id[which(NS_taxonomy$match_type != "No result")]
  
  # Bind into one data frame
  NS_common_names <- data.frame(bind_rows(NS_common_names, 
                                          .id="internal_taxon_id"))
  
  # Add column for primary name designation and populate
  NS_common_names$primary <- FALSE
  
  # Remove id column from common names
  NS_common_names$id <- NULL
  
  # Extract primary common names
  NS_primary_names <- lapply(NS_data_raw, 
                             NS_primary_comm_extract)
  
  # Append species ID numbers to common names
  names(NS_primary_names) <-
    NS_taxonomy$id[which(NS_taxonomy$match_type != "No result")]
  
  # Bind into one data frame
  NS_primary_names <- data.frame(bind_rows(NS_primary_names, 
                                           .id="internal_taxon_id"))
  
  # Bind into single data frame
  NS_common_names <<- rbind(NS_common_names,
                            NS_primary_names)
}

# Parameters: NS_common_names
# Returns: NS_common_names
# Throws: none
# Purpose: Reformat common names. Changes results to title case, removes 
#          duplicates, and converts language signifier according to 
#          IUCN standards. Source column is used for downstream prioritization.
NS_common_reformat <- function () {
  
  # Reformat into title case
  NS_common_names$name <- tools::toTitleCase(NS_common_names$name)
  
  # Remove duplicates
  NS_common_names <- unique(NS_common_names)
  
  ### Convert common name languages
  NS_common_names$language[which(
    NS_common_names$language=="EN")] <- "English"
  NS_common_names$language[which(
    NS_common_names$language=="FR")] <- "French"
  NS_common_names$language[which(
    NS_common_names$language=="ES")] <- "Spanish; Castilian"
  
  # Add source column
  NS_common_names$source <- paste("NatureServe",
                                  format(Sys.time(), "%Y"), sep=" ")
  
  # Convert to global variable
  NS_common_names <<- NS_common_names
}

#### Synonyms ####

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Define fucntion to extract synonym data from NS_data_raw
NS_syn_extract <- function (x) {
  data.frame(x$speciesGlobal$synonyms)
}

# Parameters: NS_data_raw (list of tables), NS_syn_extract (function)
# Returns: NS_synonyms (table)
# Throws: none
# Purpose: Execute synonym retrieval and consolidate into single data frame.
NS_syn_reformat <- function () {
  # Execute search
  NS_synonyms <- lapply(NS_data_raw, NS_syn_extract)
  # Consolodate into single data frame with species identifier
  NS_synonyms <<- data.frame(bind_rows(NS_synonyms,.id="id"))
}

# Parameters: NS_taxonomy (table)
# Returns: NS_data (table)
# Throws: none
# Purpose: Add author field to taxonomic summary (NS_taxonomy)
NS_author_extract <- function () {
  NS_taxonomy$ns_authority[which(
    NS_taxonomy$match_type != "No result")] <<- NS_data$ns_aut
}

# Parameters: NS_synonyms (table)
# Returns: NS_synonyms (table)
# Throws: none
# Purpose: Synonym table restructure
#          Divides trinomial string and re-routes author fields
NS_synonym_reformat2 <- function (){
  # Remove formatted synonym column
  NS_synonyms$formattedSynonym <<- NULL
  # # Add genus column
  # NS_synonyms$genus <<- lapply(NS_synonyms$synonym,genus_extract)
  # Add species column
  NS_synonyms$speciesName <<- lapply(NS_synonyms$synonym,species_extract)
  # Add infraspecific level column
  NS_synonyms$infraType <<- lapply(NS_synonyms$synonym,infra_level_extract)
  # Add infraspecific epithet column
  NS_synonyms$infrarankName <<- 
    lapply(NS_synonyms$synonym,infraspecific_extract)
  # Assign infrarank author columns
  NS_synonyms$infrarankAuthor <- NA
  NS_synonyms$infrarankAuthor[which(!is.na(NS_synonyms$infraType))] <- 
    NS_synonyms$synonymAuthor[which(!is.na(NS_synonyms$infraType))]
  # Remove species level author names
  NS_synonyms$synonymAuthor[which(!is.na(NS_synonyms$infraType))] <- NA
  # Rename column
  names(NS_synonyms) <- c("internal_taxon_id", "name", "speciesAuthor", "speciesName",
                          "infraType", "infrarankName", "infrarankAuthor")
  # Reformat infraspecific level column
  NS_synonyms$infraType[which(
    lapply(lapply(NS_synonyms$name,trinomial_to_wide),
           length)==4)] <- 
    unlist(lapply(NS_synonyms$infraType[which(
      lapply(lapply(NS_synonyms$name,trinomial_to_wide),
             length)==4)],standardize_level))
  NS_synonyms$infraType[which(NS_synonyms$infraType=="var.")] <- "variety"
  NS_synonyms$infraType[which(NS_synonyms$infraType=="subsp.")] <- "subspecies (plantae)"
  NS_synonyms$infraType[which(NS_synonyms$infraType=="f.")] <- "forma (plantae)"
    
  return(NS_synonyms)
}

# Parameters: NS_occ (table)
# Returns: references (table)
# Throws: none
# Purpose: Add NS citations to references table
NS_citations_generate <- function (){
  # Identify taxa using NS data
  NS_ids <- unique(NS_occ$id)
  # Generate references
  NS_citations <- ref.key[which(ref.key$keywords == "NATURESERVE"),]
  NS_citations <- rbind(NS_citations, NS_citations[rep(1, length(NS_ids)-1), ])
  # Append ids to table
  NS_citations$internal_taxon_id <- NS_ids
  # Bind to references table
  references <- rbind(references, NS_citations)
  # Return results
  return(references)
}

#### Execute above functions ####
NS_data_main()