#### GBIF Reformat functions ####
## Version 3.1
# Started: 9 April 2021
# Last worked on: 30 June 2021
# Author: Clay Meredith
# File: Dependent_scripts/GBIF_reformat.R
# Description: Removes problematic records from GBIF_raw table and reformats
#              table to adhere to IUCN standards.

# Load dependent functions
source("Back_end/Dependent_scripts/Occurrence_reformat.R")

#### GBIF Main Funciton ####
GBIF_manipulate_main <- function (){
  # Remove unused columns
  GBIF_raw_subset()
  # Assign unique ID to results
  GBIF_id_assign()
  # Remove problematic records
  GBIF_reject()
  # Remove potentially cultivated records
  GBIF_accepted <<- remove_cultivated_specimens(GBIF_accepted)
  # Generate GBIF citation
  GBIF_citation_generate()
  # Add GBIF citations to references table
  GBIF_references_generate()
  # Reformat ORIGIN column
  GBIF_accepted <<- occurrence_origin_reformat(GBIF_accepted)
  # Add columns with default values
  GBIF_accepted <<- occurrence_standard_columns(GBIF_accepted)
  # Reformat institution number and catalog number columns 
  # to create CATALOG_NO column
  GBIF_accepted <<- institution_id_reformat(GBIF_accepted)
  # Rename columns to adhere to IUCN standards
  GBIF_accepted <<- occurrence_column_rename(GBIF_accepted)
  # Reclassify columns as numeric
  GBIF_accepted <<- occurrence_column_reclassify(GBIF_accepted)
  # Remove imprecise records based on number of decimal places
  GBIF_accepted <<- precision_index(GBIF_accepted)
  # Reformat basisOfRec column
  GBIF_accepted <<- occurrence_basisOfRec(GBIF_accepted)
  # Rename columns associated with ID number
  GBIF_accepted <<- occurrence_id_recode(GBIF_accepted)
  # Generate SOURCE column
  GBIF_accepted <<- occurrence_generate_source(GBIF_accepted)
  # Generate BINOMIAL column
  GBIF_accepted <<- GBIF_generate_binomial()
  # # Remove extra columns and generate final table
  GBIF_point_data <<- occurrence_column_remove(GBIF_accepted)
  # Extract elevation data
  GBIF_elevation <<- elevation_extract(GBIF_accepted)
  
  
  # Remove working tables
  rm(GBIF_accepted, pos=1)
  rm(GBIF_ALL, pos=1)
  # rm(GBIF_raw)
  return(GBIF_point_data)
}

# # #### GBIF Functions ####

# Parameters: GBIF_raw (table from GBIF_download.R)
# Returns: GBIF_ALL (table)
# Throws: none
# Purpose: Removes unused columns
GBIF_raw_subset <- function () {
  # Manipulates GBIF download to remove extraneous data and 
  # Define columns to keep for gbif files
  gbif_keeps <- c("infraspecificEpithet","establishmentMeans","taxonRank",
                  "institutionCode","catalogNumber","decimalLatitude",
                  "decimalLongitude","basisOfRecord","year","elevation",
                  "speciesKey","taxonKey","acceptedScientificName","issue",
                  "datasetKey","recordedBy","goedeticDatum",
                  "coordinateUncertaintyInMeters","island","recordNumber",
                  "genus","specificEpithet","habitat","occurrenceRemarks",
                  "otherCatalogNumbers")
  
  # Subset columns to be manipulated
  GBIF_ALL <<- GBIF_raw[,-which(names(GBIF_raw) %ni% gbif_keeps)]
}

# Assign relevant ID numbers
# Parameters: GBIF_raw
# Returns: GBIF_raw
# Throws: none
# Purpose: Assigns species ID numbers
GBIF_id_assign <- function () {
  # Assign species key to key column if it matches input keys
  GBIF_ALL$usageKey <- NA
  GBIF_ALL$usageKey[which(GBIF_ALL$speciesKey %in% GBIF_key_result$usageKey)] <- 
    GBIF_ALL$speciesKey[which(GBIF_ALL$speciesKey %in% GBIF_key_result$usageKey)]
  # Assign taxon key to key column if it matches input keys
  GBIF_ALL$usageKey[which(GBIF_ALL$taxonKey %in% GBIF_key_result$usageKey)] <- 
    GBIF_ALL$taxonKey[which(GBIF_ALL$taxonKey %in% GBIF_key_result$usageKey)]
  
  # Merge internal ID indexed to GBIF species key (will duplicate records if a species key
  # corresponds to multiple taxon ids)
  GBIF_ALL <<-
    full_join(GBIF_key_result[,
                              which(names(GBIF_key_result) %in%
                                      c("internal_taxon_id", "speciesKey"))],
              GBIF_ALL, by = "speciesKey")

}

# Remove problematic records
# Parameters: GBIF_ALL (table), inat (toggle)
# Returns: GBIF_accepted (table)
# Throws: none
# Purpose: Removes records with problematic components. Records are removed if they
# contain a subset of issues specified by GBIF (gbif_reject_parameters). Records which
# lack an associated year, or have coordinate uncertainty exceeding 5 km are also 
# rejected. Function also reports the fraction of records removed. Removes iNaturalist 
# records if user toggles.
GBIF_reject <- function(){
  n_records <- nrow(GBIF_ALL)
  # Remove problematic records (typically resulting from collapsed junior synonyms
  # which are not recorded properly)
  GBIF_ALL <- GBIF_ALL[complete.cases(GBIF_ALL[, 'internal_taxon_id']),]
  # Issues to reject
  ## Rejects any record with one or more of these issues reported
  gbif_reject_parameters <- c("TAXON_MATCH_FUZZY","RECORDED_DATE_INVALID",
                   "PRESUMED_SWAPPED_COORDINATE","RECORDED_DATE_UNLIKELY",
                   "PRESUMED_NEGATED_LONGITUDE","PRESUMED_NEGATED_LATITUDE",
                   "ELEVATION_NON_NUMERIC","COORDINATE_OUT_OF_RANGE",
                   "ELEVATION_NOT_METRIC","ELEVATION_UNLIKELY","ZERO_COORDINATE")
  # Find rejected strings in issues column
  GBIF_rejects <- str_detect(gbif_reject_parameters,GBIF_ALL$issue)
  GBIF_rejects[which(is.na(GBIF_rejects))] <- FALSE
  # Subset only occurrences lacking these issues
  GBIF_ALL <- GBIF_ALL[which(GBIF_rejects==FALSE),]
  # Remove records with no associated year
  GBIF_ALL <- GBIF_ALL[which(!is.na(GBIF_ALL$year)),]
  # Remove records with uncertainty over 5km
  GBIF_ALL <- GBIF_ALL[which(GBIF_ALL$coordinateUncertaintyInMeters<default_vals$value[
    which(default_vals$var_name == "uncertainty_tolerance")]|
      is.na(GBIF_ALL$coordinateUncertaintyInMeters)),]
  # Delete duplicates based on lat, long, and event year
  GBIF_ALL <- distinct(GBIF_ALL, year,decimalLatitude,decimalLongitude, 
                       .keep_all= TRUE)
  
  # Assign GBIF_ALL as global variable
  GBIF_accepted <<- GBIF_ALL
  # Print fraction of records removed in this process
  print("Fraction of records removed:")
  print(1-nrow(GBIF_accepted)/n_records)
  # Remove iNaturalist records if the option has been selected
  if (inat == "Y"){
    GBIF_accepted <<- GBIF_accepted[which(GBIF_accepted$institutionCode!= "iNaturalist"),]
  }
}

# Generate GBIF citation
# Parameters: gbif downloads in Downloaded_datasets folder, ref.key
# Returns: GBIF_accepted$CITATION
# Throws: none
# Purpose: Generates GBIF doi for reporting in points table (citation column)
GBIF_citation_generate <- function(){
  # GBIF downloads in dataset folder
  recent_gbif <- file.info(list.files("Back_end/Downloaded_Datasets/", full.names = T))
  # Identifiy most recent download
  gbif_key <- rownames(recent_gbif[which.max(recent_gbif$mtime) & 
                                     !recent_gbif$isdir,])[1]
  gbif_key <- substring(gbif_key, 30, nchar(gbif_key)-4)
  # Retrieve metadata for download
  gbif_meta_res <- occ_download_meta(gbif_key)
  # Extract citation data from metadata
  cite_gbif_raw <- gbif_citation(gbif_meta_res)
  # extract DOI from GBIF dataset
  gbif_doi <- strsplit(cite_gbif_raw$download, " ")[[1]][4]
  # Complete GBIF citation
  ref.key$title[which(ref.key$author=="GBIF.org")] <- paste("GBIF Occurrence Download")
  ref.key$url[which(ref.key$author=="GBIF.org")] <- paste(gbif_doi)
  ref.key$access_date[which(ref.key$author=="GBIF.org")] <- 
    paste("Accessed from R via rgbif (https://github.com/ropensci/rgbif) on",
          format(Sys.time(), "%d/%m/%Y"))
  ref.key <<- ref.key
}

# Generate GBIF entries for references table
# Parameters: gbif downloads in Downloaded_datasets folder, ref.key
# Returns: GBIF_accepted$CITATION
# Throws: none
# Purpose:
GBIF_references_generate <- function (){
  # Add GBIF citations to references table
  # Generate references
  GBIF_citations <- ref.key[which(ref.key$keywords == "GBIF"),]
  GBIF_citations <- rbind(GBIF_citations, 
                          GBIF_citations[rep(1, 
                                           length(unique(GBIF_accepted$internal_taxon_id))-1), ])
  # Append ids to table
  GBIF_citations$internal_taxon_id <- unique(GBIF_accepted$internal_taxon_id)
  # Bind to references table
  references <<- rbind(references, GBIF_citations)
}


# Parameters: GBIF_accepted 
# Returns: GBIF_accepted
# Throws: none
# Purpose: Generates GBIF_accepted$BINOMIAL column from GBIF genus and species
# GBIF_column_remove <- function (){
GBIF_generate_binomial <- function () {
  GBIF_accepted$BINOMIAL <- paste(GBIF_accepted$genus,
                                   GBIF_accepted$specificEpithet, 
                                   sep = " ")
  return(GBIF_accepted)
  
}


#### Execute Script ####
GBIF_point_data <- GBIF_manipulate_main()
# Save copy of raw data for later use
GBIF_point_data_raw <- GBIF_point_data
