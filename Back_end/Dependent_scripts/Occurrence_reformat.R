#### Occurrence Reformat functions ####
## Version 3.1
# Started: 11 June 2021
# Last worked on: 11 June 2021
# Author: Clay Meredith
# File: Dependent_scripts/Occurrence_reformat.R
# Description: Loads shared functions used in GBIF_reformat and Darwincore_manual_upload


# Reformat Occurrence origin
# Parameters: GBIF_accepted (table) or DC_accepted (table)
# Returns: table with reformated columns
# Throws: none
# Purpose: Generates establishmentMeans column into ORIGIN column.
#          Removes establishmentMeans column.
occurrence_origin_reformat <- function (x){
  x$establishmentMeans <- 
    replace(x$establishmentMeans, 
            x$establishmentMeans=="",NA)
  
  x$ORIGIN <- ifelse(is.na(revalue(
    x$establishmentMeans,
    c("INTRODUCED"="3", "INVASIVE"="3", "MANAGED"="3",
      "NATIVE"="1", "NATURALISED" = "3", "UNCERTAIN" ="5"))),
    default_vals$value[which(default_vals$var_name=="origin_code")],
    revalue(
      x$establishmentMeans,
      c("INTRODUCED"="3", "INVASIVE"="3", "MANAGED"="3",
        "NATIVE"="1", "NATURALISED" = "3", "UNCERTAIN" ="5")))
  # Flag occurrences including "escaped"
  x$ORIGIN[which(str_detect(x$occurrenceRemarks, "escape"))] <- 3
  x$ORIGIN[which(str_detect(x$habitat, "escape"))] <- 3
  # Set undefined values to defaults
  x$ORIGIN[is.na(x$ORIGIN)] <- 1
  # Remove old column
  x$establishmentMeans <- NULL
  # Return results
  return(x)
}

# Parameters: GBIF_accepted or DC_accepted
# Returns: 
# Throws: none
# Purpose: Populates columns with single values. Add standardized columns 
#          (GBIF_accepted$SEASONAL, GBIF_accepted$YEAR, GBIF_accepted$COMPILER,
#          GBIF_accepted$SUBSPECIES, GBIF_accepted$SUBPOP, GBIF_accepted$DATA_SENS,
#          GBIF_accepted$SENS_COMM.)
occurrence_standard_columns <- function (x) {
  # Build SEASONAL column and set value to 1
  x$SEASONAL <- default_vals$value[which(default_vals$var_name==
                                           "seasonal_code")]
  
  # Generate and populate year column based on system time
  x$YEAR <- format(Sys.Date(), "%Y")
  
  # Generate and populate compiler column
  x$COMPILER <- compiler_name
  
  # Generate and populate SUBSPECIES column
  x$SUBSPECIES <- ifelse(x$taxonRank ==
                           "SUBSPECIES", x$infraspecificEpithet,NA)
  
  # Generate SUBPOP and fill with variety details if applicable
  x$SUBPOP <- ifelse(x$taxonRank == "VARIETY",
                     paste0("var. ", x$infraspecificEpithet),NA)
  
  # Generate DATA_SENS column and populate with specified value
  x$DATA_SENS <- default_vals$value[which(default_vals$var_name==
                                            "sens")]
  
  # Generate SENS_COMM and populate with specified value
  x$SENS_COMM <- NA
  
  # Generate DIST_COMM field
  x$DIST_COMM <- NA
  
  # Generate ISLAND field
  x$ISLAND <- x$island
  
  # Generate PRESENE column
  x$PRESENCE <- default_vals$value[which(default_vals$var_name==
                                           "presence_code")]
  
  # Generate TAX_COMM field
  x$TAX_COMM <- NA
  
  # Generate SPATIALREF column
  if ("geodeticDatum" %in% names(x)) {
    names(x)[names(x) == "geodeticDatum"] <- "SPATIALREF"
  } else {x$SPATIALREF <- "WGS84"}
  
  # Return results
  return(x)
  
}

# Parameters: DC_accepted or GBIF_accepted
# Returns: DC_accepted or GBIF_accepted
# Throws: none
# Purpose: Reformat institution id numbers and collate into single field
institution_id_reformat <- function (x){
  # Pass otherCatalogNumber field to catalogNumber field if the latter does not exist
  x$catalogNumber[which(is.na(x$catalogNumber)|
                                    x$catalogNumber == "")] <- 
    x$otherCatalogNumbers[which(is.na(x$catalogNumber)|
                                            x$catalogNumber == "")]
  # Convert Institution and Catalog Number fields to character
  x$institutionCode <- as.character(x$institutionCode)
  x$catalogNumber <- as.character(x$catalogNumber)
  
  # Generate and fill CATALOG_NO column (checks to determine if institution
  # code is included in catalog number field. Adds institution code to
  # catalog number only if it is not already present.)
  x$catalogNumber[x$catalogNumber %in% c(""," ")] <- NA
  x$institutionCode[x$institutionCode %in% c(""," ")] <- NA
  x$CATALOG_NO <- ifelse(!str_detect(x$catalogNumber,
                                               x$institutionCode),
                                   paste0(x$institutionCode, " ",
                                          x$catalogNumber),
                                   x$catalogNumber)
  return(x)
}

# Parameters: GBIF_accepted or DC_accepted
# Returns: GBIF_accepted or DC_accepted
# Throws: none
# Purpose: Rename GBIF or DC columns
occurrence_column_rename <- function (x){
  # Rename columns
  names(x)[names(x) == 
                         "decimalLatitude"] <- "DEC_LAT"
  names(x)[names(x) == 
                         "decimalLongitude"] <- "DEC_LONG"
  names(x)[names(x) == 
                         "basisOfRecord"] <- "BasisOfRec"
  names(x)[names(x) == 
                         "year"] <- "EVENT_YEAR"
  return(x)
}

# Parameters: GBIF_accepted$ID_NO, 
# GBIF_accepted$decimalLatitude, GBIF_accepted$decmialLongitude
# Returns: GBIF_accepted$ID_NO, GBIF_accepted$DEC_LAT, 
# GBIF_accepted$DEC_LONG (or DC equivalent)
# Throws: none
# Purpose: Reclassifies columns as numeric
occurrence_column_reclassify <- function (x){
  # Reclassify data types
  x$ID_NO <- 
    as.numeric(x$internal_taxon_id)
  x$DEC_LAT <- 
    as.numeric(x$DEC_LAT)
  x$DEC_LONG <- 
    as.numeric(x$DEC_LONG)
  return(x)
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Calculates precision of record based on number of decimal places.
#          Records with precision index below specified level are removed.
#          Precision index is the sum of decimal places in latitude and longitude fields

precision_index <- function (x){
  # Record number of rows for records removed printout
  n_records <- nrow(x)
  # Define columns as numeric values
  x$DEC_LAT <- as.numeric(x$DEC_LAT)
  x$DEC_LONG <- as.numeric(x$DEC_LONG)

  # Define precision of each column
  x$lat_prec <- as.numeric(lapply(x$DEC_LAT, decimalplaces))
  x$long_prec <- as.numeric(lapply(x$DEC_LONG, decimalplaces))
  # Define precision index
  x$prec <- (x$lat_prec + x$long_prec)
  # # Remove entries with precision index under the threshold value
  x <- x[which(x$prec >= as.numeric(default_vals$value[
    which(default_vals$var_name == "min_decimals")])),]
  # Remove precision columns
  x$lat_prec <- NULL
  x$long_prec <- NULL
  x$prec <- NULL
  # Print fraction of records removed
  print(paste("Fraction of imprecise records removed:",1-nrow(x)/n_records, sep = " "))
  print(paste("Total input", n_records, sep = " "))
  print(paste("Total output records", nrow(x), sep = " "))
  # Return results
  return(x)
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Removes potentially cultivated records based on key terms in
#          occurrenceRemarks and habitat fields.

remove_cultivated_specimens <- function (x){
  # Record number of records
  n_records <- nrow(x)
  # Convert fields to lowercase
  x$habitat <- tolower(x$habitat)
  x$occurrenceRemarks <- tolower(x$occurrenceRemarks)
  # Remove records which include key terms in occurrenceRemarks field
  x <- x[which(str_detect(replace_na(x$occurrenceRemarks, ''), 
                          default_vals$value[which(default_vals$var_name == "ocurrenceRemarks_cull")][1],
                          negate = TRUE)),]
  # Remove records which include key terms in habitat field
  x <- x[which(str_detect(replace_na(x$habitat, ''), 
                          default_vals$value[which(default_vals$var_name == 
                                                     "ocurrenceRemarks_cull")][1],
                          negate = TRUE)),]
  # Print number of records removed
  print(paste("Removed", n_records - nrow(x),"potentially cultivated records.", sep = " "))
  return(x)
}

# Parameters: GBIF_accepted or DC_accepted
# Returns: GBIF_accepted or DC_accepted
# Throws: none
# Purpose: Reindex BasisofRec field 
occurrence_basisOfRec <- function (x) {
  
  # # Redefine BasisofRec Field
  x[] <- lapply(x, as.character)
  
  x$BasisOfRec[grep(as.character("PRESERVED_SPECIMEN"), 
                                x$BasisOfRec)] <- "PreservedSpecimen"
  x$BasisOfRec[grep(as.character("LIVING_SPECIMEN"), 
                                x$BasisOfRec)] <- "LivingSpecimen"
  x$BasisOfRec[grep(as.character("HUMAN_OBSERVATION"), 
                                x$BasisOfRec)] <- "HumanObservation"
  x$BasisOfRec[grep(as.character("MATERIAL_SAMPLE"), 
                                x$BasisOfRec)] <- NA
  # Return results
  return(x)
}

# Parameters: GBIF_accepted or DC_accepted
# Returns: GBIF_accepted or DC_accepted (table with reformated ID_NO and PRESENCE columns)
# Throws: none
# Purpose: Rename ID_NO column
occurrence_id_recode <- function(x){
  # Build ID_NO column and define based on presence_code
  colnames(x)[which(names(x)=="internal_taxon_id")] <- "ID_NO"
  return(x)
}

# Parameters: GBIF_accepted$recordNumber, GBIF_accepted$recordedBy (or DC equivalent)
# Returns: GBIF_accepted$SOURCE
# Throws: none
# Purpose: Builds SOURCE column by stripping text from recordNumber then
#          combining recordedBy with recordNumber.
occurrence_generate_source <- function (x) {
  # Strip non-numeric characters from recordNumber column
  x$recordNumber <- gsub("[^0-9]", "", x$recordNumber)
  # Build source column from recordedBy and recordNumber
  x$SOURCE <- paste(x$recordedBy, 
                                 x$recordNumber, sep = " ")
  return(x)
}

# Parameters: GBIF_accepted or DC_accepted (table)
# Returns: GBIF_points or DC_accepted(table)
# Throws: none
# Purpose: Remove unused columns and generate final table (GBIF_points)
#          Finally, reorders columns.
occurrence_column_remove <- function (x){
  x$CITATION <- NA
  # Define names of columns to keep
  column_keeps <- c("ID_NO", "BINOMIAL", "PRESENCE", "ORIGIN", "SEASONAL",
                         "COMPILER","YEAR", "CITATION", "DEC_LAT", "DEC_LONG", 
                         "SPATIALREF", "SUBSPECIES","SUBPOP", "DATA_SENS", 
                         "SENS_COMM", "EVENT_YEAR", "SOURCE", "CATALOG_NO",
                         "DIST_COMM", "ISLAND", "TAX_COMM", "BasisOfRec")
  # Subset only accepted columns
  POINT_data <- x[,which(names(
    x) %in% column_keeps)]
  # Reorder columns
  POINT_data <- x[,which(names(x) %in% column_keeps)]
  # Return data
  return(POINT_data)
}

# Parameters: GBIF_accepted, DC_accepted(table)
# Returns: GBIF_elevation (table)
# Throws: none
# Purpose: Extracts elevation min and max for each species
elevation_extract <- function (x){
  # Remove unreasonable elevations
  elev_temp <- x[which(!is.na(x$elevation)),]
  # Elevations below the Dead Sea (-430.5 m) removed
  # Elevations above Everest (8849 m) removed
  elev_temp <- elev_temp[which(elev_temp$elevation>-430.5 &
                                           elev_temp$elevation<8849),]
  # Divide into list of tables
  elev_temp <- split(elev_temp, f = elev_temp$ID_NO)
  # Extract minimum elevation
  min_elev <- lapply(elev_temp, 
                          function (x){
                            min(x$elevation, na.rm = TRUE)
                          })
  min_elev <- stack(min_elev)
  names(min_elev) <- c("min_elev","internal_taxon_id")
  # Extract maximum elevation
  max_elev <- lapply(elev_temp, 
                          function (x){
                            max(x$elevation, na.rm = TRUE)
                          })
  max_elev <- stack(max_elev)
  names(max_elev) <- c("max_elev","internal_taxon_id")
  # Export as single table
  GBIF_elevation <- merge(max_elev, min_elev, by = "internal_taxon_id")
  return(GBIF_elevation)
}
