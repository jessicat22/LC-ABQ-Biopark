#### DarwinCore Manual Upload ####
## Version 3.1
# Started: 7 June 2021
# Last worked on: 30 June 2021
# Author: Clay Meredith
# File: Dependent_scripts/Darwincore_manual_upload.R
# Description: Load DarwinCore files in specified folder and merge 
#              with GBIF_raw table. If no files are present, or none match
#              input paramters, the script terminates without generating new
#              variables.

#### Load Packages ####
packages <- c("plyr","data.table","dplyr","tidyr")

lapply(packages, package.check)

# Load dependent functions
source("Back_end/Dependent_scripts/Occurrence_reformat.R")

#### DC Main Funciton ####

DC_manual_upload_main <- function (){
  # Load data from .zip files
  DC_data <<- DC_unzip_all()
  # Load data from .csv files
  DC_data <<- DC_individual_file_load()
  # Look up id numbers for DC data and append id to name field
  DC_data <<- DC_id_lookup(DC_data)
  # Convert to single table and set as global variable
  DC_data <<- DC_data_collapse(DC_data)
  # Check if any matched records exist
  if (!all(is.na(unique(DC_data$internal_taxon_id)))){
    # Flag species with DC data
    spec.list <<- DC_record_list()
    # Export raw data
    DC_raw_export()
    # Collapse data and remove problematic records
    DC_accepted <<- DC_data_clean(DC_data)
    # Remove unused columns
    DC_accepted <<- DC_data_subset(DC_accepted)
    # Remove probable cultivated specimens
    DC_accepted <<- remove_cultivated_specimens(DC_accepted)
    # Reformat ID numbers
    DC_accepted <<- institution_id_reformat(DC_accepted)
    # Recode origin field
    DC_accepted <<- occurrence_origin_reformat(DC_accepted)
    # Add columns with default values
    DC_accepted <<- occurrence_standard_columns(DC_accepted)
    # Rename columns to adhere to IUCN standards
    DC_accepted <<- occurrence_column_rename(DC_accepted)
    # Reclassify columns as numeric
    DC_accepted <<- occurrence_column_reclassify(DC_accepted)
    # Delete imprecise records
    DC_accepted <<- precision_index(DC_accepted)
    # Reformat basisOfRec column
    DC_accepted <<- occurrence_basisOfRec(DC_accepted)
    # Rename columns associated with ID number
    DC_accepted <<- occurrence_id_recode(DC_accepted)
    # Generate SOURCE column
    DC_accepted <<- occurrence_generate_source(DC_accepted)
    # Reformat elevation field
    DC_elevation <<- DC_elevation_create()
    # Add columns which can't be populated from dataset
    DC_accepted$ISLAND <- ""
    DC_accepted$CITATION <- ""
    DC_accepted$BINOMIAL <- spec.list$Species[match(DC_accepted$ID_NO, spec.list$id)]
    DC_accepted$BINOMIAL <- paste(lapply(DC_accepted$BINOMIAL, genus_extract), 
                                  lapply(DC_accepted$BINOMIAL, species_extract), sep = " ")
    DC_accepted <<- DC_accepted
    # Remove unused columns
    DC_point_data <- occurrence_column_remove(DC_accepted)
    # rm(DC_data,pos=1)
    # rm(DC_accepted,pos=1)
    return(DC_point_data)
  } else {
      # If no matching records exist, remove DC_data
    rm(DC_data, pos = ".GlobalEnv")
    }
}

#### Dependent functions ####

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Unzips all raw DC files downloaded from Symbiota sources and compiles
#          them into a single list of data frames.
DC_unzip_all <- function () {
  # Compile list of all zip files in specified folder
  zip_files <- list.files(path = "User_Inputs/DarwinCore_files/",
                          pattern = "*.zip",
                          full.names = TRUE)
  if (length(zip_files)) {
    # Build empty list for DC files
    DC_data <- list()
    # For loop unzips files individually and appends them to a list of data frames
    for (i in 1:length(zip_files)) {
      # Unzip files
      unzip(zip_files[i], exdir = "Back_end/Downloaded_Datasets/DarwinCore_files",
            overwrite = TRUE)
      # Read .csv file
      single_dc_file <- data.frame(read.csv(
        "Back_end/Downloaded_Datasets/DarwinCore_files/occurrences.csv"
      ))
      # Append DC file to list of data frames for other DC files
      DC_data <- c(DC_data, list(single_dc_file))
    }
    # Remove unzipped files
    do.call(file.remove, list(
      list.files("Back_end/Downloaded_Datasets/DarwinCore_files",
                 full.names = TRUE)
    ))
    # Return files
    return(DC_data)
  }
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Loads .csv files downloaded from Symbiota sources and compiles
#          them into a single list of data frames. If zip files were included earlier,
#          .csv files will be appended to this list.
DC_individual_file_load <- function (){
  # Find all .csv files in specified path
  DC_csvs <- list.files(path = "User_Inputs/DarwinCore_files/",
             pattern = "*.csv",
             full.names = TRUE)
  # Build DC_data only if it does not yet exist
  if (!exists("DC_data")){
    DC_data <- list()
  }
  # Check if .csv files exist
  if (length(DC_data)>1){
  # Load all .csv files
  DC_csv_tables <- lapply(DC_csvs, function (x){
    data.frame(read.csv(x))
  })
  # Append .csv files into single table
  DC_data <- c(DC_data, DC_csv_tables)
  # Return data
  return(DC_data)
  }
}

# Parameters: DC_data, spec.list
# Returns: DC_data
# Throws: none
# Purpose: Searches the DC datasets for spec.list$Species and appends input
#          species name as name of list elements
DC_id_lookup <- function (x) {
  # Check if DarwinCore files have been imported
  if (exists("DC_data")) {
    # Extract all binomials from DC data
    DC_binoms_temp <-
      lapply(x, function (y) {
        unique(y$scientificName)
      })
    # Identify ID number likely to be associated with the dataset
    DC_ids_temp <- lapply(DC_binoms_temp, function (y) {
      spec.list$id[which(spec.list$Species %in% y)][1]
    })
    # Append ID numbers to DC dataset
    names(DC_data) <- unlist(DC_ids_temp)
  }
  return(DC_data)
}

# Parameters: DC_data
# Returns: 
# Throws: none
# Purpose: Collapse into single data frame
DC_data_collapse <- function (a){
  # Bind into single data frame (rbindlist used because column data types may vary)
  DC_data <- data.frame(rbindlist(a, idcol = "internal_taxon_id",fill = TRUE))
  # Remove occurrences with no taxon id
  DC_data <- DC_data[which(!is.na(DC_data$internal_taxon_id)),]
  # Return results
  return(DC_data)
}

# Parameters: DC_data
# Returns: Spec.list$DC_records
# Throws: none
# Purpose: Saves a list of which records include DarwinCore Data
DC_record_list <- function (){
  record_list <- unique(DC_data$internal_taxon_id)
  # Create column for 
  spec.list$DC_records <- FALSE
  # Flag taxa with DC data
  spec.list$DC_records[which(spec.list$id %in% record_list)] <- TRUE
  return(spec.list)
}

# Parameters: DC_data
# Returns: DC_raw.csv
# Throws: none
# Purpose: Exports raw DC data with affiliated batch number
DC_raw_export <- function (){
  write.csv(DC_data, 
            file = paste("Outputs/DC_raw_data_batch_", 
                         default_vals$value[which(default_vals$var_name == "batch_no")],
                         ".csv", sep = ""))
}

# Parameters: GBIF_raw (table from GBIF_download.R)
# Returns: GBIF_ALL (table)
# Throws: none
# Purpose: Removes unused columns
DC_data_subset <- function (x) {
  # Manipulates GBIF download to remove extraneous data and 
  # Define columns to keep for gbif files
  DC_keeps <-
    c(
      "internal_taxon_id",
      "infraspecificEpithet",
      "establishmentMeans",
      "taxonRank",
      "institutionCode",
      "catalogNumber",
      "otherCatalogNumbers",
      "decimalLatitude",
      "decimalLongitude",
      "basisOfRecord",
      "year",
      "goedeticDatum",
      "coordinateUncertaintyInMeters",
      "island",
      "recordNumber",
      "recordedBy",
      "genus",
      "specificEpithet",
      "habitat",
      "occurrenceRemarks",
      "maximumElevationInMeters",
      "minimumElevationInMeters"
    )
  
  # Subset columns to be manipulated
  x <- x[,-which(names(x) %ni% DC_keeps)]
  return(x)
}

# Parameters: DC_data
# Returns: DC_data
# Throws: none
# Purpose: Removes problematic records
DC_data_clean <- function (x){
  # Count records
  n_records <- nrow(x)
  # Remove records with no associated year
  x <- x[which(!is.na(x$year)),]
  # Remove records with year which is not credible
  x <- DC_data[which(x$year > 1750),]
  x <- DC_data[which(x$year < format(Sys.time(), "%Y")),]
  # Remove records with uncertainty over 5km
  x <- x[which(x$coordinateUncertaintyInMeters<default_vals$value[
    which(default_vals$var_name == "uncertainty_tolerance")]|
                               is.na(DC_data$coordinateUncertaintyInMeters)),]
  # Remove records with no location data
  x <- x[which(!is.na(x$decimalLatitude)),]
  x <- x[which(!is.na(x$decimalLongitude)),]
  # Delete duplicates based on lat, long, and event year
  x <- distinct(x, year,decimalLatitude,decimalLongitude, 
                       .keep_all= TRUE)
  # Print fraction of records removed in this process
  print("Fraction of records removed:")
  print(1-nrow(x)/n_records)
  return(x)
}

# Parameters: DC_accepted
# Returns: DC_accepted
# Throws: none
# Purpose: Interprets DC minimum and maximum elevations. Chooses one if only one exists,
#          otherwise averages values that exist. 
DC_elevation_create <- function () {
  # Create elevation field from min and max fields
  DC_accepted$elevation <-
    ifelse(
      is.na(DC_accepted$minimumElevationInMeters),
      ifelse(
        is.na(DC_accepted$maximumElevationInMeters),
        NA,
        DC_accepted$maximumElevationInMeters
      ),
      ifelse(
        is.na(DC_accepted$maximumElevationInMeters),
        as.numeric(DC_accepted$minimumElevationInMeters),
        as.numeric(DC_accepted$minimumElevationInMeters) +
          as.numeric(DC_accepted$maximumElevationInMeters) / 2
      )
    )
  return(DC_accepted)
}



#### Execute Functions ####
DC_point_data <- DC_manual_upload_main()