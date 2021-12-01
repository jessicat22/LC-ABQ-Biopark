#### LC Pipeline Main Script ####
## Version 3.1.1
# Started: January 2021
# Last worked on: 13 October 2021
# Author: Clay Meredith
# File: LC_pipeline_main.R
# Description: Script runs dependent scripts to determine user parameters, load data,
# search databases, and generate SIS Connect output files.

version_no <- "3.1.1"

# # Notes
# 
# Run Appalachia species 1-30 again and check references table.
# Previous run failed to generate citations due to issue with indexing
# of VASCAN results (I think). Issue has been fixed, but it's unclear if
# this error propagated to create issues with NS and POWO.

#### Define working directory ####
# This line only works when the file is sourced. It will not function as expected
# when run line by line.
if (!exists("current.dir")){
  current.dir <- dirname(parent.frame(2)$ofile)
  setwd(current.dir)
  print(getwd())
}

# Load base functions
source("Back_end/Dependent_scripts/base_functions.R")

#### Prompt for user credentials ####
source("Back_end/Dependent_scripts/Credentials_Prompt.R")

#### Load Species List ####
source("Back_end/Dependent_scripts/species_input_load.R")

# Subset rows for testing purposes
# Comment out this line to run entire list
spec.list <- spec.list[c(61:65),]

#### Initiate GBIF Search ####
if(GBIF_toggle == "Y") {
  # Run GBIF_search_initiate if 
  source("Back_end/Dependent_scripts/GBIF_search_initiate.R")
} else if (GBIF_old_toggle == "Y" & 
           file.exists("Back_end/Downloaded_Datasets/GBIF_key_result.csv")) {
  # Load previous key results
  GBIF_key_result <-
    data.frame(read.csv(file = "Back_end/Downloaded_Datasets/GBIF_key_result.csv"))
  # Recode ID numbers for previous results
  GBIF_key_result$internal_taxon_id <- spec.list$id[match(GBIF_key_result$name_entered,
                                                          spec.list$Species)]
}

#### Load dependent data ####

# Load dependent data
source("Back_end/Dependent_scripts/data_load.R")

#### Taxonomic search functions ####
# ITIS Taxonomy Check #
try(source("Back_end/Dependent_scripts/ITIS_functions.R"))

# Execute VASCAN search
# TODO : Remove working tables from global variables
try(source("Back_end/Dependent_scripts/VASCAN_functions.R"))

# Execute NatureServe data scrape
# TODO : Remove working tables from global variables
try(source("Back_end/Dependent_scripts/NS_data_retrieve.R"))

# Execute Flora of North America Search
try(source("Back_end/Dependent_scripts/FNA_Search_Script/FNA_query_functions.R"))

# Run Red List search only if RL token is found
if(nrow(spec.list)<100 && RL_toggle == "Y" && 
   !is.null(key_get("RL_api"))) {
  source("Back_end/Dependent_scripts/RL_functions.R")} else 
  {print("Red List API token not found.")
    print("Red List search will not be performed.")}

# Execute Kew POWO taxonomy search
# TODO : Remove working tables from global variables
try(source("Back_end/Dependent_scripts/POW_taxonomy_functions.R"))

# Execute Kew POWO Data Retrieve
try(source("Back_end/Dependent_scripts/POW_data_retrieve.R"))

# Load DarwinCore files if they exist
source("Back_end/Dependent_scripts/Darwincore_manual_upload.R")

# Download GBIF Data
if(GBIF_toggle == "Y" || GBIF_old_toggle == "Y"){
  source("Back_end/Dependent_scripts/GBIF_download.R")
}

# Reformat GBIF data for export and merge DC data if available
if (exists("GBIF_raw")) {
  source("Back_end/Dependent_scripts/GBIF_reformat.R")
  if (exists("DC_point_data")) {
    GBIF_point_data <- rbind(DC_point_data, GBIF_point_data)
  }
}

# Recode occurrence fields and create countries table
source("Back_end/Dependent_scripts/occurrence_recode.R")

# Geospatial calculations
source("Back_end/Dependent_scripts/spatial_calculations.R")

# Collect spatial calculation data
if(spatial_collect_toggle == "Y"){
source("Back_end/Testing_scripts/spatial_variable_test.R")
}

# Run European Mask
source("Back_end/Dependent_scripts/Euro_mask_eoo_recalculate.R")

# Table exports
source("Back_end/Dependent_scripts/SIS_connect_file_generator.R")

#### Next Steps ####

# # Define previous datasets which are no longer necessary
# GBIF_deletions <- list.files("Downloaded_Datasets/", full.names = T)
# # Remove GBIF_key_result.csv from deletion list
# GBIF_deletions <- GBIF_deletions[which(str_sub(GBIF_deletions, start = -4) != ".csv")]
# # Delete previous datasets
# unlink(GBIF_deletions, force = TRUE, recursive = TRUE)