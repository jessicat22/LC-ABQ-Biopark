#### GBIF Point file downloader ####
## Version 3.1
# Started: 7 April 2021
# Last worked on: 30 June 2021
# Author: Clay Meredith
# File: Dependent_scripts/GBIF_download.R
# Description: Downloads GBIF data from search initiated by GBIF_search_initiate.R
# after GBIF signals data is compiled.

#### Load packages ####
packages <- c("bit64","rgbif","data.table","stringr")

lapply(packages, package.check)

detach("package:bit64")

#### Main GBIF Download Function ####
# Parameters: gbif.out (class: occ_download from GBIF_search_initiate.R)
# Returns: GBIF_raw (table)
# Throws: none
# Purpose: Waits for GBIF data compile to complete, then initiates download.
GBIF_download_main <- function () {
  # Check GBIF_toggle

    # Wait for GBIF to compile data for download
    gbif.out1 <- occ_download_wait(gbif.out)
    # Download data
    GBIF_download()
  # Load data into R environment
  GBIF_data_load()
}

#### GBIF Functions ####

# Parameters: gbif.out (class: occ_download from GBIF_search_initiate.R)
# Returns: Files downloaded to Downloaded_dataset folder
# Throws: none
# Purpose: Download results
GBIF_download <- function (){
  # Download results
  print("downloading raw GBIF data")
  GBIF_raw <<- occ_download_import(
    occ_download_get(gbif.out, 
                     path = "Back_end/Downloaded_Datasets/",
                     overwrite = TRUE))
  print("GBIF data downloaded.")
}

# Parameters: Files downloaded to Downloaded_dataset folder
# Returns: GBIF_raw (table)
# Throws: none
# Purpose: Loads downloaded datasets. This function is run if GBIF_old_toggle = "Y", 
# or if GBIF_toggle = "Y". In both cases, the most recent zip file in the directory is loaded.
GBIF_data_load <- function () {
  library(bit64)
  # Get most recent dataset
  # Delete previous results
  unlink("Back_end/Downloaded_Datasets/gbif", force = TRUE, recursive = TRUE)
  # Find zip file names
  recent_gbif <-
    file.info(list.files("Back_end/Downloaded_Datasets/", full.names = T))
  # Subset .zip files in directory
  recent_gbif <- recent_gbif[which(str_sub(rownames(recent_gbif), start = -4) == ".zip"),]
  
  if (exists("recent_gbif")) {
    if (nrow(recent_gbif) > 0) {
      print("file name found")
    } else {
      print("GBIF files not found")
    }
  }
  
  # Load most recent GBIF file
  unzip(rownames(recent_gbif)[recent_gbif$ctime ==
                                max(recent_gbif$ctime[which(recent_gbif$isdir ==
                                                              FALSE)])],
        exdir =
          "Back_end/Downloaded_Datasets/gbif",
        overwrite = TRUE)
  
  GBIF_raw <-
    fread("Back_end/Downloaded_Datasets/gbif/occurrence.txt",
          na.strings = c("", "NA"))
  if (exists("GBIF_raw")) {
    print("GBIF data loaded")
  }
  
  # Asign as global variable
  GBIF_raw <<- as.data.frame(GBIF_raw)
  # Detach bit64 package (may interfere with geospatial calculations)
  detach("package:bit64", unload = TRUE)
}

#### Execute script ####
GBIF_download_main()
