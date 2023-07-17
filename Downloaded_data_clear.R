#### Mapping Functions ####
## Version 3.1
# Started: 23 July 2021
# Last worked on: 23 July 2021
# Author: Clay Meredith
# File: Downloaded_data_clear.R
# Description: Removes all downloaded data (geospatial data, and
#              species occurrence data) from Back_end folder.


#### Set Working Directory ####
if (!exists("current.dir")){
  current.dir <- dirname(parent.frame(2)$ofile)
  setwd(current.dir)
  print(getwd())
}

#### Load packages ####
packages <- c("keyring")

lapply(packages, package.check)

# Define file remover function
remove_files <- function (x){
  unlink(x, recursive = TRUE, force = TRUE)
}

#### Remove Geospatial Data ####
# Define files to remove
files_to_remove <- c("Back_end/Dependencies/Geospatial_data/hotspot",
                     "Back_end/Dependencies/Geospatial_data/realm",
                     "Back_end/Dependencies/Geospatial_data/wgsrpd-master")
# Remove files
lapply(files_to_remove, remove_files)

#### Remove Downloaded GBIF Data ####
# Define files to remove
files_to_remove <- 
  list.files(path = "Back_end/Downloaded_Datasets/", 
             include.dirs = TRUE,
             full.names = TRUE)

# Remove files
lapply(files_to_remove, remove_files)

# # Remove kerying password
# key_delete(gbif_pass)
