#### Data Import ####
## Version 3.1
  # Started: 8 April 2021
  # Last worked on: 30 June 2021
# Author: Clay Meredith
# File: Dependent_scripts/data_load.R
# Description: Loads data tables from package file structure including
#         geopspatial data, and key tables from Dependencies folder.
#         Checks if geospatial data is in file structure. If not, downloads relevant
#         files. Data is then loaded as a global variable.
#         Load GIS data package only once. Functions search for files, skip download and
#         load data if files already exist.

#### Load Packages ####
#### Load packages ####
packages <- c("sf","rgdal","downloader")

lapply(packages, package.check)

#### Main function ####
DEPENDENCIES_main <- function () {
  # Load default values table
  DEPENDENCIES_load_defaults()
  # load reference key and create references table
  DEPENDENCIES_load_table_keys()
  # Load assessment template
  assessments.template <- data.frame(read.csv(
    "Back_end/Dependencies/assessments_template.csv"))
  assessments.template <<- assessments.template[0,]
  # Load allfields template
  allfields.template <<- data.frame(read.csv("Back_end/Dependencies/allfields_template.csv"))
  # Run Geospatial import if GBIF toggle is active
  if (GBIF_toggle == "Y" || GBIF_old_toggle == "Y") {
    DEPENDENCIES_GEOSPATIAL_main()
  }
  # Assign batch number
  batch_no <- data.frame(var_name = "batch_no",
                         value = as.numeric(format(Sys.time(),"%y%m%d%H%M%S")))
  default_vals <<- rbind(default_vals, batch_no)
  print("Dependent data loaded.")
}

# Parameters: "User_inputs/Default_values.csv", "Dependencies/occ_codes.csv",
#             "Dependencies/subcountry_codes.csv"
# Returns: default_vals (table), occ.codes (table), sub.occ.codes (table)
# Throws: none
# Purpose: Loads default reference tables from Dependencies folder. Defines
#          NA values (Namibia) imported from occurrence tables as "NA"
DEPENDENCIES_load_defaults <- function () {
  # Load default_values.csv
  default_vals <<- data.frame(read.csv("User_inputs/Default_values.csv"))
  # Load occurrence key
  occ.codes <- data.frame(read.csv("Back_end/Dependencies/occ_codes.csv"), 
                          stringsAsFactors = FALSE)
  # Replace Namibia codes with "NA" rather than NA
  occ.codes$iucn_code[which(is.na(occ.codes$iucn_code))] <- "NA"
  
  # Load subnations key
  sub.occ.codes <- data.frame(read.csv("Back_end/Dependencies/subcountry_codes.csv"), 
                              stringsAsFactors = FALSE)
  # Replace Namibia codes with "NA" rather than NA
  sub.occ.codes$iucn_nat[which(is.na(sub.occ.codes$iucn_nat))] <- "NA"
  
  # Assign as global variables
  occ.codes <<- occ.codes
  sub.occ.codes <<- sub.occ.codes
  # Load coding key
  coding_key <<- data.frame(read.csv("Back_end/Dependencies/code_template.csv"))
  # Replace Nearctic codes with "NA" rather than NA
  coding_key$realm_abbreviated[which(is.na(coding_key$realm_abbreviated))] <<- "NA"
  # Load credits table
  credits.template <<- data.frame(read.csv("Back_end/Dependencies/credits_template.csv"))
}

# Parameters: "Dependencies/references_template.csv"
# Returns: references (empty table) and ref.key (table)
# Throws: none
# Purpose: Loads reference table from Dependencies folder. Creates
#          template for references table output.
DEPENDENCIES_load_table_keys <- function () {
  # Load references_template.csv
  ref.key <- data.frame(read.csv("Back_end/Dependencies/references_template.csv"))
  # Assign access date
  ref.key$access_date[which(ref.key$type=="electronic source")] <- 
    format(Sys.time(), "%d/%m/%Y")
  # Assign date based on system time
  ref.key$year[which(ref.key$type=="electronic source")] <- 
    format(Sys.time(), "%Y")
  # Add in-text column
  ref.key$in_text <- paste(ref.key$author, ref.key$year, sep = " ")
  # Construct blank references table
  references <<- ref.key[0,]
  # Set values as global variables
  ref.key <<- ref.key

}

#### Geospatial load main function ####
DEPENDENCIES_GEOSPATIAL_main <- function (){
  LOAD_WGSRPD()
  LOAD_REALMS()
  LOAD_HOTSPOTS()
}
#### Geospatial load lower functions ####

# Parameters: 
# Returns: lvl3 (WGSRPD geospatial data)
# Throws: none
# Purpose: Loads WGSRPD geospatial data
LOAD_WGSRPD <- function () {
  if (!exists('lvl3')) {
    # Check if files exist
    if (!file.exists("Back_end/Dependencies/Geospatial_data/wgsrpd-master/level3")) {
      print("Downloading WGSRPD shapefiles")
      ## Download WGSRPD Dataset
      download(
        "https://github.com/tdwg/wgsrpd/archive/master.zip",
        dest = "Back_end/Dependencies/Geospatial_data/master.zip",
        mode = "wb"
      )
      # Unzip file
      unzip("Back_end/Dependencies/Geospatial_data/master.zip", 
            exdir="Back_end/Dependencies/Geospatial_data", overwrite = TRUE)
      # Remove downloaded zip file
      file.remove("Back_end/Dependencies/Geospatial_data/master.zip")
    }
    # Load data into environment
    lvl3 <<-
      st_as_sf(readOGR(dsn = "Back_end/Dependencies/Geospatial_data/wgsrpd-master/level3"))
  }
}

# Parameters: 
# Returns: realms (realms geospatial data)
# Throws: none
# Purpose: Loads realms geospatial data
LOAD_REALMS <- function () {
  if (!exists('realms')) {
    # Check if files exist
    if (!file.exists("Back_end/Dependencies/Geospatial_data/realm")) {
      print("Downloading Biogeographic realms shapefiles")
      ## Download WGSRPD Dataset
      download("https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip?1349272619",
               dest="Back_end/Dependencies/Geospatial_data/realm.zip",
               mode="wb")
      # Unzip file
      unzip("Back_end/Dependencies/Geospatial_data/realm.zip", 
            exdir="Back_end/Dependencies/Geospatial_data/realm", overwrite = TRUE)
      # Remove downloaded zip file
      file.remove("Back_end/Dependencies/Geospatial_data/realm.zip")
    }
    # Load data into environment
    realms <<- st_as_sf(readOGR(dsn = "Back_end/Dependencies/Geospatial_data/realm/official"))
  }
}

# Parameters: 
# Returns: hotspots (hotspot geospatial data)
# Throws: none
# Purpose: Loads hotspots geospatial data
LOAD_HOTSPOTS <- function () {
  if (!exists('hotspots')) {
    # Check if files exist
    if (!file.exists("Back_end/Dependencies/Geospatial_data/hotspot")) {
      print("Downloading biodiversity hotspot shapefiles")
      ## Download WGSRPD Dataset
      
      download(
        "https://zenodo.org/record/3261807/files/hotspots_2016_1.zip?download=1",
        dest = "Back_end/Dependencies/Geospatial_data/hotspot.zip",
        mode = "wb"
      )
      # Unzip file
      unzip("Back_end/Dependencies/Geospatial_data/hotspot.zip",
            exdir = "Back_end/Dependencies/Geospatial_data/hotspot", overwrite = TRUE)
      # Remove downloaded zip file
      file.remove("Back_end/Dependencies/Geospatial_data/hotspot.zip")
    }
    # Load data into environment
    hotspots <<-
      st_as_sf(readOGR(dsn = "Back_end/Dependencies/Geospatial_data/hotspot"))
  }
  
}

#### Execute main function ####
DEPENDENCIES_main()
