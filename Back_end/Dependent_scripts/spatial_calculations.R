#### Spatial Calculations ####
## Version 3.1
# Started: 26 April 2021
# Last worked on: 25 May 2021
# Author: Clay Meredith
# File: Dependent_scripts/spatial_calculations.R
# Description: Determines the position of all points in export files within
#              the WGSRPD dataset. 

# Notes #

# # Centroid calculation functions call package rgeos which may be
# # masked by other packages. If errors are produced, consider 
# # checking these packages

#### Load packages ####
packages <- c("sf","lwgeom","red","rgeos","geosphere")

lapply(packages, package.check)
library(sf)   
library(terra)
library(dplyr)
library(spData)
# Spatial calculations
 library(lwgeom) # EOO minimum convex polygon calculation
 library(red)    # AOO calculations
 library(rgeos)  # Centroid calculation
 library(geosphere) # Distance calculations for centroid distance

#### Main Function ####

#### TEMP REMOVE AFTER TESTING ####
# GBIF_point_data_hold <- GBIF_point_data
# GBIF_point_data <- GBIF_point_data_hold

SPATIAL_CALCULATIONS_MAIN <- function (){
  
  # Turn off s2 processing
  sf::sf_use_s2(FALSE)
  GBIF_lat_long_duplicate()
  WGSRPD_index()
  print("goes past index")
  WGSRPD_lvl1_add()
  WGSRPD_convert()
  POINT_data_origin_recalculate()
  distribution_extremes_compile()
  GBIF_native_subset()
  HOTSPOTS_calculate()
  REALMS_calculate()
  EOO_max_calculate()
  dist.cent <<- Centroids_calculation()
  # Calculate minimum EOO value
  eoo_min <- POINT_subset()
  # Bind EOO_min to spec.list
  spec.list <<- merge(eoo_min, spec.list, by="id", all.y = TRUE)
  # Calculate AOO values
  aoo_max <- aoo_max_calc(GBIF_point_data)
  # Bind AOO_max and AOO_min to spec.list
  spec.list <- merge(aoo_max, spec.list, by="id", all.y = TRUE)
  spec.list <- merge(aoo_min, spec.list, by="id", all.y = TRUE)
  spec.list <<- spec.list
  # Return GBIF_point_data to useable format
  GBIF_point_data <<- return_to_original_format()
  
  # Remove calculation table
  rm(GBIF_natives)
  rm(cent.calc)
  rm(dist.cent)
}

#### Subsidiary functions ####

# Parameters: x
# Returns: x
# Throws: none
# Purpose: Find intersect of point data and WGSRPD dataset
occurrence.ref <- function (x) {
  data.frame(st_intersects(x, lvl3, sparse = TRUE))
}

# Parameters: x
# Returns: x
# Throws: none
# Purpose: Add index column
index.col <- function (x) {
  x$index <- c(1:nrow(x));return(x)
}

# Parameters: GBIF_point_data (table from GBIF_download.R)
# Returns: GBIF_point_data (table)
# Throws:
# Purpose: Add temporary column to preserve lat and long (otherwise sf function)
#          removes this data from a usable format.
GBIF_lat_long_duplicate <- function(){
  GBIF_point_data <<- GBIF_point_data_raw
  # Duplicate latitude
  GBIF_point_data$DEC_LAT2 <<- as.numeric(GBIF_point_data$DEC_LAT)
  # Duplicate longitude
  GBIF_point_data$DEC_LONG2 <<- as.numeric(GBIF_point_data$DEC_LONG)
}

# Parameters: GBIF_point_data (table from GBIF_download.R)
# Returns: GBIF_point_data (table)
# Throws: none
# Purpose: Determines WGSRPD position of all points. Points which fall outside
#          the WGSRPD polygons are removed (may cause issues with coastal points)
WGSRPD_index <- function (){
  
  # Divide data into list of data frames
  GBIF_working <- split(GBIF_point_data , f = GBIF_point_data$ID_NO )
  # Add index column
  GBIF_working <- lapply(GBIF_working, index.col)
  # Redefine map_calc as sf object
  GBIF_working <- lapply(GBIF_working, sf.function)
  # Calculate intersect of GBIF point data and WGSRPD
  GBIF_lvl3_intersections <- lapply(GBIF_working,
                      occurrence.ref)
  # Collapse all lists into single data frames
  GBIF_lvl3_intersections <- data.frame(bind_rows(
    GBIF_lvl3_intersections, .id="name"))
  GBIF_working <- data.frame(bind_rows(
    GBIF_working, .id="ID_NO"))
  # Rename columns
  names(GBIF_lvl3_intersections) <- c("ID_NO","index","lvl3")
  # Merge occurrences to master data
  GBIF_working <- merge(GBIF_working, GBIF_lvl3_intersections, by=c("ID_NO","index"), 
                    all.y=TRUE)
  # Remove index column
  GBIF_working <- GBIF_working[,-which(names(GBIF_working) %in% c("index"))]
  GBIF_point_data <<- GBIF_working
}

# Parameters: GBIF_point_data (table)
# Returns: GBIF_point_data (table)
# Throws: none
# Purpose: Adds WGSRPD level 1 position of all points to GBIF_point_data.
WGSRPD_lvl1_add <- function (){
  # Create column for level 1
  GBIF_point_data$lvl1 <<- lvl3$LEVEL1_COD[GBIF_point_data$lvl3]
}

# Parameters: GBIF_point_data (table)
# Returns: GBIF_point_data (table)
# Throws: none
# Purpose: Converts WGSRPD index codes to IUCN_country codes
WGSRPD_convert <- function() {
  # Convert to WGSRPD level 3 code
  GBIF_point_data$lvl3 <- lvl3$LEVEL3_COD[GBIF_point_data$lvl3]
  # Index WGSRPD level 3 code to return IUCN code
  GBIF_point_data$lvl3 <<- occ.codes$iucn_code[match(GBIF_point_data$lvl3,
                                            occ.codes$CountryOccurrenceLookup)]
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Define function to cross reference point position with 
#          occurrence records from other sources.
cross.occ <- function (x) {
  x$ORIGIN[which(x$lvl3 %ni% countries_table$occ[
    which(countries_table$ORIGIN == 1 &
            countries_table$id == x$ID_NO[1])])] <- 3 
  return(x)
}

# Parameters: GBIF_point_data
# Returns: GBIF_point_data
# Throws: none
# Purpose: Reclassifies origin field for points based on occurrence data
#          from other sources.
POINT_data_origin_recalculate <- function() {
  # Divide data into list of data frames
  GBIF_point_data <- split( GBIF_point_data , f = GBIF_point_data$ID_NO )
  # Redefine origin field based on Kew, NS, and VC occurrence lookups
  points_redefined <- lapply(GBIF_point_data, cross.occ)
  # Convert to long
  GBIF_point_data <<- data.frame(bind_rows(points_redefined, .id="name"))
}

# Parameters: GBIF_point_data (table from GBIF_download.R)
# Returns: GBIF_natives (table)
# Throws:
# Purpose: Subsets GBIF point data to include only natives for calculations and converts to
#          sf class.
GBIF_native_subset <- function (){
  # # Convert to long format
  GBIF_natives <- GBIF_point_data
  # Subset native points
  GBIF_natives <- GBIF_natives[GBIF_natives$ORIGIN==1,]
  # Convert to sf
  GBIF_natives <<- st_as_sf(GBIF_natives)
  
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Calculate hotspot value for native points
hotspot.ref <- function (x) {
  data.frame(st_intersects(x, hotspots, sparse = TRUE))
}

# Parameters: GBIF_point_data (table), hotspot.ref (function)
# Returns: hotspot_results (table)
# Throws: none
# Purpose: Returns hotspot locations for each taxon
HOTSPOTS_calculate <- function () {
  # Calculate hotspots for native point occurrences
  GBIF_hotspots <- hotspot.ref(GBIF_natives)
  # Index taxon IDs
  GBIF_hotspots$row.id <- GBIF_natives$ID_NO[GBIF_hotspots$row.id]
  # Remove duplicates
  GBIF_hotspots <- unique(GBIF_hotspots)
  # Index hotspots
  GBIF_hotspots$col.id <- hotspots$NAME[GBIF_hotspots$col.id]
  # Rename hotspots table
  names(GBIF_hotspots) <- c("ID_NO","hotspot")
  # Convert to global variable
  hotspot_results <<- GBIF_hotspots
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Calculate realm value for native points
realm.ref <- function (x) {
  data.frame(st_intersects(x, realms, sparse = TRUE))
}

# Parameters: GBIF_point_data (table), hotspot.ref (function)
# Returns: realm_results (table)
# Throws: none
# Purpose: Returns realm locations for each taxon
REALMS_calculate <- function () {
  # Calculate realms for native point occurrences
  GBIF_realms <- realm.ref(GBIF_natives)
  # Index taxon IDs
  GBIF_realms$row.id <- GBIF_natives$ID_NO[GBIF_realms$row.id]
  # Index realms
  GBIF_realms$col.id <- realms$REALM[GBIF_realms$col.id]
  # Rename realms table
  names(GBIF_realms) <- c("ID_NO","realm")
  # Remove duplicates
  GBIF_realms <- unique(GBIF_realms[which(!is.na(GBIF_realms$realm)),])
  # Convert to global variable
  realm_results <<- GBIF_realms
}

#### EOO Calculations ####
# Parameters: 
# Returns: 
# Throws: none
# Purpose: Defines function for calculating a minimum convex hull
#          given point data of class sf
EOO_calculator <- function(x){
      st_geod_area(st_convex_hull(st_union(
        x$geometry)))
}

# Parameters: GBIF_point_data (table)
# Returns: EOO_max (column added to spec.list)
# Throws: none
# Purpose: Calculates EOO for all taxa with point data. Results added
#          to spec.list
EOO_max_calculate <- function(){
  # Split data into list of data frames
  GBIF_natives <- split(GBIF_natives , f= GBIF_natives$ID_NO)
  # Apply EOO calculation for subset
  EOO_working <- lapply(GBIF_natives, EOO_calculator)
  # Reformat table
  EOO_working <- lapply(EOO_working, as.numeric)
  EOO_working <- stack(EOO_working)
  # Rename table
  names(EOO_working) <- c("EOO","id")
  # Unit conversion from meters to square km
  EOO_working$EOO <- EOO_working$EOO/1000000
  # Append to spec.list
  spec.list$EOO_max <- NA
  spec.list$EOO_max[match(EOO_working$id, spec.list$id)] <- 
    EOO_working$EOO
  # Convert to global variable
  spec.list <<- spec.list
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Converts points to spatial points class
convert_sp <- function (x) {
  SpatialPoints(
    coords = x[, c('DEC_LONG2', 'DEC_LAT2')],
    proj4string = 
      CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  )
}

# Parameters: GBIF_natives (table)
# Returns: cent_calc (table), dist.cent (table)
# Throws: none
# Purpose: Calculates centroid of species point clouds, and distance of each point
#          to point cloud centroid. Centroid is a temporary field used for EOO_min 
#          and AOO_min calculations
Centroids_calculation <- function (){
  # Convert GBIF_natives to data frame
  GBIF_natives$geometry <- NULL
  # Split data into list of data frames
  GBIF_working <- split(GBIF_natives , f= GBIF_natives$ID_NO)
  # Add geometry class
  # # Calculate centroids (cent.calc) and distance of all points to
  # # centroids (dist.cent)
  cent.calc <- lapply(GBIF_working, convert_sp)
  cent.calc <- lapply(cent.calc, gCentroid)
  cent.calc <- lapply(cent.calc,unlist)
  dist.cent <- NULL
  dist.cent <- mapply(function(x,y){distGeo(data.frame(
    x$DEC_LONG2,x$DEC_LAT2),
    y)},GBIF_working,cent.calc)
  return(dist.cent)
}

# Parameters: GBIF_natives (table)
# Returns: eoo_min (table), 
# Throws: none
# Purpose: Subsets points based on distance from centroid
POINT_subset <- function () {
  # Split native occurrences into list of data frames
  GBIF_working <- split(GBIF_natives , f = GBIF_natives$ID_NO)
  # Subset points based on distance from centroid
  GBIF_subset <- mapply(cbind,
                        GBIF_working,
                        "distance" = dist.cent,
                        SIMPLIFY = F)
  
  # Clear most distant  points for minimum EOO estimate
  eoo_min <-
    lapply(GBIF_subset, function(x) {
      x[x$distance < quantile(x$distance,
                              as.numeric(default_vals$value[which(default_vals$var_name ==
                                                                    "outlier_threshold")])),]
    })
  # Duplicate lat/long hold fields
  eoo_min <- data.frame(bind_rows(eoo_min, .id="ID_NO"))
  eoo_min$DEC_LAT <- eoo_min$DEC_LAT2
  eoo_min$DEC_LONG <- eoo_min$DEC_LONG2
  # Duplicate table for aoo_calculations
  aoo_min <- eoo_min
  eoo_min <- sf.function(eoo_min)
  eoo_min <- split(eoo_min , f = eoo_min$ID_NO)

  # Calculate minimum AOO
  aoo_min <- aoo_max_calc(aoo_min)
  names(aoo_min) <- c("AOO_min","id")
  aoo_min <<- aoo_min

  # Calculate minimum EOO (conversion to km2 embedded in function)
  eoo_min <- lapply(eoo_min, function(x) {
    as.numeric(st_geod_area(st_convex_hull(st_union(x$geometry)))) / 1000000
  })
  
  # Reformat table
  eoo_min <- data.frame(t(bind_rows(eoo_min, .id="ID_NO")))
  eoo_min$ID_NO <- rownames(eoo_min)
  names(eoo_min) <- c("EOO_min","id")
  # Remove scientific notation
  eoo_min$EOO_min <- format(eoo_min$EOO_min, scientific=F)

  return(eoo_min)
}

# Parameters: GBIF_point_data (table)
# Returns: aoo_min (table), 
# Throws: none
# Purpose: Calculates AOO value estimate
AOO_calculation <- function (x){
  # Construct table for calculation
  AOO_table <- data.frame(
    DEC_LONG = x$DEC_LONG2,
    DEC_LAT = x$DEC_LAT2,
    id = x$ID_NO
  )
  # Split table
  AOO_table <- split(AOO_table , f = AOO_table$id)
  # Calculate AOO minimum value
  aoo_value <- lapply(AOO_table,function(y){
    aoo(data.frame(y$DEC_LONG, y$DEC_LAT))
  }
  )
  return(aoo_value)
}

# Parameters: GBIF_point_data (table)
# Returns: aoo_min (table), 
# Throws: none
# Purpose:
aoo_max_calc <- function (x){
  aoo_value <- AOO_calculation(x)
  # Reformat table
  aoo_value <- data.frame(t(bind_rows(aoo_value, .id="id")))
  aoo_value$id <- rownames(aoo_value)
  names(aoo_value) <- c("AOO_max","id")
  
  return(aoo_value)
} 

# Parameters: GBIF_point_data (table)
# Returns: extreme_occ (table), 
# Throws: none
# Purpose: Determines WGSRPD lvl 3 position of extreme points

distribution_extremes_compile <- function (){
  ## Define extreme occurrences
  # Subset only native occurrences
  native_occ_final <- subset(GBIF_point_data, ORIGIN==1)
  
  native_occ_final <- split( native_occ_final , f = native_occ_final$ID_NO )
  
  # Define southernmost occurrence
  south.occ <- lapply(native_occ_final, function (x){
    x[which.min(x$DEC_LAT),]
  })
  south.occ <- bind_rows(south.occ)
  south.occ$direction <- "south"
  # Define northernmost occurrence
  north.occ <- lapply(native_occ_final, function (x){
    x[which.max(x$DEC_LAT),]
  })
  north.occ <- bind_rows(north.occ)
  north.occ$direction <- "north"
  # Define easternmost occurrence
  east.occ <- lapply(native_occ_final, function (x){
    x[which.max(x$DEC_LONG),]
  })
  east.occ <- bind_rows(east.occ)
  east.occ$direction <- "east"
  # Define westernmost occurrence
  west.occ <- lapply(native_occ_final, function (x){
    x[which.min(x$DEC_LONG),]
  })
  west.occ <- data.frame(bind_rows(west.occ))
  west.occ$direction <- "west"
  # Bind all data into single data frame
  extreme_occ <<- bind_rows(list(north.occ,south.occ,east.occ,west.occ))
}

# Parameters: GBIF_point_data (table)
# Returns: GBIF_point_data (table)
# Throws: none
# Purpose: Returns GBIF_point data to original format (allows for script to be repeatedly
#          run during testing)
return_to_original_format <- function (){
  # return lat/long fields to original format
  GBIF_point_data$DEC_LAT <- GBIF_point_data$DEC_LAT2
  GBIF_point_data$DEC_LONG <- GBIF_point_data$DEC_LONG2
  # remove extraneous fields
  GBIF_point_data$geometry <- NULL
  GBIF_point_data$lvl3 <- NULL
  GBIF_point_data$lvl1 <- NULL
  GBIF_point_data$DEC_LAT2 <- NULL
  GBIF_point_data$DEC_LONG2 <- NULL
  return(GBIF_point_data)
}

# TODO : Add countries based on GBIF non-native occurrences

#### Run Program ####
SPATIAL_CALCULATIONS_MAIN()