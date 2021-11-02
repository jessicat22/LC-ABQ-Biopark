#### Spatial Calculations ####
## Version 3.1
# Started: 21 October 2021
# Last worked on: 21 October 2021
# Author: Clay Meredith
# File: Dependent_scripts/Euro_mask_eoo_recalculate.R
# Description: Recalculates EOO after removing points occurring outside
#              Greater Europe boundaries. Calculation is repeated after
#              an additional filter of the EU 27 is applied.


#### Load packages ####
packages <- c("sf","lwgeom","red","rgeos","geosphere")

lapply(packages, package.check)

#### Define main function ####
EU_MASK_MAIN <- function (){
  # Turn off s2 processing
  sf::sf_use_s2(FALSE)
  # Load shapefiles
  LOAD_EU27()
  LOAD_PAN_EUROPE()
  # Prepare point file for intersect calculation
  prepped_points <- point_intersect_prepare(GBIF_point_data)
  # Calculate intersect of points and paneurope polygons
  paneurope_points <- 
    euro_intersect_execute(prepped_points,
                      paneurope,
                      "paneurope_intersect")
  # Calculate intersect of points and paneurope polygons
  eu27_points <- 
    euro_intersect_execute(prepped_points,
                      eu27,
                      "eu27_intersect")
  # Merge intersect columns
  intersects_all <- intersection_merge(paneurope_points, 
                                       eu27_points, 
                                       prepped_points)
  # Calculate EOO for pan-europe and append to spec.list
  spec.list <<- paneurope_eoo_calculate(intersects_all)
  # Calculate EOO for EU27 and append to spec.list
  spec.list <<- eu27_eoo_calculate(intersects_all)
  # Finalize point data for export
  EU_points_final <<- EU_points_generate(intersects_all)
  # Export results
  write.csv(EU_points_final, 
            paste("Outputs/euro_points", 
                  default_vals$value[which(default_vals$var_name == "batch_no")],
                  ".csv", sep = ""),
            row.names = FALSE)
  # Export results
  write.csv(spec.list, 
            paste("Outputs/euro_taxonomy", 
                  default_vals$value[which(default_vals$var_name == "batch_no")],
                  ".csv", sep = ""),
            row.names = FALSE)
  
  
}

#### Load shapefiles ####
# Parameters: 
# Returns: eu27
# Throws: none
# Purpose: Loads eu27 geospatial data
LOAD_EU27 <- function () {
  if (!exists('eu27')) {
    # Load data into environment
    eu27 <<-
      st_as_sf(readOGR(dsn = 
        "Back_end/Dependencies/Geospatial_data/europe_shapefiles/EU27"))
  }
}

# Parameters: 
# Returns: paneurope
# Throws: none
# Purpose: Loads paneurope geospatial data
LOAD_PAN_EUROPE <- function () {
  if (!exists('paneurope')) {
    # Load data into environment
    paneurope <<-
      st_as_sf(readOGR(dsn = 
        "Back_end/Dependencies/Geospatial_data/europe_shapefiles/PanEurope"))
  }
}

# Parameters: x (point data), y (mask shapefile)
# Returns: x (point data with new intersect column)
# Throws: none
# Purpose: Find intersect of point data (x) and given european dataset (y)
euro.intersect <- function (x, y) {
  data.frame(st_intersects(x, y, sparse = TRUE))
}

# Parameters: GBIF_point_data (table from GBIF_download.R) (x)
# Returns: GBIF_point_data (table with spatial properties)
# Throws: none
# Purpose: Prepares point file for spatial intersect (copies latitude and
#          longitude, converts to class sf, adds index column)
point_intersect_prepare <- function (x){
  # Create GBIF_working
  GBIF_working <- x
  # Copy latitude and longitude columns
  GBIF_working$DEC_LAT2 <- GBIF_working$DEC_LAT
  GBIF_working$DEC_LONG2 <- GBIF_working$DEC_LONG
  # Divide data into list of data frames
  GBIF_working <- split(GBIF_working , f = x$ID_NO )
  # Add index column
  GBIF_working <- lapply(GBIF_working, index.col)
  # Redefine as sf object
  GBIF_working <- lapply(GBIF_working, sf.function)
  # Return results
  return(GBIF_working)
}

# Parameters: GBIF_point_data (table from GBIF_download.R) (x),
#             shapefile (y),
#             column name (z)
# Returns: GBIF_point_data (table with added column for GBIF_intersections)
# Throws: none
# Purpose: Determines position of all points. Points which fall outside
#          the EU polygons are removed (may cause issues with coastal points)
euro_intersect_execute <- function (x,euro_shapefile,z){
  # Calculate intersect of GBIF point data and shapefile
  GBIF_paneuro_intersections <- lapply(x,
                                    euro.intersect, 
                                    y=euro_shapefile)
  # Collapse all lists into single data frames
  GBIF_paneuro_intersections <- data.frame(bind_rows(
    GBIF_paneuro_intersections, .id="name"))
  x <- data.frame(bind_rows(
    x, .id="ID_NO"))
  # Rename columns
  names(GBIF_paneuro_intersections) <- c("ID_NO","index",z)
  return(GBIF_paneuro_intersections)
}

# Parameters: paneurope_points (x), eu27_points (y), prepped_points (z)
# Returns: GBIF_point_data (table with added columns for intersections)
# Throws: none
# Purpose: Merges intersection tables with 
#          the EU polygons are removed (may cause issues with coastal points)
intersection_merge <- function (x,y,z){
  # Collapse all lists into single data frames
  paneurope_long <- data.frame(bind_rows(
    x, .id="name"))
  # paneurope_long <- paneurope_long[,-1]
  eu27_long <- data.frame(bind_rows(
    y, .id="name"))
  # eu27_long <- eu27_long[,-1]
  prepped_points <- data.frame(bind_rows(
    z, .id="ID_NO"))
  # Merge into single data frame
  intersects_all <- merge(paneurope_long,
                          eu27_long,
                          by=c("ID_NO","index"),
                          all.x=TRUE)
  intersects_all <- merge(intersects_all,
                          prepped_points,
                          by=c("ID_NO","index"),
                          all.y=TRUE)
  # Remove index column
  intersects_all <- intersects_all[,-which(names(intersects_all) %in% c("index"))]
  return(intersects_all)
}

# Parameters: intersects_all
# Returns: paneurope_EOO (column added to spec.list)
# Throws: none
# Purpose: Calculates EOO for all taxa with point data. Results added
#          to spec.list
paneurope_eoo_calculate <- function(x){
  # Remove non-native points and points outside pan-Europe
  point_natives <- x[which(x$ORIGIN == 1 & !is.na(x$paneurope_intersect)),]
  # Split data into list of data frames
  point_natives <- split(point_natives , f = point_natives$ID_NO)
  # Apply EOO calculation for subset
  EOO_working <- lapply(point_natives, EOO_calculator)
  # Reformat table
  EOO_working <- lapply(EOO_working, as.numeric)
  EOO_working <- stack(EOO_working)
  # Rename table
  names(EOO_working) <- c("paneurope_EOO","id")
  # Unit conversion from meters to square km
  EOO_working$paneurope_EOO <- EOO_working$paneurope_EOO/1000000
  # Append to spec.list
  spec.list$paneurope_EOO <- NA
  spec.list$paneurope_EOO[match(EOO_working$id, spec.list$id)] <- 
    EOO_working$paneurope_EOO
  # Convert to global variable
  return(spec.list)
}

# Parameters: intersects_all
# Returns: paneurope_EOO (column added to spec.list)
# Throws: none
# Purpose: Calculates EOO for all taxa with point data. Results added
#          to spec.list
eu27_eoo_calculate <- function(x){
  # Remove non-native points and points outside pan-Europe
  point_natives <- x[which(x$ORIGIN == 1 & !is.na(x$eu27_intersect)),]
  # Split data into list of data frames
  point_natives <- split(point_natives , f = point_natives$ID_NO)
  # Apply EOO calculation for subset
  EOO_working <- lapply(point_natives, EOO_calculator)
  # Reformat table
  EOO_working <- lapply(EOO_working, as.numeric)
  EOO_working <- stack(EOO_working)
  # Rename table
  names(EOO_working) <- c("EU27_EOO","id")
  # Unit conversion from meters to square km
  EOO_working$EU27_EOO <- EOO_working$EU27_EOO/1000000
  # Append to spec.list
  spec.list$EU27_EOO <- NA
  spec.list$EU27_EOO[match(EOO_working$id, spec.list$id)] <- 
    EOO_working$EU27_EOO
  # Convert to global variable
  return(spec.list)
}

# Parameters: intersects_all
# Returns: EU_points
# Throws: none
# Purpose: Cleans data for export
EU_points_generate <- function(x){
  EU_points_export <- x
  # Remove geometry
  EU_points_export$geometry <- NULL
  # Remove unused columns
  EU_points_export <- 
    EU_points_export[,which(names(
      EU_points_export) %in% c("ID_NO",
                              "paneurope_intersect",
                              "eu27_intersect",
                              "BasisOfRec",
                              "EVENT_YEAR",
                              "ORIGIN",
                              "SEASONAL",
                              "YEAR",
                              "COMPILER",
                              "SUBSPECIES",
                              "SUBPOP",
                              "DATA_SENS",
                              "SENS_COMM",
                              "DIST_COMM",
                              "ISLAND",
                              "PRESENCE",
                              "TAX_COMM",
                              "SPATIALREF",
                              "CATALOG_NO",
                              "SOURCE",
                              "BINOMIAL",
                              "CITATION",
                              "DEC_LAT2",
                              "DEC_LONG2")
      )]
  names(EU_points_export) <- c("ID_NO",
                               "paneurope_intersect",
                               "eu27_intersect",
                               "BasisOfRec",
                               "EVENT_YEAR",
                               "ORIGIN",
                               "SEASONAL",
                               "YEAR",
                               "COMPILER",
                               "SUBSPECIES",
                               "SUBPOP",
                               "DATA_SENS",
                               "SENS_COMM",
                               "DIST_COMM",
                               "ISLAND",
                               "PRESENCE",
                               "TAX_COMM",
                               "SPATIALREF",
                               "CATALOG_NO",
                               "SOURCE",
                               "BINOMIAL",
                               "CITATION",
                               "DEC_LAT",
                               "DEC_LONG")
  # Recode EU intersects
  EU_points_export$paneurope_intersect[
    which(!is.na(EU_points_export$paneurope_intersect))] <- TRUE
  EU_points_export$paneurope_intersect[
    which(is.na(EU_points_export$eu27_intersect))] <- FALSE
  EU_points_export$eu27_intersect[
    which(!is.na(EU_points_export$eu27_intersect))] <- TRUE
  EU_points_export$eu27_intersect[
    which(is.na(EU_points_export$eu27_intersect))] <- FALSE
  return(EU_points_export)
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Defines function for calculating a minimum convex hull
#          given point data of class sf
EOO_calculator <- function(x){
  st_geod_area(st_convex_hull(st_union(
    x$geometry)))
}



#### Execute Main Function ####
EU_MASK_MAIN()