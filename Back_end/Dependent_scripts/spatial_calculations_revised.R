#### Spatial Calculations ####
## Version 3.1
# Started: 26 April 2021
# Last worked on: 11 August 2021
# Author: Clay Meredith
# File: Dependent_scripts/spatial_calculations.R
# Description: Determines the position of all points in export files within
#              the WGSRPD dataset. 

# Notes #
# # Centroid calculation functions call package rgeos which may be
# # masked by other packages. If errors are produced, consider 
# # checking these packages

#### Load packages ####
packages <- c("sf","lwgeom","red","rgeos","geosphere","dplyr")

lapply(packages, package.check)

#### Main Function ####

SPATIAL_CALCULATIONS_MAIN <- function (){
  print("Performing spatial calculations.")
  # Duplicate latitude and longitude fields
  GBIF_point_data <- GBIF_lat_long_duplicate()
  # Divide into list of data frames
  GBIF_point_data <- divide_points_list(GBIF_point_data)
  # Throttle number of points only if toggle is selected in default values
  if(default_vals$value[which(default_vals$var_name=="throttle_points")]){
    GBIF_point_data <- subset_point_data(GBIF_point_data,
          as.numeric(
            default_vals$value[which(default_vals$var_name=="throttle_level")]))
  }
  print("line 34")
  # Turn off s2 processing (introduces errors to WGSRPD dataset)
  sf::sf_use_s2(FALSE)
  # Convert to list of sf
  GBIF_point_data <- prepare_points_intersection(GBIF_point_data)
  # Calculate intersection with WGSRPD
  WGSRPD_intersect <- intersection_calculate(GBIF_point_data,
                         lvl3[,which(names(lvl3) %in% c("LEVEL3_COD","geometry"))],
                         "LEVEL3_COD")
  # Merge point data and WGSRPD intersection data
  GBIF_point_data <- merge_intersection(GBIF_point_data, 
                                        WGSRPD_intersect,
                                        "lvl3")
  # Calculate intersection with realms
  realms_intersect <- intersection_calculate(GBIF_point_data,
                          realms[,which(names(realms) %in% c("REALM","geometry"))],
                                             "REALM")
  # Merge point data and realm intersection data
  GBIF_point_data <- merge_intersection(GBIF_point_data, 
                                        realms_intersect,
                                        "realm")  
  # Calculate intersection with hotspots
  hotspot_intersect <- intersection_calculate(GBIF_point_data,
                          realms[which(hotspots$Type=="hotspot area"),
                                 which(names(realms) %in% c("NAME","geometry"))],
                                             "NAME")
  # Merge point data and hotspot intersection data
  GBIF_point_data <- merge_intersection(GBIF_point_data, 
                                        realms_intersect,
                                        "hotspot")
  

  GBIF_point_data <- POINT_data_origin_recalculate(GBIF_point_data)
  
  
  GBIF_natives <- GBIF_native_subset(GBIF_point_data)
  # Calculate maximum EOO
  EOO_max_calculate(GBIF_natives)
  # Calculate distance to centroid for all points
  dist.cent <- Centroids_calculation()
  # Calculate minimum EOO value
  eoo_min <- POINT_subset(dist.cent)
  # Bind EOO_min to spec.list
  spec.list <- merge(eoo_min, spec.list, by="id", all.y = TRUE)
  # # Calculate AOO values
  # aoo_max <- aoo_max_calc(GBIF_point_data)
  # # Bind AOO_max and AOO_min to spec.list
  # spec.list <- merge(aoo_max, spec.list, by="id", all.y = TRUE)
  # spec.list <- merge(aoo_min, spec.list, by="id", all.y = TRUE)
  # spec.list <<- spec.list
  GBIF_point_data <- WGSRPD_lvl1_add(GBIF_point_data)
  # # Return GBIF_point_data to useable format
  # GBIF_point_data <<- return_to_original_format()
  # 
  # # Remove calculation table
  # rm(GBIF_natives)
  # rm(cent.calc)
  # rm(dist.cent)
}


#### Subsidiary functions ####

# Parameters: x, y (points dataset, and shapefile, respectively)
# Returns: x
# Throws: none
# Purpose: Find intersect of point data and WGSRPD dataset
occurrence.ref <- function (x,y) {
  data.frame(st_intersects(x, y, sparse = TRUE))
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
  GBIF_point_data <- GBIF_point_data_raw
  # Duplicate latitude
  GBIF_point_data$DEC_LAT2 <- as.numeric(GBIF_point_data$DEC_LAT)
  # Duplicate longitude
  GBIF_point_data$DEC_LONG2 <- as.numeric(GBIF_point_data$DEC_LONG)
  return(GBIF_point_data)
}

# Parameters: x = GBIF_point_data (table from GBIF_download.R), 
# Returns: GBIF_point_data (table)
# Throws: none
# Purpose: Divides point dataset into list of dataframes
divide_points_list <- function (x){
  x$ID_NO <- as.factor(x$ID_NO)
  # Divide data into list of data frames
  GBIF_working <- split(x , f = x$ID_NO )
  # Add index column
  GBIF_working <- lapply(GBIF_working, index.col)
  return(GBIF_working)
}


# Parameters: x = GBIF_point_data (table from GBIF_download.R), 
#             y = point throttle threshold cutoff
# Returns: GBIF_point_data (table)
# Throws: none
# Purpose: Define function used to subset each taxon's points

subset_function <- function (x,y){
  x <- slice_sample(.data = x, n = y)
}

# Parameters: x = GBIF_point_data (table from GBIF_download.R), 
#             y = point throttle threshold cutoff
# Returns: GBIF_point_data (table)
# Throws: none
# Purpose: Randomly subsets points fields when number of results 
#          exceeds threshold
subset_point_data <- function(x,y){
  # Determine number of points in each dataset
  point_sample_size <- lapply(x, nrow)
  # Select smaller of nrow or throttle limit
  point_sample_size <- lapply(point_sample_size,
                              function(x){
    if(x<y){as.numeric(x)} else {as.numeric(y)}
  })

  # Sample points
  for (i in 1:length(point_sample_size)){
    x[[i]] <- slice_sample(x[[i]], n = unname(unlist(point_sample_size))[i])
  }
  return(x)
}

# Parameters: x = GBIF_point_data (table from GBIF_download.R), 
# Returns: GBIF_point_data (table)
# Throws: none
# Purpose: Defines geometry and divides table into list of tables
prepare_points_intersection <- function (x){
  # Redefine points data as sf object
  GBIF_working <- lapply(x, sf.function)
  return(GBIF_working)
}

# Parameters: x = GBIF_point_data (table from GBIF_download.R), 
#             y = geospatial dataset (two columns only)
#             z = column name to cross reference for results
# Returns: GBIF_point_data (table)
# Throws: none
# Purpose: Determines WGSRPD position of all points. Points which fall outside
#          the WGSRPD polygons are removed (may cause issues with coastal points)
intersection_calculate <- function (x,y,z){
  spatial_temp <- y
  spatial_temp$geometry <- NULL
  # Calculate intersect of GBIF point data and shapefile
  spatial_intersections <- lapply(x, occurrence.ref, y = y)
  # Recode results to retrieve column indicated by z
  for (i in 1:length(spatial_intersections)){

    spatial_intersections[[i]]["col.id"] <- 
      spatial_temp[z][unlist(spatial_intersections[[i]][["col.id"]]),]
  }

  return(spatial_intersections)
}

# Parameters: x = GBIF_point_data (list of dataframes), 
#             y = intersection results
#             z = column name for results (varies based on calculation)
# Returns: GBIF_point_data (table)
# Throws: none
# Purpose: Appends intersect data with GBIF point data
merge_intersection <- function(x,y,z){
  
  # Build new column in points file and assign z as name
  x <- lapply(x, function (list_of_dataframes){
    # Build new column
    cbind(list_of_dataframes, temp = NA)
  }
  )
  # Append intersect results to point file
  for (i in 1:length(x)){
    x[[i]]$temp[y[[i]]$row.id] <- y[[i]]$col.id
    # Give column appropriate name
    names(x[[i]])[names(x[[i]]) == "temp"] <- z
  }
  return(x)
}


# Parameters: 
# Returns: 
# Throws: none
# Purpose: Define function to cross reference point position with 
#          occurrence records from other sources.
cross.occ <- function (x) {
  x$ORIGIN[which(x$lvl3 %ni% countries_table$occ[
    which(x$ORIGIN == 1 &
            countries_table$id == x$ID_NO[1])])] <- 3 
  return(x)
}

# Parameters: GBIF_point_data
# Returns: GBIF_point_data
# Throws: none
# Purpose: Reclassifies origin field for points based on occurrence data
#          from other sources.
POINT_data_origin_recalculate <- function(x) {
  # Redefine origin field based on Kew, NS, and VC occurrence lookups
  GBIF_working <- lapply(GBIF_working, cross.occ)
  return(GBIF_working)
}

# Parameters: GBIF_point_data (table from GBIF_download.R)
# Returns: GBIF_natives (table)
# Throws:
# Purpose: Subsets GBIF point data to include only natives for calculations and converts to
#          sf class.
GBIF_native_subset <- function (x){
  # # Convert to long format
  GBIF_natives <- data.frame(bind_rows(x, .id="ID_NO"))
  # Subset native points
  GBIF_natives <- GBIF_natives[GBIF_natives$ORIGIN==1,]
  # Convert to sf
  GBIF_natives <- st_as_sf(GBIF_natives)
  return(GBIF_natives)
}

# Parameters: GBIF_point_data (table)
# Returns: EOO_max (column added to spec.list)
# Throws: none
# Purpose: Calculates EOO for all taxa with point data. Results added
#          to spec.list
EOO_max_calculate <- function(){
  spec.list$EOO_max <- NULL
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

# Parameters: GBIF_natives (table)
# Returns: cent_calc (table), dist.cent (table)
# Throws: none
# Purpose: Calculates centroid of species point clouds, and distance of each point
#          to point cloud centroid. Centroid is a temporary field used for EOO_min 
#          and AOO_min calculations
Centroids_calculation <- function (x){
  # Convert GBIF_natives to data frame
  x$geometry <- NULL
  # Split data into list of data frames
  GBIF_working <- split(x , f= Gx$ID_NO)
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

# Final reformatting
# 
# # Collapse all lists into single data frames
# spatial_intersections <- data.frame(bind_rows(
#   spatial_intersections, .id="name"))
# 
# GBIF_working <- data.frame(bind_rows(
#   x, .id="ID_NO"))
# # Rename columns
# names(spatial_intersections) <- c("ID_NO","index",z)
# # Merge occurrences to master data
# GBIF_working <- merge(GBIF_working, spatial_intersections, by=c("ID_NO","index"),
#                       all.y=TRUE)
# # Remove index column
# GBIF_working <- GBIF_working[,-which(names(GBIF_working) %in% c("index"))]


##### Run Main Function ####
SPATIAL_CALCULATIONS_MAIN()
# 
# intersection_calculate(GBIF_point_data, lvl3, "lvl3")
# 
# intersection_calculate(GBIF_point_data, 
#                        lvl3[,which(names(lvl3) %in% c("LEVEL3_COD","geometry"))],
#                        "lvl3")

