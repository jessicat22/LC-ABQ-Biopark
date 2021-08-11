#### Spatial Variable Test ####
## Version 3.1
# Started: 25 June 2021
# Last worked on: 25 June 2021
# Author: Clay Meredith
# File: Dependent_scripts/spatial_variable_test.R
# Description: Calculates spatial statistics by modifying default values of outlier_threshold,
#              uncertainty tolerance

VARIABLE_TEST_MAIN <- function (){
  spatial_calculations_threshold_test()
  
}

# Parameters: GBIF_natives (table)
# Returns: eoo_min (table), 
# Throws: none
# Purpose: Iteratively subsets points based on distance from centroid and reports EOO and AOO
#          for each subset.
spatial_calculations_threshold_test <- function () {
  # Split native occurrences into list of data frames
  GBIF_working <- split(GBIF_natives , f = GBIF_natives$ID_NO)
  # Subset points based on distance from centroid
  GBIF_subset <- mapply(cbind,
                        GBIF_working,
                        "distance" = dist.cent,
                        SIMPLIFY = F)
  # Loop from 1 to 50% distant points removal
  for (i in (1:50)){
    # Clear most distant  points for minimum EOO estimate
    eoo_min <-
      lapply(GBIF_subset, function(x) {
        x[x$distance < quantile(x$distance, 1-(i/100)
                                ),]
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
    names(aoo_min) <- c(paste("AOO_",i,sep = ""),"id")
 
    
    # Calculate minimum EOO (conversion to km2 embedded in function)
    eoo_min <- lapply(eoo_min, function(x) {
      as.numeric(st_geod_area(st_convex_hull(st_union(x$geometry)))) / 1000000
    })
    
    # Reformat table
    eoo_min <- data.frame(t(bind_rows(eoo_min, .id="ID_NO")))
    eoo_min$ID_NO <- rownames(eoo_min)
    names(eoo_min) <- c(paste("EOO_",i,sep = ""),"id")
    
    # Bind to eoo_min to EOO_distribution
    if(exists("EOO_distribution")){
      EOO_distribution <- merge(EOO_distribution, eoo_min)
    } else {EOO_distribution <- eoo_min}
    # Bind aoo_min to AOO_distribution
    if(exists("AOO_distribution")){
      AOO_distribution <- merge(AOO_distribution, aoo_min)
    } else {AOO_distribution <- aoo_min}
  }
  # Append batch numbers
  EOO_distribution$batch_no <- default_vals$value[which(default_vals$var_name == "batch_no")]
  AOO_distribution$batch_no <- default_vals$value[which(default_vals$var_name == "batch_no")]
  
EOO_distribution <<- EOO_distribution
AOO_distribution <<- AOO_distribution
}

#### Run Program ####
  VARIABLE_TEST_MAIN()