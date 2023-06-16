#### Basic Functions ####
## Version 3.1
# Started: 5 April 2021
# Last worked on: 11 June 2021
# Author: Clay Meredith
# File: Back_end/Dependent_scripts/base_functions.R
# Description: Defines miscellaneous functions used in multiple other 
# dependent scripts.


# Parameters: list
# Returns: list (logical)
# Throws: none
# Purpose: Checks if packages are installed and installs if necessary, 
# then loads packages
package.check <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE, verbose = FALSE, quiet = TRUE)
    library(x, character.only = TRUE, quietly = TRUE)
  }
}

#### Load packages ####
packages <- c("stringr")

lapply(packages, package.check)

# Parameters: list
# Returns: list (logical)
# Throws: none
# Purpose: Negates base function %in% to identify list elements not 
# containing a target

`%ni%` <- Negate(`%in%`)

#### Functions to split trinomials ####
# Parameters: variable of class character
# Returns: variable of class character containing multiple elements
# Throws: none
# Purpose: Converts trinomial names to wide format
trinomial_to_wide <- function (x){
  strsplit(x," ", fixed=-T)[[1]]
}

# Parameters: variable of class character
# Returns: variable of class character
# Throws: none
# Purpose: Returns character string before first space in string.
#          Used to extract genus from a trinomial name.
genus_extract <- function (x){
  strsplit(x," ", fixed=-T)[[1]][1]
}

# Parameters: variable of class character
# Returns: variable of class character
# Throws: none
# Purpose: Returns character string between first and second space in string.
#          Used to extract specific epithet from a trinomial name.
species_extract <- function (x){
  strsplit(x," ", fixed=-T)[[1]][2]
}

# Parameters: variable of class character
# Returns: variable of class character
# Throws: none
# Purpose: Returns character string between second and third space in string.
#          Used to extract infraspecific rank from a trinomial name.
infra_level_extract <- function (x){
  strsplit(x," ", fixed=-T)[[1]][3]
}

# Parameters: variable of class character
# Returns: variable of class character
# Throws: none
# Purpose: Returns character string between third and fourth space in string.
#          Used to extract infraspecific epithet from a trinomial name.
infraspecific_extract <- function (x){
  strsplit(x," ", fixed=-T)[[1]][4]
}

# Parameters: variable of class character
# Returns: variable of class character
# Throws: none
# Purpose: Converts infraspecific rank to standardized character value
#          f (forma), v (variety), or s (subspecies)
standardize_level <- function (x){
  # Subset only the first character
  first_letter <- as.list(substring(x, 1, 1))
  # Re-code results
  if (first_letter %in% c("v","V")){first_letter <- "variety"}
  if (first_letter %in% c("s","S")){first_letter <-"subspecies (plantae)"}
  if (first_letter %in% c("f","F")){first_letter <-"forma"}
  return(first_letter)
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Input with no break spaces removed
remove_no_breaks <- function (x){
  ## Old version. Works on PC but fails on MAC
  # gsub("\u00A0", " ", x, fixed = TRUE, useBytes = TRUE)
  # gsub("<a0>", " ", x, fixed = TRUE, useBytes = TRUE)
  str_replace_all(x,"U+FFF0", " ")
  str_replace_all(x,"\u00A0", " ")
  str_replace_all(x,"<a0>", " ")
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Converts common names to uppercase without including post-hyphen words
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


# Parameters: 
# Returns: 
# Throws: none
# Purpose: Count number of decimal places in numeric data (used  by precision_index 
#          to define coordinate precision)
decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    match(TRUE, round(x, 1:20) == x)
    
  } else {
    return(0)
  }
}

# Parameters: a data frame
# Returns: a data frame with all columns unlisted
# Throws: none
# Purpose:Unlists data frame columns to ensure all data can be exported to a .csv file
unlist_data <- function(x) {
  ListCols <- sapply(x, is.list)
  x[,which(ListCols)] <- unlist(x[,which(ListCols)])
  return(x)
}


# Parameters: x
# Returns: x
# Throws: none
# Purpose: Converts st to sf for spatial calculations
sf.function <- function (x) {st_as_sf(x,
                                      coords = c("DEC_LONG", "DEC_LAT"),
                                      crs = "WGS84",
                                      stringsAsFactors = FALSE,
                                      remove = TRUE)
}
