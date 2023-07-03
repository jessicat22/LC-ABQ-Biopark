#### IPNI Search Script ####
## Version 3.1.3
# Started: 16 June 2023
# Last edited: 16 June 2023
# Author: Clay Meredith
# File: IPNI_scrape.R
# Description: Script searches IPNI and returns accepted resulting names for 
# a given species

#### Load packages ####
#### Load packages ####
packages <- c("tidyverse","taxize","httr")

lapply(packages, package.check)

#### Main Function ####
# Parameters: spec.list
# Returns: IPNI_data (table), IPNI_raw (table)
# Throws: none
# Purpose: Executes IPNI taxonomy search. Reformats results. 
#          Returns IPNI_data (table)


IPNI_main <- function (x){
  
  
}

#### Functions ####

# Parameters: spec.list (table)
# Returns: IPNI_genus_raw
# Throws: none
# Purpose: Returns all members of the genera included in spec.list

IPNI_genus_raw <- lapply(unique(spec.list$genus), function(x){
  ipni_search(genus = x)
})

# Parameters: spec.list (table)
# Returns: IPNI_genus_raw
# Throws: none
# Purpose: Returns all members of the genera included in spec.list

foo <- ipni_search(genus = spec.list$genus[1])

foo$full_name_without_family_and_authors

spec.list$specific_epithet[1]

ipni_search(genus='Pinus',species='contorta', ranktoreturn = 'spec')
