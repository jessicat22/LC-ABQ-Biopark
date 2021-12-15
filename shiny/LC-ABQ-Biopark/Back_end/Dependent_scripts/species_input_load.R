#### Mapping Functions ####
## Version 3.1
# Started: 3 November 2021
# Last worked on: 3 November 2021
# Author: Clay Meredith
# File: Dependent_scripts/map_build.R
# Description: Handler for species inputs. Parses type based on title and
#              loads data into spec.list

# If taxonom.csv file exists, assign it to spec.list
# if(!is.na(match("taxonomy.csv", tolower(list.files("User_inputs/"))))){
#   spec.list <- data.frame(read.csv(
#     "User_Inputs/taxonomy.csv"))
#   # Rename columns to match existing code
#   names(spec.list)[names(spec.list) == 'species'] <- "specific_epithet"
#   names(spec.list)[names(spec.list) == 'Redlist_id'] <- "iucn_id"
#   names(spec.list)[names(spec.list) == 'internal_taxon_name'] <- "Species"
#   names(spec.list)[names(spec.list) == 'taxonomicAuthority'] <- "author"
# 
# } else 
#   # Load spec_list.csv
# {## List must be a single column .csv file with the header "Species"
#   spec.list <- data.frame(read.csv(
#     "User_Inputs/spec_list.csv"))
# }
names(spec.list)[names(spec.list) == 'species'] <- "specific_epithet"
names(spec.list)[names(spec.list) == 'Redlist_id'] <- "iucn_id"
names(spec.list)[names(spec.list) == 'internal_taxon_name'] <- "Species"
names(spec.list)[names(spec.list) == 'taxonomicAuthority'] <- "author"

# Remove no-break spaces
spec.list$Species <- remove_no_breaks(spec.list$Species)
# Convert species names to class character
spec.list$Species <- as.character(spec.list$Species)
# Asign arbitrary internal id (based on system time to avoid duplicates)
spec.list$int_id <- -1*seq(from=as.numeric(format(Sys.time(),
                                                  "%y%m%d%H%M%S")),
                           to=(as.numeric(format(Sys.time(),
                                                 "%y%m%d%H%M%S"))+
                                 length(spec.list$Species)-1))

# Replace arbitrary id with Red List ID (if it exists in input table)
spec.list$id <- spec.list$iucn_id
spec.list$id[which(spec.list$id == "")] <- NA
spec.list$id[which(is.na(spec.list$id))] <- 
  spec.list$int_id[which(is.na(spec.list$id))]
