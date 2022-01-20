#### VASCAN Search Functions ####
## Version 3.1
# Started: 7 April 2021
# Last worked on: 18 June 2021
# Author: Clay Meredith
# File: Dependent_scripts/VASCAN_functions.R
# Description: Searches VASCAN database for taxonomic matches. Returns tables 
#              VC_common_names, (including common names and associated data),
#              VC_data (containing taxonomic match results, and VASCAN ID numbers),
#              VC_occurrence (containing occurrence data for Canadian provinces),
#              and VC_raw (the raw data retrieved from VASCAN).

#### Load packages ####
packages <- c("taxize","dplyr")

lapply(packages, package.check)

#### Define Main Function ####
VC_main <- function (){
  print("Executing VASCAN search.")
  # Execute search
  VC_search()
  # Count results
  VC_count()
  # Define match type column as NA (prevents issues when num_matches>1)
  VC_data$vc_match_type <<- NA
  # Specify match type
  VC_match_type(VC_raw[which(VC_data$num_matches==1)])
  # Fill match type for species with no matches
  VC_data$vc_match_type[which(VC_data$num_matches==0)] <<-
    "no match"
  # Fill match type for uncertain taxa
  VC_data$vc_match_type[which(VC_data$num_matches>1)] <<-
    "taxonomic issues require inspection"
  # Extract data for matches
  VC_matches(VC_raw[which(VC_data$vc_match_type=="accepted")])
  # Extract data for synonyms
  VC_synonyms(VC_raw[which(VC_data$vc_match_type=="synonym" &
                             VC_data$num_matches==1)])
  # Reformat synonym table
  VC_common_reformat()
  # Extract and reformat occurrence data
  VC_occurrence_reformat()
  # Recode VC occurrence data
  VC_occurrence_recode()
  # Generate citations
  if(length(VC_occurrence)>1){
    references <<- VC_citations_generate()}
  print("VASCAN search complete.")
}

#### VASCAN Functions ####
# Parameters: spec.list$Species (list)
# Returns: VC_raw (list of tables)
# Throws: none
# Purpose: Retrieve data from VASCAN and rename with species id number
VC_search <- function (){
  # Execute search
  VC_raw <<- vascan_search(spec.list$Species)
  # Rename entries based on species ID
  names(VC_raw) <<- spec.list$id
}

# Parameters: spec.list$Species (list), VC_raw (list of tables)
# Returns: VC_data (table)
# Throws: none
# Purpose: Count number of matches for each species and append results to
#          VC_data table.
VC_count <- function () {
  VC_data <<- data.frame(id=spec.list$id)
  VC_data$num_matches <<- 
    lapply(VC_raw,function (x) {
      x$nummatches
    }
    )
}

# Parameters: VC_data (table)
# Returns: VC_data (adds columns)
# Throws: none
# Purpose: Pass on basic data for matches
VC_match_type <- function (x){
  # Add match type to VC_data
  VC_data$vc_match_type[which(VC_data$num_matches==1)] <<-
    lapply(x,function (x){
      x$matches[[1]]$taxonomicassertions$taxonomicstatus
    }
    )
}

# Parameters: VC_data (table)
# Returns: VC_data (table)
# Throws: none
# Purpose: Pass exact match data (accepted name, id) to VC_data
VC_matches <- function (x){
  # Build dummy columns
  VC_data$vc_name <<- NA
  VC_data$vc_id <<- NA
  # Pass accepted binomial
  VC_data$vc_name[which(VC_data$vc_match_type=="accepted")] <<-
    lapply(x,function (x){
      x$matches[[1]]$taxonomicassertions$acceptednameusage
    })
  # Pass accepted id
  VC_data$vc_id[which(VC_data$vc_match_type=="accepted")] <<-
    lapply(x,function (x){
      x$matches[[1]]$taxonomicassertions$acceptednameusageid
    })
}

# Parameters: VC_synonyms (table)
# Returns: VC_data (table)
# Throws: none
# Purpose: Pass synonym data if input taxon is classified as synonym with
#          one accepted result
VC_synonyms <- function (x){
  # Pass accepted binomial
  VC_data$vc_name[which(VC_data$vc_match_type=="synonym" & 
                          VC_data$num_matches==1)] <<-
    lapply(x,function (x){
      x$matches[[1]]$taxonomicassertions$acceptednameusage
    })
  # Pass accepted id
  VC_data$vc_id[which(VC_data$vc_match_type=="synonym" & 
                        VC_data$num_matches==1)] <<-
    lapply(x,function (x){
      x$matches[[1]]$taxonomicassertions$acceptednameusageid
    })
  # Re-run search to include accepted synonyms
  # Execute search
    VC_raw[which(VC_data$vc_match_type=="synonym" & 
                   VC_data$num_matches==1)] <<- vascan_search(
                     VC_data$vc_id[which(VC_data$vc_match_type=="synonym" & 
                     VC_data$num_matches==1)])
    # Rename entries based on species ID
    names(VC_raw) <<- spec.list$id
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Function to extract vernacular name data
VC_extract_common <- function (x) {
  data.frame(name = x$matches[[1]]$vernacularnames$vernacularname,
             language = x$matches[[1]]$vernacularnames$language,
             primary = x$matches[[1]]$vernacularnames$preferredname)
}

# Parameters: VC_raw (list of tables), VC_extract_common (function)
# Returns: VC_common_names
# Throws: none
# Purpose: Execute VC_extract function and append results to VC_common_names
VC_common_reformat <- function () {
  # Extract common name data
  VC_common_names <- lapply(VC_raw[which(
    VC_data$vc_match_type!="no match")], 
    VC_extract_common)
  # Reformat as single table
  VC_common_names <- data.frame(bind_rows(VC_common_names, 
                                           .id="internal_taxon_id"))
  # Convert names to title case
  VC_common_names$name <- str_to_title(VC_common_names$name)
  # Convert language to accepted versions
  VC_common_names$language[which(VC_common_names$language == "fr")] <- "French"
  VC_common_names$language[which(VC_common_names$language == "en")] <- "English"
  # Add source column
  VC_common_names$source <- "VASCAN"
  # Set as global variable
  VC_common_names <<- VC_common_names
}

# Parameters: 
# Returns: 
# Throws: none
# Purpose: Extract occurrence data from VC_raw
VC_extract_occurrence <- function (x) {
  data.frame(CountryOccurrenceLookup = 
               x$matches[[1]]$distribution$locality,
             origin = x$matches[[1]]$distribution$establishmentmeans,
             presence = x$matches[[1]]$distribution$occurrencestatus)
}

# Parameters: VC_extract_occurrence (function), VC_raw (list of tables)
# Returns: VC_occurrence
# Throws: none
# Purpose: Execute occurrence extraction and convert to single table
VC_occurrence_reformat <- function () {
  # Extract common name data
  VC_occurrence <- lapply(VC_raw[which(
    VC_data$vc_match_type!="no match")], 
    VC_extract_occurrence)
  # Reformat as single table
  VC_occurrence <- data.frame(bind_rows(VC_occurrence, 
                                         .id="internal_taxon_id"))
  # Set to global variable
  VC_occurrence <<- VC_occurrence
  
}

# Parameters: VC_occurrence (table)
# Returns: references (table)
# Throws: none
# Purpose: Add VC citations to references table
VC_citations_generate <- function (){
  # Identify taxa using GBIF data
  VC_ids <- unique(VC_occurrence$id)
  if (length(VC_ids)>0){
    # Generate references
    VC_citations <- ref.key[which(ref.key$keywords == "VASCAN"),]
    VC_citations <- rbind(VC_citations, VC_citations[rep(1, length(VC_ids)-1),])
    # Append ids to table
    VC_citations$internal_taxon_id <- VC_ids
    # Bind to references table
    references <- rbind(references, VC_citations)
    return(references)
  }
}

# Parameters: VC_occurrence (table)
# Returns: VC_occurrence (table)
# Throws: none
# Purpose: Recodes occurrence codes from VASCAN to IUCN. 
#          Recodes PRESENCE fields. Recodes ORIGIN fields.
#          Removes unused columns.
VC_occurrence_recode <- function () {
  if (exists("VC_occurrence") & nrow(VC_occurrence)>0) {
    # Index VC Code to return IUCN occ code
    VC_occurrence$occ <-
      occ.codes$iucn_code[match(VC_occurrence$CountryOccurrenceLookup,
                                occ.codes$vc_codes)]
    # Index VC code to return WGSRPD3 code
    VC_occurrence$WGSRPD3 <-
      occ.codes$lvl3_display[match(VC_occurrence$CountryOccurrenceLookup,
                                   occ.codes$vc_codes)]
    
    # Drop unused column and rename remaining columns
    names(VC_occurrence) <- c("id", "WGSRPD3", "ORIGIN", "PRESENCE",
                              "occ")
    VC_occurrence <- VC_occurrence[, which(names(VC_occurrence) %in%
                                             c("id", "WGSRPD3", "ORIGIN", "PRESENCE", "occ"))]
    # Recode origin fields
    VC_occurrence$ORIGIN[which(VC_occurrence$ORIGIN == "native")] <- 1
    VC_occurrence$ORIGIN[which(VC_occurrence$ORIGIN == "introduced")] <- 3
    VC_occurrence$ORIGIN[which(VC_occurrence$ORIGIN == "")] <- 5
    # Recode presence fields
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "native")] <- 1
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "excluded")] <- 6
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "doubtful")] <- 6
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "introduced")] <- 1
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "ephemeral")] <- 6
    VC_occurrence$PRESENCE[which(VC_occurrence$PRESENCE == "extirpated")] <- 6
    # Add source column and SEASONALITY column
    VC_occurrence$source <- "VASCAN"
    VC_occurrence$SEASONALITY <- 1

  }
  # Reorder columns
  VC_occurrence <- VC_occurrence[,c("id","WGSRPD3","ORIGIN","PRESENCE",
                                    "SEASONALITY","source","occ")]
  # Convert to global variable
  VC_occurrence <<- VC_occurrence
}

#### Execute Functions ####
VC_main()
