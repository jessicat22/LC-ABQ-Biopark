#### SIS Connect File Generator ####
## Version 3.1
# Started: 21 May 2021
# Last worked on: 1 July 2021
# Author: Clay Meredith
# File: Dependent_scripts/SIS_connect_file_generator.R
# Description: Converts existing data from various sources into SIS Connect Files
#              for upload to SIS.

#### Load packages ####
packages <- c("tidyr","data.table","dplyr","reshape2")

lapply(packages, package.check)

#### Main Function ####
# Parameters: 
# Returns: 
# Throws: 
# Purpose: 
SIS_table_generator_main <- function (){
  # # Export synonyms from NS and POWO
  # synonyms_table <- data.frame(synonym_export())
  # synonyms_table <- unnest(synonyms_table, cols = c(speciesName, infraType, infrarankName))
  # # Remove species field (necessary due to error in SIS Connect which is unlikely to be fixed)
  # synonyms_table$speciesName <- ""
  # write.csv(synonyms_table, 
  #           paste("Outputs/Synonyms_batch_", 
  #                 default_vals$value[which(default_vals$var_name == "batch_no")],
  #                 ".csv", sep = ""),
  #           row.names = FALSE)

  # Export common names from ITIS, NS, and VC
  common_name_final <- commonname_reformat()
  common_name_final <- unnest(common_name_final, cols = c(name))
  common_name_final$language <- vapply(common_name_final$language,
                                         paste, collapse = ", ", character(1L))
  common_name_final$primary <- vapply(common_name_final$primary,
                                       paste, collapse = ", ", character(1L))
  common_name_final$source <- vapply(common_name_final$source,
                                       paste, collapse = ", ", character(1L))
  # Replace NAs with blanks
  common_name_final[is.na(common_name_final)] <- ""
  common_name_final <<- common_name_final
  write.csv(common_name_final, 
            paste("Outputs/commonnames_batch_", 
                  default_vals$value[which(default_vals$var_name == "batch_no")],
                  ".csv", sep = ""),
            row.names = FALSE)
  
  # Compose distribution narrative
  dist.realm <- narrative_realm_search()
  extreme_occ <- Extreme_occurrences_narrative()
  # Build final elevation table (FNA is prioritized over GBIF)
  elevation_text <- elevation_narrative()
  # Build allfields table
  allfields <<- ALLFIELDS_TABLE_BUILD(elevation_text)
  # Write allfields table
  write.csv(allfields, 
            paste("Outputs/Allfields_batch_", 
                  default_vals$value[which(default_vals$var_name == "batch_no")],
                  ".csv", sep = ""),
            row.names = FALSE)
  # Build assessments table
  assessments <<- ASSESSMENT_TABLE_BUILD(elevation_text)
  # Write assessments table
  write.csv(assessments, 
            file=paste("Outputs/Assessments_batch_", 
                                    default_vals$value[which(default_vals$var_name == "batch_no")],
                                    ".csv", sep = ""),
            row.names = FALSE)
  # Replace NA values with blanks
  countries_table[is.na(countries_table)] <- ""
  # Define final table
  countries_final <- countries_table
  # Recode final table
  countries_final <- countries_recode(countries_final)
  # Replace countries table headers
  names(countries_final) <-
    c(
      "internal_taxon_id",
      "DELETE",
      "CountryOccurrence.CountryOccurrenceSubfield.origin",
      "CountryOccurrence.CountryOccurrenceSubfield.presence",
      "CountryOccurrence.CountryOccurrenceSubfield.seasonality",
      "CountryOccurrence.CountryOccurrenceSubfield.CountryOccurrenceLookup"
    )
  countries_final <- countries_final[,-which(names(countries_final) %in% 
                                               c("DELETE"))]
  # Write countries table
  write.csv(countries_final, 
            file = paste("Outputs/Countries_batch_", 
                  default_vals$value[which(default_vals$var_name == "batch_no")],
                  ".csv", sep = ""),
            row.names = FALSE)
  # Replace NA values with blanks
  references[is.na(references)] <- ""
  # Write references table
  write.csv(references, 
            file=paste("Outputs/References_batch_", 
                       default_vals$value[which(default_vals$var_name == "batch_no")],
                       ".csv", sep = ""),
            row.names = FALSE)
  # Build credits table
  # Build assessments table
  credits <- rbind(credits.template, credits.template[rep(1, nrow(spec.list)-1), ])
  # Populate internal_taxon_id
  credits$internal_taxon_id <- spec.list$id
  # Replace NA values with blanks
  credits[is.na(credits)] <- ""
  # Write credits table
  write.csv(credits, 
            file=paste("Outputs/Credits_batch_", 
                       default_vals$value[which(default_vals$var_name == "batch_no")],
                       ".csv", sep = ""),
            row.names = FALSE)
  # Write points file
  GBIF_point_data <- occurrence_column_remove(GBIF_point_data)
  # Replace NA values with blanks
  GBIF_point_data[is.na(GBIF_point_data)] <- ""
  # Write table as .csv
  write.csv(GBIF_point_data, 
            file = paste("Outputs/Point_data_batch_", 
                                          default_vals$value[which(default_vals$var_name == "batch_no")],
                                          ".csv", sep = ""),
            row.names = FALSE)
  # Write run parameters table
  write.csv(default_vals, 
            file = paste("Outputs/parameters_batch_", 
                         default_vals$value[which(default_vals$var_name == "batch_no")],
                         ".csv", sep = ""),
            row.names = FALSE)
  # Append taxonomy data to input table
  TAXONOMY_TABLE_BUILD()
  # Add batch number to input table
  spec.list$batch_number <- default_vals$value[which(default_vals$var_name == "batch_no")]
  # Add taxonomy outputs to input table
  
  # Write input table
  spec.list <- unlist_data(spec.list)
  write.csv(spec.list, 
            file = paste("Outputs/species_inputs_batch_", 
                         default_vals$value[which(default_vals$var_name == "batch_no")],
                         ".csv", sep = ""),
            row.names = FALSE)
}

#### Subsidiary Functions ####
# Parameters: NS_synonyms, POW_synonyms
# Returns: synonyms_table
# Throws: 
# Purpose: Collates synonym tables if they exist. If/then statements should allow
#          for any combination of tables should they exist.
synonym_export <- function (){
  # Bind tables
  if (exists("NS_synonyms")){
    synonyms_table <- NS_synonyms
    if (exists("POW_synonyms")){
      synonyms_table <- rbind(synonyms_table, POW_synonyms)
    }
  } else if (exists("POW_synonyms")){
    synonyms_table <- POW_synonyms
  }
  if (exists("synonyms_table")) {
    synonyms_table[is.na(synonyms_table)] <- ""
    return(synonyms_table)
  }
}

# Parameters: ITIS_common_names (table), NS_common_names, VC_common_names
# Returns: commonnames
# Throws: none
# Purpose: Collates common name tables
common_name_collate <- function (){
  # Collate all tables
  commonnames <- data.frame(rbindlist(list(get0("VC_common_names"), 
                                           get0("NS_common_names"), 
                                           get0("ITIS_common_names")),
                                      fill = TRUE))
  # Return collated table
  return(commonnames)
}

# Parameters: commonnames
# Returns: commonnames
# Throws: none
# Purpose: Reformats commonnames table to remove duplicates, convert to title case,
#          assign single primary name, and reformat language column. The function
#          prioritizes VC names, then NS, then ITIS.
commonname_reformat <- function (){
  commonnames <- common_name_collate()
  # Reformat languages
  commonnames$language[which(commonnames$language %in% c("en","EN"))] <- "English"
  commonnames$language[which(commonnames$language %in% c("fr","FR"))] <- "French"
  # Remove primary designation from French names
  commonnames$primary[which(commonnames$language == "French")] <- FALSE
  # Convert all names to lowercase
  commonnames$name <- tolower(commonnames$name)
  # Remove second priority common name if it exists
  commonnames$primary[which(duplicated(
    commonnames[,which(names(commonnames) %in% c("internal_taxon_id","primary"))]))] <-
    FALSE
  # Sort common names by primary column
  commonnames <- commonnames[order(-commonnames$primary),]
  # Remove duplicates
  commonnames <- unique(commonnames)
  # Remove rows with NA (typically introduced by ITIS)
  commonnames <- commonnames[-which(is.na(commonnames$name)),]
  # Convert case of names
  commonnames$name <- lapply(commonnames$name, simpleCap)
  return(commonnames)
}

# Parameters: extreme_occ
# Returns: extreme_temp
# Throws: none
# Purpose: Compiles narrative for extreme occurrences
Extreme_occurrences_narrative <- function () {
  extreme_temp <-
    extreme_occ[,which(names(extreme_occ) %in% c("ID_NO", 
                                                  "lvl3", 
                                                  "lvl1", 
                                                  "direction", 
                                                  "geometry"))]
  # Redefine occurrence codes to readable text
  extreme_temp$lvl3 <- occ.codes$iucn_country[match(extreme_temp$lvl3,
                                                occ.codes$iucn_code)]
  # Remove geometry
  extreme_temp$geometry <- NULL
  # convert table to wide format
  extreme_temp <- pivot_wider(extreme_temp,id_cols = c(ID_NO),
                              names_from = direction,
                              values_from = lvl3)
  # Compose narrative
  extreme_temp$narrative <- paste("It occurs from", extreme_temp$north, "to",extreme_temp$south,
                                  "and from",extreme_temp$east,"to",extreme_temp$west)
  extreme_temp <- data.frame(extreme_temp)
  return(extreme_temp)
}

# Parameters: realm_results
# Returns: 
# Throws: none
# Purpose: Compose distribution narrative section.
# Split long form data frame into list 
narrative_realm_search <- function  () {
  # Split long form data frame into list
  realms.long1 <- split(realm_results , f = realm_results$ID_NO)
  # Build final table
  dist.prefix <- data.frame(matrix(ncol = 3, nrow = nrow(realm_results)))
  colnames(dist.prefix) <- c("internal_taxon_id",
                             "dist_code",
                             "dist_prefix")
  # Populate internal id numbers
  dist.prefix$internal_taxon_id <- as.numeric(realm_results$ID_NO)
  # Order of following functions allows for more inclusive definitions
  # to override more restricted ones.
  
  # Populate prefix for Oceanic distribution
  dist.prefix$dist_code[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "OC"})))] <- 11
  dist.prefix$dist_prefix[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "OC"})))] <- 
    "has an Oceanic distribution"
  
  # Populate prefix for Australasian distribution
  dist.prefix$dist_code[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "AA"})))] <- 10
  dist.prefix$dist_prefix[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "AA"})))] <- 
    "has an Australasian distribution"
  
  # Populate prefix for indomalayan distribution
  dist.prefix$dist_code[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "IM"})))] <- 9
  dist.prefix$dist_prefix[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "IM"})))] <- 
    "has an Indomalayan distribution"
  
  # Populate prefix for afrotropical distribution
  dist.prefix$dist_code[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "AT"})))] <- 8
  dist.prefix$dist_prefix[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "AT"})))] <- 
    "has an Afrotropical distribution"
  
  # Populate prefix for palearctic distribution
  dist.prefix$dist_code[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "PA"})))] <- 7
  dist.prefix$dist_prefix[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "PA"})))] <- 
    "has a Palearctic distribution"
  
  # Populate prefix for neotropical distribution
  dist.prefix$dist_code[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "NT"})))] <- 6
  dist.prefix$dist_prefix[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "NT"})))] <- 
    "has a Neotropical distribution"
  
  # Populate prefix for nearctic distribution
  dist.prefix$dist_code[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "NA"})))] <- 5
  dist.prefix$dist_prefix[which(unlist(
    lapply(realms.long1, function(x) {x$realm == "NA"})))] <- 
    "has a Nearctic distribution"
  
  # Populate prefix for neotropical + nearctic distribution
  dist.prefix$dist_code[which(unlist(
    lapply(realms.long1, function(x) {all(c("NA", "NT")
                                          %in% x$realm)})))] <- 4
  dist.prefix$dist_prefix[which(unlist(
    lapply(realms.long1, function(x) {all(c("NA", "NT")
                                          %in% x$realm)})))] <- 
    "has a Neotropical and Nearctic distribution"
  
  
  # Populate prefix for paleotropical distribution
  dist.prefix$dist_code[which(unlist(
    lapply(realms.long1, function(x) {all(c("IM", "AT")
                                          %in% x$realm)})))] <- 3
  dist.prefix$dist_prefix[which(unlist(
    lapply(realms.long1, function(x) {all(c("IM", "AT")
                                          %in% x$realm)})))] <- 
    "has a Paleotropical distribution"
  
  # Populate prefix for holarctic distribution
  dist.prefix$dist_code[which(unlist(
    lapply(realms.long1, function(x) {all(c("NA","PA")
                                          %in% x$realm)})))] <- 2
  dist.prefix$dist_prefix[which(unlist(
    lapply(realms.long1, function(x) {all(c("NA", "PA")
                                          %in% x$realm)})))] <- 
    "has a Holarctic distribution"
  
  # Populate prefix for global distribution
  dist.prefix$dist_code[which(unlist(
    lapply(realms.long1, function(x) {all(c("NA", "NT", "IM", "AA", "AT", "PA")
                                          %in% x$realm)})))] <- 1
  dist.prefix$dist_prefix[which(unlist(
    lapply(realms.long1, function(x) {all(c("NA", "NT", "IM", "AA", "AT", "PA")
                                          %in% x$realm)})))] <- 
    "has a global distribution"
  
  return(dist.prefix)
}

# Parameters: GBIF_elevation, fna_elev_data
# Returns: elev_narrative
# Throws: none
# Purpose: Compose elevation narrative
elevation_narrative <- function (){
  if (exists("fna_tax_check_final")){

  # Build final elevation table
  elevation_final <- fna_tax_check_final[,which(names(fna_tax_check_final) %in% 
                                                 c("id","elev_min","elev_upper"))]
  
  # Rename fna_elevation_data columns
  names(elevation_final) <- c("internal_taxon_id","min_elev","max_elev")
  
  # Add FNA citations
  elevation_final <- merge(elevation_final, references[
    which(references$author == "Flora of North America Editorial Committee"),
    which(names(references) %in% c("internal_taxon_id","in_text"))])
  # Rename columns to match GBIF table
  names(elevation_final) <- c("internal_taxon_id","min_elev","max_elev","source")

  # Remove rows without data
  elevation_final <- elevation_final[which(!is.na(elevation_final$min_elev)),]
  elevation_final <- elevation_final[which(elevation_final$min_elev != ""),]
  # Assign GBIF source to table
  GBIF_elevation$source <- ref.key$in_text[which(ref.key$keywords=="GBIF")]
  
  # Merge tables
  elevation_final <- rbind(elevation_final, GBIF_elevation)

  } else { # if no FNA table exists use only GBIF values
    elevation_final <- GBIF_elevation
    elevation_final$source <- ref.key$in_text[which(ref.key$keywords=="GBIF")]
  }
  # Remove duplicates
  elevation_final <- elevation_final[which(!duplicated(elevation_final$internal_taxon_id)),]
  return(elevation_final)
}

# Parameters: references (table)
# Returns: occurrence (table)
# Throws: none
# Purpose: Generates citations for distribution narrative fields
distribution_citations_generate <- function (){
  # Build table with each source for taxa with distributions
  dist_cite <- data.frame(id = unique(countries_table$id))
  dist_cite$POWO <- unique(countries_table$id) %in% 
    references$internal_taxon_id[which(references$keywords == "POWO")]
  dist_cite$NS <- unique(countries_table$id) %in% 
    references$internal_taxon_id[which(references$keywords == "NATURESERVE")]
  dist_cite$VC <- unique(countries_table$id) %in% 
    references$internal_taxon_id[which(references$keywords == "VASCAN")]
  # Build citations
  dist_cite$POWO[which(dist_cite$POWO)] <- 
    references$in_text[which(references$keywords=="POWO")]
  dist_cite$NS[which(dist_cite$NS)] <- 
    references$in_text[which(references$keywords=="NATURESERVE")]
  dist_cite$VC[which(dist_cite$VC)] <- 
    references$in_text[which(references$keywords=="VASCAN")]
  dist_cite[dist_cite==FALSE] <- NA
  
  rownames(dist_cite) <- dist_cite$id
  dist_cite$id <- NULL
  dist_cite <- as.list(as.data.frame(t(dist_cite)))
  dist_cite <- lapply(dist_cite, function(x){
    x <- x[!is.na(x)]
  })
  dist_cite <- lapply(dist_cite, function (x){paste(x, collapse=" , ")})
  dist_cite <- as.data.frame(t(bind_rows(dist_cite, .id = "id")))
  dist_cite$id <- rownames(dist_cite)
  return(dist_cite)
}

ASSESSMENT_TABLE_BUILD <- function (x){
  # Load assessment template
  assessments.template <- data.frame(read.csv(
    "Back_end/Dependencies/assessments_template.csv"))
  assessments.template <- assessments.template[0,]
  # Build assessments table
  assessments <- rbind(assessments.template, 
                       assessments.template[rep(1, nrow(spec.list)), ])
  # Populate internal_taxon_id
  assessments$internal_taxon_id <- spec.list$id
  # Merge distribution narrative sections
  extreme_temp <- Extreme_occurrences_narrative()
  dist_citations <- distribution_citations_generate()
  dist_nar1 <- narrative_realm_search()
  dist_nar1 <- merge(dist_nar1, x)

  dist_nar1 <- merge(dist_nar1, extreme_temp, 
                     by.x = "internal_taxon_id", by.y = "ID_NO")
  
  dist_narrative <- merge(dist_citations,dist_nar1, by.x = "id", 
        by.y = "internal_taxon_id")
  # Find common name
  dist_narrative$common <- NA
  dist_narrative$common <- common_name_final$name[match(dist_narrative$id,
                  common_name_final$internal_taxon_id[which(common_name_final$primary==TRUE)])]
  # Add Latin name if common name exists
  dist_narrative$common[which(!is.na(dist_narrative$common))] <- 
    paste(dist_narrative$common[which(!is.na(dist_narrative$common))], " (",
          spec.list$Species[
            match(dist_narrative$id[which(!is.na(dist_narrative$common))], 
                  spec.list$id)], ")",
          sep = "")
  
  # Use Latin name if common name does not exist
  dist_narrative$common[which(is.na(dist_narrative$common))] <- 
    paste(spec.list$Species[
            match(dist_narrative$id[which(is.na(dist_narrative$common))], 
                  spec.list$id)],
          sep = "")
  
  # Add commas to elevation text
  dist_narrative$min_elev <- prettyNum(as.numeric(dist_narrative$min_elev), 
                                     big.mark=",")
  dist_narrative$max_elev <- prettyNum(as.numeric(dist_narrative$max_elev), 
                                       big.mark=",")
  # Compose final narrative
  dist_narrative$final <- paste(dist_narrative$common, " ", dist_narrative$dist_prefix,
                                ". ", dist_narrative$narrative, " (", dist_narrative$V1, "). ",
                                "It occurs at elevations ranging from ", dist_narrative$min_elev,
                                " to ", dist_narrative$max_elev, " m (", dist_narrative$source, 
                                ").", sep = "")
  # Add distribution narrative to assessment table
  assessments$RangeDocumentation.narrative[which(
    assessments$internal_taxon_id %in% dist_narrative$id)] <-
    dist_narrative$final[match(assessments$internal_taxon_id[which(
      assessments$internal_taxon_id %in% dist_narrative$id)], dist_narrative$id)]
  # Re-code realm_results table
  realm_results2 <- realm_results
  realm_results2$realm <- coding_key$realm_text[match(realm_results$realm, 
                                                           coding_key$realm_abbreviated)]
  # Collapse realm_results table
  realm_results2 <- aggregate(realm ~ ID_NO,
                             data = realm_results2,
                             FUN = paste,
                             collapse = '|')
  # Populate assessments BiogeographicRealm.realm field
  assessments$BiogeographicRealm.realm[which(assessments$internal_taxon_id %in% 
                                               realm_results2$ID_NO)] <- realm_results2$realm
  # Populate assessment date
  assessments$RedListAssessmentDate.value <- format(Sys.time(), "%d/%m/%Y")
  
  # Populate habitat and ecology section
  if (exists ("fna_tax_check_final")){
  assessments$HabitatDocumentation.narrative[
    which(assessments$internal_taxon_id %in% 
            fna_tax_check_final$id[which(!is.na(fna_tax_check_final$accepted_match))])] <- 
    paste('The species occurs on "',
          fna_tax_check_final$habitat[which(!is.na(fna_tax_check_final$accepted_match))],
'" (', references$in_text[which(references$author == 
                                  "Flora of North America Editorial Committee" &
  references$internal_taxon_id %in% 
    fna_tax_check_final$id[which(!is.na(fna_tax_check_final$accepted_match))])], ").",
sep = "")
  }
  # Populate language field
  assessments$Language.value <- 
    default_vals$value[which(default_vals$var_name == "language.value")]

  # Populate assessment category
  assessments$RedListCriteria.manualCategory[which(assessments$internal_taxon_id %in%
                                                        allfields$internal_taxon_id[which(
                                                          allfields$AreaRestricted.isRestricted ==
                                                            FALSE
                                                        )])] <- "LC"
  
  
  # Populate category is manual field  
  assessments$RedListCriteria.isManual[which(
    assessments$RedListCriteria.manualCategory == "LC"
  )] <- TRUE
  
  # Populate population narrative
  assessments$PopulationDocumentation.narrative[which(
    assessments$RedListCriteria.manualCategory == "LC"
  )] <- 
    default_vals$value[which(default_vals$var_name == "pop.narrative")]

  # Populate threats narrative
  assessments$PopulationDocumentation.narrative[which(
    assessments$RedListCriteria.manualCategory == "LC"
  )] <-
    default_vals$value[which(default_vals$var_name == "threats.narrative")]
  # Populate map status
  assessments$MapStatus.status[which(
    assessments$RedListCriteria.manualCategory == "LC"
  )] <- "Done"
  # Populate population trend
  assessments$PopulationTrend.value <- "Unknown"
  # Populate assessment rationale
  assessments$RedListRationale.value[which(
    assessments$RedListCriteria.manualCategory == "LC"
  )] <- default_vals$value[which(default_vals$var_name == "rationale.text")]
  # Replace NA values with blanks
  assessments[is.na(assessments)] <- ""
  # Add Red List Criteria Version
  assessments$redlistcriteria.critversion <- 3.1
  
  return(assessments)
}

ALLFIELDS_TABLE_BUILD <- function (x){
    # Load allfields template
  allfields.template <- data.frame(read.csv("Back_end/Dependencies/allfields_template.csv"))
  # Build allfields table
  allfields <- rbind(allfields.template, 
                     allfields.template[rep(1, nrow(spec.list)), ])
  # Populate internal_taxon_id
  allfields$internal_taxon_id <- spec.list$id
  # Populate AOO field
  allfields$AOO.range[which(allfields$internal_taxon_id %in% spec.list$id[which(
    !is.na(spec.list$AOO_min))])] <- paste(spec.list$AOO_min[match(spec.list$id[which(
      !is.na(spec.list$AOO_min))], allfields$internal_taxon_id)], 
      "-", 
      spec.list$AOO_max[match(spec.list$id[which(
        !is.na(spec.list$AOO_min))], allfields$internal_taxon_id)])
  # Round EOO
  spec.list$EOO_max <- signif(as.numeric(spec.list$EOO_max), digits = 3)
  spec.list$EOO_min <- signif(as.numeric(spec.list$EOO_min), digits = 3)
  # Build EOO range field
  spec.list$eoo_range <- NA
  spec.list$eoo_range[which(!is.na(spec.list$EOO_min))] <- 
    paste(format(spec.list$EOO_min[which(!is.na(spec.list$EOO_min))], scientific=F),
          format(spec.list$EOO_max[which(!is.na(spec.list$EOO_min))], scientific=F)
          , sep=" - ")
  # Populate EOO field
  allfields$EOO.range[which(allfields$internal_taxon_id %in% spec.list$id[which(
    !is.na(spec.list$eoo_range))])] <- spec.list$eoo_range[match(spec.list$id[which(
      !is.na(spec.list$eoo_range))], allfields$internal_taxon_id)]
  # Populate elevation minimum
  allfields$ElevationLower.limit[
    which(allfields$internal_taxon_id %in% x$internal_taxon_id)] <- 
    x$min_elev[match(allfields$internal_taxon_id[
      which(allfields$internal_taxon_id %in% x$internal_taxon_id)],
                                           x$internal_taxon_id)]
  # Populate elevation maximum
  allfields$ElevationUpper.limit[
    which(allfields$internal_taxon_id %in% x$internal_taxon_id)] <- 
    x$max_elev[match(allfields$internal_taxon_id[
      which(allfields$internal_taxon_id %in% x$internal_taxon_id)],
      x$internal_taxon_id)]
  # Populate AOO justification field
  allfields$AOO.justification[which(!is.na(allfields$AOO.range))] <- 
    default_vals$value[which(default_vals$var_name == "aoo.just")]
  # Populate EOO justification field
  allfields$EOO.justification[which(!is.na(allfields$EOO.range))] <- 
    default_vals$value[which(default_vals$var_name == "eoo.just")]
  # Removed as this field is included in assessments table
  # Populate population trend field
  # allfields$populationtrend.value <- "Unknown"
  # Check EOO and AOO limits for area restricted designation
  # Flag widespread taxa as area restricted = FALSE
  allfields$AreaRestricted.isRestricted[
    which(allfields$internal_taxon_id %in%
     spec.list$id[which(spec.list$EOO_min >=
                  as.numeric(default_vals$value[which(
                    default_vals$var_name == "is.restricted.eoo.cutoff")]) &
     spec.list$AOO_min >=
                  as.numeric(default_vals$value[which(
                    default_vals$var_name == "is.restricted.aoo.cutoff")])
                                                )])] <- "FALSE"
  # Fill area restricted justification
  allfields$AreaRestricted.justification[which(allfields$AreaRestricted.isRestricted == FALSE)] <-
    default_vals$value[which(default_vals$var_name == "is.restricted.justification")]
  # Fill no threats field
  allfields$NoThreats.noThreats[which(allfields$AreaRestricted.isRestricted == FALSE)] <-
    TRUE
  # Fill threats unknown field
  allfields$threatsunknown.value <- FALSE
  # Replace NA values with blanks
  allfields[is.na(allfields)] <- ""

  return(allfields)
                                           
}

# Parameters: spec.list (table), fna_tax_check_final, ITIS_data,
#             NS_data, POW_data, VC_data
# Returns: spec.list
# Throws: none
# Purpose: Appends taxonomy data from various tables to spec.list prior to export
TAXONOMY_TABLE_BUILD <- function (){
  # Add ITIS results
  if (exists("ITIS_data")){
    # Build ITIS accepted column
    spec.list$ITIS_accepted <- "No Results"
    # Append matched values
    spec.list$ITIS_accepted[
      which(spec.list$id %in% ITIS_data$id)] <- 
      ITIS_data$itis_match[
        match(spec.list$id[which(spec.list$id %in% ITIS_data$id)],ITIS_data$id)]
    # Build ITIS accepted name column
    spec.list$ITIS_name <- "No Results"
    # Append matched values
    spec.list$ITIS_name[which(spec.list$id %in% ITIS_data$id)] <- 
      ITIS_data$scientificName[
        match(spec.list$id[which(spec.list$id %in% ITIS_data$id)],ITIS_data$id)]
    # Build ITIS author column
    spec.list$ITIS_author <- "No Results"
    # Append matched values
    spec.list$ITIS_author[which(spec.list$id %in% ITIS_data$id)] <- 
      ITIS_data$author[match(spec.list$id[which(spec.list$id %in% ITIS_data$id)],ITIS_data$id)]
  }
  if (exists("NS_data")){
    # Build NS accepted column
    spec.list$NS_accepted <- "No Results"
    # Append matched values
    spec.list$NS_taxonomy[which(spec.list$id %in% NS_taxonomy$id)] <- 
      NS_taxonomy$match_type[match(spec.list$id[
        which(spec.list$id %in% NS_taxonomy$id)],NS_taxonomy$id)]
    # Build NS accepted name column
    spec.list$NS_name <- "No Results"
    # Append matched values
    spec.list$NS_name[which(spec.list$id %in% NS_taxonomy$id)] <- 
      NS_taxonomy$ns_binom[match(spec.list$id[
        which(spec.list$id %in% NS_taxonomy$id)],NS_taxonomy$id)]
    # Build NS author column
    spec.list$NS_author <- "No Results"
    # Append matched values
    # spec.list$NS_author[which(spec.list$id %in% NS_taxonomy$id)] <- 
    #   NS_taxonomy$ns_authority[match(spec.list$id[
    #     which(spec.list$id %in% NS_taxonomy$id)],NS_taxonomy$id)]
  }
  if (exists("VC_data")){
    # Build VC accepted column
    spec.list$VC_accepted <- "No Results"
    # Append matched values
    spec.list$VC_accepted[which(spec.list$id %in% VC_data$id)] <- 
      unlist(VC_data$vc_match_type[match(spec.list$id[
        which(spec.list$id %in% VC_data$id)],VC_data$id)])
    # Build VC accepted name column
    spec.list$VC_name <- "No Results"
    # Append matched values
    spec.list$VC_name[which(spec.list$id %in% VC_data$id)] <- 
      unlist(VC_data$vc_name[match(spec.list$id[
        which(spec.list$id %in% VC_data$id)],VC_data$id)])
  }
  if (exists("POW_data")){
  # Build POW accepted column
  spec.list$POW_accepted <- "No Results"
  # Append matched values
  spec.list$POW_accepted[which(spec.list$id %in% POW_data$id)] <- 
    POW_data$match_type[match(spec.list$id[
      which(spec.list$id %in% POW_data$id)],POW_data$id)]
  # Build POW accepted name column
  spec.list$POW_name <- "No Results"
  # Append matched values
  spec.list$POW_name[which(spec.list$id %in% POW_data$id)] <- 
    POW_data$name[match(spec.list$id[
      which(spec.list$id %in% POW_data$id)],POW_data$id)]
  # Build POW author column
  spec.list$POW_author <- "No Results"
  # Append matched values
  spec.list$POW_author[which(spec.list$id %in% POW_data$id)] <- 
    POW_data$author[match(spec.list$id[
      which(spec.list$id %in% POW_data$id)],POW_data$id)]
  }
  if (exists("fna_tax_check_final")){
    # Build FNA accepted column
    spec.list$FNA_accepted <- "No Results"
    # Append matched values
    spec.list$FNA_accepted[which(spec.list$id %in% fna_tax_check_final$id)] <- 
      fna_tax_check_final$match_type
    # Build FNA accepted name column
    spec.list$FNA_name <- "No Results"
    # Append matched values
    spec.list$FNA_name[which(spec.list$id %in% fna_tax_check_final$id)] <- 
      fna_tax_check_final$accepted_match
    # Unlist FNA_name
    spec.list$FNA_name <- unlist(spec.list$FNA_name)
    # Build FNA author column
    spec.list$FNA_author <- "No Results"
    # Append matched values
    spec.list$FNA_author[which(spec.list$id %in% fna_tax_check_final$id)] <- 
      fna_tax_check_final$fna_author
    # Unlist FNA_author
    spec.list$FNA_author <- unlist(spec.list$FNA_author)
  }
  # Set as global variable
  spec.list <<- spec.list
}

# Parameters: countries_table (x)
# Returns: countries_table
# Throws: none
# Purpose: Recodes countries table codes from numeric values to text accepted
#          by SIS Connect.

countries_recode <- function (x){
  # Build temp table
  countries_temp <- x
  # Build temp origin table
  origin_codes <- coding_key$origin_text
  names(origin_codes) <- coding_key$origin_code
  # Recode ORIGIN field
  countries_temp$ORIGIN <- unname(origin_codes[countries_temp$ORIGIN])
  # Build temp presence table
  presence_codes <- coding_key$presence_text
  names(presence_codes) <- coding_key$presence_code
  # Recode PRESENCE field
  countries_temp$PRESENCE <- unname(presence_codes[countries_temp$PRESENCE])
  # Build temp origin table
  seasonality_codes <- coding_key$seasonality_text
  names(seasonality_codes) <- coding_key$seasonality_code
  # Recode ORIGIN field
  countries_temp$SEASONALITY <- 
    unname(seasonality_codes[countries_temp$SEASONALITY])
  return(countries_temp)
}

SIS_table_generator_main()

#### Pseudocode below this line ####

# Build main function which compiles distribution narrative and adds in-text citations
# Add citation drops for POWO, VASCAN, and NS, and GBIF in relevant dependent scripts
# Trigger population, threats, conservation, narratives via EOO min value in spec.list
# Use same trigger to populate allfields table toggles
# allfields table: add aoo_min, eoo range, default text, 
# Remove references table columns which are not relevant and export
# Export Countries table
# Export credits table
# Export taxonomy summary
