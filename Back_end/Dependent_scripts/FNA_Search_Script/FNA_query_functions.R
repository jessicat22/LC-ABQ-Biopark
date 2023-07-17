#### FNA Taxonomy Check ####
## Version 3.1
# Started: 2 April 2021
# Last worked on: 10 July 2021
# Author: Clay Meredith
# File: Back_end/Dependent_scripts/FNA_Search_Script/FNA_query_functions.R
# Description: Construct queries for API search of Flora of North America Online.
#              Results are compiled into a single table (fna_tax_check_final)

# Load source functions
source("Back_end/Dependent_scripts/FNA_Search_Script/src/query.R")

# Load plyr package
library(plyr)

#### Compiled Search Function ####
fna_search_main <- function (){
  print("Executing Flora of North America search.")
  
  # Build query list and output table
  fna_data_draft <- fna_query_build(spec.list$id, spec.list$Species)
  
  fna_data_draft %<>%
    # Search for species level FNA matches
    fna_sp_lvl_check() %>%
    # Reorganize FNA data at species level
    fna_sp_lvl_reorg()
  
  if(!all(fna_data_draft$match_type=="No Match")){
    # Determine number of subtaxa for each taxon
    fna_data_draft <- count_fna_subtaxa(fna_data_draft)
    subtaxa_flag <- !all(fna_data_draft$num_subtaxa == 0)
    #### FNA Subtaxon search ####
    # Retrieve subtaxa
    fna_subtaxa_results <- fna_subtaxa_retrieve(fna_data_draft)
    # Merge subtaxa and reorganize
    fna_data_w_subtaxa <- fna_subtaxa_merge(fna_data_draft, fna_subtaxa_results)
    #### FNA Data retrieval ####
    fna_w_cons_flag <<- fna_data_w_subtaxa %>%
      fna_author_retrieve() %>%
      fna_volume_retrieve() %>%
      fna_habitat_retrieve() %>%
      fna_elevation_retrieve() %>%
      fna_conservation_retrieve()
    # Aggregate subtaxa elevation values and fill at species level
    fna_elev <- fna_elevation_clean(fna_w_cons_flag)
    fna_data_w_elev <- merge_fna_elevation(fna_elev, fna_w_cons_flag)
    # Merge habitat narratives for subtaxa
    if ("SUBTAXON" %in% fna_data_w_elev$entered_name)
    {fna_habitat_merge(fna_data_w_elev)}
    # Elevate conservation flags
    fna_data_draft <- fna_cons_elevate(fna_data_w_elev)
    
    # Replace "NULL" values in taxonomic data
    fna_data_draft <- as.matrix(fna_data_draft)
    fna_data_draft[fna_data_draft=="NULL"]<-NA
    fna_data_draft <- as.data.frame(fna_data_draft)
    
    # Generate FNA references and append to references table #
    fna_data_draft <- fna_list_fix(fna_data_draft)
    # Set fna data with subtaxa as global variable
    fna_data_all <<- fna_data_draft
    # Remove subtaxa
    fna_data_draft <- fna_data_draft[which(
      fna_data_draft$entered_name!="SUBTAXON"),]
    # Delete dummy output file
    file.remove("Outputs/output_file_name.csv")
    #### FNA Citations ####
    fna_citation_build(fna_data_draft)
    # Unnest data #
    fna_data_draft <- unnest(fna_data_draft, cols = names(fna_data_draft))
    # Generate in-text citations
    fna_tax_check_final <<- fna_in_text(fna_data_draft)
    print("FNA functions complete.")
  } else {
    # Delete dummy output file
    file.remove("Outputs/output_file_name.csv")
    print("No matches found in Flora of North America")}
}

#### Function List ####

fna_prop_query_catch <- function (x,y) {
  # Parameters: fna query table built by fna_query_build
  # Returns: query table for submission to fna
  # Throws: none
  # Purpose: Builds structured table for submission to query builder
  return(tryCatch(ask_query_titles_properties(x,y), error=function(e) NA))
}

# Deprecated?
fna_title_query_catch <- function (x,y) {
  return(tryCatch(ask_query_titles_properties(x,y), error=function(e) NA))
}

fna_query_build <- function (x,y) {
  # Parameters: species ids (list), species binomials (list)
  # Returns: results from fna query
  # Throws: none
  # Purpose: Submits queries to fna
  data.frame(id=x,
             query1 = paste("[[Taxon name::",y,"]]"),
             query2 = paste("[[Basionyms::",y,"]]|?Taxon name"),
             query3 = paste("[[Synonyms::",y,"]]|?Taxon name"),
             entered_name=y)
}

# search FNA for exact matches
fna_sp_lvl_check <- function(x){
  # Parameters: fna data table draft
  # Returns: fna data table draft appended with fna taxonomy results
  # Throws: none
  # Purpose: Searches fna taxonomy. Marks exact matches. Searches inexact
  # matches for synonym matches. Appends synonym matches. Labels match types.
  fna_matches_temp <- x
  fna_matches_temp$accepted_match <- 
    unname(mapply(ask_query_titles, 
                  fna_matches_temp$query1, 
                  "Outputs/output_file_name.csv"))
  # Reformat results
  fna_matches_temp$accepted_match[which(
    fna_matches_temp$accepted_match=="NULL")] <- NA
  fna_matches_temp$basionym_match <- NA
  fna_matches_temp$synonym_match <- NA
  
  # Skip synonym search if all species accepted
  if (!all(!is.na(fna_matches_temp$accepted_match))){
    # search FNA for basionym matches
    fna_matches_temp$basionym_match[which(is.na(
      fna_matches_temp$accepted_match))] <-
      unname(mapply(ask_query_titles,
                    fna_matches_temp$query2[which(is.na(
                      fna_matches_temp$accepted_match))],
                    "Outputs/output_file_name.csv"))
    
    # search FNA for synonym matches
    fna_matches_temp$synonym_match[which(is.na(
      fna_matches_temp$accepted_match))] <-
      unname(mapply(ask_query_titles,
                    fna_matches_temp$query3[which(is.na(
                      fna_matches_temp$accepted_match))],
                    "Outputs/output_file_name.csv"))
  }
  
  # Reformat results (replaces "NULL" with NA in results)
  fna_matches_temp$basionym_match[which(
    fna_matches_temp$basionym_match=="NULL")] <- NA
  fna_matches_temp$synonym_match[which(
    fna_matches_temp$synonym_match=="NULL")] <- NA
  
  # Transfer basionym match to accepted match if no accepted match exists
  fna_matches_temp$accepted_match[which(is.na(
    fna_matches_temp$accepted_match))] <-
    fna_matches_temp$basionym_match[which(is.na(
      fna_matches_temp$accepted_match))]
  
  # Transfer synonym match to accepted match if no accepted match exists
  fna_matches_temp$accepted_match[which(is.na(
    fna_matches_temp$accepted_match))] <-
    fna_matches_temp$synonym_match[which(is.na(
      fna_matches_temp$accepted_match))]

  # Unlist results
  # Method selects first element in each list as FNA reports species
  # level first. This may produce unpredictable results when infrataxa are run
  fna_matches_temp$accepted_match <- lapply(
    fna_matches_temp$accepted_match, '[[',1)
  print("Searched for FNA exact matches.")
  # Return table
  return(fna_matches_temp)
}

# Reorganize FNA results
fna_sp_lvl_reorg <- function(x){
  # Parameters: fna data table draft
  # Returns: fna data table draft with species result levels appended
  # Throws: none
  # Purpose: 
  fna_accepted_flags <- x
  # Compare result with input
  fna_accepted_flags$exact_match <- 
    fna_accepted_flags$entered_name==fna_accepted_flags$accepted_match
  fna_accepted_flags$exact_match[which(is.na(
    fna_accepted_flags$exact_match))] <- 
    FALSE
  
  # Specify match type
  fna_accepted_flags$match_type <- NA
  fna_accepted_flags$match_type[which(
    fna_accepted_flags$exact_match==TRUE)] <- 
    "Exact Match"
  fna_accepted_flags$match_type[which(!is.na(
    fna_accepted_flags$synonym_match))] <- 
    "Synonym Match"
  fna_accepted_flags$match_type[which(!is.na(
    fna_accepted_flags$basionym_match))] <- 
    "Basionym Match"
  fna_accepted_flags$match_type[which(is.na(
    fna_accepted_flags$match_type))] <- 
    "No Match"
  fna_accepted_flags <- 
    fna_accepted_flags[ , -which(names(fna_accepted_flags) %in% 
                                   c("query1","query2", "query3",
                                     "basionym_match", "synonym_match",
                                     "exact_match"))]
  print("Initial results reorganized.")
  # Return results
  return(fna_accepted_flags)
  }

# Determine number of subtaxa
count_fna_subtaxa <- function(x){
  # Parameters: fna_data_draft (table)
  # Returns: fna_subtaxa_count (table)
  # Throws: none
  # Purpose: Queries fna to determine the number of subtaxa associated with each
  # input taxon
  
  # Assign data variables
  fna_subtaxa_count <- x
  # Search for number of subtaxa (circumvents some issues with query function)
  # Query function will not return vectors of length >1
  # This prevents searches for attributes where number of subtaxa > 0
  # Build queries column for number of subtaxa
  fna_subtaxa_count$parent_query <- paste("[[Taxon parent::",
                                      fna_subtaxa_count$accepted_match,"]]")
  # Run query
  fna_num_subtaxa <- mapply(ask_query_titles,fna_subtaxa_count$parent_query[
    which(!is.na(fna_subtaxa_count$accepted_match))],
    "Outputs/output_file_name.csv")
  
  # Caclulate number of subtaxa for each taxon
  fna_subtaxa_count$num_subtaxa <- 0
  fna_subtaxa_count$num_subtaxa[which(!is.na(fna_subtaxa_count$accepted_match))] <-
    if(!is(fna_num_subtaxa, "matrix")){
      unlist(lapply(fna_num_subtaxa,length))
    } else {
      nrow(fna_num_subtaxa)
    }
  print("FNA subtaxa count performed.")
  # Return results
  return(fna_subtaxa_count)
}

# Search for subtaxa                     
fna_subtaxa_retrieve <- function (x){
    if(all(x$num_subtaxa == 0)){##all zeros
      # Fill data if no subtaxa
    print("subtaxa not found");
    return(x);
  }
  else{
    # Subset only species with multiple subtaxa
    fna_subtaxa <- x[which(x$num_subtaxa>0),]
    # Construct query
    fna_subtaxa$query <- paste("[[Taxon parent::",fna_subtaxa$accepted_match,
                               "]]",sep="")
    # Search for all subtaxa
    fna_subtaxa_results <- mapply(ask_query_titles,fna_subtaxa$query,
                           "Outputs/output_file_name.csv")
    
    # Append species ID numbers to results
    if (length(fna_subtaxa_results) > 1){
      print("multiple species with subtaxa")
      names(fna_subtaxa_results) <- x$id[which(
        x$num_subtaxa>0)]
      # Convert to data frame
      fna_subtaxa_results <- data.frame(stack(fna_subtaxa_results))
    } 
    else {    
      print("one species with subtaxa")
      # Reformat results into single data frame
      fna_subtaxa_results <- data.frame(fna_subtaxa_results)
      # Rename columns with species ID
      names(fna_subtaxa_results) <- x$id[which(
        x$num_subtaxa>0)]
      # Convert from wide to long
      fna_subtaxa_results <- data.frame(stack(as.data.frame(fna_subtaxa_results)),stringsAsFactors=FALSE)
      # Convert columns to character and numeric types
      fna_subtaxa_results$id <- as.numeric(as.character(fna_subtaxa_results$id))
      # Deprecated function not remembered
      # fna_subtaxa_results <- data.frame(Values = fna_subtaxa_results,
      #                            Ind = x$id[which(
      #                              x$num_subtaxa>0)])
    }
    print("FNA subtaxa data retrieved.")
    return(fna_subtaxa_results)
  }
}

# Merge species and subtaxa into single table
fna_subtaxa_merge <- function(x,y){
  # Parameters: x = fna_data_draft, y = fna_subtaxa_count
  # Returns: fna_data_draft with appended subtaxon data.
  # Also modifies fna_subtaxa_results.
  # Throws: none
  # Purpose: Merges data tables for fna_data_draft and fna_subtaxon_count
  fna_merged_subtaxa <- x
  fna_subtaxa_count <- y
  # Drop parent query
  fna_merged_subtaxa <- fna_merged_subtaxa[,
                                  -which(names(
                                  fna_merged_subtaxa) == "parent_query")]
  
  if(all(fna_merged_subtaxa$num_subtaxa == 0)){##all zeros
    print("subtaxa not found");
    return(x);
  }
  else{
    # Rename columns and merge
    names(fna_merged_subtaxa) <- c("id","entered_name",
                                   "accepted_match","match_type","num_subtaxa")
    
    names(fna_subtaxa_count) <- c("accepted_match","id")
    fna_subtaxa_count$entered_name <- "SUBTAXON"
    
    # Convert species id to numeric
    # Reclass columns before merge
    fna_subtaxa_count$id <- as.numeric(as.character(fna_subtaxa_count$id))
    fna_merged_subtaxa$accepted_match <- unlist(fna_merged_subtaxa$accepted_match)

    # Bind subtaxa to taxa
    fna_merged_subtaxa <- rbind.fill(fna_merged_subtaxa,fna_subtaxa_count)
    # # Add temp index column
    # Define number of subtaxa as 0 for subtaxa
    fna_merged_subtaxa$num_subtaxa[which(
      is.na(fna_merged_subtaxa$num_subtaxa))] <- 0
    print("FNA subtaxa merged.")
    return(fna_merged_subtaxa)
  }
}

# Retrieve Authority for accepted taxa
fna_author_retrieve <- function(x){
  fna_author_added <- x
  # Build queries column for authority
  author_query <- paste("[[Taxon name::",
                        x$accepted_match,
                        "]]|?Authority")
  # Run query for authority
  fna_auth <- mapply(fna_prop_query_catch,
                      author_query,
                      "Outputs/output_file_name.csv")
  # Add column for FNA authority
  fna_author_added$fna_author <- NA
  # append to fna_tax_check  
    # Handler for results as lists (occurs when there are no subtaxa)
  if(all(class(fna_auth) == "list")){
    fna_author_added[which(!is.na(fna_auth)),]$fna_author <- 
      lapply(fna_auth[which(!is.na(fna_auth))], function(y){
        y$Authority
      })
    } else {
      fna_author_added$fna_author <- fna_auth[1,]
    }
  print("FNA Authors retrieved.")
  return(fna_author_added)
}

# Retrieve volume for each species
fna_volume_retrieve <- function(x){
  fna_volume_added <- x
  # Build queries for volume
  volume_query <- paste("[[Taxon name::",
                        fna_volume_added$accepted_match,"]]|?Volume")
  # Run query for volume
  fna_vol <- mapply(fna_prop_query_catch,volume_query,
                    "Outputs/output_file_name.csv")
  # Add column for volume
  fna_volume_added$fna_vol <- NA

    # append to fna_tax_check  
    # Handler for results as lists (occurs when there are no subtaxa)
    if(all(class(fna_vol) == "list")){
      fna_volume_added[which(!is.na(fna_vol)),]$fna_vol <- 
        lapply(fna_vol[which(!is.na(fna_vol))], function(y){
          y$Volume
        })
    } else {
      fna_volume_added$fna_vol <- fna_vol[1,]
    }
  print("FNA volumes retrieved.")
    return(fna_volume_added)
  }
  
# Retrieve habitat for each species
fna_habitat_retrieve <- function(x){
  # Build queries for habitat
  habitat_query <- paste("[[Taxon name::",
                         x$accepted_match,"]]|?Habitat")
  habitat_text <- list()
  # Run query for habitat works
  fna_habitat <- mapply(fna_prop_query_catch,habitat_query,
                         "Outputs/output_file_name.csv")
  if (!is(fna_habitat, "matrix")){
    fna_habitat <- as.list(fna_habitat)
    for (i in 1:length(t(fna_habitat))) {
      habitat_text[i] <-list(t(fna_habitat)[[i]][[1]])
    }
  }else{
    habitat_text <- data.frame(as.list(fna_habitat[1,]))
  }
  fna_habitat_data <- x
  fna_habitat_data$habitat <- NA
  fna_habitat_data$habitat <- unname(habitat_text)
  print("FNA habitat info retrieved.")
  return(fna_habitat_data)
}

# Retrieve elevation data
fna_elevation_retrieve <- function(x){
  fna_elevation_data <- x
  # Build queries column for elevation
  elev_query <- paste("[[Taxon_name::",fna_elevation_data$accepted_match,
    "]]|?Elevation")
  # Run query
  fna_elev <<- mapply(fna_prop_query_catch,elev_query,
                      "Outputs/output_file_name.csv")
  
  fna_elev_simple <- c()
  if (!is(fna_elev, "matrix")){
    fna_elev <- as.list(fna_elev)
    fna_elev_simple <- lapply(fna_elev, function (x){t(x)[[1]]})
  }else{
    fna_elev_simple <- fna_elev[1,][]
  }
  fna_elevation_data$elevation_raw <- NA
  fna_elevation_data$elevation_raw <-
    as.list(unlist(unname(fna_elev_simple)))
  print("FNA elevation data retrived.")
  return(fna_elevation_data)
}

# Retrieve conservation concern flag
fna_conservation_retrieve <- function(x){
  fna_cons_flag_temp <- x
  # Build queries column to check for species of conservation concern
  fna_cons_query <- paste("[[Taxon name::",fna_cons_flag_temp$accepted_match,
    "]][[Special status::Conservation concern]]|?Taxon name",sep="")
  
  # Run query
  fna_cons_status <- mapply(fna_prop_query_catch,fna_cons_query,
                            "Outputs/output_file_name.csv")
  
  fna_cons_flag <- c()
  for(i in 1:length(fna_cons_status)){
    fna_cons_flag[i] <- fna_cons_status[[i]][[1]]
  }
  # Append to table
  fna_cons_flag_temp$fna_cons_flag <- fna_cons_flag
  print("FNA conservation flags retrieved.")
  return(fna_cons_flag_temp)
}

# Reformat elevation data
fna_elevation_clean <- function(x){
  fna_elevation_isolated <- x
    if (!all(is.na(fna_elevation_isolated$elevation_raw))){
  # Reformat and merge subtaxa for final output #
  # Isolate lower bound value
  elev_min <- 
    unlist(lapply(strsplit(as.character(fna_elevation_isolated$elevation_raw),
                           "-|\u2013"), `[[`, 1))
  # Isolate upper bound values
  elev_upper <- unlist(lapply(strsplit(as.character(
    fna_elevation_isolated$elevation_raw),
    "-|\u2013"),`[`, 2))
  
  elev_max <- unlist(lapply(strsplit(
    as.character(fna_elevation_isolated$elevation_raw),
    "-|\u2013"),`[`, 3))
  # Remove extraneous characters
  fna_elev_data <- data.frame(id = fna_elevation_isolated$id,
                              elev_min = gsub("[^0-9\u2013]", "", elev_min),
                              elev_upper = gsub("[^0-9\u2013]", "", elev_upper),
                              elev_max = gsub("[^0-9\u2013]", "", elev_max))
  
  # Replace elev_upper with elev_max if it exists
  fna_elev_data$elev_upper[which(!is.na(fna_elev_data$elev_max))] <-
    fna_elev_data$elev_max[which(!is.na(fna_elev_data$elev_max))]

    # Find total elevation bounds for species with subtaxa
    elev_min <- aggregate(elev_min~id, fna_elev_data,min,na.rm=TRUE)
    elev_max <- aggregate(elev_upper~id, fna_elev_data,max,na.rm=TRUE)
    # Merge elevation data
    fna_elev_data <- merge(elev_min, elev_max)
    print("FNA elevation reformat complete.")
  } else {
    fna_elev_data <- data.frame(
      id = x$id[which(x$match_type != "No Match")],
      elev_min = NA,
      elev_upper = NA
    )
    
    print("FNA provides no elevation data")
  }
  return(fna_elev_data)
}

# Merge fna elevation with fna taxonomy data
merge_fna_elevation <- function (x,y) {
  fna_elev_merged <-
    merge(y,x,by = "id",all.x=TRUE)
  # Remove elevation bounds for subtaxa
  fna_elev_merged$elev_min[which(
    fna_elev_merged$entered_name=="SUBTAXON")] <- NA
  fna_elev_merged$elev_upper[which(
    fna_elev_merged$entered_name=="SUBTAXON")] <- NA
  print("FNA elevation merged.")
  return(fna_elev_merged)
}

# Generate in-text citation
fna_in_text <- function(x){
  fna_in_text <- x
  # Load FNA citation key
  fna_citations <- data.frame(read.csv(
    "Back_end/Dependent_scripts/FNA_Search_Script/fna_key.csv"), 
    stringsAsFactors = FALSE)
  fna_in_text$in_text <- NA
  fna_in_text$in_text <- fna_citations$in_text[match(
    fna_in_text$fna_vol,fna_citations$volume_result)]
  print("FNA in-text citations written.")
  return(fna_in_text)
}

# Merge habitat narratives for subtaxa
fna_habitat_merge <- function(x){
  fna_habitat_working <- x
  fna_habitat_working$habitat <- unlist(fna_habitat_working$habitat)
  # Concatenate habitat narratives
  if (!all(is.na(fna_habitat_working$habitat[which(
    fna_habitat_working$entered_name == "SUBTAXON")]))){
  
  fna_habitat_concatenated <-
    aggregate(habitat ~ id,
              fna_habitat_working[which(
                fna_habitat_working$entered_name == "SUBTAXON"), ],
              paste, collapse = ", ")
  # Append to fna_tax_check table
  fna_habitat_working$habitat[which(
    fna_habitat_working$entered_name!="SUBTAXON")][match(
    fna_habitat_concatenated$id,
    fna_habitat_working$id[which(
      fna_habitat_working$entered_name!="SUBTAXON")]
  )] <- fna_habitat_concatenated$habitat
  }
  print("FNA habitat data reformatted.")
  return(fna_habitat_working)
}

# Elevate Conservation Concern flags for subtaxa to the species level
# This is done to highlight the need for human oversight
fna_cons_elevate <- function(x){
  # assign input variable
  fna_cons_flag_working <- x
  fna_cons_flag_working$fna_cons_flag[which(fna_cons_flag_working$id %in% 
                        fna_cons_flag_working$id[which(!is.na(
                        fna_cons_flag_working$fna_cons_flag))])] <- "FLAGGED"
  print("FNA conservation flag elevated to species level.")
  return(fna_cons_flag_working)
}

# Generate FNA citations
fna_citation_build <- function(x){
  fna_data_citations <- x
  # references1 <- ref.key[0,]
  fna_hits <- fna_data_citations$id[which(
    !is.na(fna_data_citations$fna_vol))]
  # Cross-reference fna results with citation list
  fna_references_used <- 
    ref.key[match(fna_data_citations$fna_vol[which(!is.na(
      fna_data_citations$fna_vol))], ref.key$keywords),]
  # Append taxon id
  fna_references_used$internal_taxon_id <- fna_hits
  # Remove duplicates
  if (!is.null(fna_references_used)){
    references <<- rbind(references, 
                         unique(fna_references_used))
    print("FNA citations built.")
  }
}

# Purpose: fixes erroroneous data type in fna_data_draft. Removes list elements.
fna_list_fix <- function (x){
  fna_list_in_process <- x
  # Unlist author field
  fna_list_in_process$fna_author <- lapply(fna_list_in_process$fna_author,unlist)
  fna_list_in_process$fna_author <- lapply(fna_list_in_process$fna_author,unname)
  fna_list_in_process$fna_author <- lapply(
    fna_list_in_process$fna_author,function(y){y[1]})
  # Unlist fna_vol
  fna_list_in_process$fna_author <- lapply(fna_list_in_process$fna_author, 
                                           function(z){
    z <- z[[1]][[1]]
    unlist(z)
  }
  )
  # Unlist volume field
  fna_list_in_process$fna_vol <- lapply(fna_list_in_process$fna_vol,unlist)
  fna_list_in_process$fna_vol <- lapply(fna_list_in_process$fna_vol,unname)
  fna_list_in_process$fna_vol <- lapply(fna_list_in_process$fna_vol,function(a){a[1]})
  return(fna_list_in_process)
  }

#### Execute Search ####
fna_search_main()
