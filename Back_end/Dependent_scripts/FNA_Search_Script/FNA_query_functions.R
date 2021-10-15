#### FNA Taxonomy Check ####
## Version 3.1
# Started: 2 April 2021
# Last worked on: 1 July 2021
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
  # Build query list and output table
  fna_tax_check <<- fna_query_build(spec.list$id, spec.list$Species)
  
  # Search for species level FNA matches
  fna_sp_lvl_check()
  # Reorganize FNA data at species level
  fna_sp_lvl_reorg()
  
  if(!all(fna_tax_check$match_type=="No Match")){
  # Determine number of subtaxa for each taxon
  fna_subtaxa_number()
  
  #### FNA Subtaxon search ####
  # Retrieve subtaxa
  fna_subtaxa_retrieve()
  # Merge subtaxa and reorganize
  fna_subtaxa_merge()
  
  #### FNA Data retrieval ####
  # Retrieve author for all taxa
  fna_author_retrieve()
  # Retrieve volume for all taxa
  fna_volume_retrieve()
  # Retrieve habitat for each species
  fna_habitat_retrieve()
  # Retrieve elevation data
  fna_elevation_retrieve()
  # Retrieve conservation flags
  fna_conservation_retrieve()
  # Aggregate subtaxa elevation values and fill at species level
  fna_elevation_clean()
  merge_fna_elevation()
  # Merge habitat narratives for subtaxa
  if ("SUBTAXON" %in% fna_tax_check$entered_name)
  {fna_habitat_merge()}
  # Elevate conservation flags
  fna_cons_elevate()
  
  # Replace "NULL" values in taxonomic data
  fna_tax_check[fna_tax_check=="NULL"]<<-NA
  
  # Generate FNA references and append to references table #
  FNA_list_fix()
  
  # Remove subtaxa
  fna_tax_check_final <<- fna_tax_check[which(
    fna_tax_check$entered_name!="SUBTAXON"),]
  
  # Delete dummy output file
  file.remove("Outputs/output_file_name.csv")
  
  #### FNA Citations ####
  fna_citation_build()
  # Generate in-text citations
  fna_in_text()
  # Remove working variables and tables
  rm(fna_tax_check,pos = ".GlobalEnv")
  rm(fna_elev,pos = ".GlobalEnv")
  rm(fna_habitat,pos = ".GlobalEnv")
  rm(fna_sub_res,pos = ".GlobalEnv")
  } else {
    # Delete dummy output file
    file.remove("Outputs/output_file_name.csv")
    print("No matches found in Flora of North America")}
}


# Parameters: spec.list (table)
# Returns: NS_taxonomy (table), NS_tax_raw (list of tables)
# Throws: none
# Purpose:
fna_prop_query_catch <- function (x,y) {
  return(tryCatch(ask_query_titles_properties(x,y), error=function(e) NA))
}
fna_title_query_catch <- function (x,y) {
  return(tryCatch(ask_query_titles_properties(x,y), error=function(e) NA))
}

fna_query_build <- function (x,y) {
  data.frame(id=x,
             query1 = paste("[[Taxon name::",y,"]]"),
             query2 = paste("[[Basionyms::",y,"]]|?Taxon name"),
             query3 = paste("[[Synonyms::",y,"]]|?Taxon name"),
             entered_name=y)
}

# search FNA for exact matches
fna_sp_lvl_check <- function(){
  fna_tax_check$accepted_match <<- 
    unname(mapply(ask_query_titles, 
                  fna_tax_check$query1, 
                  "Outputs/output_file_name.csv"))
  # Reformat results
  fna_tax_check$accepted_match[which(
    fna_tax_check$accepted_match=="NULL")] <<- NA
  fna_tax_check$basionym_match <<- NA
  fna_tax_check$synonym_match <<- NA
  
  # Skip synonym search if all species accepted
  if (!all(!is.na(fna_tax_check$accepted_match))){
    # search FNA for basionym matches
    fna_tax_check$basionym_match[which(is.na(
      fna_tax_check$accepted_match))] <<-
      unname(mapply(ask_query_titles,
                    fna_tax_check$query2[which(is.na(
                      fna_tax_check$accepted_match))],
                    "Outputs/output_file_name.csv"))
    
    # search FNA for synonym matches
    fna_tax_check$synonym_match[which(is.na(
      fna_tax_check$accepted_match))] <<-
      unname(mapply(ask_query_titles,
                    fna_tax_check$query3[which(is.na(
                      fna_tax_check$accepted_match))],
                    "Outputs/output_file_name.csv"))
  }
  
  # Reformat results (replaces "NULL" with NA in results)
  fna_tax_check$basionym_match[which(
    fna_tax_check$basionym_match=="NULL")] <<- NA
  fna_tax_check$synonym_match[which(
    fna_tax_check$synonym_match=="NULL")] <<- NA
  
  # Transfer basionym match to accepted match if no accepted match exists
  fna_tax_check$accepted_match[which(is.na(
    fna_tax_check$accepted_match))] <<-
    fna_tax_check$basionym_match[which(is.na(
      fna_tax_check$accepted_match))]
  
  # Transfer synonym match to accepted match if no accepted match exists
  fna_tax_check$accepted_match[which(is.na(
    fna_tax_check$accepted_match))] <<-
    fna_tax_check$synonym_match[which(is.na(
      fna_tax_check$accepted_match))]

  # Unlist results
  # Method selects first element in each list as FNA reports species
  # level first. This may produce unpredictable results when infrataxa are run
  fna_tax_check$accepted_match <<- lapply(
    fna_tax_check$accepted_match, '[[',1)
  
}

# Reorganize FNA results
fna_sp_lvl_reorg <- function(){
  # Compare result with input
  fna_tax_check$exact_match <<- 
    fna_tax_check$entered_name==fna_tax_check$accepted_match
  fna_tax_check$exact_match[which(is.na(
    fna_tax_check$exact_match))] <<- 
    FALSE
  
  # Specify match type
  fna_tax_check$match_type <<- NA
  fna_tax_check$match_type[which(
    fna_tax_check$exact_match==TRUE)] <<- 
    "Exact Match"
  fna_tax_check$match_type[which(!is.na(
    fna_tax_check$synonym_match))] <<- 
    "Synonym Match"
  fna_tax_check$match_type[which(!is.na(
    fna_tax_check$basionym_match))] <<- 
    "Basionym Match"
  fna_tax_check$match_type[which(is.na(
    fna_tax_check$match_type))] <<- 
    "No Match"
  fna_tax_check <<- fna_tax_check[ , -which(names(fna_tax_check) %in% 
                                              c("query1","query2","query3","basionym_match",
                                                "synonym_match","exact_match"))]
}

# Determine number of subtaxa
fna_subtaxa_number <- function(){
  
  # Search for number of subtaxa (circumvents some issues with query function)
  # Query function will not return vectors of length >1
  # This prevents searches for attributes where number of subtaxa > 0
  # Build queries column for number of subtaxa
  fna_tax_check$parent_query <- paste("[[Taxon parent::",
                                      fna_tax_check$accepted_match,"]]")
  # Run query
  fna_num_subtaxa <- mapply(ask_query_titles,fna_tax_check$parent_query[
    which(!is.na(fna_tax_check$accepted_match))],
    "Outputs/output_file_name.csv")
  
  # Caclulate number of subtaxa for each taxon
  fna_tax_check$num_subtaxa <<- 0
  fna_tax_check$num_subtaxa[which(!is.na(fna_tax_check$accepted_match))] <<-
    unlist(lapply(fna_num_subtaxa,length))
  
}

# Search for subtaxa                     
fna_subtaxa_retrieve <- function (){
  
  if(all(fna_tax_check$num_subtaxa == 0)){##all zeros
    print("subtaxa not found");
    return;
  }
  else{
    # Subset only species with multiple subtaxa
    fna_subtaxa <- fna_tax_check[which( fna_tax_check$num_subtaxa>0),]
    # Construct query
    fna_subtaxa$query <- paste("[[Taxon parent::",fna_subtaxa$accepted_match,
                               "]]",sep="")
    # Search for all subtaxa
    fna_sub_res <<- mapply(ask_query_titles,fna_subtaxa$query,
                           "Outputs/output_file_name.csv")
    
    # Append species ID numbers to results
    # If one species returned, fna_sub_res will be a matrix,
    # if >1 species returned, fna_sub_res will be a list
    if (class(fna_sub_res)[1] == "list"){
      print("multiple species with subtaxa")
      names(fna_sub_res) <- fna_tax_check$id[which(
        fna_tax_check$num_subtaxa>0)]
      # Convert to data frame
      fna_sub_res <<- data.frame(stack(fna_sub_res))
    } 
    else {    
      print("one species with subtaxa")
      # Reformat results into single data frame
      fna_sub_res <- data.frame(fna_sub_res)
      # Rename columns with species ID
      names(fna_sub_res) <- fna_tax_check$id[which(
        fna_tax_check$num_subtaxa>0)]
      # Convert from wide to long
      fna_sub_res <- data.frame(stack(fna_sub_res),stringsAsFactors=FALSE)
      # Convert columns to character and numeric types
      fna_sub_res$ind <- as.numeric(as.character(fna_sub_res$ind))
      # Convert to global variable
      fna_sub_res <<- fna_sub_res
      # fna_sub_res <- data.frame(Values = fna_sub_res,
      #                            Ind = fna_tax_check$id[which(
      #                              fna_tax_check$num_subtaxa>0)])
    }
  }
}

# Merge species and subtaxa into single table
fna_subtaxa_merge <- function(){
  
  if(all(fna_tax_check$num_subtaxa == 0)){##all zeros
    print("subtaxa not found");
    return;
  }
  else{
    # Rename columns and merge
    names(fna_tax_check) <<- c("id","entered_name",
                               "accepted_match","match_type","num_subtaxa")
    
    names(fna_sub_res) <- c("accepted_match","id")
    fna_sub_res$entered_name <- "SUBTAXON"
    
    # Convert species id to numeric
    # Revisions make this line unnecessary
    # if (length(unique(fna_sub_res$id)) != 1){
    #   fna_sub_res$id <- 
    #     as.numeric(levels(fna_sub_res$id))[fna_sub_res$id]
    # }
    # Bind subtaxa to taxa
    fna_tax_check <<- rbind.fill(fna_tax_check,fna_sub_res)
    
    # Add temp index column
    fna_tax_check$temp_id <<- c(1:nrow(fna_tax_check))
    # Define number of subtaxa as 0 for subtaxa
    fna_tax_check$num_subtaxa[which(is.na(fna_tax_check$num_subtaxa))] <<- 0
    
  }
}

# Retrieve Authority for accepted taxa
fna_author_retrieve <- function(){
  # Build queries column for authority
  ###BUG
  
  author_query <- paste("[[Taxon name::",
                        fna_tax_check$accepted_match,
                        "]]|?Authority")
  
  # Run query for authority
  fna_auth <- mapply(fna_prop_query_catch,
                      author_query,
                      "Outputs/output_file_name.csv")
  
  # append to fna_tax_check
  if(class(fna_auth) != "list"){fna_auth<- as.list(unname(fna_auth[1,]))
  } #else {
    
  #}
  
  # Add column for FNA authority
  fna_tax_check$fna_author <- NA
  # Fill column
  fna_tax_check$fna_author <- as.list(fna_auth)
  # Assign fna_tax_check as global variable
  fna_tax_check <<- fna_tax_check
  
}

# Retrieve volume for each species
fna_volume_retrieve <- function(){
  # Build queries for volume
  volume_query <- paste("[[Taxon name::",fna_tax_check$accepted_match,"]]|?Volume")
  # Run query for volume
  fna_vol <- mapply(fna_prop_query_catch,volume_query,
                    "Outputs/output_file_name.csv")
  # Append to results
  if(class(fna_vol) != "list"){fna_vol<- as.list(unname(fna_vol[1,]))}
  
  # Unlist fna_vol
  fna_vol <- lapply(fna_vol, function(x){
    x <- x[[1]][[1]]
    unlist(x)
  }
  )

  fna_tax_check$fna_vol <<- fna_vol
  
  # # Replace "NULL" values with NA
  # fna_tax_check$fna_vol[which(fna_tax_check$fna_vol=="NULL")] <<-
  #   NA
}

# Retrieve habitat for each species
fna_habitat_retrieve <- function(){
  # Build queries for habitat
  habitat_query <- paste("[[Taxon name::",
                         fna_tax_check$accepted_match,"]]|?Habitat")
  habitat_text <- list()
  # Run query for habitat works
  fna_habitat <<- mapply(fna_prop_query_catch,habitat_query,
                         "Outputs/output_file_name.csv")
  
  if (class(fna_habitat) == "list"){
    for (i in 1:length(t(fna_habitat))) {
      habitat_text[i] <-list(t(fna_habitat)[[i]][[1]])
    }
  }else{
    habitat_text <- fna_habitat[1,]
  }
  fna_tax_check$habitat <- NA
  fna_tax_check$habitat <<- unname(habitat_text)
  
}

# Retrieve elevation data
fna_elevation_retrieve <- function(){
  
  # Build queries column for elevation
  elev_query <- paste("[[Taxon_name::",fna_tax_check$accepted_match,
    "]]|?Elevation")
  # Run query
  fna_elev <<- mapply(fna_prop_query_catch,elev_query,
                      "Outputs/output_file_name.csv")
  
  fna_elev_simple <- c()
  if (class(fna_elev) == "list"){
    fna_elev_simple <- lapply(fna_elev, function (x){t(x)[[1]]})
  }else{
    fna_elev_simple <- fna_elev[1,][]
  }
  
  # for(i in 1:length(fna_elev)){
  #   fna_elev_simple[i] <- fna_elev[[i]][[1]]
  # }
  fna_tax_check$elevation_raw <- NA
  fna_tax_check$elevation_raw <<-
    as.list(unlist(unname(fna_elev_simple)))
}

# Retrieve conservation concern flag
fna_conservation_retrieve <- function(){
  
  # Build queries column to check for species of conservation concern
  fna_cons_query <- paste("[[Taxon name::",fna_tax_check$accepted_match,
    "]][[Special status::Conservation concern]]|?Taxon name",sep="")
  
  # Run query
  fna_cons_status <- mapply(fna_prop_query_catch,fna_cons_query,
                            "Outputs/output_file_name.csv")
  
  fna_cons_flag <- c()
  for(i in 1:length(fna_cons_status)){
    fna_cons_flag[i] <- fna_cons_status[[i]][[1]]
  }
  # Append to table
  fna_tax_check$fna_cons_flag <<- 
    fna_cons_flag
}

# Reformat elevation data
fna_elevation_clean <- function(){
  # Reformat and merge subtaxa for final output #
  # Isolate lower bound value
  elev_min <- 
    unlist(lapply(strsplit(as.character(fna_tax_check$elevation_raw),
                           "-|\u2013"), `[[`, 1))
  # Isolate upper bound values
  elev_upper <- unlist(lapply(strsplit(as.character(fna_tax_check$elevation_raw),
                                       "-|\u2013"),`[`, 2))
  
  elev_max <- unlist(lapply(strsplit(as.character(fna_tax_check$elevation_raw),
                                     "-|\u2013"),`[`, 3))
  # Remove extraneous characters
  fna_elev_data <- data.frame(id = fna_tax_check$id,
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
  fna_elev_data <<- merge(elev_min, elev_max)
  print("elevation reformat complete")
}

# Merge fna elevation with fna taxonomy data
merge_fna_elevation <- function () {
  fna_tax_check <<-
    merge(fna_tax_check,fna_elev_data,by = "id",all.x=TRUE)
  # Remove elevation bounds for subtaxa
  fna_tax_check$elev_min[which(fna_tax_check$entered_name=="SUBTAXON")] <<- NA
  fna_tax_check$elev_upper[which(fna_tax_check$entered_name=="SUBTAXON")] <<- NA
  print("elevation merged")
}

# Generate in-text citation
fna_in_text <- function(){
  # Load FNA citation key
  fna_citations <<- data.frame(read.csv(
    "Back_end/Dependent_scripts/FNA_Search_Script/fna_key.csv"), 
    stringsAsFactors = FALSE)
  fna_tax_check$in_text <- NA
  fna_tax_check$in_text <<- fna_citations$in_text[match(
    fna_tax_check$fna_vol,fna_citations$volume_result)]
  print("in-text citations done")
}

# Merge habitat narratives for subtaxa
fna_habitat_merge <- function(){
  fna_tax_check$habitat <<- as.character(fna_tax_check$habitat)
  # Concatenate habitat narratives
  fna_habitat_concatenated <- aggregate(habitat ~ id, 
                                        fna_tax_check[which(fna_tax_check$entered_name=="SUBTAXON"),],
                                        paste, collapse = ", ")
  # Append to fna_tax_check table
  fna_tax_check$habitat[which(fna_tax_check$entered_name!="SUBTAXON")][match(
    fna_habitat_concatenated$id,
    fna_tax_check$id[which(fna_tax_check$entered_name!="SUBTAXON")]
  )] <<- fna_habitat_concatenated$habitat
  print("habitat done")
}

# Elevate Conservation Concern flags for subtaxa to the species level
# This is done to highlight the need for human oversight
fna_cons_elevate <- function(){
  fna_tax_check$fna_cons_flag[which(fna_tax_check$id %in% 
                                      fna_tax_check$id[which(!is.na(
                                        fna_tax_check$fna_cons_flag))])] <<- "FLAGGED"
  print("conservation flag done")
}

# Generate FNA citations
fna_citation_build <- function(){
  # references1 <- ref.key[0,]
  fna_hits <- fna_tax_check$id[which(
    !is.na(fna_tax_check$fna_vol))]
  # Cross-reference fna results with citation list
  fna_references_used <- 
    ref.key[match(fna_tax_check$fna_vol[which(!is.na(
      fna_tax_check$fna_vol))], ref.key$keywords),]
  # Append taxon id
  fna_references_used$internal_taxon_id <- fna_hits
  # Remove duplicates
  if (!is.null(fna_references_used)){
    references <<- rbind(references, 
                         unique(fna_references_used))
    print("fna citations built")
  }
}

# Purpose: fixes erroroneous data type in fna_tax_check. Removes list elements.
FNA_list_fix <- function (){
  # Unlist author field
  fna_tax_check$fna_author <- lapply(fna_tax_check$fna_author,unlist)
  fna_tax_check$fna_author <- lapply(fna_tax_check$fna_author,unname)
  fna_tax_check$fna_author <- lapply(fna_tax_check$fna_author,function(x){x[1]})
  # Unlist fna_vol
  fna_tax_check$fna_author <- lapply(fna_tax_check$fna_author, function(x){
    x <- x[[1]][[1]]
    unlist(x)
  }
  )
  # Unlist volume field
  fna_tax_check$fna_vol <- lapply(fna_tax_check$fna_vol,unlist)
  fna_tax_check$fna_vol <- lapply(fna_tax_check$fna_vol,unname)
  fna_tax_check$fna_vol <- lapply(fna_tax_check$fna_vol,function(x){x[1]})
  # Set as global variable
  fna_tax_check <<- fna_tax_check
  
  }

  

#### Execute Search ####
fna_search_main()
