#### Credentials Prompt ####
## Version 3.1
# Started: 9 April 2021
# Last worked on: 18 June 2021
# Author: Clay Meredith
# File: Back_end/Dependent_scripts/credentials_prompt.R
# Description: Prompts user for credentials and stores user credentials using keyring.

#### Install packages ####
# Parameters: list of installed packages
# Returns: installs/loads keyring package
# Throws: None
# Purpose: Checks if keyring is installed. If yes, loads package, 
#          if not, installs then loads.
#### Load packages ####
packages <- c("keyring")

lapply(packages, package.check)

#### Prompt user for GBIF credentials ####

# Parameters: User input
# Returns: var: gbif_user
# Throws: If username is already stored, returns text confirmation.
# Purpose: Prompt user for GBIF username
GBIF_username <- function (){
  # Ask user if GBIF data is to be used
  if (GBIF_toggle == "Y"){
    # Prompt user to input GBIF password (if statement ensures prompt only appears once)
    if (!exists("gbif_user")){
      gbif_user <<- readline(prompt="Enter GBIF username: ")
    } else {print("GBIF username stored")}
  }
}

# Parameters: User input
# Returns: var: gbif_email
# Throws: If email is already stored, returns text confirmation.
# Purpose: Prompt user for GBIF email
GBIF_email_prompt <- function (){
  # Ask user if GBIF data is to be used
  if (GBIF_toggle == "Y"){
    # Prompt user to input GBIF password (if statement ensures prompt only appears once)
    if (!exists("gbif_email")){
      gbif_email <<- readline(prompt="Enter your GBIF account email address: ")
    } else {print("GBIF email stored")}
  }
}

# Parameters: User input
# Returns: "gbif_pass" keyring value
# Throws: If email is already stored, returns text confirmation.
# Purpose: Prompt user for GBIF password using keyring

GBIF_password_prompt <- function () {
  tryCatch(
    expr = if(!is.null(key_get("gbif_pass"))){
      print("GBIF password stored")},
    error = function(e) {
      print("If promted for password, enter your GBIF account password.")
      key_set("gbif_pass")}
  )
}

#### Prompt user for Red List API information ####
# Parameters: User input
# Returns: keyring key for "RL_api"
# Throws: If API code is already stored, returns text confirmation.
# Purpose: 
RL_credentials <- function (){
  # If user has a Red List API Token, proceed
  if(RL_toggle=="Y"){
    
    # Prompt for GBIF password if no password is available using keyring
    print("If prompted for password, enter Red List API information.")
    
    tryCatch(
      expr = if(!is.null(key_get("RL_api"))){
        print("Red List API token already stored stored")},
      error = function(e) {
        key_set("RL_api")}
    )
  }
}

#### Execute Functions ####
# Ask user if they'd like to search the Red List
# RL_toggle <- readline(prompt="Search the IUCN Red List (requires a Red List API Token? Enter Y or N:")
RL_toggle <- "N"

# Ask user if they'd like to search GBIF records
GBIF_toggle <- readline(prompt="Search GBIF databse (requires GBIF credentials and password)? Enter Y or N:")

# Parameters: User input
# Returns: var: GBIF credentials via user input or GBIF_old_toggle
# Throws: 
# Purpose: Prompt user for GBIF username, email, and password
# Runs GBIF_credentials only if user wishes to use data and no credentials are present. If 
# GBIF_toggle is not "Y", prompts user for GBIF_old_toggle which bypasses GBIF query, and loads
# previous downloaded results (used primarily for testing).
if (GBIF_toggle == "Y"){
  tryCatch(
    expr = if(exists("gbif_user") & exists("gbif_email") & 
              !is.null(key_get("gbif_pass"))) {} else {
                GBIF_username()
                GBIF_email_prompt()
                GBIF_password_prompt()
              },
    error = function(e) {
      GBIF_username()
      GBIF_email_prompt()
      GBIF_password_prompt() 
    }
  )
} else {
  GBIF_old_toggle <- 
    readline(prompt="Use previously downloaded GBIF results? Enter Y or N:")
}

# Run Red List API check only if user indicates they want to and no credentials are present
if (RL_toggle == "Y"){
  tryCatch(
    expr = if(!is.null(key_get("RL_api"))){print("Red List API Key stored.")},
    error = function(e) {RL_credentials()}
  )
}

# Prompt user for spatial statistics collection
spatial_collect_toggle <- readline(
  prompt="Collect spatial statistics data (significantly slows runtime)? Enter Y or N:")

