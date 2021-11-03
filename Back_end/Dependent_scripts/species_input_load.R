#### Mapping Functions ####
## Version 3.1
# Started: 3 November 2021
# Last worked on: 3 November 2021
# Author: Clay Meredith
# File: Dependent_scripts/map_build.R
# Description: Handler for species inputs. Parses type based on title and
#              loads data into spec.list



choose_map_source <- function (){
  # Prompt user to select action
  selected_action <- select.list(c("Choose previous batch results",
                                   "Continue with current batch"
  ), 
  preselect = NULL, multiple = FALSE,
  title = "Select data source",
  graphics = getOption("menu.graphics"))
  return(selected_action)
}