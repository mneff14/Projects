##### ----- GLOBAL ----- #####




#### Load Libraries ####
  
## Using pacman is very fast!
if (!require("pacman")) install.packages("pacman")
#pacman will not accept a character vector so the same packages are repeated
pacman::p_load(
  "shiny",
  "shinyjs",
  "shinyBS",
  "shinybusy",
  "shinyalert",
  "tibble",
  # "readr",
  # "haven",
  "readxl",
  # "dplyr",
  # "ggplot2",
  "ggthemes",
  "tidyverse",
  "rlist",
  "stringr",
  "colourpicker",
  "stringi",
  "topicmodels",
  "tm",
  "quanteda",
  "slam",
  "profvis"
)

  


#### Enable Bookmarking ####
  
enableBookmarking(store = "server")




#### Define Global Functions ####

## Function that returns 1 if keyword and match_value match according to sort_option
check_match <- function(keyword = NA, match_value = NA, standardize = TRUE, sort_option = "Contains") {
  
  ## Variable to be returned
  found_match <- as.integer(0)
  
  ## Make response and keyword lower-case if standardize is true
  if (!is.na(match_value) & keyword != "" & !is.na(keyword)) { 
    if (standardize) {
      match_value <- str_to_lower(match_value)
      keyword <- str_to_lower(keyword)
    } 
    
    ## Change matching pattern (if needed) according to sort option
    #
    # Sort Options
    #   - CONTAINS:     match_value contains keyword
    #   - EXACTLY:      match_value equals keyword
    #   - BEGINS WITH:  match_value begins with keyword
    #   - ENDS WITH:    match_value ends with keyword
    
    pattern <- keyword # Default is "Exactly"
    if (sort_option == "Begins With") {
      # Change pattern to regex to search for begins with
      pattern <- paste0("^",keyword)
    } else {
      if (sort_option == "Ends With") {
        pattern <- paste0(keyword,"$")
      } 
    }
    
    ## Check for an exact match
    if (sort_option == "Exactly") {
      if (match_value == keyword) {
        # They match!
        found_match <- as.integer(1)
      }
    } else {
      ## Check for any other match
      if (str_detect(match_value, pattern)) {
        # They match!
        found_match <- as.integer(1)
      } 
    }
  }
  
  # Function returns TRUE if there was a match
  return(found_match)
}










