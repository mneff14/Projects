##### ----- GLOBAL ----- #####



#### Load Libraries ####

library(shiny)
library(shinyjs)
library(shinyBS)
library(shinybusy)
library(shinyalert)
library(tibble)
library(readxl)
library(ggthemes)
library(tidyverse)
library(rlist)
library(stringr)
library(colourpicker)
library(stringi)
library(tools)


# To get the app to publish, do two things:

# 1. USE R VERSION 3.6.0!
#    (Tools >> Global Options >> General >> R Sessions >> "Change" button to switch.)
#    To set up RStudio Connect on R with your BYU-Idaho account, follow the steps here:
#    https://byuistats.github.io/M335/rstudioconnect.html

# 2. Make sure to download the right XML package.
#    RStudio Connect currently uses R version 3.6.0, which does not support 
#    the newest XML package (version "3.99-0.5"). 
#    Try and see if any of these will work:
# 
#    - install.packages("XML", type = "binary") <- This one worked for me!
# 
#    - old_xml_cran_url <- "https://cran.r-project.org/src/contrib/Archive/XML/XML_3.98-1.17.tar.gz"
#    - install.packages(old_xml_cran_url, repos=NULL, type="source", lib = .libPaths()[1])
#    - devtools::install_version("XML", version = "3.98-1.17", lib = .libPaths()[1])
#    You can remove a package from any library if you need to with this code:
#    - remove.packages("XML", .libPaths()[1])
#    To get a list of library paths they are downloading to, use this code:
#    - .libPaths()   (The first one is used by default)


library(XML)





#### Enable Bookmarking ####
  
enableBookmarking(store = "server")




#### Define Global Functions ####

## Function that returns 1 if keyword and match_value match according to sort_option
check_match <- function(keyword = NA, match_value = NA, standardize = TRUE, sort_option = "Contains") {
  
  ## Variable to be returned
  found_match <- 0
  
  ## If the match_value or keyword are blank, then skip and return 0
  if (!is.na(match_value) & keyword != "" & !is.na(keyword)) { 
    
    ## Make response and keyword lower-case if standardize is true
    if (standardize) {
      match_value <- str_to_lower(match_value)
      keyword <- str_to_lower(keyword)
    } 
    
    ## Find a match according to the logic of the passed sort option
    #
    # Sort Options
    #   - "Exact"/"Anything But":              match_value (does not) equal keyword
    #   - "Contains"/"Does Not Contain":       match_value (does not) contain keyword
    #   - "Begins With"/"Does Not Begin With": match_value (does not) begin with keyword
    #   - "Ends With"/"Does Not End With":     match_value (does not) end with keyword
    
    found_match <- switch (sort_option,
      "Exact"               = match_value == keyword,
      "Anything But"        = match_value != keyword,
      "Contains"            = str_detect(match_value, keyword, negate = FALSE),
      "Does Not Contain"    = str_detect(match_value, keyword, negate = TRUE),
      "Begins With"         = str_detect(match_value, paste0("^", keyword), negate = FALSE),
      "Does Not Begin With" = str_detect(match_value, paste0("^", keyword), negate = TRUE),
      "Ends With"           = str_detect(match_value, paste0(keyword, "$"), negate = FALSE),
      "Does Not End With"   = str_detect(match_value, paste0(keyword, "$"), negate = TRUE)
    )
    
    # Return 1 if found_match is TRUE and 0 if FALSE
    return(sum(found_match))
  } else {
    
    # Arguments are invalid. Return a 0 (found_match default)
    return(found_match)
  }
}


















































			
			
	 
	 
	   
			
      
			
     
			
	    
		
		
     
		
	
	
		





