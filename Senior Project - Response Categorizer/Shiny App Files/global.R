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
library(tm)
library(quanteda)
library(slam)
library(topicmodels)


# This app will run but WILL NOT PUBLISH if "library(topicmodels)" is included.
# This is because this package was built after R version 3.6.0, which is the
# version RStudio Connect uses.


# To publish this app to include the Topic Finder, we would need to:
#
# - Download RShiny Server Open Source 
#   (https://www.rstudio.com/products/shiny/download-server/)
#
# - Create a server using one of the Linux platforms (Red Hat, Ubuntu, etc..)
#
# - Modify the shiny server through a Linux platform to include multiple versions of R
#   (versions 4.0.3 and 3.6.0, specifically)
#   (https://support.rstudio.com/hc/en-us/articles/360002242413-Multiple-versions-of-R)


# Below shows my very painful journey to figure this out...



# ### Install Packages ####
# 
# # RStudio Connect seems to not install the "topicmodels" package,
# # which needs the gsl package installed. Currently I am having a very hard time
# # downloading the gsl package to install topicmodels...
# 
# # Use a URL to install gsl for topicmodels (run ONCE)
# remove.packages("gsl")
# # url <- "https://cran.r-project.org/src/contrib/gsl_2.1-6.tar.gz"
# # url <- "https://mran.microsoft.com/snapshot/2017-02-04/src/contrib/gsl_1.9-10.3.tar.gz"
# # url <- "https://cran.r-project.org/bin/windows/contrib/4.1/gsl_2.1-6.zip"
# # install.packages(url, repos=NULL, type="source")
# 
# # Try installing gsl binaries first
# url_r_devel <- "https://cran.r-project.org/bin/windows/contrib/4.1/gsl_2.1-6.zip"
# install.packages(url_r_devel, repos=NULL, type = "binary")
# url_r_release <- "https://cran.r-project.org/bin/windows/contrib/4.0/gsl_2.1-6.zip"
# install.packages(url_r_release, repos=NULL, type = "binary")
# 
# 
# # Install older version of topicmodels
# remove.packages("topicmodels")
# url2 <- "https://cran.r-project.org/src/contrib/Archive/topicmodels/topicmodels_0.2-8.tar.gz"
# install.packages(url2, repos=NULL, type="source")
# # Try with devtools
# devtools::install_version("topicmodels", version = "0.2-8")
# # Still needs gsl!!!
# 
# 
# # This shows how to get proper gsl for Ubuntu, but not sure how to do this in
# # RStudio...
# #
# # Terminal Command: 
# #   sudo apt-get install libgsl0-dev
# #
# # Reference: https://stackoverflow.com/questions/25759007/error-installing-topicmodels-package-non-zero-exit-status-ubuntu
# 
# 
# install.packages('libgsl0-dev')
# # Not available for R version 3.6.0...
# 
# 
# 
# 
# # library(gsl) # For installing topicmodels
# library(topicmodels)
# 
# # RStudio Connect is having a problem downloading the "XML" package (which is
# # of a newer version than connect's CRAN).
# # Error: r-package-version-not-available
# # Reference: https://docs.rstudio.com/connect/user/publishing/#r-package-version-not-available
# 
# # Choose the closest CRAN mirror (73 for me)
# # chooseCRANmirror()
# 
# # Now install the package with binary (run once), then include it in the app
# # Reference: https://stackoverflow.com/questions/26042751/cannot-install-package-xml-to-r
# # install.packages("XML", lib = .libPaths()[1], type = "binary")
# # library(XML)




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




















































