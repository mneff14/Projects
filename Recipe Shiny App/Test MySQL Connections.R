
pacman::p_load(tidyverse, odbc)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "SQL Server",  
                      Server = "C:/ProgramData/MySQL/MySQL Server 8.0/Data/",
                      Database = "art",
                      UID = "root",
                      PWD = "cit111",
                      host = "Topsy",
                      port = 3306
                      )

