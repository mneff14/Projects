# RECIPE SHINY APP





##### --- Libraries --- #####


require(shiny)
require(tidyverse)
require(rsconnect)
require(RSQLite)





##### --- UI --- #####


ui <- fluidPage(
  ## App Title
  titlePanel("Recipe App"),
  
  ## Main Navigation Buttons
  sidebarLayout(
    sidebarPanel(
      navlistPanel(
        id = "main_navigation",
        widths = c(12,8),
        well = FALSE,
        # Panel of Buttons
        tabPanel("Recipes"),
        tabPanel("Add Recipes"),
        tabPanel("Search Recipes")
      )
    ),
    
    ## Main Content
    mainPanel(
      # My Recipes Page
      conditionalPanel(
        condition = "input.main_navigation == 'Recipes'",
        tabsetPanel(id = "recipes_tabs",
          # My Recipes Tab
          tabPanel(
            value = "my_recipes_tab",
            "My Recipes",
            h3("My Recipes"),
            # Reserve space to load recipes the User added to the database
            tags$div(
              id = "my_recipes_ui"
            )
          ),
          # My Favorites Tab
          tabPanel(
            value = "my_favorites_tab",
            "My Favorites",
            h3("My Favorites"),
            # Reserve space to load the User's favorite recipes
            tags$div(
              id = "my_favorites_ui"
            )
          )
        )
      ),
      
      # Add Recipes
      conditionalPanel(
        condition = "input.main_navigation == 'Add Recipes'",
        h3("Add Recipes Content")
      ),
      
      # Search Recipes
      conditionalPanel(
        condition = "input.main_navigation == 'Search Recipes'",
        h3("Search Recipes Content")
      )
    )
  )
)






server <- function(input, output){
  
  #### --- Define Variables --- ####
  
  
  # The path to the Sqlite database
  db_path <- "C:\\Users\\mneff\\Documents\\Git\\Projects\\Projects\\Recipe Shiny App\\Recipe Shiny App Data.db"
  
  # Reactive Values
  rv <- reactiveValues()
  
  
  
  
  
  #### --- Define Functions --- ####
  
  
  getRecipes <- function(name = NULL){
    ### Returns an R dataframe that shows all recipes from the database created by the name passed
    
    # Create a connection to the database (a SQLite3 file on the computer)
    conn <- dbConnect(SQLite(), db_path)

    # The names of the tables in the database
    db_tables <- DBI::dbListTables(conn)

    # Retrieve the "Recipes" table from the database and filter to show the user's recipes
    all_recipes <- DBI::dbReadTable(conn, db_tables[3]) %>% 
      as.data.frame()

    # Disconnect from the database
    dbDisconnect(conn)
    
    # Return the filtered dataframe
    return(all_recipes)
  }
  
  
  # CHANGE: Will run when the reactive values are changed, NOT when the tab is opened!
  generateUITable <- function(insert_at = NULL, df = NULL, columns = NULL, filters = NULL, 
                              button_cust = NULL, max_view = 15, current_page = 1) {
    ### Inserts UI's dynamically that form a table-like structure, allowing the implementation of 
    ### customizable buttons and filters.
    ### 
    ### -- ARGUMENTS --
    ### insert_at:    The tag id for the placeholder to insert the UI table at
    ### df:           The dataframe carrying the UI table's data
    ### columns:      A vector specifying the names of which columns from the df to keep
    ### filters:      A list of logic statement strings that determine which rows to keep (done in order)
    ### button_cust:  A dataframe listing all customizations for the buttons (if any)
    ### max_view:     Integer stating how many rows to view at once
    
    
    # Proceed if certain arguments are not NULL
    if (!is.null(insert_at) && !is.null(df)) {
      # Initial vairalbes
      num_pages <- ceiling(nrow(df) / max_view)
      
      # Apply passed filters, if any
      if (!is.null(filters) && is.numeric(length(filters))) {
        i <- 1
        for (i in 1:length(filters)) {
          df <- filter(df, eval(parse(text = filters[i])))
        }
      }
      
      # Add an incrementing ID column
      df <- tibble::rowid_to_column(df, "ID")
      
      # Add a column that splits the df into pages (so user only sees <max_view> rows on each page)
      df$Page <- lapply(df[,1], function(x) ceiling(x / max_view) )
      
      # Exclude any recipes not on the current page
      df <- filter(df, Page == current_page)
      
      # Keep only the columns listed in "columns"
      if (!is.null(columns)) {
        df <- df[ , columns, drop = FALSE]
      }
      
      print(df)
      
      # Remove any previous UI's
      # removeUI(selector = paste0("div:has(> ", paste0("#", insert_at), ")"), immediate = TRUE)
      
      # Create and insert the UI's
      lapply(1:nrow(df), function(r) {
        insertUI(
          selector = paste0("#", insert_at),
          where = "afterEnd",
          immediate = TRUE,
          ui = wellPanel(
            flowLayout(
              lapply(1:ncol(df), function(c) {
                h6(df[r,c]) 
              })
            )
          )
        )
      })
    }
  }
  
  
    
  
  
  #### --- Render Outputs --- ####
  
  
  # Render the "My Recipes" tabs
  observeEvent(input$recipes_tabs, {
    
    if (input$recipes_tabs == "my_recipes_tab") {
      
      
      # Define the filter
      my_recipes_filter <- character(0)
      my_recipes_filter <- c(my_recipes_filter, "Author == 'me'")
      # my_recipes_filter <- c(my_recipes_filter, "Yield >= 4")
      
      # Define the columns to be kept
      cols <- c("RN", "Name", "Source", "Date_Added", "Date_Updated")

      # Load My Recipes Tab
      generateUITable(insert_at = "my_recipes_ui", 
                      df = getRecipes(),
                      # filters = my_recipes_filter,
                      columns = cols)
      
    } else if (input$recipes_tabs == "my_favorites_tab") {
      # Load My Favorites Tab
      
            
    }
  })

  
  
  
  
}











shinyApp(ui = ui, server = server)
