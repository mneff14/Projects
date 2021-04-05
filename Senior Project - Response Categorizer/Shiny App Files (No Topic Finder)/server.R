##### ----- SERVER ----- #####


# USE R VERSION 3.6.0 TO PUBLISH THIS APP!!!

# (Tools >> Global Options >> General >> R Sessions >> "Change" button to switch.)

# To publish to RStudio Connect with your BYU-Idaho account, follow these steps:
# https://byuistats.github.io/M335/rstudioconnect.html




server <- function(input, output, session) {
  
  
  #### Reactive Values ####
  
  ## A reactive values object whose variables can be created, altered, and 
  ## removed by any function in server
  rv <- reactiveValues(
    file_is_valid = FALSE,
    num_ctgs = 0, # For keeping track of how many categories have been created
    show_results = FALSE, # For showing/hiding main outputs
    plot = NULL,
    table = NULL,
    # These errors will be printed above the plot and below Update and Save
    error_to_display = NULL,
    # Values to help show/hide sections
    # show_help = FALSE,
    # Allows multiple sources to create Categories 
    ctgs_to_add = data.frame( 
      ui_has_been_made = logical(),
      ctg_num = integer(),
      new_ctg_id = character(),
      new_ctg_name = character(),
      stringsAsFactors = FALSE
    ),
    # Allows multiple sources to create Rules
    rules_to_add = data.frame(
      ui_has_been_made = logical(),
      rule_num = integer(),
      new_rule_ctg_id = character(),
      new_rule_id = character(),
      new_rule_keywords = character(),
      stringsAsFactors = FALSE
    ),
    # Stores the UI ids and values for all existing Categories
    all_categories = data.frame(
      id_add = character(),
      ctg_id = character(),
      ctg_num_rules = integer(),
      ctg_add_rule_button_id = character(),
      remove_ctg_id = character(),
      remove_ctg_button_id = character(),
      ctg_name_id = character(),
      ctg_name = character(),
      stringsAsFactors = FALSE
    ),
    # Stores the UI ids and values for all existing Rules
    all_rules = data.frame(
      id_add_rule = character(),                
      rule_id = character(),                
      rule_ctg_id = character(),                
      remove_rule_id = character(),                
      remove_rule_button_id = character(),                
      rule_keywords_id = character(),                
      rule_keywords = character(),                
      rule_sort_options_id = character(),  
      rule_sort_options = character(),                 
      rule_apply_to_id = character(),  
      rule_apply_to = character(),                
      rule_standardize_id = character(),                
      rule_standardize = logical(),
      stringsAsFactors = FALSE              
    ),
    # Various variables for converting topics to Categories
    num_topics = 0,
    ## The color inputs don't save to server, so need to save them in these data frames
    # All color inputs' values created in UI above
    plot_colors_static = data.frame(
      grid_lines_all = "",
      grid_lines_x_all = "",
      grid_lines_x_major = "",
      grid_lines_x_minor = "",
      grid_lines_y_all = "",
      grid_lines_y_major = "",
      grid_lines_y_minor = "",
      plot_background_fill = "",
      plot_background_color = "",
      panel_background_fill = "",
      panel_background_color = "",
      stringsAsFactors = FALSE
    ),
    # The colors generated for individual bar aesthetics
    ctg_aesthetics = data.frame(
      id = character(),
      category = character(),
      fill = character(),
      color = character(),
      linewidth = integer(),
      linetype = character(),
      stringsAsFactors = FALSE
    )
  )
  
  
  
  #### onBookmark() ####
  ## Store all reactive values and other things when saving to state (bookmarking)
  ## Reference: https://shiny.rstudio.com/articles/advanced-bookmarking.html
  onBookmark(function(state) {
    
    # How much to progress the progress bar by after each step
    inc_amt <- 1 / 7
    
    # Show a bar to show the progress of saving the state to the server.
    withProgress(min = 0, max = 1, message = "Saving State to Server", detail = "Preparing inputs...", {
      
      # Save all EXISTING Category and Rule Values
      #
      # The rows from the "ctgs_to_add" and "rules_to_add" RV tables don't get removed,
      # so we must only save the categories/rules from those RV tables that actually 
      # exist (i.e. are in the "all_categories" and "all_rules" RV tables) so that
      # we don't produce empty categories or rules when loading the saved state.
      ctgs_to_save <- rv$ctgs_to_add %>% 
        filter(new_ctg_id %in% rv$all_categories$ctg_id)
      
      rules_to_save <- rv$rules_to_add %>% 
        filter(new_rule_id %in% rv$all_rules$rule_id)
      
      incProgress(amount = inc_amt, detail = "Storing Category Inputs...")
      state$values$ctgs_to_add <- ctgs_to_save   
      
      incProgress(amount = inc_amt, detail = "Storing Rule Inputs...")
      state$values$rules_to_add <- rules_to_save
      
      # Save all Color Input values
      incProgress(amount = inc_amt, detail = "Storing Bar Color Inputs...")
      state$values$ctg_aesthetics <- rv$ctg_aesthetics
      incProgress(amount = inc_amt, detail = "Storing Plot Aesthetic Inputs...")
      state$values$plot_colors_static <- rv$plot_colors_static
      
      # Save the plot
      incProgress(amount = inc_amt, detail = "Storing Plot...")
      state$values$plot <- rv$plot
      
      # Save the table
      incProgress(amount = inc_amt, detail = "Storing Table...")
      state$values$table <- rv$table
      
      # Show the sharable URL modal
      incProgress(amount = inc_amt, detail = "Finished. Showing URL...")
      showBookmarkUrlModal
    })
})
  
  
  
  #### onRestored() ####
  ## Read in all reactive values and other things AFTER restoring a saved (bookmarked) state
  ## Reference: https://shiny.rstudio.com/articles/advanced-bookmarking.html
  onRestored(function(state) {
    
    ## When the saved state is restored, all simple UI inputs will be 
    ## updated automatically with the UI input values from the saved state. 
    ##
    ## However, advanced and dynmaically created UIs like colorInputs and 
    ## Category/Rule UIs WILL NOT be updated.
    ##
    ## This function, therefore, runs AFTER the saved state is restored to make sure 
    ## all UI's are loaded properly.
    
    
    # How much to progress the progress bar by after each step
    inc_amt <- 1 / 6
    
    # Show a bar to show the progress of saving the state to the server.
    withProgress(min = 0, max = 1, message = "Loading Saved State", detail = "Loading Category Inputs...", {
      
      ## Load Categories
      
      saved_ctgs <- state$values$ctgs_to_add 
      # Change the "ui_has_been_made" column to FALSE so the app will create the UIs
      saved_ctgs$ui_has_been_made <- FALSE
      # Replace the current RV data frame and let the app do the rest!
      rv$ctgs_to_add <- saved_ctgs
      
      
      ## Load Rules  
      
      incProgress(amount = inc_amt, detail = "Loading Rule Inputs...")
      
      saved_rules <- state$values$rules_to_add 
      # Change the "ui_has_been_made" column to FALSE so the app will create the UIs
      saved_rules$ui_has_been_made <- FALSE
      
      # Replace the current RV data frame and let the app do the rest!
      rv$rules_to_add <- saved_rules
      
      
      ## Load/Update Static Color Input UIs
      
      incProgress(amount = inc_amt, detail = "Updating Static Plot Color Inputs...")
      
      # Load static color UI's
      rv$plot_colors_static <- state$values$plot_colors_static 
      # Update all static colorInput UI's to match the loaded colors
      for (i in 1:ncol(rv$plot_colors_static)) {
        local({
          # Get the correct id for the UI based on the column name
          ui_id <- switch (colnames(rv$plot_colors_static)[i],
                           'grid_lines_all'         = 'glAllColor',
                           'grid_lines_x_all'       = 'glXAllColor',
                           'grid_lines_x_major'     = 'glMajorXColor',
                           'grid_lines_x_minor'     = 'glMinorXColor',
                           'grid_lines_y_all'       = 'glYAllColor',
                           'grid_lines_y_major'     = 'glMajorYColor',
                           'grid_lines_y_minor'     = 'glMinorYColor',
                           'plot_background_fill'   = 'plotBackgroundFill',
                           'plot_background_color'  = 'plotBackgroundColor',
                           'panel_background_fill'  = 'plotBackgroundColor',
                           'panel_background_color' = 'panelBackgroundFill',
                           'plotBackgroundColor'    = 'panelBackgroundColor'
          )
          # Update the colorInput UI
          colourpicker::updateColourInput(session, inputId = ui_id, value = rv$plot_colors_static[1,i])
        })
      }
      
      
      ## Load/Update Dynamic Color Input UIs
      
      incProgress(amount = inc_amt, detail = "Loading Dynamic Plot Color Inputs...")
      
      # Load dynamic color UI's
      rv$ctg_aesthetics <- state$values$ctg_aesthetics
      # Update all dynamic colorInput UI's to match the loaded colors (either Uniform or Individual Bar Colors)
      if (input$aesOptions == "Uniform Bar Colors") {
        # Change just the Uniform Bar Color Fill/Color colorInput UI's
        colourpicker::updateColourInput(session, inputId = 'aes_uniform_fill', value = rv$ctg_aesthetics$fill[1])
        colourpicker::updateColourInput(session, inputId = 'aes_uniform_color', value = rv$ctg_aesthetics$color[1])
      } else {
        # Change all Individual Bar Color Fill/Color colorInput UI's
        for (i in 1:nrow(rv$ctg_aesthetics)) {
          local({
            # Initial Variables
            fill_ui_id <- paste0(rv$ctg_aesthetics$id[i], '_fill')
            color_ui_id <- paste0(rv$ctg_aesthetics$id[i], '_color')
            new_fill <- rv$ctg_aesthetics$fill[i]
            new_color <- rv$ctg_aesthetics$color[i]
            # Update the colorInput UI's
            colourpicker::updateColourInput(session, inputId = fill_ui_id, value = new_fill)
            colourpicker::updateColourInput(session, inputId = color_ui_id, value = color_ui_id)
          })
        }
      }
      
      
      ## Load the plot
      
      incProgress(amount = inc_amt, detail = "Loading Plot...")
      rv$plot <- state$values$plot
      
      ## Load the table
      
      incProgress(amount = inc_amt, detail = "Loading Table...")
      rv$table <- state$value$table 
      
      
      ## Show the Plot, Table, and Customizations
      
      incProgress(amount = inc_amt, detail = "Finishing...")
      rv$show_results <- TRUE
      
    })
})
  
  
  #### setBookmarkExclude() ####
  ## Excludes buttons and id's from being saved (bookmarked) and
  ## prevents the from running when bookmark is loaded
  ## Reference: https://shiny.rstudio.com/articles/advanced-bookmarking.html
  setBookmarkExclude(c("update","tf_go"))
  
  
  
  #### getData() ####
  ## Reads in file
  getData <- reactive({
    
    # Get the file
    inFile <- input$responses
    
    # Return error message if input is empty
    if (length(inFile) < 1) {
      rv$error_to_display <- "Please import a 1-Column Excel or CSV file."
      return(NULL)
    }
    
    # Retrieve the extension of the file
    ext <- tools::file_ext(inFile)[1]
    
    # Proceed to read in file if its extension is allowed
    allowed_exts <- c('xlsx','csv')
    if (ext %in% allowed_exts) {
      
      # Read in the file based on its extension
      data <- switch (ext,
                      "csv" = readr::read_csv(inFile$datapath, col_names = "Responses"),
                      "xlsx" = readxl::read_excel(inFile$datapath, col_names = "Responses")
      )
      # Make sure error goes away
      rv$error_to_display <- NULL
      
      # Return the read-in data
      return(data)
      
    } else {
      # Return Invalid extension error message
      rv$error_to_display <- paste0('File extension "', ext, '" is not supported. 
                                    Please upload one of these files instead: ', 
                                    str_c(allowed_exts, collapse = ", "))
      return(NULL)
    }
  })
  
  
  #### sortData_GG() ####
  ## Sorts the data
  sortData_GG <- reactive({
    
    ## Ensure specific values are available ("truthy") before running
    # req(input$ctgAdd, input$defaultName, input$multCtgsPerResponse)
    
    ## Initiate a Progress Bar
    withProgress(message = "Initiating", min = 0, max = 1, value = 0, {
      
      ## Retrieve the Imported Data
      data <- getData()
      
      ## Skip process if data contains an error
      if (is.null(data)) {
        return(NULL)
      }
      
      ## Read in reactive value data frames containing current data for all categories and rules
      # all_categories <- rv$all_categories 
      all_rules <- rv$all_rules
      
      ## Category Varialbes
      N <- nrow(rv$all_categories) # Number of categories
      num_responses <- nrow(data) # Number of responses
      # num_rules <- nrow(all_rules) # Number of rules
      default_ctg_name <- paste0(input$defaultName)
      
      ## Categorization Options
      count_by <- input$count_by
      multiple_categories_per_response <- input$multCtgsPerResponse # If false, will assign one ctg per response
      
      ## Add a new Default Category column to the variables data
      default_column = data.frame(matrix(nrow = num_responses, ncol = 1),
                                  stringsAsFactors = FALSE) %>% 
        `colnames<-`(default_ctg_name)
      default_column[,1] <- as.integer(1) # Responses are by default in the default category
      data <- bind_cols(data, default_column)
      
      ## Calculate the number of steps and increment amount for the Progress bar
      ## Number of Steps  = Number of Total Categories + Number of Last Steps
      ## Increment Amount = 1 / Number of Steps
      num_progress_steps <- N + 1
      progress_inc_amt <- 1 / num_progress_steps
      
      ## Make sure categories exist before adding new columns to data
      if (N > 0) {
        
        ## Loop through all current Categories 
        ## Will add a new column to the response data if at least one rule has been added
        ## (Note: "data" will not update if any loop besides "for" is used)
        for (c in 1:N) {
          
          # Initial Current Category variables (for convenience and readability)
          # Each of these will be distinguished with a "."
          ctg.id <- rv$all_categories$ctg_id[c]
          ctg.num_rules <- rv$all_categories$ctg_num_rules[c]
          ctg.name <- paste0(rv$all_categories$ctg_name[c])
          
          # Increment Progress Bar
          incProgress(progress_inc_amt, message = paste0("Matching Category: ", ctg.name))
          
          # Proceed if this category has any rules and exists
          if (ctg.num_rules > 0) {
            
            # Create a new Category column to hold each response's categorization 
            # (will be added to the main Table)
            new_column = data.frame(matrix(nrow = num_responses, ncol = 1),
                                    stringsAsFactors = FALSE) %>%
              `colnames<-`(ctg.name) # Name column after Category's current name
            new_column[,1] <- as.integer(0) # Values are set to zero (rather than FALSE) by default
            
            # Retrieve all rule data for this category
            ctg.all_rules <- all_rules %>% 
              filter(rule_ctg_id == ctg.id) %>% 
              mutate(Num_Matches = as.integer(0)) # For tracking number of matches for each rule
            
            # Update the new column
            # Will increase the values based on the categorization options and
            # matches found in the adjacent response text
            new_column[,1] <- sapply(1:nrow(new_column), function(new_col_row_idx) {
              
              # Will skip categorizing response only if: 
              # - Multiple Categories per Response is False AND
              # - Adjacent response has already been categorized
              #   (Is already categorized if there's a number > 0 in another column other than the default)
              if (!multiple_categories_per_response && sum(data[new_col_row_idx,2:ncol(data)]) > 1) {
                
                # Return a zero
                return(as.integer(0))
                
              } else {
                
                # Initial New Column Variables
                current_response_text <- data[new_col_row_idx,1]
                
                # Loop through each Category Rule
                # Will update Num_Matches with number of matches found in the response text
                ctg.all_rules$Num_Matches <- sapply(1:nrow(ctg.all_rules), function(rule_idx) {
                  
                  # Initial Rule variables
                  # Each of these will be distinguished with a "." for readability
                  rule.sort_option <- ctg.all_rules$rule_sort_options[rule_idx]
                  rule.apply_to <- ctg.all_rules$rule_apply_to[rule_idx]
                  rule.standardize <- ctg.all_rules$rule_standardize[rule_idx]
                  rule.keywords <- ctg.all_rules$rule_keywords[rule_idx] %>% 
                    stri_extract_all_words() %>% # Split the keywords into list of words
                    as.data.frame() %>%          # Convert to data frame
                    `colnames<-`("Keyword") %>%  # Change the column name
                    mutate(Num_Matches = as.integer(0)) # For tracking number of matches for each Rule's keywords
                  
                  # Compare each Keyword to the response and return the number of matches
                  rule.keywords$Num_Matches <- sapply(1:nrow(rule.keywords), function(keyword_idx) {
                    
                    # Initial Variables
                    current_keyword <- rule.keywords$Keyword[keyword_idx]
                    
                    # Proceed if the keyword is not blank
                    if (!is.na(current_keyword) & current_keyword != "") {
                      
                      # By default, compare this keyword to the entire response by treating
                      # the entire response as one word in a dataframe.
                      # (Apply To = "Response")
                      compare_text <- current_response_text %>% 
                        as.data.frame() %>% 
                        `colnames<-`("Response_Word")
                      
                      # If Apply To = "Word", then compare this keyword to each Word in the response
                      if (rule.apply_to == "Word") {
                        compare_text <- current_response_text %>%
                          stri_extract_all_words() %>%   # Split the response into list of words
                          as.data.frame() %>%            # Convert to data frame
                          `colnames<-`("Response_Word")  # Change column name
                      }
                      
                      # Skip this response and return a 0 if there are no rows in the df
                      if (nrow(compare_text) < 1) { return(0) }
                      
                      # Use the check_match() function defined in global.R to create a new column that 
                      # counts the number of times the keyword "matches" each word in the compare text. 
                      # The string comparison used to find a "match" is determined by the selected Sort Option.
                      compare_text$Num_Matches <- sapply(1:nrow(compare_text), function(word_idx) {
                        
                        # Extract the current word from the compare text
                        # (Will be the entire response if Apply To = "Response")
                        word <- compare_text$Response_Word[word_idx]
                        
                        # Call the function to return a 1 if a match was found and 0 if not
                        found_match <- check_match(keyword = current_keyword,
                                                   match_value = word, 
                                                   standardize = rule.standardize, 
                                                   sort_option = rule.sort_option) 
                      
                        # print(paste0("Category [",N,"] Rule [",rule_idx,"] Keyword [",current_keyword,"] Compare Text [",word,"] Found Match [",found_match,"]"))
                      
                        return(found_match)
                      })
                      
                      # Update this keyword's column in the rule's Num_Matches
                      return(sum(compare_text$Num_Matches, na.rm = TRUE))
                      
                    } else { return(as.integer(0)) } # Return no matches found if keyword is NA
                  })
                  
                  
                  # Update this rule's Num_Matches column based on the selected "Count By" option:
                  #   - First Match: Count the FIRST time this response matches this rule (as 1 or 0)
                  #   - All Matches: Count ALL times this response matches this rule (as sum)
                  return( ifelse(count_by == "All Matches",
                                 sum(rule.keywords$Num_Matches),
                                 ifelse(sum(rule.keywords$Num_Matches) > 0, 1, 0)) )
                })
                
                # Change current new_column row value based on the selected Count By option
                #   - First Match: Count the FIRST time this response matches this category (as 1 or 0)
                #   - All Matches: Count ALL times this response matches this category (as sum)
                return( ifelse(count_by == "All Matches",
                         sum(ctg.all_rules$Num_Matches),
                         ifelse(sum(ctg.all_rules$Num_Matches) > 0, 1, 0)) )
              }
            })
            
            # Add the new column to the responses data
            data <- bind_cols(data, new_column)
            
          } else { } # There are no rules for this category, so do nothing
          
        }
        
        # Increment Progress Bar
        incProgress(progress_inc_amt, message = "Finishing / Plotting...")
        
        ## Set Default Column values to 0 if the response is assigned to any other Category
        ## (Need to exclude the response text in apply() so sum() will work)
        data[,2] <- apply(data %>% select(-c("Responses")), 1, function(data_row) {
          as.integer(if_else(sum(data_row) > 1, 0, 1)) # Sum every column, except the first
        })
      }
      
      ## Return the responses data with all added Category columns
      return(data)
    })
  })
  
  
  
  
  #### plotGG() ####
  ## Will plot the ggplot to show how many responses are in which category
  plotGG <- reactive({
    
    ## Get the responses data sorted into categories
    data <- sortData_GG() 
    
    ## Skip plotting if data contains an error
    if (is.null(data)) {
      return(NULL)
    } 
    
    ## Display new error if no categories have been created
    if (nrow(rv$all_categories) == 0) {
      rv$error_to_display <- "Please add at least one category before updating."
      return(NULL)
    }
    
    ## Variables
    num_columns <- ncol(data)
    aesthetics <- NA
    barWidth <- input$aesBarWidth
    # Position variable 
    position <- switch (
      EXPR = input$aesPosition,
      "Dodge" = "position_dodge()",
      "Stack" = "position_stack()",
      "Fill"  = "position_fill()",
      "position_dodge2()"
    )
    # Order variable
    order <- switch (
      EXPR = input$aesOrder,
      "Incr (1->9)" = "reorder(Category, n)",
      "Decr (9->1)" = "reorder(Category, desc(n))",
      "Decr (Z->A)" = "reorder(Category, desc(Category))",
      "Category" # Increasing (A->Z)
    )
    
    ## Create plotting table 
    ## Transform into a table with two columns:
    ##  - Category (name of each column except the first (contains responses text))
    ##  - n        (total number of responses/mentions for each category)
    ##  - id       (each category's id for convenience in filling in the aes row)
    # Remove the Responses column
    data.categories <- data %>% 
      select(-1)
    # Create the table for plotting
    plot_data <- data.frame(
      Category = colnames(data.categories),
      n = colSums(data.categories),
      stringsAsFactors = FALSE
    ) 
    # Add the id for each column
    #  - Default Column: id = "default"
    #  - Otherwise:      id = ctg_id from rv$all_categories
    col_ids <- lapply(1:nrow(plot_data), function(col_idx) {
      ifelse(col_idx == 1, # The Default Column is always the first row in plot-data
             "default",
             rv$all_categories$ctg_id[col_idx-1])
    })
    plot_data$id <- col_ids
    
    
    ## Returns a new ctg_aesthetics row (which is used to color the bars on the graph)
    ## (Uses the default category and NA's as the default variables)
    fill_aes_row <- function(ctgInputID = NA, ctg_name = NA, id_num = NA) {
      
      # Stop and return NA if the arguments passed are NA
      if (is.na(ctgInputID) | is.na(ctg_name) | is.na(id_num)) {
        return(NA)
      } else {
        
        # Create variables
        fill <- "transparent"
        color <- "transparent"
        lineWidth <- 0
        lineType <- "blank"
        input_no_fill <- eval(parse(text = paste0("input$", ctgInputID, "_no_fill")))
        input_fill <- eval(parse(text = paste0("input$", ctgInputID, "_fill")))
        input_no_color <- eval(parse(text = paste0("input$", ctgInputID, "_no_color")))
        input_color <- eval(parse(text = paste0("input$", ctgInputID, "_color")))
        input_lineWidth <- eval(parse(text = paste0("input$", ctgInputID, "_line_width")))
        input_lineType <- eval(parse(text = paste0("input$", ctgInputID, "_line_type")))
        
        # Change variables based on input values (if not NULL)
        if (!is.null(input_fill) && !is.null(input_no_fill)) {
          if (input_no_fill == FALSE) {
            fill <- input_fill
          }
        }
        if (!is.null(input_color) && !is.null(input_no_color)) {
          if (input_no_color == FALSE) {
            color <- input_color
            if (!is.null(input_lineWidth)) {
              lineWidth <- input_lineWidth
            }
            if (!is.null(input_lineType)) {
              lineType <- input_lineType
            }
          }
        }
        
        # Create a new row to be returned
        # new_aes <- data.frame(matrix(nrow = 1, ncol = 5)) %>% 
        #   `colnames<-`(c("category","fill","color","lineWidth","lineType"))
        new_aes <- data.frame(
          id = paste0(ctgInputID, "_", id_num),
          category = ctg_name,
          fill = fill,
          color = color,
          linewidth = lineWidth,
          linetype = lineType,
          stringsAsFactors = FALSE 
        )
        
        return(new_aes)
      }
    }
    
    
    ## Create a new ctg aesthetics data frame to replace the RV one
    new_ctg_aesthetics <- data.frame(
      id = character(),
      category = character(),
      fill = character(),
      color = character(),
      linewidth = integer(),
      linetype = character(),
      stringsAsFactors = FALSE
    )
    
    ## Fill the new_ctg_aesthetics data frame with the aes values for each existing category
    for (i in 0:nrow(rv$all_categories)) { # Need +1 to include Default Category
      
      # Initial Variables 
      # (From Default Category by default)
      id <- ifelse(input$aesOptions == "Individual Bar Colors",
                   "default_aes_indiv",
                   "aes_uniform") # The id determines where to retrieve input UI values      
      name <- input$defaultName 
      
      # Change initial variables if this category is not the default category
      if (i != 0) {
        
        # Proceed with changes if this Category has any rules
        if (rv$all_categories$ctg_num_rules[i] > 0) {
          
          # Change id if aesthetics are not Uniform
          id <- ifelse(input$aesOptions == "Individual Bar Colors",
                       paste0(rv$all_categories$ctg_id[i], "_aes_indiv"),
                       "aes_uniform")
          
          # Change name to current category's name
          name <- rv$all_categories$ctg_name[i]
        }
      }
      
      # Use custom function to retrieve all aesthetic values using id and name
      new_aes_row <- fill_aes_row(ctgInputID = id, ctg_name = name, id_num = i)
      
      # Update the new_ctg_aethetics table if the new row is not empty
      if (!is.na(new_aes_row[1])) {
        # Replace the existing row by matching id
        if (paste0(id, "_", i) %in% new_ctg_aesthetics$id) {
          new_ctg_aesthetics[new_ctg_aesthetics$aes_id == paste0(id, "_", i)] <- new_aes_row
        } else {
          # Add the new row
          new_ctg_aesthetics <- bind_rows(new_ctg_aesthetics, new_aes_row)
        }
      }
    }
    
    ## Replace the RV ctg aesthetics with the new ctg aesthetics
    rv$ctg_aesthetics <- new_ctg_aesthetics
    
    ## Plot the data according to non-conditional user inputs
    plot <- ggplot(plot_data) +
      labs(title = input$mainTitle, x = input$xTitle, y = input$yTitle) + 
      eval(parse(text = paste0("theme_", input$selectTheme, "()"))) # The theme of the plot
    
    ## Add Conditional Aesthetics
    # Add the base plot with aesthetics, reordering as specified
    plot <- plot + geom_bar(aes(x = eval(parse(text = order)), y = n, 
                                fill = Category, 
                                color = Category, 
                                size = Category, 
                                linetype = Category),
                            width = barWidth,
                            stat = "identity",
                            position = eval(parse(text = position)), show.legend = FALSE)
    # Category Aesthetic variables
    scale_fill_values <- NA
    scale_color_values <- NA
    scale_lineWidth_values <- NA
    scale_lineType_values <- "blank"
    # Add graph aesthetics (from RV data frame)
    if (nrow(rv$ctg_aesthetics) > 0) {
      # Turn the vectors of chars in aesthetics into a single string separated by a comma
      scale_fill_values <- rv$ctg_aesthetics$fill
      scale_color_values <- rv$ctg_aesthetics$color
      scale_lineWidth_values <- rv$ctg_aesthetics$linewidth
      scale_lineType_values <- rv$ctg_aesthetics$linetype
      
      # Create scale_ functions using variables to add to the plot
      plot <- plot + scale_fill_manual(values = c(scale_fill_values))
      plot <- plot + scale_color_manual(values = c(scale_color_values)) 
      plot <- plot + scale_size_manual(values = c(scale_lineWidth_values)) 
      plot <- plot + scale_linetype_manual(values = c(scale_lineType_values))
    }
    
    ## Implement additional conditional customizations
    ## Subtitle
    if (input$includeSubtitle) {
      # Add subtitle
      plot <- plot + labs(subtitle = input$subtitle)
    }
    ## Data Labels
    if (input$includeDataLabels) {
      # Add customized data labels
      plot <- plot + geom_text(aes(x = Category, y = n, label = n),
                               nudge_x = input$dataLabelNudgeX,
                               nudge_y = input$dataLabelNudgeY, show.legend = FALSE)
    }
    ## Grid Lines
    if (input$editGridLines) {
      # New theme elements to be changed conditionally
      new_all_grid_lines <- element_blank()
      new_all_x <- element_blank()
      new_all_y <- element_blank()
      new_major_x <- element_blank()
      new_minor_x <- element_blank()
      new_major_y <- element_blank()
      new_minor_y <- element_blank()
      # Edit all grid lines
      if (input$gridLinesToEdit == "All") {
        # Change new all grid lines theme element if blank is not selected
        if (!input$glAllBlank) {
          new_all_grid_lines <- element_line(color = rv$plot_colors_static$grid_lines_all, size = input$glAllLineWidth, 
                                             linetype = input$glAllLineType, lineend = input$glAllLineEnd)
        }
        # Add new all grid lines theme element to plot
        plot <- plot + theme(panel.grid = new_all_grid_lines)
        
      } else {
        # Edit individual grid lines
        if (input$gridLinesToEdit == "Individual") {
          # Edit all X-Axis grid lines
          if (input$xGridLinesToEdit == "All") {
            # Change all x-axis theme element if blank is not selected
            if (!input$glXAllBlank) {
              new_all_x <- element_line(color = rv$plot_colors_static$grid_lines_x_all, size = input$glXAllLineWidth, 
                                        linetype = input$glXAllLineType, lineend = input$glXAllLineEnd)}
            # Add new all x-axis theme element
            plot <- plot + theme(panel.grid.major.x = new_all_x, 
                                 panel.grid.minor.x = new_all_x)
          } else {
            # Edit individual X-Axis grid lines
            if (input$xGridLinesToEdit == "Individual") {
              # Major X
              if (input$glEditMajorX) {
                # Edit if blank is not selected
                if (!input$glMajorXBlank) {
                  new_major_x <- element_line(color = rv$plot_colors_static$grid_lines_x_major, size = input$glMajorXLineWidth, 
                                              linetype = input$glMajorXLineType, lineend = input$glMajorXLineEnd)
                }
                # Add new grid line theme element to the plot
                plot <- plot + theme(panel.grid.major.x = new_major_x)
              }
              # Minor X
              if (input$glEditMinorX) {
                # Edit if blank is not selected
                if (!input$glMinorXBlank) {
                  new_minor_x <- element_line(color = rv$plot_colors_static$grid_lines_x_minor, size = input$glMinorXLineWidth, 
                                              linetype = input$glMinorXLineType, lineend = input$glMinorXLineEnd)
                }
                # Add new grid line theme element to the plot
                plot <- plot + theme(panel.grid.minor.x = new_minor_x)
              }
            }
          }
          # Edit all Y-Axis grid lines
          if (input$yGridLinesToEdit == "All") {
            # Change all y-axis theme element if blank is not selected
            if (!input$glYAllBlank) {
              new_all_y <- element_line(color = rv$plot_colors_static$grid_lines_y_all, size = input$glYAllLineWidth, 
                                        linetype = input$glYAllLineType, lineend = input$glYAllLineEnd)}
            # Add new all y-axis theme element
            plot <- plot + theme(panel.grid.major.y = new_all_y, 
                                 panel.grid.minor.y = new_all_y)
          } else {
            # Edit individual X-Axis grid lines
            if (input$yGridLinesToEdit == "Individual") {
              # Major Y
              if (input$glEditMajorY) {
                # Edit if blank is not selected
                if (!input$glMajorYBlank) {
                  new_major_y <- element_line(color = rv$plot_colors_static$grid_lines_y_major, size = input$glMajorYLineWidth, 
                                              linetype = input$glMajorYLineType, lineend = input$glMajorYLineEnd)
                }
                # Add new grid line theme element to the plot
                plot <- plot + theme(panel.grid.major.y = new_major_y)
              }
              # Minor Y
              if (input$glEditMinorY) {
                # Edit if blank is not selected
                if (!input$glMinorYBlank) {
                  new_minor_y <- element_line(color = rv$plot_colors_static$grid_lines_y_minor, size = input$glMinorYLineWidth, 
                                              linetype = input$glMinorYLineType, lineend = input$glMinorYLineEnd)
                }
                # Add new grid line theme element to the plot
                plot <- plot + theme(panel.grid.minor.y = new_minor_y)
              }
            }
          }
        }
      }
    }
    ## Plot Background
    if (input$editPlotBackground) {
      # Either element_blank or element_rect
      new_plot_background <- element_blank()
      if (!input$plotBackgroundBlank) {
        new_plot_background <- element_rect(fill = rv$plot_colors_static$plot_background_fill, 
                                            color = rv$plot_colors_static$plot_background_color,
                                            size = input$plotBackgroundLineWidth, 
                                            linetype = input$plotBackgroundLineType)}
      # Add new theme element to the plot
      plot <- plot + theme(plot.background = new_plot_background)
    }
    ## Panel Background
    if (input$editPanelBackground) {
      # Either element_blank or element_rect
      new_panel_background <- element_blank()
      if (!input$panelBackgroundBlank) {
        new_panel <- element_rect(fill = rv$plot_colors_static$panel_background_fill, 
                                  color = rv$plot_colors_static$panel_background_color,
                                  size = input$panelBackgroundLineWidth, 
                                  linetype = input$panelBackgroundLineType)}
      # Add new theme element to the plot
      plot <- plot + theme(panel.background = new_panel)
    }
    
    ## Save plot to Reactive Values
    # rv$plot <- plot
    
    ## Make sure errors are not displayed
    rv$error_to_display <- NULL
    
    ## Return the finished plot to be plotted
    return(plot)
  })
  
  
  #### plotTable() ####
  ## Will plot a table of the data
  plotTable <- reactive({
    ## Get the responses data sorted into categories
    data <- sortData_GG() 
    
    ## Skip process if data contains an error message
    if (is.null(data)) {
      return(NULL)
    } 
    
    ## Return new error code if no categories have been created
    if (nrow(rv$all_categories) == 0) {
      rv$error_to_display <- "Please add at least one category before updating."
      return(NULL)
    }
    
    ## Re-structure the data to be plotted as a table
    ## (Add a surrogate Index as the first column for numbering the responses)
    table_data <- bind_cols(data.frame(ID = seq(1,nrow(data)),
                                       stringsAsFactors = FALSE),
                            data %>% as.data.frame())
    
    ## Save table to reactive values to be plotted
    rv$table <- table_data
    
    ## Make sure no errors are displayed
    rv$error_to_display <- NULL
    
    ## Plot the table of the data
    return(table_data)
  })
  
  
  
  
  #### Observe Event -- updatePushed_plotGG ####
  ## Calls plotGG when "Update" button is pushed and updates the RV to trigger renderPlot()
  # updatePushed_plotGG <- eventReactive(input$update, {
  observeEvent(input$update, {
    
    # Call the plotting function
    plot <- plotGG()
    
    # If there's no errors, save the plot to RV
    if (!is.null(plot)) {
      rv$plot <- plot
    } else {
      # There is an error. Make sure the RV is set to NULL
      rv$plot <- NULL
    }
  })
  
  
  #### Observe Event -- updatePushed_plotTable ####
  ## Calls plotTable when "Update" button is pushed and updates the RV to trigger renderTable()
  # updatePushed_plotTable <- eventReactive(input$update, {
  observeEvent(input$update, {
    
    # Call the plotting function
    table <- plotTable()
    
    # If there's no errors, save the table to RV
    if (!is.null(table)) {
      rv$table <- table
    } else {
      # There is an error. Make sure the RV is set to NULL
      rv$table <- NULL
    }
  })
  
  
  
  
  #### Observe Event -- ctgAdd ####
  ## Adds a new row to rv$ctgs_to_add to trigger making a category 
  observeEvent(input$ctgAdd, ignoreInit = TRUE, {
    
    # Initial Variables
    id_add <- rv$num_ctgs + 1
    # Increase id_add if there is already a category with the same ctg_num
    if (id_add %in% rv$ctgs_to_add$ctg_num) { id_add <- max(rv$ctgs_to_add$ctg_num) + 1 }
    ctg_id <- paste0("ctg_", id_add)
    ctg_name <- paste0("Category ", id_add)
    # Add new row to the RV dataframe
    new_ctg_to_add <- data.frame(
      ui_has_been_made = FALSE,
      ctg_num = id_add,
      new_ctg_name = ctg_name,
      new_ctg_id = ctg_id ,
      stringsAsFactors = FALSE
    )
    rv$ctgs_to_add <- bind_rows(rv$ctgs_to_add, new_ctg_to_add)
  })
  
  
  #### Observe Event -- Add Category (rv$ctg_to_add) ####
  ## Adds the Category Name text input and Add Rule button when the RV dataframe is updated.
  observeEvent(rv$ctgs_to_add, ignoreInit = TRUE, {
    
    ## Loop through each row in the RV data frame and create categories as necessary
    for (i in 1:nrow(rv$ctgs_to_add)) {
      
      # Make sure each category runs in its own environment to run properly
      local({
        
        ## Initial Variables
        ui_has_been_made <- rv$ctgs_to_add$ui_has_been_made[i]
        new_add_id <- rv$ctgs_to_add$ctg_num[i]
        new_ctg_id <- rv$ctgs_to_add$new_ctg_id[i]
        new_ctg_name <- rv$ctgs_to_add$new_ctg_name[i]
        
        ## Proceed to create category only if category has NOT already been created
        ## AND the new_ctg_id is not NA
        if ( ((!ui_has_been_made | is.na(ui_has_been_made)) & !is.na(new_ctg_id)) &
             !new_ctg_id %in% rv$all_categories$ctg_id ) {
          
          ## Make dynamic id's for all category UI's and the category itself
          id_add <- new_add_id
          ctg_id <- new_ctg_id
          ctg_name_id <- paste0(ctg_id, "_name")
          ctg_add_rule_button_id <- paste0(ctg_id, "_add_rule")
          remove_ctg_id <- paste0("remove_ctg", id_add)
          remove_ctg_button_id <- paste0("remove_ctg_button_", id_add)
          ctg_init_name <- new_ctg_name
          
          
          ## Define Initial Variables
          # if (nrow(rv$topics_to_add) > 0) {
          #   # Insert Topic's Title if new Category is a topic to be converted
          #   ctg_init_name <- rv$topics_to_add$title[rv$topics_to_add$ctg_id == ctg_id]
          # }
          
          
          #### * Insert Category UI's ####
          insertUI("#categories",
                   "beforeEnd",
                   tags$div(
                     # Has this id for removal
                     id = ctg_id,
                     # Category Title
                     h3(paste0("Category ", id_add)),
                     # The category name
                     textInput(ctg_name_id, 
                               ctg_init_name, 
                               label = paste0("Category ", id_add, " Name")),
                     # Layout for the Add and Remove Rules action buttons
                     fluidRow(
                       column(width = 6,
                              # Button to Add Rules
                              actionButton(ctg_add_rule_button_id, "+ Rule")),
                       
                       column(width = 6,
                              # Placeholder for the Remove Rules buttons
                              tags$div(id = paste0(ctg_id, "_remove_rule_buttons")))
                     ),
                     # Placeholder for all cateogry rule UI's
                     wellPanel(
                       # Make original div to insert 1+ rules for the category
                       tags$div(id = paste0(ctg_id, "_rules"))
                     )
                   ), immediate = TRUE
          )
          
          ## Insert Remove Category Button ##
          insertUI(
            selector = "#removeCtgButton",
            ui = tags$div(
              id = remove_ctg_id,
              # Check box to signify which category to remove
              actionButton(remove_ctg_button_id, paste0("Remove Category ", id_add))
            )
          )
          
          #### * Update Category RV's ####
          
          ## Increase the number of all categories that have been created
          rv$num_ctgs <- rv$num_ctgs + 1
          
          ## Specify that the category's UI has been made
          rv$ctgs_to_add$ui_has_been_made[i] <- TRUE
          
          ## Append new category id's and initial values to reactive value data frame
          rv$all_categories = bind_rows(rv$all_categories,
                                        data.frame(
                                          id_add = as.character(id_add),
                                          ctg_id = ctg_id,
                                          remove_ctg_id = remove_ctg_id,
                                          ctg_num_rules = 0,
                                          ctg_add_rule_button_id = ctg_add_rule_button_id,
                                          remove_ctg_button_id = remove_ctg_button_id,
                                          ctg_name_id = ctg_name_id,
                                          ctg_name = ctg_init_name,
                                          stringsAsFactors = FALSE
                                        )    
          )
          
          ## Save other values to rv for inserting new Topics
          rv$topics_to_add$rule_id[rv$topics_to_add$ctg_id == ctg_id] <- ctg_add_rule_button_id
          
          ## Update Category Name in rv data frame when changed
          observeEvent(input[[ctg_name_id]], {
            rv$all_categories$ctg_name[rv$all_categories$ctg_id == ctg_id] <- input[[ctg_name_id]]
          })
          
          
          
          #### * Add/Remove All Category Tooltips ####
          observeEvent(input$show_tooltips,  {
            if (input$show_tooltips) {
              ## Add Category + Rule Button Tooltip
              addPopover(session, id = ctg_add_rule_button_id, trigger = "hover", 
                         title = "+ Rule Info",
                         content = "
                 When multiple Rules are created for a Category, a response must 
                 meet the conditions of every Rule to be assigned to that Category.
                 ")
            } else {
              
              # Remove All Category Tooltips
              removePopover(session, id = ctg_add_rule_button_id)
            }
          })
          
          
          #### * Observe Event -- Remove Category ####
          ## Removes all elements in category when remove category button is pushed
          observeEvent(input[[remove_ctg_button_id]], {
            # Removes all ui's within the category
            removeUI(
              selector = paste0("#", ctg_id)
            )
            # Remove the remove button
            removeUI(
              selector = paste0("#", remove_ctg_id)
            )
            # Decrease the number of all categories that have been created
            rv$num_ctgs <- rv$num_ctgs - 1
            
            # Remove category from the Category rv data frame
            rv$all_categories <- rv$all_categories[!(rv$all_categories$ctg_id == ctg_id), ]
            
          }, ignoreInit = TRUE, once = TRUE)
          
          
          #### * Observe Event -- Add Rule Button Pushed ####
          ## Adds a new row to rv$ctgs_to_add to trigger making a rule
          observeEvent(input[[ctg_add_rule_button_id]], 
                       ignoreNULL = TRUE, ignoreInit = TRUE, {
                         
                         ## Initial Variables
                         rule_ctg_id <- ctg_id
                         id_add_rule <- 1 
                         # If this is not the first rule, set the new rule number to  
                         # one above the current max number of rules ever made for this category
                         all_rules_made_for_ctg <- rv$rules_to_add %>% 
                           filter(new_rule_ctg_id == rule_ctg_id & ui_has_been_made)
                         if (nrow(all_rules_made_for_ctg) > 0) {
                           id_add_rule <- max(all_rules_made_for_ctg$rule_num) + 1 
                         }
                         rule_id <- paste0("rule_", id_add_rule, rule_ctg_id)
                         
                         ## Add new row to the RV data frame
                         new_rule_to_add = data.frame(
                           ui_has_been_made = FALSE,
                           rule_num = id_add_rule,
                           new_rule_ctg_id = rule_ctg_id,
                           new_rule_id = rule_id,
                           new_rule_keywords = "",
                           stringsAsFactors = FALSE
                         )        
                         rv$rules_to_add <- bind_rows(rv$rules_to_add, new_rule_to_add)
                       })
        }
      })
    }
  })
  
  
  #### Observe Event -- Add Rule (rv$rules_to_add) ####
  ## Creates all Rule UI when the RV dataframe is updated.
  observeEvent(rv$rules_to_add, ignoreInit = TRUE, {
    
    ## Loop through each row in the RV dataframe and create rules as necessary
    for (i in 1:nrow(rv$rules_to_add)) {
      
      # Make sure each rule runs in its own environment to run properly
      local({
        
        ## Initial Variables
        ui_has_been_made <- rv$rules_to_add$ui_has_been_made[i]
        new_rule_num = rv$rules_to_add$rule_num[i]
        new_rule_ctg_id <- rv$rules_to_add$new_rule_ctg_id[i]
        new_rule_id <- rv$rules_to_add$new_rule_id[i]
        new_rule_keywords = rv$rules_to_add$new_rule_keywords[i]
        
        ## Proceed to create rule only if rule has NOT already been created
        ## AND the new_rule_id is not NA
        if ( ((!ui_has_been_made | is.na(ui_has_been_made)) & !is.na(new_rule_id)) &
             !new_rule_id %in% rv$all_rules$rule_id) {
          
          ## Make id's for rule UI's and the rule itself
          id_add_rule <- new_rule_num
          rule_id <- new_rule_id
          ctg_id <- new_rule_ctg_id
          remove_rule_id <- paste0("remove_rule_", id_add_rule, ctg_id)
          remove_rule_button_id <- paste0("remove_rule_button_", id_add_rule, ctg_id)
          rule_keywords_id <- paste0(rule_id, "_keywords")
          rule_sort_options_id <- paste0(rule_id, "_sort_options")
          rule_apply_to_id <- paste0(rule_id, "_apply_to")
          rule_standardize_id <- paste0(rule_id, "_standardize")
          
          ## Define Initial Variables
          rule_init_keywords <- new_rule_keywords
          rule_choices_sort_options <- c("Exact", "Anything But",
                                         "Contains", "Does Not Contain",
                                         "Begins With", "Does Not Begin With",
                                         "Ends With", "Does Not End With")
          rule_init_sort_options <- "Contains"
          rule_choices_apply_to <- c("Word", "Response")
          rule_init_apply_to <- "Response"
          rule_init_standardize <- TRUE
          
          
          #### * Insert Rule UI's ####
          insertUI(immediate = TRUE,
                   selector = paste0("#", ctg_id, "_rules"),
                   ui = tags$div(
                     # Has this id for removal
                     id = rule_id,
                     # Rule Title
                     h4(paste0("Rule ", id_add_rule)),
                     # Key words text input
                     textInput(rule_keywords_id,
                               value = rule_init_keywords,
                               label = paste0("Keywords for Rule ", id_add_rule)),
                     fluidRow(
                       column(width = 6,
                              # Sorting Options list
                              selectInput(rule_sort_options_id,
                                          label = "Sorting Options",
                                          choices = rule_choices_sort_options,
                                          selected = rule_init_sort_options,
                                          width = 120 )
                              ),
                       column(width = 6,
                              # Apply To Radio Buttons
                              radioButtons(rule_apply_to_id,
                                           label = "Apply To",
                                           choices = rule_choices_apply_to,
                                           selected = rule_init_apply_to,
                                           inline = FALSE)
                              )
                     ),
                     # Standardize check box that will, when checked, set all keywords and responses to lowercase
                     checkboxInput(rule_standardize_id, 
                                   label = "Standardize All to Lowercase", 
                                   value =  rule_init_standardize),
                     hr()
                   )
          )
          
          ## Insert Remove Rule Button
          insertUI(immediate = TRUE,
                   selector = paste0("#", ctg_id, "_remove_rule_buttons"),
                   ui = tags$div(
                     # For removing the button
                     id = remove_rule_id,
                     # The Remove Rule button
                     actionButton(remove_rule_button_id, paste0("- Rule ", id_add_rule))
                   )
          )
          
          
          #### * Update Rule RV's ####
          
          ## Specify that the rule's UI has been made
          rv$rules_to_add$ui_has_been_made[i] <- TRUE
          
          ## Append new rule id's and initial values to reactive value data frame
          rv$all_rules = bind_rows(rv$all_rules,
                                   data.frame(
                                     id_add_rule = as.character(id_add_rule),
                                     rule_id = rule_id,
                                     rule_ctg_id = ctg_id,
                                     remove_rule_id = remove_rule_id,
                                     remove_rule_button_id = remove_rule_button_id,
                                     rule_keywords_id = rule_keywords_id,
                                     rule_keywords = rule_init_keywords,
                                     rule_sort_options_id = rule_sort_options_id,
                                     rule_sort_options = rule_init_sort_options,                
                                     rule_apply_to_id = rule_apply_to_id,  
                                     rule_apply_to = rule_init_apply_to, 
                                     rule_standardize_id = rule_standardize_id,
                                     rule_standardize = rule_init_standardize,
                                     stringsAsFactors = FALSE
                                   )
          )
          
          ## Increase number of rules in Category rv data frame
          rv$all_categories$ctg_num_rules[rv$all_categories$ctg_id == ctg_id] <- rv$all_categories$ctg_num_rules[rv$all_categories$ctg_id == ctg_id] + 1
          
          ## Update Keywords in rv data frame when changed
          observeEvent(input[[rule_keywords_id]], {
            rv$all_rules$rule_keywords[rv$all_rules$rule_id == rule_id] <- input[[rule_keywords_id]]
          })
          
          ## Update Sort Options in rv data frame when changed
          observeEvent(input[[rule_sort_options_id]], {
            rv$all_rules$rule_sort_options[rv$all_rules$rule_id == rule_id] <- input[[rule_sort_options_id]]
          })
          
          ## Update Apply To in rv data frame when changed
          observeEvent(input[[rule_apply_to_id]], {
            rv$all_rules$rule_apply_to[rv$all_rules$rule_id == rule_id] <- input[[rule_apply_to_id]]
          })
          
          ## Update Standardize in rv data frame when changed
          observeEvent(input[[rule_standardize_id]], {
            rv$all_rules$rule_standardize[rv$all_rules$rule_id == rule_id] <- input[[rule_standardize_id]]
          })
          
          
          #### * Add/Remove All Rule Tooltips ####
          observeEvent(input$show_tooltips, {
            if (input$show_tooltips) {
              
              ## Add Rule Keywords Tooltip
              addPopover(session, id = rule_keywords_id, trigger = "hover", 
                         title = "Keywords Info",
                         content = "
                 Each Keywords listed here will be compared to each response's text, and if 
                 they “match”, then that response will be assigned to this Rule’s Category. 
                 The Keywords will be separated by any delimiter, including whitespaces.
                 (Ex: . , | ; : )                 
                 ")
              
              ## Add Rule Sorting Options Tooltip
              addPopover(session, id = rule_sort_options_id, trigger = "hover", 
                         title = "Sorting Options Info",
                         content = '
                 Defines when a Keyword will "match" the text it is compared to.
                 (Ex: if "Contains" is selected, then if the text contains any of 
                 the Keywords listed above, then they will "match".)
                 ')
              
              ## Add Rule Apply To Tooltip
              addPopover(session, id = rule_apply_to_id, trigger = "hover", 
                         title = "Apply To Info",
                         content = "
                 Determines whether each Keyword will compared to the entire response text 
                 or each word in the response text.
                 ")
              
              ## Add Rule Standardize Tooltip
              addPopover(session, id = rule_standardize_id, trigger = "hover", 
                         title = "Standardize Info",
                         content = "
                 If checked, converts the Keywords and the text to lower-case before comparing.
                 ")
              
            } else {
              
              # Remove All Rule Tooltips
              removePopover(session, id = rule_keywords_id)
              removePopover(session, id = rule_sort_options_id)
              removePopover(session, id = rule_apply_to_id)
              removePopover(session, id = rule_standardize_id)
            }
          })
          
          
          #### * Observe Event -- Remove Rule ####
          ## Removes all elements in rule when remove rule button is pushed
          observeEvent(input[[remove_rule_button_id]], {
            # Removes all ui's within the rule
            removeUI(
              selector = paste0("#", rule_id)
            )
            # Remove the remove button
            removeUI(
              selector = paste0("#", remove_rule_id)
            )
            # Remove rule from the Rule rv data frame
            rv$all_rules <- rv$all_rules[!(rv$all_rules$rule_id == rule_id), ]
            
            # Decrease number of rules in Category rv data frame
            rv$all_categories$ctg_num_rules[rv$all_categories$ctg_id == ctg_id] <- rv$all_categories$ctg_num_rules[rv$all_categories$ctg_id == ctg_id] - 1
            
          }, ignoreInit = TRUE, once = TRUE)
        }
      })
    }
  })
  
  #### Observe Event -- Show Tooltips ####
  ## Add tooltips only if checkbox is checked
  observeEvent(input$show_tooltips, {
    if (input$show_tooltips) {
      
      #### * Add Tooltips ####
      
      # # Template Popover
      # addPopover(session, id = "", trigger = "hover",
      #            title = " Info",
      #            content = "
      #            
      #            ")
      
      # Import Responses Popover
      addPopover(session, id = "import_title", trigger = "hover",
                 title = "Importing Info",
                 content = "
                 Allows you to upload a 1-column CSV or Excel file containing your open-ended responses. 
                 You can either: 
                 1) Browse for the file from your local computer by pressing the “Browse” button or 
                 2) Drag and drop the file next to the button.
                 ")
      
      # Counting Options Title Popover
      addPopover(session, id = "categorization_options_title", trigger = "hover",
                 title = "Categorization Options Info",
                 content = '
                 These options tell the app what and how to count when assigning responses to
                 categories. 
                 Press the links below to visualize and play around with what each option does.
                 ')
      
      # Count By Radio Buttons Popover
      addPopover(session, id = "count_by", trigger = "hover",
                 title = "Count By Info",
                 content = '
                 First Match: Only the first time a response "matches" a category will be counted (with a 1).
                 All Matches: All times a response "matches" a category will be counted (with a sum).
                 ')
      
      # Multiple Categories Per Response Checkbox Popover
      addPopover(session, id = "multCtgsPerResponse", trigger = "hover",
                 title = "Multiple Categories Per Response Info",
                 content = '
                 If selected, each response can be assigned to multiple categories. 
                 If not selected, each response will be assigned to the first category it "matches".
                 ')
      
      # Add Category Popover
      addPopover(session, id = "ctgAdd", trigger = "hover",
                 title = "Category Info",
                 content = "
                 A Category is a general theme that survey responses can belong to. 
                 Each Category has a name and Rules to control how/when responses would 
                 fall under it.                  
                 ")
      
      # Default Category Popover
      addPopover(session, id = "defaultName", trigger = "hover",
                 title = "Default Category Info",
                 content = "
                 Every response will, by default, belong to this category, 
                 unless assigned to any other Category.
                 ")
      
    } else {
      
      #### * Remove Tooltips ####
      
      removePopover(session, id = "import_title")
      removePopover(session, id = "categorization_options_title")
      removePopover(session, id = "count_by")
      removePopover(session, id = "multCtgsPerResponse")
      removePopover(session, id = "defaultName")
      removePopover(session, id = "ctgAdd")
    }
  })
  
  
  #### Observe Event -- Update All Static RV Plot Colors ####
  ## Save each non-dynamic color input from Plot Customizations to RV
  ## (Need this because the colourInputs cannot be saved to the server)
  observeEvent(input$glAllColor, {
    rv$plot_colors_static$grid_lines_all <- input$glAllColor
  }) 
  observeEvent(input$glXAllColor, {
    rv$plot_colors_static$grid_lines_x_all <- input$glXAllColor
  }) 
  observeEvent(input$glMajorXColor, {
    rv$plot_colors_static$grid_lines_x_major <- input$glMajorXColor
  }) 
  observeEvent(input$glMinorXColor, {
    rv$plot_colors_static$grid_lines_x_minor <- input$glMinorXColor
  }) 
  observeEvent(input$glYAllColor, {
    rv$plot_colors_static$grid_lines_y_all <- input$glYAllColor
  }) 
  observeEvent(input$glMajorYColor, {
    rv$plot_colors_static$grid_lines_y_major <- input$glMajorYColor
  }) 
  observeEvent(input$glMinorYColor, {
    rv$plot_colors_static$grid_lines_y_minor <- input$glMinorYColor
  }) 
  observeEvent(input$plotBackgroundFill, {
    rv$plot_colors_static$plot_background_fill <- input$plotBackgroundFill
  }) 
  observeEvent(input$plotBackgroundColor, {
    rv$plot_colors_static$plot_background_color <- input$plotBackgroundColor
  }) 
  observeEvent(input$plotBackgroundColor, {
    rv$plot_colors_static$plotBackgroundColor<- input$plotBackgroundColor
  }) 
  observeEvent(input$panelBackgroundFill, {
    rv$plot_colors_static$panel_background_fill <- input$panelBackgroundFill
  }) 
  observeEvent(input$panelBackgroundColor, {
    rv$plot_colors_static$panel_background_color <- input$panelBackgroundColor
  }) 
  
  
  #### Observe Event -- Update Pushed ####
  ## Changes rv$show_results to true when "Update" is pushed if there are categories
  ## (This will show the Plot, Table, and Customizations)
  observeEvent(input$update, {
    rv$show_results <- ifelse(rv$num_ctgs > 0, TRUE, FALSE)
  })
  
  
  
  
  #### OUTPUT FUNTIONS ####
  
  
  
  
  #### Event Reactive -- fileValid ####
  ## Returns true if uploaded file is an accepted type
  output$fileValid <- eventReactive(input$responses, {
    is_valid <- !is.null(getData()) # Is valid if data does not contain an error code
    rv$file_is_valid <- is_valid    # Save as a reactive value
    return(is_valid)
  })
  outputOptions(output, 'fileValid', suspendWhenHidden = FALSE)
  
  
  #### Event Reactive -- showResults ####
  ## Shows the Plot, Table, and Customizations when "Update" is pushed
  output$showResults <- eventReactive(rv$show_results, {
    return(rv$show_results)
  })
  outputOptions(output, 'showResults', suspendWhenHidden = FALSE)
  
  
  #### Event Reactive -- displayError ####
  ## When the RV error value changes, this will trigger the displaying of the error
  output$displayError <- eventReactive(rv$error_to_display, {
    # Returns TRUE if the error to display is not empty
    return(!is.null(rv$error_to_display))
  })
  outputOptions(output, 'displayError', suspendWhenHidden = FALSE)
  
  
  #### Event Reactive -- plotDisplayed ####
  ## Returns true if plot has been created
  output$plotDisplayed <- eventReactive(input$update, {
    # Returns TRUE if there's no error code (a number)
    return(!is.null(plotGG()))
  })
  outputOptions(output, 'plotDisplayed', suspendWhenHidden = FALSE)
  
  
  #### Event Reactive -- tableDisplayed ####
  ## Returns true if table has been created
  output$tableDisplayed <- eventReactive(input$update, {
    # Returns TRUE if there's no error code (a number)
    return(!is.null(plotTable()))
  })
  outputOptions(output, 'tableDisplayed', suspendWhenHidden = FALSE)
  
  
  
  #### Render Plot ####
  ## Displays plot of responses when the RV value changes
  output$plot <- renderPlot({
    # Render plot if the RV plot is not NULL
    if (!is.null(rv$plot)) { return(rv$plot) }
  })


  #### Render Table ####
  ## Displays table of responses
  output$table <- renderDataTable({
    # Render table if the RV table is not NULL
    if (!is.null(rv$table)) { return(rv$table) }
  }, options = list(pageLength = 5))
  
  
  
  #### Render Error Text ####
  ## Displays the RV error under the Update and Save buttons
  output$error_text <- renderText({
    return(rv$error_to_display)
  })
  
  
  #### Render Aesthetic Customization UI's ####
  ## Renders UI's based on whether Uniform or Individual aesthetics is selected in the 'aesthetics' customization tab
  output$aesUIs <- renderUI({
    
    ## Individual aesthetics
    if (input$aesOptions == "Individual Bar Colors") {
      
      # Data frame to hold all of the id's and values for creating the new UI's
      # (For creating the UI's and observe events later on)
      new_ui_vals <- data.frame(
        id = character(),
        ctg_name = character(),
        fill_id = character(),
        color_id = character(),
        no_fill_id = character(),
        no_color_id = character(),
        linewidth_id = character(),
        linetype_id = character(),
        fill_init_val = character(),
        color_init_val = character(),
        stringsAsFactors = FALSE
      )
      
      ## Add Default Category UI values to the new_ui_vals data frame
      new_def_vals <- lapply(1:1, function(i) {
        
        # Initial ColorInput UI Values
        def_id <- "default_aes_indiv"
        def_fill_id <- "default_aes_indiv_fill" 
        def_color_id <- "default_aes_indiv_color" 
        def_fill_init <- "gray"
        def_color_init <- "black"
        
        # Create and fill a new row of values
        vals <- data.frame(
          id = def_id,
          ctg_name = input$defaultName,
          fill_id = def_fill_id,
          color_id = def_color_id,
          no_fill_id = "default_aes_indiv_no_fill",
          no_color_id = "default_aes_indiv_no_color",
          linewidth_id = "default_aes_indiv_line_width",
          linetype_id = "default_aes_indiv_line_type",
          fill_init_val = def_fill_init,
          color_init_val = def_color_init,
          stringsAsFactors = FALSE
        )
        # Return the new row of values
        return(vals)
      })
      new_ui_vals <-  bind_rows(new_ui_vals, new_def_vals)
      # Add all other Category UI values to the new_ui_vals data frame 
      # (if there are any other categories)
      if (nrow(rv$all_categories) > 0) {
        new_ctg_vars <- lapply(1:nrow(rv$all_categories), function(i) {
          
          # Initial Variables
          current_ctg_id <- rv$all_categories$ctg_id[i]
          current_ctg_name <- rv$all_categories$ctg_name[i]
          aes_id <- paste0(current_ctg_id, "_aes_indiv")
          aes_fill_id <- paste0(current_ctg_id, "_aes_indiv_fill")  
          aes_color_id <- paste0(current_ctg_id, "_aes_indiv_color")
          aes_fill_init <- "gray"
          aes_color_init <- "black"
          
          # Proceed if the current category has at least one rule
          if (rv$all_categories$ctg_num_rules[i] > 0) {
            
            # Generate new row of Default Category UI values and id's
            new_ctg_vars <- data.frame(
              id = aes_id,
              ctg_name = current_ctg_name,
              fill_id = aes_fill_id,
              color_id = aes_color_id,
              no_fill_id = paste0(aes_id, "_no_fill"),
              no_color_id = paste0(aes_id, "_no_color"),
              linewidth_id = paste0(aes_id, "_line_width"),
              linetype_id = paste0(aes_id, "_line_type"),
              fill_init_val = aes_fill_init,
              color_init_val =  aes_color_init,
              stringsAsFactors = FALSE
            )
          }
        })
        new_ui_vals <-  bind_rows(new_ui_vals, new_ctg_vars)
      }
      
      ## Create a list of UI's for each Category (row) in the RV indiv_aes_ui_vals df
      uis_list <- lapply(1:nrow(new_ui_vals), function(i) {
        
        # Current Category UI Variables
        aes_id <- new_ui_vals$id[i]
        aes_ctg_name <- new_ui_vals$ctg_name[i]
        aes_fill_id <- new_ui_vals$fill_id[i]
        aes_color_id <- new_ui_vals$color_id[i]
        aes_no_fill_id <- new_ui_vals$no_fill_id[i]
        aes_no_color_id <- new_ui_vals$no_color_id[i]
        aes_linewidth_id <- new_ui_vals$linewidth_id[i]
        aes_linetype_id <- new_ui_vals$linetype_id[i]
        aes_fill_init_val <- new_ui_vals$fill_init_val[i]
        aes_color_init_val <- new_ui_vals$color_init_val[i]
        header_text <- if_else(i == 1, 
                               paste0("Default Category: ", aes_ctg_name),
                               paste0("Category ", i-1, ": ", aes_ctg_name))
        
        # Create list of new UI's
        list <- list(
          # Category Header
          hr(),
          h4(header_text),
          br(),
          flowLayout(
            # Fill
            colourpicker::colourInput(aes_fill_id, label = "Fill", value = aes_fill_init_val, 
                                      allowTransparent = TRUE, returnName = TRUE),
            # No Fill?
            checkboxInput(aes_no_fill_id, label = "No Fill", value = FALSE),
            # No Color?
            checkboxInput(aes_no_color_id, label = "No Color", value = TRUE),
            conditionalPanel(
              condition = paste0("!input.",aes_no_color_id),
              flowLayout(
                # Color
                colourpicker::colourInput(aes_color_id, label = "Color", value = aes_color_init_val,
                                          allowTransparent = TRUE, returnName = TRUE),
                # Line Width
                numericInput(aes_linewidth_id, label = "Line Width", 
                             value = 0.5, min = 0, max = 10, step = 0.01),
                # Line Type
                selectInput(aes_linetype_id, label = "Line Type", selected = "solid", 
                            choices = c("blank","solid","dashed","dotted","dotdash","longdash","twodash"))
              )
            )
          )
        )
        # Return the new UI's
        return(list)
      })
      
      ## Render the UI's
      do.call(tagList, uis_list)
      
    } else {
      
      ## Uniform aesthetics
      if (input$aesOptions == "Uniform Bar Colors") {
        
        # Unique id for all ui's
        aes_id <- paste0("aes_uniform")
        
        # Render this list of ui's
        unif_uis_list <- tagList(
          hr(),
          flowLayout(
            
            # Fill
            colourpicker::colourInput(paste0(aes_id, "_fill"), label = "Fill", value = "gray", 
                                      allowTransparent = TRUE, returnName = TRUE),
            
            # No Fill?
            checkboxInput(paste0(aes_id, "_no_fill"), label = "No Fill", value = FALSE),
            
            # No Color?
            checkboxInput(paste0(aes_id, "_no_color"), label = "No Color", value = TRUE),
            conditionalPanel(
              condition = paste0("!input.",aes_id, "_no_color"),
              flowLayout(
                # Color
                colourpicker::colourInput(paste0(aes_id, "_color"), label = "Color", value = "black",
                                          allowTransparent = TRUE, returnName = TRUE),
                # Line Width
                numericInput(paste0(aes_id, "_line_width"), label = "Line Width", 
                             value = 0.5, min = 0, max = 10, step = 0.01),
                # Line Type
                selectInput(paste0(aes_id, "_line_type"), label = "Line Type", selected = "solid", 
                            choices = c("blank","solid","dashed","dotted","dotdash","longdash","twodash"))
              )
            )
          )
        )
        
        ## Render the UI's
        do.call(tagList, unif_uis_list)
      }
    }
  })
  
  # #### Render Categorization Demo Image ###
  # output$ctgzn_img <- renderImage(deleteFile = FALSE, {
  #   return(list(
  #     src = "./www/Categorization_Options_Demo.PNG",
  #     contentType = "image/png",
  #     alt = "Image"
  #   ))
  # })
  
  
  # #### Render RV Table ####
  # output$show_rvs <- renderPrint({
  #   # Show all reactive values as a data table
  #   print(reactiveValuesToList(rv))
  # })
  
  
}


