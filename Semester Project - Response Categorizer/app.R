# RESPONSE CATEGORIZER

# Categorizes uploaded responses into specified categories based on specified key words

pacman::p_load(shiny,tibble,readr,haven,readxl,dplyr,ggplot2,tidyverse,pander,stringr,tidytext,stringi)




## MODULES




## Rule Module UI Function (Inner)

ruleUI <- function(id, label) {
  # Makes a namespace function using the provided id
  ns <- NS(id)
  # Holds all of the ui's for the rule
  tagList(
    # Key words text input
    textInput(paste0(ns("keywords")), "",
              label = label),
    # Sorting Options list
    selectInput(ns("sortOptions"),
                label = "Sorting Options",
                choices = c("Exactly","Contains","Begins With","Ends With"),
                selected = "Exactly",
                width = 120),
    # Standardize check box that will, when checked, set all keywords and responses to lowercase
    checkboxInput(ns("standardize"), "Standardize All to Lowercase", FALSE)
  )
}


## Category Module UI Function (Outer)

categoryUI <- function(id, label, ctg_num) {
  # Makes a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    # Enclosed with a div for removal
    div(id = id,
      # The category name
      textInput(ns("ctg_name"), 
                paste0("Category ", ctg_num), 
                label = label),
      # Button to Add Rules
      actionButton(ns("add_rule"), "Add Rule"),
      # Rules UI
      wellPanel(
        ruleUI(id = paste0("rule_", ctg_num),
               label = paste0("Insert Key Words for Rule ", ctg_num))
      )
    )
  )
}


## Rule Module Server Function (Inner)

rule <- function(input, output, session) {
  return(input)
}


## Category Module UI Function (Outer)
category <- function(input, output, session, ctg_num) {
  ruleResult <- callModule(categoryUI, "category1", ctg_num)
  return(ruleResult)
}


#### UI


ui <- fluidPage(

  ## INPUT FUNCTIONS
  
  sidebarPanel(
  # Browse for responses and saves the datapath  
  fileInput("responses", "Import Responses", 
            buttonLabel = "Browse",
            placeholder = "Please upload an Excel file",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".xlsx")
            ),
  
  # These allow other categories to be added / removed when file is uploaded
  conditionalPanel(
    condition = "output.fileUploaded",
    # Button to add a category
    actionButton("ctgAdd", "Add Category"),
      # Default Category and Remove button
      conditionalPanel(
        condition = "input.ctgAdd",
        wellPanel(
          # Name of Default Category
          textInput("defaultName", "Default",
                    label = "Default Category Name"),
          # Button to Remove a category
          tags$div(id = "removeCtgButton")
          
        ),
        
        # To hold all of the categories and their rules
        fluidRow(
          wellPanel(
            tags$div(id = "categories")
          )
        )
      )
    )
  ),
  
  # Makes a button labeled "Update""
  actionButton("update", "Update"),
 
  mainPanel(
    plotOutput("plot"),
    
    # Creates the UI's for Plot Customization
    conditionalPanel(
      condition = "output.plotDisplayed",
      # Main Title
      textInput("mainTitle", "Main Title", ""),
      # X Label
      textInput("xLabel", "X Axis Title", ""),
      # Y Label
      textInput("yLabel", "Y Axis Title", "")
    ),

    # Adds a check box to view responses for the Default Category
    conditionalPanel(
      condition = "input.ctgAdd",
      checkboxInput("defaultCheck", "Default Category")
    ),
    
    # Makes a tag to signify where to put buttons to view responses for categories
    tags$div(id = 'categoryButtons')
    
  )
)



#### SERVER



server <- function(input, output, session) {
  
  
  ## REACTIVE FUNCTIONS

  
  # Reads in file
  getData <- reactive({
    
    if (is.null(input$responses)) {
      print("No loaded File")
      return(NULL)
    }
    else {
      inFile <- input$responses
      print(inFile$datapath)
      data <- readxl::read_excel(inFile$datapath, col_names = "Responses")
      data <- data.frame(data, stringsAsFactors = FALSE)
      print("getData")
      return(data)
    }
  })

  # Sorts the data
  sortData_GG <- reactive({
    data <- getData()
    
    # Skip if data is NULL or no Categories were added
    if (is.null(data) | !input$ctgAdd) {
      return(data)
    }
    
    
    # Number of categories
    N <- input$ctgAdd
    print(paste0("Num of Ctg's: ", N))
    # Number of responses
    r <- nrow(data)
    print(paste0("Num of Responses: ", r))
    # Create an empty data frame to be added with key words and sorting options
    keywords <- data.frame(matrix(nrow = 1, ncol = 2)) %>% 
      `colnames<-`(c("Keywords","Sort_Option"))

    
    ### Loop through each category to create needed data frames
    for (c in 1:N) {
      # Proceed if this category has not been removed
      if (eval(parse(text = paste0("input$ctgCheck", c))) == FALSE) {
        
        # Current category name as a string
        currentCtgName <- eval(parse(text = paste0("input$ctgName", c)))
        print(paste0("Current Category: ", currentCtgName))
        
        ## Add a new column to the responses data to hold whether the response lies within this category
        new_column = data.frame(matrix(nrow = r, ncol = 1)) %>% 
          `colnames<-`(paste0(currentCtgName))
        new_column[,1] <- 0 # Values set to zero and not FALSE for functionality
        data <- bind_cols(data, new_column)
        
        ## Will add rows to the Keywords data frame
        words <- eval(parse(text = paste0("input$ruleWords", c))) %>% 
          stri_extract_all_words() %>%              # Turn string of keywords into a list, separated into words
          data.frame(stringsAsFactors = FALSE) %>%  # Turn list into a data frame
          `colnames<-`("Keywords") %>%              # Change the name of the first column
          mutate(Sort_Option = eval(parse(text = paste0("input$sortOptions", c)))) # New column of sorting options
        # Add the words to the keywords data frame
        keywords <- bind_rows(keywords, words)

      }
      else {
        print(paste0("Category ",c, " has been removed"))
      }
    }
    
    print(keywords)
    print(head(data, r / 4))
    return(data)
    
  })
  
  # Sorts the data 
  # sortData_Table <- reactive({
  #   
  #   data <- getData()
  #   print("data table")
  #   str(data)
  #   
  #   # How many responses there currently are
  #   rows <- nrow(data)
  #   # How many categories there currently are (not including the default category)
  #   N <- input$ctgAdd 
  #   
  #   # Check which categories have been removed to skip that ctgAdd number in the code below
  #   ctgRemoved <- data.frame(matrix(nrow = N))
  #   ctgRemoved <- rep(FALSE, N)
  #   
  #   # Keeps track of how many categories have been removed
  #   numRemoved <- 0
  #   
  #   d <- 1
  #   while (d <= N) {
  #     if ( eval(parse(text = paste0("input$ctgCheck", d))) == TRUE ) {
  #       ctgRemoved[d] <- TRUE
  #       numRemoved <- numRemoved + 1
  #     }
  #     d <- d + 1
  #   }
  #   
  #   # Makes a dataframe "sortedtxt" that holds every response separated into single, lowercase words
  #   txt <- data_frame(line = 1:rows, text = data[,1])
  #   
  #   sortedtxt <- txt %>% 
  #     unnest_tokens(word, text)           # Separate each comment into individual, lowercase words
  #   sortedtxt <- as.data.frame(sortedtxt) 
  #   # Total Categories counter
  #   i <- 1
  #   # Total Words of all Responses
  #   s <- nrow(sortedtxt)
  #   
  #   # Keeps a numerical count of the number of instances a category appears to plot later
  #   u <- N + 1
  #   ctgCount <- data.frame(matrix(nrow = u, ncol = 2))
  #   ctgCount[,1]  <- rep(0, u)                 # Category Count
  #   ctgCount[,2]  <- rep(input$defaultName, u) # Category Name
  #   ctgCount[u,1] <- rows+1                    # By default, the num in default category = num of responses
  #   
  #   # Data frame containing the response and the category
  #   tableData <- data.frame(matrix(nrow = rows, ncol = 2))
  #   tableData[,1] <- rep("blank", rows)
  #   tableData[,2] <- rep(input$defaultName, rows)
  #   
  #   # Sorts Responses according to key words in each category
  #   
  #   while (i <= N) {  
  #     
  #     # Continue if the current category has NOT been removed
  #     if (ctgRemoved[i] == FALSE) {
  #       
  #       # Get text from category k's key words and make it similar to sorted txt
  #       w <- eval(parse(text=paste0("input$ctgWords", i)))
  #       words <- data_frame(line = 1, text = w)
  #       keyWords <- words %>% 
  #         unnest_tokens(word, text)           # Separate the key words into individual words
  #       keyWords <- as.data.frame(keyWords)
  #       
  #       # How many key words there are
  #       n <- nrow(keyWords)
  #       # Holds the current category name as a string
  #       currentCtgName <- eval(parse(text=paste0("input$ctgName", i)))
  #       # Add the category name to the ctgCount dataframe
  #       ctgCount[i,2] <- currentCtgName
  #       # Save just the text columns of sorted text and key words
  #       st <- sortedtxt$word
  #       kw <- keyWords$word
  #       # Reset Counters
  #       j <- 1
  #       k <- 1
  #       
  #       # Now we loop through sortedtxt, and if any match a key word, 
  #       # a string containing the category name is added to "c"
  #       
  #       while (j <= s) {
  #         if (k <= rows) { 
  #           if (!is.na(st[j])) { 
  #             hasCategory <- FALSE
  #             for (l in 1:n) {
  #               if (st[j] == kw[l]) {
  #                 ctgCount[i,1] <- ctgCount[i,1] + 1     # Increase count for current category
  #                 tableData[k,2] <- currentCtgName       # Save category name to table data
  #                 hasCategory <- TRUE
  #               }
  #             } # Number of key words
  #             if (hasCategory) {
  #               ctgCount[u,1] <- ctgCount[u,1] - 1   # Decrease count for default category
  #             }
  #           }
  #           tableData[k,1] <- txt[k,2]             # Save response to table data
  #           k <- k + 1
  #         } # Number of responses
  #         j <- j + 1
  #       } # Number of all words
  #     }
  #     i <- i + 1
  #   } # Number of categories
  #   
  #   
  #   # Graph the results
  #   
  #   # Make ctgCount a data frame
  #   ctgCount <- as.data.frame(ctgCount) %>% 
  #     group_by(X2) %>% 
  #     filter(X1 != 0)
  #   
  #   return(tableData)
  #   
  # })
  
  # Will plot the ggplot to show how many responses are in which category
  plotGG <- reactive({
    data <- sortData_GG()  
    
    # Skip if data is null, no categories have been made
    if (is.null(data) | !input$ctgAdd) {
      return(NULL)
    }

    print("plotGG")

    # ggplot(data) +
    #   geom_col(aes(X2, y = X1)) + 
    #   geom_text(aes(x = data$X2, y = data$X1 + 2.5, label = data$X1)) + 
    #   labs(title = input$mainTitle, x = input$xTitle, y = input$yTitle) + 
    #   theme_grey()
    
    return(TRUE)
    
  })
  
  
  # Updates input when "Update" button is pushed
  updatePushed_plotGG <- eventReactive(input$update, {
    print("Update Pushed")
    # Returns the graph
    plotGG()
  })

  
  ## OUTPUT FUNTIONS
  
  
  ## Adds the Category Name text input and Add Rule button when the Add Category button is pushed
  observeEvent(input$ctgAdd, {
    # Make id's for all of the ui's and the category itself
    id_add <- paste0(input$ctgAdd)
    ctg_id <- paste0("ctg_", id_add)
    remove_ctg_id <- paste0("remove_ctg", id_add)
    remove_ctg_button_id <- paste("remove_ctg_button_", id_add)

    # Add the category ui's
    insertUI("#categories",
             "beforeEnd",
             tags$div(
               # Has this id for removal
               id = ctg_id,
               # Include all category UI's
               categoryUI(id = paste0("category", id_add), 
                          label = paste0("Category ",id_add," Name"), 
                          ctg_num = id_add)))
    
    # Add the remove category button
    insertUI(
      selector = "#removeCtgButton",
      ui = tags$div(
        id = remove_ctg_id,
        # Check box to signify which category to remove
        actionButton(remove_ctg_button_id, paste0("Remove Category ", id_add))
      )
    )

    ## Remove all elements in category when remove category button is pushed
    observeEvent(input[[remove_ctg_button_id]], {
      # Removes all ui's within the category
      removeUI(
          selector = paste0("#", ctg_id)
      )
      # Remove the remove button
      removeUI(
        selector = paste0("#", remove_ctg_id)
      )
    })
  })
  
  
  
  
  
  # # Adds a category name text input
  # observeEvent(input$ctgAdd, {
  #   insertUI(
  #     selector = '#placeholder',
  #     ui =  
  #       # The category name
  #       textInput(paste0("ctgName", input$ctgAdd), paste0("Category ", input$ctgAdd),
  #                 label = paste0("Category ", input$ctgAdd, " Name"))
  #   )
  # })
  # # Adds an Add Rule Button
  # observeEvent(input$ctgAdd, {
  #   insertUI(
  #     selector = '#addRuleButton',
  #     ui =  
  #       # The add rule button for the category
  #       actionButton(paste0("addRule", input$ctgAdd), "Add Rule")
  #   )
  # })
  # # Adds rule key words text input
  # observeEvent(eval(parse(text = paste0("input$addRule", input$ctgAdd))), {
  #   insertUI(
  #     selector = '#keywords',
  #     ui =  
  #       # The category key words
  #       textInput(paste0("ruleWords", input$ctgAdd, paste0("input$addRule", input$ctgAdd)), "",
  #                 label = paste0("Insert Key Words for Rule ", 
  #                                eval(parse(text = paste0("input$addRule", input$ctgAdd)))))
  #   )
  # })
  # # Adds a Sorting Options list for rule
  # observeEvent(eval(parse(text = paste0("input$addRule", input$ctgAdd))), {
  #   insertUI(
  #     selector = '#placeholder',
  #     ui =  
  #       # List to choose from of different ways to sort through the key words
  #       selectInput(paste0("sortOptions", input$ctgAdd, input$addRule),
  #                   label = "Sorting Options",
  #                   choices = c("Exactly","Contains","Begins With","Ends With"),
  #                   selected = "Exactly",
  #                   width = 120)
  #   )
  # })
  # # Adds an option to standardize key words and responses in rule
  # observeEvent(eval(parse(text = paste0("input$addRule", input$ctgAdd))), {
  #   insertUI(
  #     selector = '#placeholder',
  #     ui =  
  #       # Check box that will, when checked, set all keywords and responses to lowercase
  #       checkboxInput(paste0("optionStandardize", paste0("input$addRule", input$ctgAdd)), "Standardize All to Lowercase", FALSE)
  #   )
  # })
  # # Adds a single check box for each category for removing
  # observeEvent(input$ctgAdd, {
  #   insertUI(
  #     selector = '#removeCtgCheckBoxes',
  #     ui =  
  #       # Check box to signify which category to delete
  #       checkboxInput(paste0("ctgCheck",input$ctgAdd), paste0("Remove Category ", input$ctgAdd), FALSE)
  #   )
  # })
  # # Adds a single check box for each category to view responses for a category
  # observeEvent(input$ctgAdd, {
  #   insertUI(
  #     selector = '#categoryButtons',
  #     where = "beforeBegin",
  #     ui =  
  #       # Check box to signify which category to delete
  #       checkboxInput(paste0("tableCheck",input$ctgAdd), paste0("Category ", input$ctgAdd), FALSE)
  #   )
  # })
  
  # Removes appropriate category if the Remove button is pushed
  # Returns a logical data frame
  # observeEvent(input$ctgRemove, {
  #   n <- input$ctgAdd 
  #   # Holds boolean values to indicate whether or not a category has been removed
  #   removed <- data.frame(matrix(nrow = n))
  #   removed <- rep(FALSE, n)
  #   # Loops through and removes categories whose remove check boxes are selected
  #   for (i in 1:n) {
  #     # If corresponding check box is selected, delete the appropriate category
  #     if (eval(parse(text = paste0("input$ctgCheck",i)))) {
  #       # Delete the category and everything associated with it 
  #       removeUI(
  #         selector = sprintf('.shiny-input-container:has(#%s)', paste0("ctgName",i)),
  #         multiple = TRUE, immediate = TRUE
  #       )
  #       removeUI(
  #         selector = sprintf('.shiny-input-container:has(#%s)', paste0("ctgCheck",i)),
  #         multiple = TRUE, immediate = TRUE
  #       )
  #       removeUI(
  #         selector = paste0('div:has(> #addRule', i, ')'),
  #         multiple = TRUE, immediate = TRUE
  #       )
  #       removeUI(
  #         selector = sprintf('.shiny-input-container:has(#%s)', paste0("ruleWords",i)),
  #         multiple = TRUE, immediate = TRUE
  #       )
  #       removeUI(
  #         selector = sprintf('.shiny-input-container:has(#%s)', paste0("sortOptions",i)),
  #         multiple = TRUE, immediate = TRUE
  #       )
  #       removeUI(
  #         selector = sprintf('.shiny-input-container:has(#%s)', paste0("tableCheck",i)),
  #         multiple = TRUE, immediate = TRUE
  #       )
  #     }
  #   }
  # })
  
  # Returns true if file has been uploaded
  output$fileUploaded <- eventReactive(input$update, {
    return(!is.null(getData()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  # Returns true if plot has been created
  output$plotDisplayed <- eventReactive(input$update, {
    return(!is.null(plotGG()))
  })
  outputOptions(output, 'plotDisplayed', suspendWhenHidden = FALSE)
  
  # Displays plot of responses
  output$plot <- renderPlot({
    updatePushed_plotGG()
    print("renderPlot")
  })

}
shinyApp(ui = ui, server = server)










