### RESPONSE CATEGORIZER APP
### - Michael Neff - 
###
### Categorizes uploaded responses into specified categories based on specified key words



#### Libraries ####

pacman::p_load(shiny,shinyjs,shinyBS,shinybusy, shinyalert,tibble,readr,haven,readxl,dplyr,ggplot2,ggthemes,
               tidyverse,rlist,stringr,colourpicker,stringi,topicmodels,tm,quanteda,slam)




##### ----- UI ----- #####

ui <- fluidPage(

  ##### INPUT FUNCTIONS #####
  
  useShinyjs(), # Set up shinyjs for clicking buttons with code
  useShinyalert(), # For pop-up message modals (Reference: https://deanattali.com/blog/shinyalert-package/)
  
  # Add Busy Spinner
  add_busy_spinner(
    spin = "semipolar",
    color = "#337ab7",
    timeout = 100,
    position = "top-right",
    onstart = FALSE,
    margins = c(10, 10), # first element is distance from top/bottom, second element distance from right/left.
    height = "50px",
    width = "50px"
  ),
  
  # Title
  fluidRow(
    column(width = 5, h1(em("Survey Response Categorizer"))),
    column(width = 1, actionLink('info_btn_main_title', '', icon = icon('question-circle')))
  ),

  #### Sidebar Panel ####
  sidebarPanel(
    
    #### * Browse File Button ####
    fluidRow(
      column(width = 11,
             # Browse for responses and saves the datapath  
             fileInput("responses", "Import Responses", 
                       buttonLabel = "Browse",
                       placeholder = "Excel / CSV",
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".xlsx", 
                                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
             ),
      ),
      column(width = 1,
             actionLink("info_btn_import_responses", "", icon = icon("question-circle"))
      )
    ),
    
    #### * Browse File Conditional Panel ####
    # These allow other categories to be added / removed when file is uploaded
    conditionalPanel(
      condition = "output.fileValid",
      
      #### * * Show Tooltips ####
      checkboxInput("show_tooltips", "Show Tooltips", value = TRUE),
      
      fluidRow(
        column(width = 11,
               #### * * Categorize for Me Button ####
               actionButton("find_categories", "Find Categories for Me",
                            style = "color: #fff; background-color: #337ab7; border-color: #2d6a9f"),
        ),
        column(width = 1,
               actionLink("info_btn_find_categories", "", icon = icon("question-circle"))        
        )
      ),

      br(),
      br(),
      
      #### * * Add Category Button ####
      actionButton("ctgAdd", "Add Category"),
    
      #### * * Add Category Conditional Panel ####
      conditionalPanel(
        condition = "input.ctgAdd",
        wellPanel(
          
          #### * * * Default Category Name ####
          textInput("defaultName", "Default",
                    label = "Default Category Name"),
          
          #### * * * Remove Category Buttons Placeholder ####
          tags$div(id = "removeCtgButton")
          
        ),
        
        #### * * Multiple Categories Checkbox ####
        # Option to signify whether to have one or multiple categories per response
        checkboxInput("multCtgsPerResponse", 
                      label = "Multiple Categories per Response", value = TRUE),
        
        #### * * Category Placeholder ####
        # To hold all of the categories and their rules
        fluidRow(
          wellPanel(
            tags$div(id = "categories")
          )
        )
      )
    )
  ),
  
  #### Update Button ####
  # Makes a button labeled "Update""
  actionButton("update", "Update"),
 
  #### Main Panel ####
  mainPanel(
    
    #### * Tutorial Video Pop-ups ####
    
    ## All Tutorial Videos ##
    ## (Contains Action Links to all tutorial video modals (pop-ups) with embedded videos
    bsModal("tutorial_popup_all", "All Tutorials",
            trigger = "tutorial_link_all", size = "s",
            br()
            # Links to All Tutorial Videos
            # actionLink("tutorial_link_", "Watch ")
    ),
    
    ## Tutorial Video - ##
    
    
    
    #### * Info Button Pop-ups ####
    
    ## Template info modal
    # bsModal("info_popup_", " Information",
    #         trigger = "info_btn_", size = "s",
    #         # Info Text
    #         tags$h5(''),
    #         br(),
    #         ## Link to specific Tutorial Video
    #         # actionLink("tutorial_link_", "Watch Tutorial: ")
    #         ## Link to All Tutorial Videos Pop-up
    #         # actionLink("tutorial_link_all", "View All Tutorials")
    # ),
    
    
    ## Template Info button
    # actionLink("info_btn_topic_finder", "", icon = icon("question-circle"))
    
    
    ## Info - Main Title ##
    bsModal("info_popup_main_title", "Main App Information",
            trigger = "info_btn_main_title", size = "s",
            # Info Text
            tags$h5("This R Shiny App enables you to easily organize open-ended responses from a survey question into categories and visualize them for your reports."),
            tags$h5("To learn how to use it, click any info button for an explanation, or watch these tutorials:"),
            # Link to All Tutorial Videos Pop-up
            actionLink("tutorial_link_all", "View All Tutorials"),
            tags$h5("Happy categorizing!")
    ),
    
    ## Info - Import Responses ##
    bsModal("info_popup_import_responses", "Importing Information",
            trigger = "info_btn_import_responses", size = "s",
            # Info Text
            tags$h5("Allows you upload a CSV or Excel file. You can either: "),
            tags$h5("   1) Browse for the file from your local computer by pressing the “Browse” button or "),
            tags$h5("   2) Drag and drop the file next to the button."),
            br()
            ## Link to specific Tutorial Video
            # actionLink("tutorial_link_", "Watch Tutorial: ")
            ## Link to All Tutorial Videos Pop-up
            # actionLink("tutorial_link_all", "View All Tutorials")
    ),
    
    ## Info - Find Categories for Me Button ##
    bsModal("info_popup_find_categories", '"Find Categories for Me" Information',
            trigger = "info_btn_find_categories", size = "s",
            # Info Text
            tags$h5("A great place to start if you don’t know what categories to add."),
            tags$h5("The button opens the Topic Finder window, which will use machine learning to find abstract topics across responses.
                    It will also convert a topic to a category and add it to your collection (as if it were entered manually)."),
            br()
            ## Link to specific Tutorial Video
            # actionLink("tutorial_link_", "Watch Tutorial: ")
            ## Link to All Tutorial Videos Pop-up
            # actionLink("tutorial_link_all", "View All Tutorials")
    ),
    
    ## Info -	Topic Finder Title ##
    # bsModal("info_popup_topic_finder", "Topic Finder Information",
    #         trigger = "info_btn_topic_finder", size = "s",
    #         # Info Text
    #         # Reference for embedding link: https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny
    #         tagList(
    #           'The Topic Finder uses the “topicmodels” R package to clean the responses and implement LDA and CTM
    #           Topic Modeling Machine Learning models to identify a specified number of abstract topics across all
    #           responses (see ',
    #           a("here", href = "https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf"),
    #           ' for full documentation).'
    #         ),
    #         tags$h5("Although two separate models are fitted (one LDA and one CTM), only the model with the lowest
    #                 mean entropy is used in prediction. (The mean entropy measures how widely the model's topics are
    #                 distributed across the responses. Therefore, the lower the mean entropy, the more precise and
    #                 descriptive the predicted topics are.) "),
    #         br()
    #         ## Link to specific Tutorial Video
    #         # actionLink("tutorial_link_", "Watch Tutorial: ")
    #         ## Link to All Tutorial Videos Pop-up
    #         # actionLink("tutorial_link_all", "View All Tutorials")
    # ),
    
    ## * Info -	Number Topics to Find ##
    # bsModal("info_popup_num_topics_to_find", "Number of Topics to Find Information",
    #         trigger = "info_btn_num_topics_to_find", size = "s",
    #         # Info Text
    #         tags$h5(''),
    #         br(),
    #         ## Link to specific Tutorial Video
    #         # actionLink("tutorial_link_", "Watch Tutorial: ")
    #         ## Link to All Tutorial Videos Pop-up
    #         # actionLink("tutorial_link_all", "View All Tutorials")
    # ),
    
    ## * Info -	Words Not to Include ##
    
    ## * Info -	Convert Topics Button ##
    
    ## * Info -	Results TabPanel Title ##
    
    ## * Info -	Terms ##
    
    ## Info -	Default Category ##
    
    ## Info -	Multiple Categories Per Response ##
    
    ## Info -	Category Title ##
    
    ## Info -	Rule Title ##
    
    ## Info -	Plot Customizations ##
    
    
    
    
    
    #### * Topic Finder Pop-up Window ####
    ## Reference: https://ebailey78.github.io/shinyBS/docs/Modals.html
    bsModal("topic_finder", "Topic Finder", 
            trigger = "find_categories", size = "l", # Large
            
            # Topic Finder Info Button
            # actionLink("info_btn_topic_finder", "", icon = icon("question-circle")),
            
            # Main Info
            # Reference for embedding link: https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny
            tagList(
              'The Topic Finder uses the “topicmodels” R package to clean the responses and implement LDA and CTM
              Topic Modeling Machine Learning models to identify a specified number of abstract topics across all
              responses (see ',
              a("here", href = "https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf"),
              ' for full documentation).'
            ),
            tags$h5("Although two separate models are fitted (one LDA and one CTM), only the model with the lowest
                    mean entropy is used in prediction. (The mean entropy measures how widely the model's topics are
                    distributed across the responses. Therefore, the lower the mean entropy, the more precise and
                    descriptive the predicted topics are.) "),
            
            
            
            #### * * TF Buttons ####
            fluidRow(
              column(width = 2,
                     # Go Button
                     actionButton("tf_go", "Go!",
                                  style = "color: #fff; background-color: #337ab7; border-color: #2d6a9f")
              ),
              column(width = 10,
                     conditionalPanel(
                       condition = "output.topicsFound",
                       # Convert Topics Button
                       actionButton("tf_convert_topics", "Convert Topics",
                                    style = "color: #fff; background-color: #17c217; border-color: #15ad15")
                     )
              )
            ),
            #### * * TF Initial Variables ####
            wellPanel(
              
              # Number of Topics to Find
              numericInput("tf_num_topics", "Number of Topics to Find",
                           value = 5, min = 2, max = 10),

              # Words not to Include
              textInput("tf_words_not_include", "Words Not to Include",
                        value = "for, as, when, you, also")
            ),
            br(),
            
            #### * * TF Results UIs ####
            conditionalPanel(
              condition = "output.topicsFound",
              
              # Results Tabset Panel
              tabsetPanel(id = "tf_results", 
                          tabPanel("Topics",
                                   # Placeholder for Topic UIs
                                   tags$div(id = "tf_topics")
                          ),
                          tabPanel("Details",
                                   # Details Tabs
                                   navbarPage(id = "tf_details", title = "",
                                               tabPanel("View Graph",
                                                        # Output for Details Plot
                                                        plotOutput("tf_details_plot", height = "500px")
                                               ),
                                               tabPanel("View Table",
                                                        # Output for Details Data Table
                                                        dataTableOutput("tf_details_tbl"),
                                                                        # Add Vertical Scroll Bar
                                                                        # Reference: https://stackoverflow.com/questions/47505893/adding-a-vertical-and-horizontal-scroll-bar-to-the-dt-table-in-r-shiny
                                                                        style = "height:500px; overflow-y: scroll; overflow-x: scroll;"
                                               )
                                   )
                          )
              )            
            )
    ), 
    
    br(),
    
    #### * Update Conditional Panel ####
    # The Plot and Table tabsets will appear when a category has been added
    conditionalPanel(
      condition = "input.update",
      
      #### * * Plot and Table Tabset ####
      tabsetPanel(
        
        #### * * * Plot Tab ####
        # Tab for the main plot and customizations
        tabPanel("Plot", br(), 
                 
                 #### * * * * Main Plot Output ####
                 plotOutput("plot"),
                 br(),
                 
                 #### * * * * Main Plot Conditional Panel ####
                 # Displays the UI's for Plot Customizations as long as a plot is displayed
                 conditionalPanel(
                   condition = "output.plotDisplayed",
                   
                   # Plot Customizations Panel
                   wellPanel(
                     h2("Plot Customizations"),
                     h4(em('Push "Update" to apply changes')),
                     tabsetPanel(
                       
                       #### * - - - - - Aesthetics Customizations ####
                       tabPanel("Aesthetics", br(), 
                                fluidRow(
                                  column(width = 4,
                                         # Bar Position
                                         selectInput("aesPosition", label = "Position", selected = "Dodge 2", 
                                                     choices = c("Dodge 2","Dodge","Stack","Fill"))),
                                  column(width = 4,
                                         # Bar Width
                                         numericInput("aesBarWidth", label = "Bar Width", 
                                                      value = 0.5, min = 0.1, max = 20, step = 0.1)),
                                  column(width = 4,
                                         # Order of Bars
                                         selectInput("aesOrder", label = "Order", selected = "Incr (A->Z)",
                                                     choices = c("Incr (A->Z)", "Decr (Z->A)",
                                                                 "Incr (1->9)", "Decr (9->1)")))
                                ),
                                # Radio buttons to specify if graph colors will be uniform or individual
                                radioButtons("aesOptions", label = NA, inline = TRUE, 
                                             selected = "Uniform Graph aesthetics", 
                                             choices = c("Uniform Graph aesthetics","Individual Graph aesthetics")),
                                # UI rendered depending on whether Uniform or Individual aesthetics is chosen
                                uiOutput("aesUIs")
                                ),
                       
                       #### * - - - - - Labels Customizations ####
                       tabPanel("Labels", br(),
                                fluidRow(
                                  # Options for Plot Labels
                                  column(width = 7,
                                    # Main Title
                                    textInput("mainTitle", "Main Title", "Survey Responses by Category"),
                                    # Option to include Sub-title
                                    checkboxInput("includeSubtitle", "Include Subtitle", value = FALSE),
                                    conditionalPanel(
                                      condition = "input.includeSubtitle",
                                      # Subtitle
                                      textInput("subtitle", "Subtitle", "")
                                    ),
                                    # X Label
                                    textInput("xTitle", "X Axis Title", "Category"),
                                    # Y Label
                                    textInput("yTitle", "Y Axis Title", "Number of Responses")
                                  ),
                                  # Options for Plot Labels
                                  column(width = 5,
                                    # Include data labels check box
                                    checkboxInput("includeDataLabels", "Include Data Labels"),
                                    conditionalPanel(
                                      condition = "input.includeDataLabels",
                                      # Nudge X
                                      numericInput("dataLabelNudgeX", "Nudge X", value = 0, 
                                                   min = -20, max = 20, step = 1),
                                      # Nudge Y
                                      numericInput("dataLabelNudgeY", "Nudge Y", value = 3, 
                                                   min = -20, max = 20, step = 1)
                                    )
                                  )
                                )
                       ),
                       
                       #### * - - - - - Plot Customizations ####
                       tabPanel("Plot", br(),
                                h4("To Save Plot:"),
                                h5(em("Right click (Ctrl + Click for Macs) on plot and select 'Save Image'")),
                                br(),
                                  # -- NOT WORKING -- 
                                  # # Save plot button
                                  # downloadButton("downloadPlot", label = "Download Plot "),
                                  # h5(strong("Save As")),
                                  # # Save As
                                  # fluidRow(
                                  #   column(width = 7,
                                  #     # Plot Title
                                  #     textInput("plotName", label = NA, value = "plot")),
                                  #   column(width = 5,
                                  #     # Extension
                                  #     selectInput("extension", label = NA, selected = ".png",
                                  #                 choices = c(".png",".jpeg",".pdf",".eps",".ps",".tex",
                                  #                             ".tiff",".bmp",".svg",".wmf")))
                                  # ),
                                  # fluidRow(
                                  #   column(width = 4,
                                  #     # Plot Width
                                  #     numericInput("plotWidth", label = "Width", value = 7, width = "100%", 
                                  #                  min = 1, max = 50, step = 1)),
                                  #   column(width = 4, 
                                  #     # Plot Height
                                  #     numericInput("plotHeight", label = "Height", value = 5, width = "100%", 
                                  #                  min = 1, max = 50, step = 1)),
                                  #   column(width = 4,
                                  #     # Width Units
                                  #     selectInput("units", label = "Units", selected = "in", width = "100%",
                                  #                 choices = c("in","cm","mm")))
                                fluidRow(
                                  column(width = 6,
                                         # Select a theme for the plot
                                         selectInput("selectTheme", label = "Select a Theme", selected = "gray", 
                                                     choices = c("base","bw","calc","classic","clean","dark",
                                                                 "economist","economist_white","excel","excel_new",
                                                                 "few","fivethirtyeight","foundation","gdocs","gray",
                                                                 "grey","hc","igray","light","linedraw","map",
                                                                 "minimal","pander","par","solarized","solarized_2",
                                                                 "solid","stata","test","tufte","void","wsj"))),
                                  column(width = 6,
                                         # Edit Grid Lines
                                         checkboxInput("editGridLines", "Edit Grid Lines", value = FALSE),
                                         conditionalPanel(
                                           condition = "input.editGridLines",
                                           # Edit All or Individual Grid Lines
                                           radioButtons("gridLinesToEdit", label = "Grid Lines to Edit",
                                                        selected = "All", choices = c("All","Individual"),
                                                        inline = TRUE),
                                           # Edit All Grid Lines
                                           conditionalPanel(
                                             condition = "input.gridLinesToEdit == 'All'",
                                             # Blank?
                                             checkboxInput("glAllBlank", "Blank", value = TRUE),
                                             conditionalPanel(
                                               condition = "!input.glAllBlank",
                                               # Color
                                               colourpicker::colourInput("glAllColor", label = "Color", value = "black", 
                                                           allowTransparent = TRUE, returnName = TRUE),
                                               # Line Width
                                               numericInput("glAllLineWidth", label = "Line Width", 
                                                            value = 0.5, min = 0, max = 10, step = 0.01),
                                               # Line Type
                                               selectInput("glAllLineType", label = "Line Type",
                                                           selected = "solid", 
                                                           choices = c("blank","solid","dashed","dotted",
                                                                       "dotdash","longdash","twodash")),
                                               # Line End
                                               selectInput("glAllLineEnd", label = "Line End",
                                                           selected = "square",
                                                           choices = c("square","round","butt"))
                                             )
                                           ),
                                           # Edit Individual Grid Lines
                                           conditionalPanel(
                                             condition = "input.gridLinesToEdit == 'Individual'",
                                             br(),
                                             # Edit All or Individual Major Grid Lines
                                             radioButtons("xGridLinesToEdit",label = "X-Axis Grid Lines to Edit",
                                                          selected = "Individual", choices = c("All","Individual"),
                                                          inline = TRUE),
                                             # Edit All X Grid Lines
                                             conditionalPanel(
                                               condition = "input.xGridLinesToEdit == 'All'",
                                               # Blank?
                                               checkboxInput("glXAllBlank", "Blank", value = TRUE),
                                               conditionalPanel(
                                                 condition = "!input.glXAllBlank",
                                                 # Color
                                                 colourpicker::colourInput("glXAllColor", label = "Color", value = "black", 
                                                             allowTransparent = TRUE, returnName = TRUE),
                                                 # Line Width
                                                 numericInput("glXAllLineWidth", label = "Line Width", 
                                                              value = 0.5, min = 0, max = 10, step = 0.01),
                                                 # Line Type
                                                 selectInput("glXAllLineType", label = "Line Type",
                                                             selected = "solid", 
                                                             choices = c("blank","solid","dashed","dotted",
                                                                         "dotdash","longdash","twodash")),
                                                 # Line End
                                                 selectInput("glXAllLineEnd", label = "Line End",
                                                             selected = "square",
                                                             choices = c("square","round","butt"))
                                               ),
                                               hr()
                                             ),
                                             # Edit Individual Major Grid Lines
                                             conditionalPanel(
                                               condition = "input.xGridLinesToEdit == 'Individual'",
                                               # Edit Major X
                                               checkboxInput("glEditMajorX", "Edit Major X", value = FALSE),
                                               conditionalPanel(
                                                 condition = "input.glEditMajorX",
                                                 # Blank?
                                                 checkboxInput("glMajorXBlank", "Blank", value = TRUE),
                                                 conditionalPanel(
                                                   condition = "!input.glMajorXBlank",
                                                   # Color
                                                   colourpicker::colourInput("glMajorXColor", label = "Color", value = "black", 
                                                               allowTransparent = TRUE, returnName = TRUE),
                                                   # Line Width
                                                   numericInput("glMajorXLineWidth", label = "Line Width", 
                                                                value = 0.5, min = 0, max = 10, step = 0.01),
                                                   # Line Type
                                                   selectInput("glMajorXLineType", label = "Line Type",
                                                               selected = "solid", 
                                                               choices = c("blank","solid","dashed","dotted",
                                                                           "dotdash","longdash","twodash")),
                                                   # Line End
                                                   selectInput("glMajorXLineEnd", label = "Line End",
                                                               selected = "square",
                                                               choices = c("square","round","butt"))
                                                 ),
                                                 hr()
                                               ),
                                               # Edit Minor X
                                               checkboxInput("glEditMinorX", "Edit Minor X", value = FALSE),
                                               conditionalPanel(
                                                 condition = "input.glEditMinorX",
                                                 # Blank?
                                                 checkboxInput("glMinorXBlank", "Blank", value = TRUE),
                                                 conditionalPanel(
                                                   condition = "!input.glMinorXBlank",
                                                   # Color
                                                   colourpicker::colourInput("glMinorXColor", label = "Color", value = "black", 
                                                               allowTransparent = TRUE, returnName = TRUE),
                                                   # Line Width
                                                   numericInput("glMinorXLineWidth", label = "Line Width", 
                                                                value = 0.5, min = 0, max = 10, step = 0.01),
                                                   # Line Type
                                                   selectInput("glMinorXLineType", label = "Line Type",
                                                               selected = "solid", 
                                                               choices = c("blank","solid","dashed","dotted",
                                                                           "dotdash","longdash","twodash")),
                                                   # Line End
                                                   selectInput("glMinorXLineEnd", label = "Line End",
                                                               selected = "square",
                                                               choices = c("square","round","butt"))
                                                 )
                                               ),
                                               hr()
                                             ),
                                             # Edit All or Individual Minor Grid Lines
                                             radioButtons("yGridLinesToEdit",label = "Y-Axis Grid Lines to Edit",
                                                          selected = "Individual", choices = c("All","Individual"),
                                                          inline = TRUE),
                                             # Edit All Major Grid Lines
                                             conditionalPanel(
                                               condition = "input.yGridLinesToEdit == 'All'",
                                               # Blank?
                                               checkboxInput("glYAllBlank", "Blank", value = TRUE),
                                               conditionalPanel(
                                                 condition = "!input.glYAllBlank",
                                                 # Color
                                                 colourpicker::colourInput("glYAllColor", label = "Color", value = "black", 
                                                             allowTransparent = TRUE, returnName = TRUE),
                                                 # Line Width
                                                 numericInput("glYAllLineWidth", label = "Line Width", 
                                                              value = 0.5, min = 0, max = 10, step = 0.01),
                                                 # Line Type
                                                 selectInput("glYAllLineType", label = "Line Type",
                                                             selected = "solid", 
                                                             choices = c("blank","solid","dashed","dotted",
                                                                         "dotdash","longdash","twodash")),
                                                 # Line End
                                                 selectInput("glYAllLineEnd", label = "Line End",
                                                             selected = "square",
                                                             choices = c("square","round","butt"))
                                               )
                                             ),
                                             # Edit Individual Minor Grid Lines
                                             conditionalPanel(
                                               condition = "input.yGridLinesToEdit == 'Individual'",
                                               # Edit Major Y
                                               checkboxInput("glEditMajorY", "Edit Major Y", value = FALSE),
                                               conditionalPanel(
                                                 condition = "input.glEditMajorY",
                                                 # Blank?
                                                 checkboxInput("glMajorYBlank", "Blank", value = TRUE),
                                                 conditionalPanel(
                                                   condition = "!input.glMajorYBlank",
                                                   # Color
                                                   colourpicker::colourInput("glMajorYColor", label = "Color", value = "black", 
                                                               allowTransparent = TRUE, returnName = TRUE),
                                                   # Line Width
                                                   numericInput("glMajorYLineWidth", label = "Line Width", 
                                                                value = 0.5, min = 0, max = 10, step = 0.01),
                                                   # Line Type
                                                   selectInput("glMajorYLineType", label = "Line Type",
                                                               selected = "solid", 
                                                               choices = c("blank","solid","dashed","dotted",
                                                                           "dotdash","longdash","twodash")),
                                                   # Line End
                                                   selectInput("glMajorYLineEnd", label = "Line End",
                                                               selected = "square",
                                                               choices = c("square","round","butt"))
                                                 ),
                                                 hr()
                                               ),
                                               # Edit Minor Y
                                               checkboxInput("glEditMinorY", "Edit Minor Y", value = FALSE),
                                               conditionalPanel(
                                                 condition = "input.glEditMinorY",
                                                 # Blank?
                                                 checkboxInput("glMinorYBlank", "Blank", value = TRUE),
                                                 conditionalPanel(
                                                   condition = "!input.glMinorYBlank",
                                                   # Color
                                                   colourpicker::colourInput("glMinorYColor", label = "Color", value = "black", 
                                                               allowTransparent = TRUE, returnName = TRUE),
                                                   # Line Width
                                                   numericInput("glMinorYLineWidth", label = "Line Width", 
                                                                value = 0.5, min = 0, max = 10, step = 0.01),
                                                   # Line Type
                                                   selectInput("glMinorYLineType", label = "Line Type",
                                                               selected = "solid", 
                                                               choices = c("blank","solid","dashed","dotted",
                                                                           "dotdash","longdash","twodash")),
                                                   # Line End
                                                   selectInput("glMinorYLineEnd", label = "Line End",
                                                               selected = "square",
                                                               choices = c("square","round","butt"))
                                                 )
                                               )
                                             )
                                           ),
                                           hr()
                                         ),
                                         # Edit Plot Background
                                         checkboxInput("editPlotBackground", "Edit Plot Background", value = FALSE),
                                         conditionalPanel(
                                           condition = "input.editPlotBackground",
                                           # Blank?
                                           checkboxInput("plotBackgroundBlank", "Blank", value = TRUE),
                                           conditionalPanel(
                                             condition = "!input.plotBackgroundBlank",
                                             # Fill
                                             colourpicker::colourInput("plotBackgroundFill", label = "Fill", 
                                                         allowTransparent = TRUE, returnName = TRUE),
                                             # Color
                                             colourpicker::colourInput("plotBackgroundColor", label = "Color", value = "black", 
                                                         allowTransparent = TRUE, returnName = TRUE),
                                             # Line Width
                                             numericInput("plotBackgroundLineWidth", label = "Line Width", 
                                                          value = 0.5, min = 0, max = 10, step = 0.01),
                                             # Line Type
                                             selectInput("plotBackgroundLineType", label = "Line Type",
                                                         selected = "solid", 
                                                         choices = c("blank","solid","dashed","dotted",
                                                                     "dotdash","longdash","twodash"))
                                           ),
                                           hr()
                                         ),
                                         # Edit Panel Background
                                         checkboxInput("editPanelBackground", "Edit Panel Background",value = FALSE),
                                         conditionalPanel(
                                           condition = "input.editPanelBackground",
                                           # Blank?
                                           checkboxInput("panelBackgroundBlank", "Blank", value = TRUE),
                                           conditionalPanel(
                                             condition = "!input.panelBackgroundBlank",
                                             # Fill
                                             colourpicker::colourInput("PanelBackgroundFill", label = "Fill", 
                                                         allowTransparent = TRUE, returnName = TRUE),
                                             # Color
                                             colourpicker::colourInput("panelBackgroundColor", label = "Color", value = "black", 
                                                         allowTransparent = TRUE, returnName = TRUE),
                                             # Line Width
                                             numericInput("panelBackgroundLineWidth", label = "Line Width", 
                                                          value = 0.5, min = 0, max = 10, step = 0.01),
                                             # Line Type
                                             selectInput("panelBackgroundLineType", label = "Line Type",
                                                         selected = "solid", 
                                                         choices = c("blank","solid","dashed","dotted",
                                                                     "dotdash","longdash","twodash"))
                                           ),
                                           hr()
                                         ),
                                         # Edit Panel Border
                                         checkboxInput("editPanelBorder", "Edit Panel Border", value = FALSE),
                                         conditionalPanel(
                                           condition = "input.editPanelBorder",
                                           # Blank?
                                           checkboxInput("panelBorderBlank", "Blank", value = TRUE),
                                           conditionalPanel(
                                             condition = "!input.panelBorderBlank",
                                             # Color
                                             colourpicker::colourInput("panelBorderColor", label = "Color", value = "black", 
                                                         allowTransparent = TRUE, returnName = TRUE),
                                             # Line Width
                                             numericInput("panelBorderLineWidth", label = "Line Width", 
                                                          value = 0.5, min = 0, max = 10, step = 0.01),
                                             # Line Type
                                             selectInput("panelBorderLineType", label = "Line Type",
                                                         selected = "solid", 
                                                         choices = c("blank","solid","dashed","dotted",
                                                                     "dotdash","longdash","twodash"))
                                           )
                                         ))
                                  )
                       )
                     )
                   )
                 )
        ),
        
        #### * * * Table Tab ####
        tabPanel("Table", br(), 
                 # Table showing which responses are in which category
                 dataTableOutput("table"),
                 # Add Vertical Scroll Bar
                 # Reference: https://stackoverflow.com/questions/47505893/adding-a-vertical-and-horizontal-scroll-bar-to-the-dt-table-in-r-shiny
                 style = "height:500px; overflow-y: scroll; overflow-x: scroll;")
      )
    ),
    br(),
    br(),
    br(),
    br()
  )
)




##### ----- SERVER ----- #####

server <- function(input, output, session) {
  
  
  ## A reactive values object whose variables can be created, altered, and removed by any function in server
  rv <- reactiveValues(num_topics = 0,
                       tf_check_topics = 0,
                       file_is_valid = FALSE,
                       topics_to_add = data.frame(
                         t = integer(),
                         title = character(),
                         terms = character(),
                         ctg_id = character(),
                         rule_id = character(),
                         buttons_pushed = logical()
                       ))

  
  
  
  #### REACTIVE FUNCTIONS ####
  
  
  
  
  #### getData() ####
  ## Reads in file
  getData <- reactive({
  
    inFile <- input$responses
    # Display nice error message if file hasn't been uploaded or is an unaccepted type
    validate(
      need(!is.null(input$responses), "No File Loaded"),
      need(inFile$type %in% c("text/csv", "text/comma-separated-values,text/plain", ".xlsx", 
                              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"), 
           "Please upload an Excel or CSV file with 1 column containing survey responses"),
      need(try(!is.null(readxl::read_excel(inFile$datapath, col_names = "Responses"))), 
           "Please upload an Excel or CSV file with 1 column containing survey responses")
    )
    # Read in and return the file
    data <- readxl::read_excel(inFile$datapath, col_names = "Responses")
    data <- data.frame(data, stringsAsFactors = FALSE)
    return(data)
  })

  
  #### sortData_GG() ####
  ## Sorts the data
  sortData_GG <- reactive({
    data <- getData()
    
    ## Skip if data is NULL or no Categories were added
    if (is.null(data) | !input$ctgAdd) {
      return(NULL)
    }
    
    ## Variables
    N <- input$ctgAdd # Number of categories
    num_responses <- nrow(data) # Number of responses
    default_ctg_name <- paste0(input$defaultName)
    multiple_categories_per_response <- input$multCtgsPerResponse # If false, will assign one ctg per response

    
    ## Add a new Default Category column to the variables data
    default_column = data.frame(matrix(nrow = num_responses, ncol = 1)) %>% 
      `colnames<-`(default_ctg_name)
    default_column[,1] <- 1 # Responses are by default in the default category
    data <- bind_cols(data, default_column)
    
    
    ## Reset all response match variables in reactive values to FALSE
    for (r in 1:num_responses) {
      r_match_id <- paste0("response_", r, "_matched")
      rv[[r_match_id]] <- FALSE
    }
    
    
    ## Function that returns true if keyword and match_value match according to sort_option
    check_match <- function(keyword = NA, match_value = NA, standardize = FALSE, sort_option = "Contains") {
      ## Variable to be returned
      found_match <- FALSE
      ## Make response and keyword lower-case if standardize is true
      if (!is.na(match_value) & keyword != "") { 
        if (standardize) {
          match_value <- str_to_lower(match_value)
          keyword <- str_to_lower(keyword)
        } 
        
        ## Change matching pattern (if needed) according to sort option
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
            found_match <- TRUE
          }
        } else {
          ## Check for any other match
          if (str_detect(match_value, pattern)) {
            # They match!
            found_match <- TRUE
          } 
        }
      }
      # Function returns TRUE if there was a match
      return(found_match)
    }
    
    
    ## Loop through each category to create needed data frames
    for (c in 1:N) {
      print(paste0("======== Category ", c, " ========"))
      ## Proceed if category hasn't been removed (id in reactive values is TRUE)
      current_ctg_id <- paste0("ctg_",c)
      if (!is.na(rv[[current_ctg_id]])) {
        ## Category variables
        currentCtgName <- eval(parse(text = paste0("input$ctg_", c, "_name")))
        num_rules <- eval(parse(text = paste0("input$ctg_", c, "_add_rule")))

        ## Proceed if rules have been added
        if (num_rules) {
          ## Create a new column to be added to responses data to hold whether the response lies within the category
          new_column = data.frame(matrix(nrow = num_responses, ncol = 1)) %>% 
            `colnames<-`(paste0(currentCtgName))
          new_column[,1] <- 0 # Values set to zero and not FALSE for functionality
  
          ## Create an empty data frame for this category containing all rule inputs
          rule_inputs <- data.frame(matrix(nrow = 0, ncol = 4)) %>%
            `colnames<-`(c("Keywords","Sort_Option","Standardize","Search_By"))
  
          ## Loop through each rule in the current category and make data set for all rule inputs
          for (u in 1:num_rules) {
            ## Proceed if rule hasn't been removed (id in reactive values is TRUE)
            current_rule_id <- paste0("rule_", u, "ctg_", c)
            if (!is.na(rv[[current_rule_id]])) {
              ## Rule variables
              rule_keywords_id <- paste0(current_rule_id, "_keywords")
              rule_sort_options_id <- paste0(current_rule_id, "_sort_options")
              rule_standardize_id <- paste0(current_rule_id, "_standardize")
              rule_search_by_id <- paste0(current_rule_id, "_search_by")
              current_keywords <- eval(parse(text = paste0("input$", rule_keywords_id)))
              current_sort_option <- eval(parse(text = paste0("input$", rule_sort_options_id)))
              standardize_all <- eval(parse(text = paste0("input$", rule_standardize_id)))
              current_search_by <- eval(parse(text = paste0("input$", rule_search_by_id)))
    
              ## Proceed if there are any keyowrds in this rule
              if (current_keywords != "") {
                # Create a new data frame to be added to the category's keywords data
                new_rule_inputs <- stri_extract_all_words(current_keywords) %>% # Make keywords a list of words
                  data.frame(stringsAsFactors = FALSE) %>%  # Turn list into a data frame
                  `colnames<-`("Keywords") %>%              # Change the name of the first column
                  mutate(Sort_Option = current_sort_option, # New column for sorting option
                         Standardize = standardize_all,     # New column indicating whether to standardize
                         Search_By = current_search_by)     
                # Add the new rule inputs to the existing rule inputs
                rule_inputs <- bind_rows(rule_inputs, new_rule_inputs)
              }
            }
          }

          ## Proceed if the rule inputs have been added
          num_rule_inputs <- nrow(rule_inputs)
          if (num_rule_inputs > 0) {
            ## Loop through all keywords and +1 if any match the category's keywords (according to sort option)
            for (k in 1:num_rule_inputs) {
              ## Key Input variables
              current_keyword <- rule_inputs$Keywords[k]
              current_sort_option <- rule_inputs$Sort_Option[k]
              standardize <- rule_inputs$Standardize[k]
              current_search_by <- rule_inputs$Search_By[k]
              print(paste0("----- ", current_keyword, " -----"))

              ## Loop through all responses
              for (r in 1:num_responses) {
                ## Matching variables
                current_response <- data$Responses[r]
                r_match_id <- paste0("response_", r, "_matched")
                r_has_match <- rv[[r_match_id]]
                split_response <- FALSE
                match_found <- FALSE
                
                ## Will skip response if only one ctg per response is specified AND response has been assigned a ctg 
                if (multiple_categories_per_response | !r_has_match) {
                  ## Split Response into individual words if specified by Search_By
                  if (current_search_by == "Word") {
                    split_response <- stri_extract_all_words(current_response) %>% 
                      data.frame(stringsAsFactors = FALSE) %>% 
                      `colnames<-`("Response_Words")
  
                    ## Loop through words in split response and check for matches
                    for (w in 1:nrow(split_response)) {
                      current_response_word <- split_response$Response_Words[w]
                      # Pass the check match function the current word in response
                      match_found <- check_match(match_value = current_response_word, keyword = current_keyword, 
                                                  standardize = standardize, sort_option = current_sort_option)
                    }
                  } else {
                    ## Pass the check match function the entire currernt response
                    match_found <- check_match(match_value = current_response, keyword = current_keyword, 
                                                standardize = standardize, sort_option = current_sort_option)
                  }
                  ## Update new_column and responses data if a match was found
                  if (match_found) {
                    print("Match!")
                    # Specify in reactive variables that response has a match
                    rv[[r_match_id]] <- TRUE
                    # +1 to current category column
                    new_column[r,1] <- new_column[r,1] + 1 
                    # Remove response from the Default Category 
                    data[r,2] <- 0
                  } else {
                    # Specify in reactive variables that response is not matched
                    rv[[r_match_id]] <- FALSE
                  }
                } else {
                  print("No more than 1 category per response. Response has ctg. Skip response.")
                }
              }
            }
          }
          ## Add the new column to the responses data after all of the for loops
          data <- bind_cols(data, new_column)
        }
      }
    }
    
    return(data)
    
  })
  
  
  
  
  #### plotGG() ####
  ## Will plot the ggplot to show how many responses are in which category
  plotGG <- reactive({
    ## Get the responses data sorted into categories
    data <- sortData_GG()  
    
    ## Skip if data is null or no categories have been made
    if (is.null(data) | !input$ctgAdd) {
      return(NULL)
    }
    
    ## Variables
    num_columns <- ncol(data)
    aesthetics <- NA
    barWidth <- input$aesBarWidth
    # Position variable 
    ifelse(input$aesPosition == "Dodge", position <- "position_dodge()", 
           ifelse(input$aesPosition == "Stack", position <- "position_stack()", 
                  ifelse(input$aesPosition == "Fill", position <- "position_fill()", 
                         position <- "position_dodge2()")))
    # Order variable
    ifelse(input$aesOrder == "Incr (1->9)", order <- "reorder(Category, n)",
           ifelse(input$aesOrder == "Decr (1->9)", order <- "reorder(Category, desc(n))",
                  ifelse(input$aesOrder == "Decr (Z->A)", order <- "reorder(Category, desc(Category))",
                         order <- "Category"))) # Increasing (A->Z)
    
    
    ## Tidy the data to plot 
    plot_data <- data %>% 
      # Make one Category column
      gather("Category","n",2:num_columns) %>% 
      group_by(Category) %>% 
      summarise(n = sum(n)) %>% 
      as.data.frame()
    

    ## A function that fills all columns of any one row in aesthetics with passed arguments
    ## (Uses the default category and NA's as the default variables)
    fill_aes_row <- function(ctgInputID = "default_aes_indiv", ctg_name = input$defaultName) {
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
      if (!is.null(input_fill) & !is.null(input_no_fill)) {
        if (input_no_fill == FALSE) {
          fill <- input_fill
        }
      }
      if (!is.null(input_color) & !is.null(input_no_color)) {
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
      # Create a blank row to be returned
      new_aes <- data.frame(matrix(nrow = 1, ncol = 5)) %>% 
        `colnames<-`(c("category","fill","color","lineWidth","lineType"))
      # Replace specified row in new_aes with previously defined variables, if the category exists
      new_aes[1,1] <- ctg_name
      new_aes[1,2] <- fill
      new_aes[1,3] <- color
      new_aes[1,4] <- lineWidth
      new_aes[1,5] <- lineType

      return(new_aes)
    }
      
    
    ## Fill the aestetic values based on plot_data
    # Aestetic values (by Category) for the graph
    aesthetics <- as.data.frame(matrix(ncol = 5, # Category, Fill, Color, Line Width, Line Type, Bar Width
                                      nrow = num_columns - 1)) %>% # Number of Categories (including Default)
      `colnames<-`(c("category","fill","color","lineWidth","lineType"))
    # Loop through each category added and fill the aesthetics data frame
    for (c in 0:input$ctgAdd) {
      # Category variables (Default category by default)
      ctg_id <- "default"
      ctg_name <- input$defaultName
      if (c != 0) {
        ctg_id <- paste0("ctg_",c)
        ctg_name <- eval(parse(text = paste0("input$", ctg_id, "_name")))
      }
      # Loop through each row of plot_data for assigning aesthetics and category validation
      for (r in 1:nrow(plot_data)) { 
        ctg_name_plot_data <- plot_data$Category[r]
        # Proceed to assign aesthetics if current ctg name matches the current Category in plot_data
        # (if it's listed in plot_data, the category exists)
        if (str_detect(ctg_name_plot_data, ctg_name)) {
          # Create aesthetic id for retrieving aesthetics
          aes_id <- "aes_uniform" # Uniform by default
          if (input$aesOptions == "Individual Graph aesthetics") {
            aes_id <- paste0(ctg_id, "_aes_indiv")
          } 
          # Fill aesthetics row
          aesthetics[r,] <- fill_aes_row(ctgInputID = aes_id, ctg_name = ctg_name)
        }
      }
    }


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
    # Aestetic variables
    scale_fill_values <- NA
    scale_color_values <- NA
    scale_lineWidth_values <- NA
    scale_lineType_values <- "blank"
    # Add graph aesthetics
    if (!is.na(aesthetics[1,1])) {
      # Turn the vectors of chars in aesthetics into a single string separated by a comma
      scale_fill_values <- aesthetics$fill
      scale_color_values <- aesthetics$color
      scale_lineWidth_values <- aesthetics$lineWidth
      scale_lineType_values <- aesthetics$lineType
      
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
          new_all_grid_lines <- element_line(color = input$glAllColor, size = input$glAllLineWidth, 
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
              new_all_x <- element_line(color = input$glXAllColor, size = input$glXAllLineWidth, 
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
                  new_major_x <- element_line(color = input$glMajorXColor, size = input$glMajorXLineWidth, 
                                              linetype = input$glMajorXLineType, lineend = input$glMajorXLineEnd)
                }
                # Add new grid line theme element to the plot
                plot <- plot + theme(panel.grid.major.x = new_major_x)
              }
              # Minor X
              if (input$glEditMinorX) {
                # Edit if blank is not selected
                if (!input$glMinorXBlank) {
                  new_minor_x <- element_line(color = input$glMinorXColor, size = input$glMinorXLineWidth, 
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
              new_all_y <- element_line(color = input$glYAllColor, size = input$glYAllLineWidth, 
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
                  new_major_y <- element_line(color = input$glMajorYColor, size = input$glMajorYLineWidth, 
                                              linetype = input$glMajorYLineType, lineend = input$glMajorYLineEnd)
                }
                # Add new grid line theme element to the plot
                plot <- plot + theme(panel.grid.major.y = new_major_y)
              }
              # Minor Y
              if (input$glEditMinorY) {
                # Edit if blank is not selected
                if (!input$glMinorYBlank) {
                  new_minor_y <- element_line(color = input$glMinorYColor, size = input$glMinorYLineWidth, 
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
        new_plot_background <- element_rect(fill = input$plotBackgroundFill, color = input$plotBackgroundColor,
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
        new_panel_background <- element_rect(fill = input$panelBackgroundFill, color = input$panelBackgroundColor,
                                             size = input$panelBackgroundLineWidth, 
                                             linetype = input$panelBackgroundLineType)}
      # Add new theme element to the plot
      plot <- plot + theme(panel.background = new_panel_background)
    }
    ## Panel Border
    if (input$editPanelBorder) {
      # Either element_blank or element_rect
      new_panel_border <- element_blank()
      if (!input$panelBorderBlank) {
        new_panel_border <- element_rect(fill = NA, color = input$panelBorderColor,
                                         size = input$panelBorderLineWidth, 
                                         linetype = input$panelBorderLineType)}
      # Add new theme element to the plot
      plot <- plot + theme(panel.border = new_panel_border)
    }

    return(plot)

  })
  
  
  #### plotTable() ####
  ## Will plot a table of the data
  plotTable <- reactive({
    ## Get the responses data sorted into categories
    data <- sortData_GG() 
    
    
    ## Skip if data is null or no categories have been made
    if (is.null(data) | !input$ctgAdd) {
      return(NULL)
    }  
    
    ## Re-structure the data to be plotted as a table
    table_data <- data %>% 
      as.data.frame()
    

    ## Plot the table of the data
    return(table_data)

  })
  
  
  
  
  #### find_topics() ####
  ## Uses LDA models to automatically find topics in the responses 
  find_topics <- reactive({
    
    # Start the progress bar
    # Reference: https://shiny.rstudio.com/articles/progress.html
    withProgress(message = "Finding Topics", value = 0, {
      
      ### Initialize Progress Bar
      num_steps <- 7
      incr_val <- 1/num_steps # How far to increment the progress bar at each step
      incProgress(incr_val, detail = "Reading in Data") # Increments the progress bar
      
      
      ### Read in the Data
      
      df <- read_excel("E:/mneff/Desktop/College Stuff/Spring 2019/SRC/R Reports/HCQuestions.xlsx")
      
      # Increment Progress Bar
      incProgress(amount = incr_val, detail = "Pre-processing the data")
      
      
      ### Pre-Process the Data
      ### Reference: https://stackoverflow.com/questions/55832844/remove-words-from-a-dtm
      
      # Retrieve the words not to be included in the models
      words_to_remove <- input$tf_words_not_include %>% 
        str_split(pattern = ",") %>% 
        unlist() %>% 
        str_trim(side = "both")
      
      # Convert column to Corpus (collection of "documents")
      corpus <- corpus(df, text_field = "Responses")
      
      # Create a Document Term (Feature) Matrix from the corpus
      #   (Each response becomes its own document split into words.) 
      dtm <- dfm(corpus, stem = TRUE, 
                 remove = stopwords("english"),
                 remove_punct = TRUE) %>% 
        dfm_trim(min_termfreq = 3) %>% # Remove rare terms
        dfm_remove(words_to_remove) %>% # Remove words specified by user
        convert(to = "tm") # Convert back to 'tm' library object for fitting
      
      # Remove very frequent and in-frequent terms
      #   (Will calculate the mean term frequency-inverse document frequency (tg-idf)
      #    of each term across documents and keep terms with a tf-idf of at least 0.1,
      #    which is close to the median.
      #    Then the documents will be filtered by the tf-idf to ensure steady frequency)
      # Reference: p.12 of https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf    
      term_tfidf <-
        tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
        log2(nDocs(dtm)/col_sums(dtm > 0))
      dtm <- dtm[,term_tfidf >= 0.1]
      dtm <- dtm[row_sums(dtm) > 0,]
      
      # Increment Progress Bar
      incProgress(amount = incr_val, detail = "Fitting LDA Model")
      
      
      ### Fit the models
      ### Reference: p.13 of https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf
      
      # Initial parameters
      k <- input$tf_num_topics
      SEED <- as.integer(Sys.time())
      
      # Fit several models for comparison
      #   Both models use a VEM procedure for calculating the Maximum Likelihood
      #   (This is a Variable version of the EM algorithm. It iterates between
      #    an (E)xpectation step and a (M)aximization step to calculate parameters
      #    that the EM algorithm cannot.) 
      #   See section 2.2 of https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf 
      models <- list()
      
      # Latent Dirichlet Allocation (LDA) Model (see p.1)
      #   Topics are assumed to be uncorrelated
      #   Can use the VEM or Gibbs algorithm
      models[["LDA"]] <- LDA(dtm, k = k, method = "VEM",
                             control = list(seed = SEED))
      # Increment Progress Bar
      incProgress(amount = incr_val, detail = "Fitting CTM Model")
      
      # Correlated Topic Model (CTM) (see p.2)
      #   Topics are assumed to be correlated
      #   Only uses the VEM algorithm
      #   Set a larger tolerance for the relative change in the likelihood
      models[["CTM"]] <- CTM(dtm, k = k,
                             control = list(seed = SEED,
                                            var = list(tol = 10^-4), 
                                            em  = list(tol = 10^-3)))
      # Increment Progress Bar
      incProgress(amount = incr_val, detail = "Selecting 'Best' Model")
      
      
      ### Select the "best" model
      ### Reference: p.14 of https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf
      
      # Calculate the mean entropy for each model.
      #   This measures how much the model's topics are distributed across the documents.
      #   The lower the entropy, the more precise and useful the predicted topics are.
      entropies <- sapply(models, function(x)
        mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
      
      # Save the "best" model (the one with the lowest mean entropy)
      # Reference: https://stackoverflow.com/questions/21422188/how-to-get-name-from-a-value-in-an-r-vector-with-names
      best_model <- models[[names(entropies)[entropies == min(entropies)]]]
      
      # Increment Progress Bar
      incProgress(amount = incr_val, detail = "Retrieving Final Results")
      
      
      ### Get Final Results
      
      # Retrieve the best model's top terms for each topic
      top_terms <- as.data.frame( terms(best_model, 25) ) 
      
      # Retrieve and clean the best model's most likely topic for each response
      response_topics <- as.data.frame( topics(best_model) ) %>% 
        rownames_to_column("Response") %>% 
        rename(Topic = 'topics(best_model)')
      response_topics$Response <- as.integer(str_remove(response_topics$Response, "text"))
      
      # Add original response text to the response topics
      response_topics$Text <- unlist( sapply(response_topics$Response, function(r) df[r,1]) )
      
      # Increment Progress Bar
      incProgress(amount = incr_val, message = "Done", detail = "")
      
      # Return the results in a vector
      return( c(top_terms, response_topics) )    })
  })
  
  
  
  
  #### updatePushed_plotGG() ####
  ## Calls and returns return value of plotGG when "Update" button is pushed
  updatePushed_plotGG <- eventReactive(input$update, {
    # Saves graph of the data to reactive values
    return(plotGG())
  })
  
  
  #### updatePushed_plotTable() ####
  ## Calls and returns return value of plotTable when "Update" button is pushed
  updatePushed_plotTable <- eventReactive(input$update, {
    # Plots the table of the data
    return(plotTable())
  })
             
  
  
  
  #### Observe Event -- Add Category ####
  ## Adds the Category Name text input and Add Rule button when the Add Category button is pushed
  observeEvent(input$ctgAdd, {
    
    ## Make dynamic id's for all category UI's and the category itself
    id_add <- paste0(input$ctgAdd)
    ctg_id <- paste0("ctg_", id_add)
    ctg_name_id <- paste0(ctg_id, "_name")
    ctg_add_rule_button_id <- paste0(ctg_id, "_add_rule")
    remove_ctg_id <- paste0("remove_ctg", id_add)
    remove_ctg_button_id <- paste0("remove_ctg_button_", id_add)
    
    ## Insert Topic's Title if new Category is a topic to be converted
    ctg_name <- paste0("Category ", id_add)
    
    if (nrow(rv$topics_to_add) > 0) {
      ctg_name <- rv$topics_to_add$title[rv$topics_to_add$ctg_id == ctg_id]
    }
    
    ## Create rv variable for tracking values
    ctg_num_rules_id <- paste0(ctg_id, "_num_rules")
    rv[[ctg_num_rules_id]] <- 0

    
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
                         ctg_name, 
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
    ## Set category id's in reactive values to TRUE when category is created
    rv[[ctg_id]] <- TRUE
    
    ## Save values to rv for inserting new Topics
    rv$topics_to_add$rule_id[rv$topics_to_add$ctg_id == ctg_id] <- ctg_add_rule_button_id
    
    
    #### * Insert Remove Category Button ####
    insertUI(
      selector = "#removeCtgButton",
      ui = tags$div(
        id = remove_ctg_id,
        # Check box to signify which category to remove
        actionButton(remove_ctg_button_id, paste0("Remove Category ", id_add))
      )
    )
    ## Set category remove button id in reactive values to TRUE
    rv[[remove_ctg_id]] <- TRUE
    
    
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
      # Set all category-related id's to NA in reactive values
      rv[[ctg_id]] <- NA
      rv[[remove_ctg_id]] <- NA

    }, ignoreInit = TRUE, once = TRUE)
    
    
    #### * Observe Event -- Add Rule ####
    ## Add all rule UI's when category Add Rule button is pushed
    observeEvent(input[[ctg_add_rule_button_id]], {
      ## Make id's for rule UI's and the rule itself
      id_add_rule <- input[[ctg_add_rule_button_id]]
      rule_id <- paste0("rule_", id_add_rule, ctg_id)
      rule_keywords_id <- paste0(rule_id, "_keywords")
      rule_sort_options_id <- paste0(rule_id, "_sort_options")
      rule_standardize_id <- paste0(rule_id, "_standardize")
      rule_search_by_id <- paste0(rule_id, "_search_by")
      remove_rule_id <- paste0("remove_rule_", id_add_rule, ctg_id)
      remove_rule_button_id <- paste0("remove_rule_button_", id_add_rule, ctg_id)
      
      ## Insert Topic's Title if new Category is a topic to be converted
      rule_keywords <- ""
      
      if (nrow(rv$topics_to_add) > 0) {
        rule_keywords <- rv$topics_to_add$terms[rv$topics_to_add$ctg_id == ctg_id]
      }
      
         
      #### * * Insert Rule UI's ####
      insertUI(
        selector = paste0("#", ctg_id, "_rules"),
        ui = tags$div(
          # Has this id for removal
          id = rule_id,
          # Rule Title
          h4(paste0("Rule ", id_add_rule)),
          # Key words text input
          textInput(rule_keywords_id,
                    value = rule_keywords,
                    label = paste0("Keywords for Rule ", id_add_rule)),
          # Sorting Options list
          selectInput(rule_sort_options_id,
                      label = "Sorting Options",
                      choices = c("Exactly","Contains","Begins With","Ends With"),
                      selected = "Contains",
                      width = 120),
          # Standardize check box that will, when checked, set all keywords and responses to lowercase
          checkboxInput(rule_standardize_id, "Standardize All to Lowercase", FALSE),
          # Radio buttons indicating whether to search through responses by "Response" or by "Word"
          radioButtons(rule_search_by_id, label = "Search By", 
                       selected = "Response", choices = c("Response","Word")),
          hr()
        )
      )

      ## Set rule id in reactive values to TRUE when rule is created
      rv[[rule_id]] <- TRUE
      
      ## Increase number of rules in reactive values
      rv[[ctg_num_rules_id]] <- rv[[ctg_num_rules_id]] + 1


      #### * * Insert Remove Rule Button ####
      insertUI(
        selector = paste0("#", ctg_id, "_remove_rule_buttons"),
        ui = tags$div(
          # For removing the button
          id = remove_rule_id,
          # The Remove Rule button
          actionButton(remove_rule_button_id, paste0("- Rule ", id_add_rule))
        )
      )
      ## Set rule remove button id in reactive values to TRUE
      rv[[remove_rule_id]] <- TRUE
      
      
      #### * * Observe Event -- Remove Rule ####
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
        # Set rule and remove rule button id's to NULL in reactive values
        rv[[rule_id]] <- NA
        rv[[remove_rule_id]] <- NA
        # Decrease number of rules in reactive values
        rv[[ctg_num_rules_id]] <- rv[[ctg_num_rules_id]] - 1

      }, ignoreInit = TRUE, once = TRUE)
      
    })

  })
  
  
  #### Observe Event -- Find Topics (Go!) ####
  ## Calls find_topics() when the Topic Finder "Go!" button is pushed.
  ## Returns TRUE if topics were found
  observeEvent(input$tf_go, {
    
    # Call function to find topics
    results <- find_topics()
    n <- input$tf_num_topics # The number of topics to find
    
    # Make sure topics were found and proper results are saved
    if (is.null(results)) {
      # Save NULL to reactive values and return FALSE
      rv[["topics"]] <- NULL
      rv[["response_topics"]] <- NULL
      rv[["num_topics"]] <- 0
      
    } else {
      #### * Remove All Existing Topic UIs #### 
      ## Need to do this before updating the number of topics found
      lapply(1:rv$num_topics, function (t)
        removeUI(paste0("#topic_tag_",t))
      )
      
      # Save results to reactive values and return TRUE
      rv[["topics"]] <- results[1:n] # The first n list items are the topics
      rv[["response_topics"]] <- results[n+1:length(results)] # The rest are response topics
      rv[["num_topics"]] <- n
      
      # Convert response topics from list to data frame for Results Tabs
      rv$response_topics <- data.frame(
        Response = rv$response_topics$Response,
        Topic = rv$response_topics$Topic,
        Text = rv$response_topics$Text
      ) 
      
      #### * Insert New Topic UIs ####
      lapply(1:n, function(t) {
        
        # Initial Variables
        topic <- rv$topics[t]
        topic_id <- paste0("topic_", t)
        topic_tag_id <- paste0("topic_tag_", t)
        topic_name <- names(topic)
        topic_terms <- str_c(unlist(topic[1])[1:10], collapse = " | ")
        topic_checkbox_id <- paste0("topic_checkbox_", t)
        topic_title_id <- paste0("topic_title_", t)
        topic_terms_id <- paste0("topic_terms_", t)
        topic_num_terms_id <- paste0("topic_num_terms_", t)
        
        # Insert the New UIs ana Placeholders
        insertUI("#tf_topics", "beforeBegin",
                 tags$div(
                   id = topic_tag_id,
                   wellPanel(
                     fluidRow(
                       column(width = 1,
                              # Convert to Category Checkbox
                              checkboxInput(topic_checkbox_id, label = "")
                       ),
                       column(width = 3,
                              # Topic Title Textbox
                              textInput(topic_title_id, label = "", 
                                        value = topic_name)
                       ),
                       column(width = 6,
                              # Terms Text Output
                              # Need this here so user can change the number of terms to show
                              # without having to run the model again
                              textOutput(topic_terms_id)
                       ),
                       column(width = 2,
                              # Number of Terms to Show
                              numericInput(topic_num_terms_id, "Terms",
                                           value = 10, min = 1, max = 25)
                       )
                     )
                   )
                 )
        )
        
        # Render the Topic's Terms (default is first 10)
        output[[topic_terms_id]] <- renderText({
          rv[[topic_terms_id]] <- topic_terms # Save as a reactive value
          return(topic_terms)
        })
        
        #### * * Observe Event -- Update Number of Terms Showing ####
        ## Updates the number of terms to show whenever the topic's input changes
        observeEvent(input[[topic_num_terms_id]], {
          
          num_terms <- input[[topic_num_terms_id]]
          
          # Do nothing if input is not a number
          if (!is.numeric(num_terms)) {  } else {
            
            # Proceed if input is greater than zero
            if (num_terms > 0) {
              
              # Retrieve and format the new number of terms
              new_topic_terms <- str_c(unlist(topic[1])[1:input[[topic_num_terms_id]]], 
                                       collapse = " | ")
              
              # Render the Topic's New Terms
              output[[topic_terms_id]] <- renderText({
                rv[[topic_terms_id]] <- new_topic_terms # Save as a reactive value
                return(new_topic_terms)
              })          
            }
          }
        })
      })
    }
  })
  
  
  #### Observe Event -- Convert Topics ####
  ## Adds a new Category "manually" for each selected topic
  observeEvent(input$tf_convert_topics, {
    
    # Initial variables
    num_topics_checked <- 0
    n <- rv$num_topics # Number of topics
    current_ctg_id <- input$ctgAdd + 1
    prg_incr <- 1 / n # How much to increment the Progress bar
    
    # Start the Progress Bar
    withProgress(message = "Converting Topics to Categories", value = 0, {
      
      # Loop through each topic
      for (t in 1:n) {
        
        # Proceed only if the topic is currently selected (checked)
        if (input[[paste0("topic_checkbox_", t)]] == TRUE) {
          
          # Create New Category 
          # (by pushing existing action buttons and updating the created UI)
          if (rv$file_is_valid == TRUE) {
            
            # Retrieve topic info (from id's)
            topic_title_id <- paste0("topic_title_", t)
            topic_terms_id <- paste0("topic_terms_", t)
            title <- input[[topic_title_id]]
            terms <- rv[[topic_terms_id]]
            new_ctg_id <- paste0("ctg_", current_ctg_id)
            
            # Click the 'Add Category' button
            local({ 
              click("ctgAdd", asis = TRUE) 
            })
            
            # Save variables to data frame
            new_topic_to_add <- data.frame(
              t = t,
              title = title,
              terms = terms,
              ctg_id = new_ctg_id,
              rule_id = "",
              buttons_pushed = FALSE
            )
            
            # Add new topic variables to data frame reactive value
            rv$topics_to_add <- bind_rows(rv$topics_to_add, new_topic_to_add)
            
            # Increment counter
            current_ctg_id <- current_ctg_id + 1
            
          } else {
            # No file imported
          }
          # Increment counter
          num_topics_checked <- num_topics_checked + 1
          
        } else { } # Nothing happens
        
        # Increment Progress Bar
        incProgress(amount = prg_incr)
      }
      
      # Show pop-up message to select topics before pushing button
      if (num_topics_checked == 0) { 
        shinyalert(title = "Oops!",
                   text = "Please select (check) at least one topic to convert.",
                   type = "error") 
      }
    })
  })
  
  
  #### Observe Event -- New Topic to Convert Added ####
  ## Will finish converting a newly added topic to a category
  observeEvent(rv$topics_to_add, ignoreInit = TRUE, {
    
    # Loop through each topic to add and finish conversion, if applicable
    for (t in 1:nrow(rv$topics_to_add)) {

      # Check if "+ Rule" button has been created and hasn't been pushed yet
      if (str_length(rv$topics_to_add$rule_id[t]) > 0 & 
          rv$topics_to_add$buttons_pushed[t] == FALSE) {
        
        # Push the "+ Rule" button
        local({
          click(rv$topics_to_add$rule_id[t], asis = TRUE)
        })
        
        # Make sure the "+ Rule" button isn't pushed twice
        rv$topics_to_add$buttons_pushed[t] <- TRUE
        
      } else { } # Nothing Happens
    }
  })
  
  
  #### Toggle Tooltips ####
  ## Add tooltips only if checkbox is checked
  observeEvent(input$show_tooltips, {
    if (input$show_tooltips) {
      
      ### Add Tooltips
      
      # Add Category Tooltip
      addTooltip(session, id = "ctgAdd", trigger = "hover",
                 title = "
                 A Category is a general theme that survey responses can belong to. 
                 Each Category has a name and Rules to control how/when responses would 
                 fall under it.                  
                 ")
      
      # ## Find Categories For Me Tooltip
      # addTooltip(session, id = "find_categories", trigger = "hover",
      #            title = "A great place to start if you don’t know what categories to add.
      #        Opens the Topic Finder window.")
      
      ## Find Categories For Me Popover
      addPopover(session, id = "find_categories", trigger = "hover",
                 title = "Information",
                 content = paste0(
                 "A great place to start if you don’t know what categories to add.
                 \n
                 Opens the Topic Finder window."))
      
      ## Number of Topics to Find Tooltip
      addTooltip(session, id = "tf_num_topics", trigger = "focus", 
                 title = "
                 For the machine learning models to work, you must first specify
                 how many abstract topics there might be across all responses. The minimum 
                 is 2, and the maximum is 10.
                 ")
      
      ## Words Not to Include Tooltip
      addPopover(session, id = "tf_words_not_include", trigger = "focus", 
                 title = "Information",
                 content = "
                 These words will be taken out of all responses before running the 
                 machine learning models. Each word/phrase MUST be separated by a comma!
                 ")
    
    } else {
      
      ### Remove Tooltips  ###
      
      removeTooltip(session, id = "find_categories")
      removePopover(session, id = "find_categories")
      removeTooltip(session, id = "ctgAdd")
      removeTooltip(session, id = "tf_num_topics")
      removeTooltip(session, id = "tf_words_not_include")
    }
  })
  

  
  #### OUTPUT FUNTIONS ####
  
  
  
  #### Event Reactive -- fileValid ####
  ## Returns true if uploaded file is an accepted type
  output$fileValid <- eventReactive(input$responses, {
    is_valid <- !is.null(getData())
    
    rv$file_is_valid <- is_valid # Save as a reactive value
    return(is_valid)
  })
  outputOptions(output, 'fileValid', suspendWhenHidden = FALSE)

  
  #### Event Reactive -- categoriesAdded ####
  ## Returns true if categories have been added
  output$categoriesAdded <- eventReactive(input$update, {
    ifelse(input$ctgAdd > 0, return(TRUE), return(FALSE))    
  })
  outputOptions(output, 'categoriesAdded', suspendWhenHidden = FALSE)
  
  
  #### Event Reactive -- plotDisplayed ####
  ## Returns true if plot has been created
  output$plotDisplayed <- eventReactive(input$update, {
    return(!is.null(plotGG()))
  })
  outputOptions(output, 'plotDisplayed', suspendWhenHidden = FALSE)
  
  
  #### Event Reactive -- tableDisplayed ####
  ## Returns true if table has been created
  output$tableDisplayed <- eventReactive(input$update, {
    return(!is.null(plotTable()))
  })
  outputOptions(output, 'tableDisplayed', suspendWhenHidden = FALSE)
  
  
  #### Event Reactive -- topicsFound ####
  ## Makes sure the results are shown when topics are found
  output$topicsFound <- eventReactive(rv$num_topics, {
    # Returns true if any topics were found (as updated by Go! observe event)
    return(rv$num_topics > 0) 
  })
  outputOptions(output, 'topicsFound', suspendWhenHidden = FALSE)


  
  #### Render Plot ####
  ## Displays plot of responses
  output$plot <- renderPlot({
    # Save created plot to reactive values
    rv$plot <- updatePushed_plotGG()
    # Render plot saved in reactive values
    if (!is.null(rv$plot)) {
      print(rv$plot)
    } else {
      print("Cannot render plot; data is NULL")
    }
  })
  
  
  #### Render Table ####
  ## Displays table of responses
  output$table <- renderDataTable({
    # Save created table to reactive values
    rv$table <- updatePushed_plotTable()
    # Plot if return value is not NA
    if (!is.null(rv$table)) {
      return(rv$table)
    } else {
      print("Cannot render table; data is NULL")
    }
  }, options = list(pageLength = 10))
  
  
  #### Render Aesthetic Customization UI's ####
  ## Renders UI's based on whether Uniform or Individual aesthetics is selected in the 'aesthetics' customization tab
  output$aesUIs <- renderUI({
    ## Individual aesthetics
    if (input$aesOptions == "Individual Graph aesthetics") {
      ## A list of UI's for the default category
      default_uis_list <- lapply(1:1, function(i) {
        # Default Category id
        default_id <- paste0("default_aes_indiv")
        # List of Category Default UI's for individual aes customization
        list(
          hr(),
          h4(paste0("Default Category: ", input$defaultName)),
          br(),
          flowLayout(
            # Fill
            colourpicker::colourInput(paste0(default_id, "_fill"), label = "Fill", value = "gray", 
                        allowTransparent = TRUE, returnName = TRUE),
            # No Fill?
            checkboxInput(paste0(default_id, "_no_fill"), label = "No Fill", value = FALSE),
            # Color
            colourpicker::colourInput(paste0(default_id, "_color"), label = "Color", value = "black", 
                        allowTransparent = TRUE, returnName = TRUE),
            # No Color?
            checkboxInput(paste0(default_id, "_no_color"), label = "No Color", value = TRUE),
            conditionalPanel(
              condition = paste0("!input.",default_id, "_no_color"),
              fluidRow(
                column(width = 5,
                       # Line Width
                       numericInput(paste0(default_id, "_line_width"), label = "Line Width", 
                                    value = 0.5, min = 0, max = 10, step = 0.01)),
                column(width = 7,
                       # Linetype
                       selectInput(paste0(default_id, "_line_type"), label = "Line Type", selected = "solid", 
                                   choices = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")))
              )
            )
          )
        )
      })
      ## A list of new UI's for each valid category (including default)
      uis_list <- lapply(1:input$ctgAdd, function(i) {
        # Category variables
        current_ctg_id <- paste0("ctg_", i)
        current_ctg_num_rules_id <- paste0(current_ctg_id, "_num_rules")
        current_ctg_name <- eval(parse(text = paste0("input$", current_ctg_id, "_name")))
        # Proceed if the current category exists (is not NA in the reactive values) AND has at least one rule
        if (!is.na(rv[[current_ctg_id]]) & rv[[current_ctg_num_rules_id]] > 0) {
          # Unique id for all ui's in category
          aes_id <- paste0(current_ctg_id, "_aes_indiv")
          list(
            # Category Header
            hr(),
            h4(paste0("Category ", i, ": ", current_ctg_name)),
            br(),
            flowLayout(
              # Fill
              colourpicker::colourInput(paste0(aes_id, "_fill"), label = "Fill", value = "gray", 
                          allowTransparent = TRUE, returnName = TRUE),
              # No Fill?
              checkboxInput(paste0(aes_id, "_no_fill"), label = "No Fill", value = FALSE),
              # Color
              colourpicker::colourInput(paste0(aes_id, "_color"), label = "Color", value = "black",
                          allowTransparent = TRUE, returnName = TRUE),
              # No Color?
              checkboxInput(paste0(aes_id, "_no_color"), label = "No Color", value = TRUE),
              conditionalPanel(
                condition = paste0("!input.",aes_id, "_no_color"),
                fluidRow(
                  column(width = 5,
                         # Line Width
                         numericInput(paste0(aes_id, "_line_width"), label = "Line Width", 
                                      value = 0.5, min = 0, max = 10, step = 0.01)),
                  column(width = 7,
                         # Linetype
                         selectInput(paste0(aes_id, "_line_type"), label = "Line Type", selected = "solid", 
                                     choices = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")))
                )
              )
            )
          )
        }
      })
      ## Unlist the UI lists
      default_uis <- unlist(default_uis_list, recursive = FALSE)
      ctgs_uis <- unlist(uis_list, recursive = FALSE)
      ## Proceed if other category list was not NULL
      if (!is.null(ctgs_uis)) {
        # Combine all category UI's to default category, if not NULL
        default_uis <- append(default_uis, ctgs_uis)
      }
      ## Render the UI's
      do.call(tagList, default_uis)
        
    } else {
      ## Uniform aesthetics
      if (input$aesOptions == "Uniform Graph aesthetics") {
        # Unique id for all ui's
        aes_id <- paste0("aes_uniform")
        # Make a list
        tagList(
          hr(),
          flowLayout(
            # Fill
            colourpicker::colourInput(paste0(aes_id, "_fill"), label = "Fill", value = "gray", 
                        allowTransparent = TRUE, returnName = TRUE),
            # No Fill?
            checkboxInput(paste0(aes_id, "_no_fill"), label = "No Fill", value = FALSE),
            # Color
            colourpicker::colourInput(paste0(aes_id, "_color"), label = "Color", value = "black",
                        allowTransparent = TRUE, returnName = TRUE),
            # No Color?
            checkboxInput(paste0(aes_id, "_no_color"), label = "No Color", value = TRUE),
            conditionalPanel(
              condition = paste0("!input.",aes_id, "_no_color"),
              fluidRow(
                column(width = 5,
                       # Line Width
                       numericInput(paste0(aes_id, "_line_width"), label = "Line Width", 
                                    value = 0.5, min = 0, max = 10, step = 0.01)),
                column(width = 7,
                       # Linetype
                       selectInput(paste0(aes_id, "_line_type"), label = "Line Type", selected = "solid", 
                                   choices = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")))
              )
            )
          )
        )
      }
    }
  })
  
  
  #### Render Topic Details Plot ####
  output$tf_details_plot <- renderPlot({
    # Plot the number of responses within each found topic
    rv$response_topics %>%
      group_by(Topic) %>% 
      summarise(Count = n()) %>% # Data Label Column
      ggplot() +
      geom_col(aes(x = Topic, y = Count), fill = "dodgerblue4", color = "dodgerblue4") +
      geom_text(aes(x = Topic, y = Count, label = Count), 
                nudge_y = 15, color = "dodgerblue4") +
      scale_x_continuous(breaks = seq(1, rv$num_topics)) +
      labs(title = "Number of Responses per Topic", y = "Responses") +
      theme_hc()
  })
  
  
  #### Render Topic Details Table ####
  output$tf_details_tbl <- renderDataTable({
    # Render Data Table
    return(rv$response_topics)
  }, options = list(pageLength = 10))
  
  
  
  
  ## -- NOT WORKING --
  ## Downloads the plot to user's computer 
  # output$downloadPlot <- downloadHandler(
  #   filename = function() {
  #     # Name of plot and extension combined
  #     paste0(input$plotName, input$extension)
  #   },
  #   content = function(file) {
  #     # Saves plot to user's computer
  #     ggsave(file, plot = rv$plot, width = input$plotWidth, height = input$plotHeight, units = input$units)
  #   })
  
  
}
shinyApp(ui = ui, server = server)










