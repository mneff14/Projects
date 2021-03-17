##### ----- UI ----- #####




# Need to encase UI in a function to enable bookmarking the state
# Reference: https://shiny.rstudio.com/articles/bookmarking-state.html
ui <- function(request) {
  
  fluidPage(
    
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
    
    # Main Title
    h1(em("Survey Response Categorizer")),
    
    
    #### Sidebar Panel ####
    sidebarPanel(
      
      #### * Browse File Button ####
      wellPanel(
        h4(id = "import_title", "Import Responses"),
        fileInput("responses", "", 
                  buttonLabel = "Browse",
                  placeholder = "Excel / CSV",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".xlsx", 
                             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
        )
      ),
      
      
      #### * Help Section ####
      
      wellPanel(
        h4("Help"),
        br(),
        flowLayout(
          #### * * Main Info Action Link 
          actionLink("info_btn_main", "About", icon = icon("question-circle")),
          #### * * Watch Tutorials Action Link 
          actionLink("watch_tutorials", "Tutorials", icon = icon("graduation-cap")),
          #### * * Show Tooltips Checkbox 
          checkboxInput("show_tooltips", "Show Tooltips", value = TRUE)
        ),
      ),
      
      
      #### * Browse File Conditional Panel ####
      # These allow other categories to be added / removed when file is uploaded
      conditionalPanel(
        condition = "output.fileValid",
        
        #### * * Find Categories for Me Button ####
        actionButton("find_categories", "Find Categories for Me",
                     style = "color: #fff; background-color: #337ab7; border-color: #2d6a9f"),
        
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
                        label = "Multiple Categories per Response", value = FALSE),
          
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
    
    flowLayout(
      #### Update Button ####
      actionButton("update", "Update"),
      #### Save (Bookmark) Button ####
      bookmarkButton(label = "Save", icon = icon("bookmark"))
    ),
    
    
    br(),
    
    #### Profvis UI ####
    # profvis_ui("profiler"),
    
    #### Show Reactive Values ####
    # wellPanel(
    #   h3("Reactive Values"),
    #   verbatimTextOutput("show_rvs", placeholder = TRUE),
    #   style = "height:500px; overflow-y: scroll; overflow-x: scroll;"
    # ),
    
    
    #### Main Panel ####
    mainPanel(
      
      #### * Tutorial Pop-ups ####
      
      ## All Tutorial Videos ##
      ## (Contains Action Links to all tutorial video modals (pop-ups) with embedded videos
      bsModal("tutorials_all", "Tutorials",
              trigger = "watch_tutorials", size = "s",
              # Opening
              h5('Learn how to use the Survey Response Categorizer App by watching these tutorials!'),
              hr(),
              # Links to All Tutorial Videos
              flowLayout(
                wellPanel(
                  actionLink("tutorial_link_demo", "Demo", icon = icon("chalkboard-teacher") 
                             , onclick = "window.open('https://web.microsoftstream.com/video/fae9cf0e-0d4b-45d8-9b0f-278dd999403c')"
                  ),
                )
                # , wellPanel(
                #   actionLink("tutorial_link_", "Tutorial", icon = icon("video"))
                # )
              )
      ),
      
      
      #### * Info Button Pop-ups ####
      
      ## Help Section Info Button
      bsModal("info_popup_main", "About",
              trigger = "info_btn_main", size = "s",
              # Info Text
              tags$h5('This R Shiny App enables you to easily organize open-ended 
                    responses from a survey question into categories and visualize 
                    them for your reports.'),
              tags$h5('To watch a demo or other tutorials, click the "Tutorials" link.'),
              tags$h5('For quick information, tooltips will appear over buttons and input fields
                    while the "Show Tooltips" checkbox is checked.'),
              tags$h5('Happy Categorizing!')
      ),
      
      
      #### * Topic Finder Pop-up Window ####
      ## Reference: https://ebailey78.github.io/shinyBS/docs/Modals.html
      bsModal("topic_finder", "Topic Finder", 
              trigger = "find_categories", size = "l", # Large
              
              # Main Info
              # Reference for embedding link: https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny
              tagList(
                'The Topic Finder identifies a specific number of abstract topics across all responses using a process called Topic Modeling (see ',
                a("here", href = "https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf"),
                ' for full documentation).'
              ),
              br(),
              br(),
              
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
                
                # Adjust width of Tabs
                tags$style(HTML(".tabbable > .nav > li > a {width: 250PX;}")),
                
                # Results Tabset Panel
                tabsetPanel(id = "tabset_tf_results",
                            # Topics Tab
                            tabPanel(title = strong(id = "topics_tab", "Topics"),
                                     # Placeholder for Topic UIs
                                     tags$div(id = "tf_topics")
                            ),
                            tabPanel(title = strong(id = "details_tab", "Details"),
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
        condition = "output.showResults",
        
        #### * * Plot and Table Tabset ####
        tabsetPanel(id = "tabset_main_results",
                    
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
                                 tabsetPanel(id = "tabset_plot_customizations",
                                             
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
                                                               selectInput("aesOrder", label = "Order", selected = "Decr (9->1)",
                                                                           choices = c("Incr (A->Z)", "Decr (Z->A)",
                                                                                       "Incr (1->9)", "Decr (9->1)")))
                                                      ), br(),
                                                      # Radio buttons to specify if graph colors will be uniform or individual
                                                      radioButtons("aesOptions", label = "", inline = TRUE, 
                                                                   selected = "Uniform Bar Colors", 
                                                                   choices = c("Uniform Bar Colors","Individual Bar Colors")),
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
                                                               checkboxInput("editPanelBackground", "Edit Panel",value = FALSE),
                                                               conditionalPanel(
                                                                 condition = "input.editPanelBackground",
                                                                 # Blank?
                                                                 checkboxInput("panelBackgroundBlank", "Blank", value = TRUE),
                                                                 conditionalPanel(
                                                                   condition = "!input.panelBackgroundBlank",
                                                                   # Fill
                                                                   colourpicker::colourInput("panelBackgroundFill", label = "Fill", 
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
                                                               )
                                                        )
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
}




